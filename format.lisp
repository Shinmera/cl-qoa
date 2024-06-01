(in-package org.shirakumo.qoa)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant MAX-CHANNELS 8)
  (defconstant SLICE-LENGTH 20)
  (defconstant SLICES/FRAME 256)
  (defconstant FRAME-LENGTH (* SLICE-LENGTH SLICES/FRAME))
  (defconstant LMS-LENGTH 4))

(alexandria:define-constant QUANTIZATION-TABLE
    (make-array 17 :element-type '(unsigned-byte 8) :initial-contents
                '(7 7 7 5 5 3 3 1 0 0 2 2 4 4 6 6 6))
  :test 'equalp)

(alexandria:define-constant SCALE-FACTOR-TABLE
    (make-array 16 :element-type '(unsigned-byte 16) :initial-contents
                '(1 7 21 45 84 138 211 304 421 562 731 928 1157 1419 1715 2048))
  :test 'equalp)

(alexandria:define-constant RECIPROCAL-TABLE
    (make-array 16 :element-type '(unsigned-byte 32) :initial-contents
                '(65536 9363 3121 1457 781 475 311 216 156 117 90 71 57 47 39 32))
  :test 'equalp)

(alexandria:define-constant DEQUANTIZATION-TABLE
    (make-array '(16 8) :element-type '(signed-byte 32) :initial-contents
                '((   1    -1    3    -3    5    -5     7     -7)
	              (   5    -5   18   -18   32   -32    49    -49)
	              (  16   -16   53   -53   95   -95   147   -147)
	              (  34   -34  113  -113  203  -203   315   -315)
	              (  63   -63  210  -210  378  -378   588   -588)
	              ( 104  -104  345  -345  621  -621   966   -966)
	              ( 158  -158  528  -528  950  -950  1477  -1477)
	              ( 228  -228  760  -760 1368 -1368  2128  -2128)
	              ( 316  -316 1053 -1053 1895 -1895  2947  -2947)
	              ( 422  -422 1405 -1405 2529 -2529  3934  -3934)
	              ( 548  -548 1828 -1828 3290 -3290  5117  -5117)
	              ( 696  -696 2320 -2320 4176 -4176  6496  -6496)
	              ( 868  -868 2893 -2893 5207 -5207  8099  -8099)
	              (1064 -1064 3548 -3548 6386 -6386  9933  -9933)
	              (1286 -1286 4288 -4288 7718 -7718 12005 -12005)
	              (1536 -1536 5120 -5120 9216 -9216 14336 -14336)))
  :test 'equalp)

;; TODO: should change this to be a single sint16 array instead to avoid the pointerage
(bs:define-io-structure (lms (:copier NIL) (:predicate NIL))
  (history (vector sint16 #.LMS-LENGTH))
  (weights (vector sint16 #.LMS-LENGTH)))

(defun copy-lms (lms)
  (make-lms :history (copy-seq (lms-history lms))
            :weights (copy-seq (lms-weights lms))))

(defun copy-lms* (source)
  (let ((copy (make-array (length source))))
    (loop for i from 0 below (length copy)
          for lms = (aref source i)
          do (setf (aref copy i) (make-lms :history (copy-seq (lms-history lms))
                                           :weights (copy-seq (lms-weights lms)))))
    copy))

(bs:define-io-structure frame
  (channels uint8)
  (samplerate uint24)
  (samples/channel uint16)
  (size uint16)
  (state (vector lms (bs:slot channels)))
  (slices (vector uint64 (* 256 (bs:slot channels)))))

(defmethod print-object ((frame frame) stream)
  (print-unreadable-object (frame stream :type T :identity T)
    (format stream "~d channels @ ~d Hz" (channels frame) (samplerate frame))))

(bs:define-io-structure file
  "qoaf"
  (samples uint32)
  (frames (vector frame (ceiling (bs:slot samples) (* 256 20)))))

(defun samplerate (file)
  (frame-samplerate (aref (file-frames file) 0)))

(defun channels (file)
  (frame-channels (aref (file-frames file) 0)))

(defmethod print-object ((file file) stream)
  (print-unreadable-object (file stream :type T :identity T)
    (if (< 0 (length (file-frames file)))
        (format stream "~d channels @ ~d Hz" (channels file) (samplerate file))
        (format stream "INVALID"))))

(defun lms-predict (lms)
  (declare (type lms lms))
  (declare (optimize speed))
  (let ((weights (lms-weights lms))
        (history (lms-history lms)))
    (ash (loop for i from 0 below LMS-LENGTH
               sum (* (aref weights i) (aref history i)) of-type (signed-byte 64))
         -13)))

(defun lms-update (lms sample residual)
  (declare (type lms lms))
  (declare (type (signed-byte 32) sample residual))
  (declare (optimize speed))
  (let ((delta (ash residual -4))
        (weights (lms-weights lms))
        (history (lms-history lms)))
    (dotimes (i LMS-LENGTH)
      (incf (aref weights i) (if (< (aref history i) 0) (- delta) (+ delta))))
    (dotimes (i (1- LMS-LENGTH))
      (setf (aref history i) (aref history (1+ i))))
    (setf (aref history (1- LMS-LENGTH)) sample)))

(defun div (v scale-factor)
  (declare (type (signed-byte 32) v scale-factor))
  (declare (optimize speed))
  (let* ((reciprocal (aref RECIPROCAL-TABLE scale-factor))
         (n (ash (+ (* v reciprocal) (ash 1 15)) -16)))
    (+ n
       (cond ((< 0 v) +1) ((< v 0) -1) (T 0))
       (cond ((< 0 n) -1) ((< n 0) +1) (T 0)))))

(declaim (ftype (function ((signed-byte 32) (signed-byte 32) (signed-byte 32)) (signed-byte 32)) clamp))
(declaim (inline clamp))
(defun clamp (v min max)
  (declare (type (signed-byte 32) v min max))
  (declare (optimize speed))
  (cond ((< v min) min)
        ((< max v) max)
        (T v)))

(declaim (ftype (function ((signed-byte 32)) (signed-byte 16)) clamp-16))
(declaim (inline clamp-16))
(defun clamp-16 (v)
  (declare (type (signed-byte 32) v))
  (if (< 65535 (ldb (byte 32 0) (+ v 32768)))
      (cond ((< v -32768) -32768)
            ((< +32767 v) +32767)
            (T v))
      v))

(declaim (inline compute-frame-size))
(defun compute-frame-size (channels slices)
  (+ 8 (* LMS-LENGTH 4 CHANNELS) (* 8 slices channels)))

(defstruct state
  (channels 2 :type (unsigned-byte 8))
  (samplerate 44100 :type (unsigned-byte 32))
  (samples 0 :type (unsigned-byte 32))
  (lms (map-into (make-array MAX-CHANNELS) #'make-lms) :type (simple-array lms (#.MAX-CHANNELS))))

(defmethod print-object ((state state) stream)
  (print-unreadable-object (state stream :type T :identity T)))

(defun encode-frame (samples start state frame-len)
  (declare (type (unsigned-byte 32) frame-len))
  (declare (type (simple-array (signed-byte 16) (*)) samples))
  (declare (type state state))
  (let* ((channels (state-channels state))
         (slices (truncate (+ frame-len -1 SLICE-LENGTH) SLICE-LENGTH))
         (frame-size (compute-frame-size channels slices))
         (prev-scale-factor (make-array 256 :element-type '(signed-byte 32)))
         (frame-state (state-lms state))
         (frame-slices (make-array slices :element-type '(unsigned-byte 64))))
    (declare (dynamic-extent prev-scale-factor))
    (loop with p = 0
          for sample-index from 0 below frame-len by SLICE-LENGTH
          do (loop for c from 0 below channels
                   for slice-len = (clamp SLICE-LENGTH 0 (- frame-len sample-index))
                   for slice-start = (+ c (* channels sample-index))
                   for slice-end = (+ c (* channels (+ sample-index slice-len)))
                   for best-rank = most-positive-fixnum
                   for best-slice = 0
                   for best-lms = NIL
                   for best-scale-factor = 0
                   do (loop for sfi from 0 below 16
                            for scale-factor = (mod (+ sfi (aref prev-scale-factor c)) 16)
                            for lms = (copy-lms (aref (state-lms state) c))
                            for slice = scale-factor
                            for current-rank = 0
                            do (loop for si from slice-start below slice-end by channels
                                     for sample = (aref samples (+ start si))
                                     for predicted = (lms-predict lms)
                                     for residual = (- sample predicted)
                                     for scaled = (div residual scale-factor)
                                     for clamped = (clamp scaled -8 8)
                                     for quantized = (aref QUANTIZATION-TABLE (+ clamped 8))
                                     for dequantized = (aref DEQUANTIZATION-TABLE scale-factor quantized)
                                     for reconstructed = (clamp-16 (+ predicted dequantized))
                                     for sum = (loop for i from 0 below LMS-LENGTH
                                                     sum (expt (aref (lms-weights lms) 0) 2))
                                     for weights-penalty = (max 0 (- (ash sum -18) #x8FF))
                                     for error = (- sample reconstructed)
                                     for error-sq = (* error error)
                                     do (incf current-rank (+ error-sq (* weights-penalty weights-penalty)))
                                        (when (< best-rank current-rank) (return))
                                        (lms-update lms reconstructed dequantized)
                                        (setf slice (logior (ash slice 3) quantized)))
                               (when (< current-rank best-rank)
                                 (setf best-rank current-rank)
                                 (setf best-slice slice)
                                 (setf best-lms lms)
                                 (setf best-scale-factor scale-factor)))
                      (setf (aref prev-scale-factor c) best-scale-factor)
                      (setf (aref frame-state c) best-lms)
                      (setf best-slice (ash best-slice (- (* 3 (- SLICE-LENGTH slice-len)))))
                      (setf (aref frame-slices p) best-slice)
                      (incf p)))
    (make-frame :channels (state-channels state)
                :samplerate (state-samplerate state)
                :samples/channel frame-len
                :size frame-size
                :state frame-state
                :slices frame-slices)))

(defun encode (samples &key (channels 2) (samplerate 44100))
  (check-type samples (simple-array (signed-byte 16) (*)))
  (assert (<= 0 channels MAX-CHANNELS))
  (assert (<= 0 samplerate #xffffff))
  (let* ((state (make-state :channels channels :samplerate samplerate :samples (length samples)))
         (frames (make-array (truncate (+ (length samples) -1 FRAME-LENGTH) FRAME-LENGTH))))
    (declare (dynamic-extent state))
    (dotimes (c channels)
      (let ((lms (aref (state-lms state) c)))
        (setf (aref (lms-weights lms) 0) 0)
        (setf (aref (lms-weights lms) 1) 0)
        (setf (aref (lms-weights lms) 2) (- (ash 1 13)))
        (setf (aref (lms-weights lms) 3) (+ (ash 1 14)))
        (dotimes (i LMS-LENGTH)
          (setf (aref (lms-history lms) i) 0))))
    (loop with frame-len = FRAME-LENGTH
          for frame from 0
          for sample-index from 0 below (length samples) by frame-len
          do (setf frame-len (clamp FRAME-LENGTH 0 (- (length samples) sample-index)))
             (setf (aref frames frame) (encode-frame samples sample-index state frame-len)))
    (make-file :samples (length samples) :frames frames)))

(defun decode-frame (frame samples start end)
  (declare (type (simple-array (signed-byte 16) (*))))
  (let* ((size (- end start))
         (channels (frame-channels frame))
         (slices (frame-slices frame))
         (sample-count (frame-samples/channel frame))
         (lms (copy-lms* (frame-state frame))))
    (declare (dynamic-extent lms))
    (when (< size (+ 8 (* LMS-LENGTH 4 (frame-channels frame))))
      (return-from decode-frame start))
    (loop with p = 0
          for sample-index from 0 below sample-count by SLICE-LENGTH
          do (loop for c from 0 below channels
                   for slice = (aref slices p)
                   for scale-factor = (ldb (byte 4 60) slice)
                   do (loop for si from (+ c (* channels sample-index))
                            below (+ c (* channels (clamp (+ sample-index SLICE-LENGTH) 0 sample-count)))
                            by channels
                            for predicted = (lms-predict (aref lms c))
                            for quantized = (logand #x7 (ash slice -57))
                            for dequantized = (aref DEQUANTIZATION-TABLE scale-factor quantized)
                            for reconstructed = (clamp-16 (+ predicted dequantized))
                            do (setf (aref samples start) reconstructed)
                               (incf start)
                               (setf slice (ash slice 3))
                               (lms-update (aref lms c) reconstructed dequantized))
                      (incf p))
          finally (return start))))

(defun decode-to-buffer (file samples &key (frame-start 0) (start 0) end)
  (check-type samples (simple-array (signed-byte 16) (*)))
  (assert (<= 0 start (1- (length samples))))
  (assert (<= 0 end (1- (length samples))))
  (assert (<= 0 frame-start (1- (length (file-frames file)))))
  (loop for frame = (aref (file-frames file) frame-start)
        do (setf start (decode-frame frame samples start end))
           (incf frame-start)
        while (< end start)
        finally (return (values start frame-start))))

(defun decode (file)
  (let ((buffer (make-array (file-samples file) :element-type '(signed-byte 16))))
    (decode-to-buffer file buffer)
    buffer))

(defun channel-layout (count)
  (ecase count
    (1 '(:center))
    (2 '(:left :right))
    (3 '(:left :right :center))
    (4 '(:left-front :right-front :left-rear :right-rear))
    (5 '(:left-front :right-front :center :left-rear :right-rear))
    (6 '(:left-front :right-front :center :subwoofer :left-rear :right-rear))
    (7 '(:left-front :right-front :center :subwoofer :center-rear :left-side :right-side))
    (8 '(:left-front :right-front :center :subwoofer :left-rear :right-rear :left-side :right-side))))
