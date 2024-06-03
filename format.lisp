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

(deftype lms () '(simple-array (signed-byte 16) (8)))

(declaim (inline make-lms copy-lms copy-lms*))
(defun make-lms ()
  (make-array 8 :element-type '(signed-byte 16)))

(defun copy-lms (lms)
  (declare (optimize speed (safety 0)))
  (declare (type lms lms))
  (copy-seq lms))

(defun copy-lms* (source)
  (declare (optimize speed (safety 0)))
  (declare (type simple-vector source))
  (let ((copy (make-array (length source))))
    (loop for i from 0 below (length copy)
          do (setf (aref copy i) (copy-lms (aref source i))))
    copy))

(bs:define-io-structure frame
  (channels uint8)
  (samplerate uint24-be)
  (samples/channel uint16-be)
  (size uint16-be)
  (state (vector (vector sint16-be 8) (bs:slot channels)))
  (slices (vector uint64-be (* (truncate (+ (bs:slot samples/channel) SLICE-LENGTH -1) SLICE-LENGTH) (bs:slot channels)))))

(defmethod print-object ((frame frame) stream)
  (print-unreadable-object (frame stream :type T :identity T)
    (format stream "~d samples, ~d channel~:p @ ~d Hz"
            (frame-samples/channel frame) (frame-channels frame) (frame-samplerate frame))))

(defmethod octet-size ((frame frame))
  (bs:octet-size frame))

(defmethod samplerate ((frame frame))
  (frame-samplerate frame))

(defmethod channels ((frame frame))
  (frame-channels frame))

(bs:define-io-structure file
  "qoaf"
  (samples uint32-be)
  (frames (vector frame (ceiling (bs:slot samples) (* 256 20)))))

(defmethod frames ((file file))
  (file-frames file))

(defmethod samplerate ((file file))
  (frame-samplerate (aref (file-frames file) 0)))

(defmethod channels ((file file))
  (frame-channels (aref (file-frames file) 0)))

(defmethod octet-size ((file file))
  (bs:octet-size file))

(defmethod print-object ((file file) stream)
  (print-unreadable-object (file stream :type T :identity T)
    (if (< 0 (length (file-frames file)))
        (format stream "~d sample~:p, ~d frame~:p, ~d channel~:p @ ~d Hz"
                (file-samples file) (length (file-frames file)) (channels file) (samplerate file))
        (format stream "INVALID"))))

(defun lms-predict (lms)
  (declare (type lms lms))
  (declare (optimize speed (safety 0)))
  (ash (loop for i from 0 below LMS-LENGTH
             sum (* (aref lms i) (aref lms (+ 4 i))) of-type (signed-byte 64))
       -13))

(defun lms-update (lms sample residual)
  (declare (type lms lms))
  (declare (type (signed-byte 32) sample residual))
  (declare (optimize speed (safety 0)))
  (let ((delta (ash residual -4)))
    (dotimes (i LMS-LENGTH)
      (incf (aref lms (+ 4 i)) (if (< (aref lms i) 0) (- delta) (+ delta))))
    (dotimes (i (1- LMS-LENGTH))
      (setf (aref lms i) (aref lms (1+ i))))
    (setf (aref lms (1- LMS-LENGTH)) sample)))

(defun div (v scale-factor)
  (declare (type (signed-byte 32) v))
  (declare (type (unsigned-byte 16) scale-factor))
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
  (declare (optimize speed))
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
         (slices (truncate (+ frame-len SLICE-LENGTH -1) SLICE-LENGTH))
         (frame-size (compute-frame-size channels slices))
         (prev-scale-factor (make-array MAX-CHANNELS :element-type '(signed-byte 32) :initial-element 0))
         (state-lms (state-lms state))
         (frame-state (map-into (make-array channels) #'copy-lms state-lms))
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
                            for lms = (copy-lms (aref state-lms c))
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
                                                     sum (expt (aref lms (+ 4 i)) 2))
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
                      (setf (aref state-lms c) best-lms)
                      (setf best-slice (ash best-slice (* (- SLICE-LENGTH slice-len) 3)))
                      (setf (aref frame-slices p) best-slice)
                      (incf p)))
    (make-frame :channels (state-channels state)
                :samplerate (state-samplerate state)
                :samples/channel frame-len
                :size frame-size
                :state frame-state
                :slices frame-slices)))

(defun encode-from-buffer (samples &key (channels 1) (samplerate 4800))
  (check-type samples (simple-array (signed-byte 16) (*)))
  (assert (<= 1 channels MAX-CHANNELS))
  (assert (<= 1 samplerate #xffffff))
  (let* ((state (make-state :channels channels :samplerate samplerate :samples (length samples)))
         (frames (make-array (truncate (+ (length samples) -1 FRAME-LENGTH) FRAME-LENGTH))))
    (declare (dynamic-extent state))
    (dotimes (c channels)
      (let ((lms (aref (state-lms state) c)))
        (dotimes (i LMS-LENGTH)
          (setf (aref lms i) 0))
        (setf (aref lms (+ 4 0)) 0)
        (setf (aref lms (+ 4 1)) 0)
        (setf (aref lms (+ 4 2)) (- (ash 1 13)))
        (setf (aref lms (+ 4 3)) (+ (ash 1 14)))))
    (loop with frame-len = FRAME-LENGTH
          with sample-index = 0
          for frame from 0
          while (< sample-index (length samples))
          do (setf frame-len (clamp FRAME-LENGTH 0 (- (length samples) sample-index)))
             (setf (aref frames frame) (encode-frame samples sample-index state frame-len))
             (incf sample-index (* channels frame-len)))
    (make-file :samples (length samples) :frames frames)))

(defun encode-file (samples output &rest args &key (channels 1) (samplerate 48000) &allow-other-keys)
  (remf args :channels)
  (remf args :samplerate)
  (let ((file (encode-from-buffer samples :channels channels :samplerate samplerate)))
    (etypecase output
      ((member vector NIL)
       (let ((vec (make-array (octet-size file) :element-type '(unsigned-byte 8))))
         (write-file file vec)
         vec))
      ((or string pathname simple-array stream cffi:foreign-pointer)
       (apply #'write-file file output args)))))

(defun decode-frame (frame samples start end)
  (declare (type frame frame))
  (declare (type (simple-array (signed-byte 16) (*)) samples))
  (declare (type (unsigned-byte 32) start end))
  (declare (optimize speed (safety 1)))
  (let* ((size (- end start))
         (channels (frame-channels frame))
         (slices (frame-slices frame))
         (sample-count (frame-samples/channel frame))
         (lms (copy-lms* (frame-state frame))))
    (declare (type (simple-array (unsigned-byte 64) (*)) slices))
    (declare (type simple-vector lms))
    (declare (dynamic-extent lms))
    (when (< size (+ 8 (* LMS-LENGTH 4 (frame-channels frame))))
      (return-from decode-frame start))
    (loop with p = 0
          for sample-index from 0 below sample-count by SLICE-LENGTH
          do (loop for c of-type (unsigned-byte 8) from 0 below channels
                   for slice of-type (unsigned-byte 64) = (aref slices p)
                   for scale-factor of-type (integer 0 16) = (ldb (byte 4 60) slice)
                   do (loop for si from sample-index below (clamp (+ sample-index SLICE-LENGTH) 0 sample-count)
                            for predicted = (lms-predict (aref lms c))
                            for quantized = (logand (ash slice -57) #x7)
                            for dequantized = (aref DEQUANTIZATION-TABLE scale-factor quantized)
                            for reconstructed = (clamp-16 (+ predicted dequantized))
                            do (setf (aref samples start) reconstructed)
                               (incf start)
                               (setf slice (ldb (byte 64 0) (ash slice 3)))
                               (lms-update (aref lms c) reconstructed dequantized))
                      (incf p))
          finally (return start))))

(defun decode-to-buffer (file samples &key (frame-start 0) (start 0) (end (length samples)))
  (declare (optimize speed))
  (check-type samples (simple-array (signed-byte 16) (*)))
  (assert (<= 0 start (length samples)))
  (assert (<= 0 end (length samples)))
  (assert (<= 0 frame-start (1- (length (file-frames file)))))
  (locally (declare (type (unsigned-byte 32) start end frame-start))
    (loop for frame = (aref (the simple-vector (file-frames file)) frame-start)
          do (setf start (decode-frame frame samples start end))
             (incf frame-start)
          while (< start end)
          finally (return (values start frame-start)))))

(defun decode-file (file &rest args)
  (etypecase file
    (file
     (let ((buffer (make-array (file-samples file) :element-type '(signed-byte 16))))
       (decode-to-buffer file buffer)
       (values buffer (channels file) (samplerate file))))
    (T
     (decode-file (apply #'read-file file args)))))

(defun wav-samples (file)
  (with-open-file (stream file :element-type '(unsigned-byte 8))
    (flet ((read-token ()
             (map-into (make-string 4) (lambda () (code-char (read-byte stream))))))
      (let ((samplerate 44100)
            (channels 1))
        (assert (string= "RIFF" (read-token)))
        (dotimes (i 4) (read-byte stream))
        (assert (string= "WAVE" (read-token)))
        (loop for label = (read-token)
              for length = (nibbles:read-ub32/le stream)
              do (cond ((string= "data" label)
                        (let ((samples (make-array (truncate length 2) :element-type '(signed-byte 16))))
                          (nibbles:read-sb16/le-into-sequence samples stream)
                          (return (values samples channels samplerate))))
                       ((string= "fmt " label)
                        (assert (= 1 (nibbles:read-ub16/le stream)))
                        (setf channels (nibbles:read-ub16/le stream))
                        (setf samplerate (nibbles:read-ub32/le stream))
                        (dotimes (i 6) (read-byte stream))
                        (assert (= 16 (nibbles:read-ub16/le stream))))
                       (T
                        (dotimes (i length) (read-byte stream)))))))))

(defun convert-wav (in &key (out (make-pathname :type "qoa" :defaults in)) (if-exists :error))
  (multiple-value-bind (samples channels samplerate) (wav-samples in)
    (encode-file samples out :channels channels :samplerate samplerate :if-exists if-exists)
    out))

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
