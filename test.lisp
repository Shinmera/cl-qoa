(defpackage #:org.shirakumo.qoa.test
  (:use #:cl #:parachute)
  (:local-nicknames
   (#:qoa #:org.shirakumo.qoa)))

(in-package #:org.shirakumo.qoa.test)

(defparameter *qoaconv* #p "~/Projects/c/qoa/qoaconv")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *samples-directory*
    #.(merge-pathnames "samples/" (make-pathname :name NIL :type NIL :defaults (or *compile-file-truename* *load-truename*)))))

(define-test qoa)

(defun generate (count gen)
  (map-into (make-array count :element-type '(signed-byte 16)) gen))

(defun silence (&optional (length 128))
  (generate length (constantly 0)))

(defun wav-samples (file)
  (with-open-file (stream file :element-type '(unsigned-byte 8))
    (dotimes (i 12) (read-byte stream))
    (loop for label = (map-into (make-string 4) (lambda () (code-char (read-byte stream))))
          for length = (nibbles:read-ub32/le stream)
          do (if (string= "data" label)
                 (let ((samples (make-array (truncate length 2) :element-type '(signed-byte 16))))
                   (nibbles:read-sb16/le-into-sequence samples stream)
                   (return samples))
                 (dotimes (i length) (read-byte stream))))))

(defun diff (a b)
  (if (/= (length a) (length b))
      (list :length :expected (length a) :got (length b))
      (loop for i from 0 below (length a)
            do (when (/= (aref a i) (aref b i))
                 (return (list :mismatch :at i :expected (aref a i) :got (aref b i)))))))

(defun sample-files (file)
  (values (make-pathname :type "wav" :defaults file)
          (make-pathname :type "qoa" :defaults file)
          (make-pathname :type "wav" :name (format NIL "~a.qoa" (pathname-name file)) :defaults file)))

(defun generate-reference (file)
  (multiple-value-bind (source encoded decoded) (sample-files file)
    (uiop:run-program (list *qoaconv* (uiop:native-namestring source) (uiop:native-namestring encoded))
                      :output T :error-output T)
    (uiop:run-program (list *qoaconv* (uiop:native-namestring encoded) (uiop:native-namestring decoded))
                      :output T :error-output T)))

(define-test interface
  :parent qoa
  (finish (qoa:encode-from-buffer (silence)))
  (finish (qoa:decode-file (qoa:encode-from-buffer (silence))))
  (is = 4800 (qoa:samplerate (qoa:encode-from-buffer (silence))))
  (is = 1 (qoa:channels (qoa:encode-from-buffer (silence)))))

(define-test samples
  :parent qoa
  :depends-on (interface))

(defmacro define-sample-test (name file)
  (multiple-value-bind (source encoded decoded) (sample-files file)
    `(define-test ,name
       :parent samples
       (let ((source (wav-samples ,source))
             (encoded (alexandria:read-file-into-byte-vector ,encoded))
             (decoded (wav-samples ,decoded)))
         (is eql NIL (diff encoded (qoa:encode-file source 'vector)))
         (is eql NIL (diff decoded (qoa:decode-file encoded)))))))

(macrolet ((define-all-samples ()
             `(progn ,@(loop for file in (directory (make-pathname :name :wild :type "qoa" :defaults *samples-directory*))
                             collect `(define-sample-test ,(intern (string-upcase (pathname-name file))) ,file)))))
  (define-all-samples))


(DEFINE-SAMPLE-TEST SILENCE
  #P"/home/linus/Projects/cl/cl-qoa/samples/silence.qoa")
