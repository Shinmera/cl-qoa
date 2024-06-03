(defpackage #:org.shirakumo.qoa.test
  (:use #:cl #:parachute)
  (:local-nicknames
   (#:qoa #:org.shirakumo.qoa)))

(in-package #:org.shirakumo.qoa.test)

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
                   (nibbles:read-sb16/be-into-sequence samples stream)
                   (return samples))
                 (dotimes (i length) (read-byte stream))))))

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
  `(define-test ,name
     :parent samples
     (let ((source (wav-samples ,(make-pathname :type "wav" :defaults file)))
           (decoded (wav-samples ,(make-pathname :type "wav" :name (format NIL "~a.qoa" (pathname-name file)) :defaults file)))
           (encoded (alexandria:read-file-into-byte-vector ,file)))
       (is equalp encoded (qoa:encode-file source 'vector))
       (is equalp decoded (qoa:decode-file encoded)))))

(macrolet ((define-all-samples ()
             `(progn ,@(loop for file in (directory (make-pathname :name :wild :type "qoa" :defaults *samples-directory*))
                             collect `(define-sample-test ,(intern (string-upcase (pathname-name file))) ,file)))))
  (define-all-samples))
