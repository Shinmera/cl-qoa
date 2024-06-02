(defpackage #:org.shirakumo.qoa.test
  (:use #:cl #:parachute)
  (:local-nicknames
   (#:qoa #:org.shirakumo.qoa)))

(in-package #:org.shirakumo.qoa.test)

(defvar *samples-directory*
  #.(merge-pathnames "samples/" (make-pathname :name NIL :type NIL :defaults (or *compile-file-truename* *load-truename*))))

(define-test qoa)

(defun generate (count gen)
  (map-into (make-array count :element-type '(signed-byte 16)) gen))

(defun silence (&optional (length 128))
  (generate length (constantly 0)))

(defun wav-samples (file)
  )

(define-test trivial-en/decode
  :parent qoa
  (finish (qoa:encode (silence)))
  (finish (qoa:decode (qoa:encode (silence))))
  (is equalp (silence) (qoa:decode (qoa:encode (silence)))))

(define-test samples
  :parent qoa
  (dolist (encoded (directory (make-pathname :name :wild :type "qoa" :defaults *samples-directory*)))
    (let ((source (wav-samples (make-pathname :type "wav" :defaults encoded)))
          (decoded (wav-samples (make-pathname :name (format NIL "~a.qoa" (pathname-name source)) :defaults source)))
          (encoded (alexandria:read-file-into-byte-vector encoded)))
      (is equalp encoded (qoa:write-file (qoa:encode source) 'vector))
      (is equalp decoded (qoa:decode (qoa:read-file encoded))))))
