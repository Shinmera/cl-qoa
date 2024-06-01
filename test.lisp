(defpackage #:org.shirakumo.qoa.test
  (:use #:cl #:parachute)
  (:local-nicknames
   (#:qoa #:org.shirakumo.qoa)))

(in-package #:org.shirakumo.qoa.test)

(define-test qoa)

(defun generate (count gen)
  (map-into (make-array count :element-type '(signed-byte 16)) gen))

(defun silence (&optional (length 128))
  (generate length (constantly 0)))

(define-test trivial-en/decode
  :parent qoa
  (finish (qoa:encode (silence) :channels 1))
  (finish (qoa:decode (qoa:encode (silence) :channels 1)))
  (is equalp (silence) (qoa:decode (qoa:encode (silence) :channels 1))))
