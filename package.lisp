(defpackage #:org.shirakumo.qoa
  (:use #:cl #:org.shirakumo.binary-structures.types)
  (:local-nicknames
   (#:bs #:org.shirakumo.binary-structures))
  (:shadow #:open)
  (:export
   #:file
   #:samplerate
   #:channels
   #:read-file
   #:write-file
   #:encode
   #:decode-frame
   #:decode-to-buffer
   #:decode
   #:channel-layout))
