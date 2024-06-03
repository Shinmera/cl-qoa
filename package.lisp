(defpackage #:org.shirakumo.qoa
  (:use #:cl #:org.shirakumo.binary-structures.types)
  (:local-nicknames
   (#:bs #:org.shirakumo.binary-structures))
  (:shadow #:open)
  (:export
   #:file
   #:frame
   #:frames
   #:octet-size
   #:samplerate
   #:channels
   #:read-file
   #:write-file
   #:encode-from-buffer
   #:encode-file
   #:decode-frame
   #:decode-to-buffer
   #:decode-file
   #:convert-wav
   #:channel-layout))
