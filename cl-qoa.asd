(asdf:defsystem cl-qoa
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "An implementation of the Quite Okay Audio format."
  :homepage "https://shinmera.github.io/cl-qoa/"
  :bug-tracker "https://github.com/shinmera/cl-qoa/issues"
  :source-control (:git "https://github.com/shinmera/cl-qoa.git")
  :serial T
  :components ((:file "package")
               (:file "format")
               (:file "documentation"))
  :depends-on (:documentation-utils
               :binary-structures)
  :in-order-to ((asdf:test-op (asdf:test-op :cl-qoa/test))))

(asdf:defsystem cl-qoa/test
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Tests for the cl-qoa system."
  :homepage "https://shinmera.github.io/cl-qoa/"
  :bug-tracker "https://github.com/shinmera/cl-qoa/issues"
  :source-control (:git "https://github.com/shinmera/cl-qoa.git")
  :serial T
  :components ((:file "test"))
  :depends-on (:cl-qoa :parachute)
  :perform (asdf:test-op (op c) (uiop:symbol-call :parachute :test :org.shirakumo.qoa.test)))
