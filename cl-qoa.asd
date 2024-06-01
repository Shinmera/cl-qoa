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
               :binary-structures))
