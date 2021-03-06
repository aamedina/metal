(asdf:defsystem :metal
  :description "LispWorks Metal Interface for OS X"
  :version "0.1.0"
  :author "Adrian Medina <adrian.medina@mail.yu.edu>"
  :license "GNU General Public License"
  :pathname "src/lisp"
  :components ((:file "package")
               (:file "foreign-interface")
               (:file "objc")
               (:file "device")
               (:file "command-buffer")
               (:file "command-encoder")
               (:file "metal")))
