
(defpackage :jbuffer-asd
  (:use :cl :asdf))

(in-package :jbuffer-asd)

(defsystem jbuffer
  :version "0.0"
  :author  "Peter Elliott"
  :license "LGPLv3"
  :components ((:file "istring")
               (:file "rope")
               (:file "editor"))
  :description "a text editor buffer")
