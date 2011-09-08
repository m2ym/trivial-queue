(in-package :cl-user)

(defpackage :trivial-queue-asd
  (:use :cl :asdf))
(in-package :trivial-queue-asd)

(defsystem :trivial-queue
  :version "0.1"
  :author "Tomohiro Matsuyama"
  :license "LLGPL"
  :components ((:file "queue")))
