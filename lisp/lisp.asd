;;;; lisp.asd

(asdf:defsystem #:lisp
  :description "Simple Websocket Chat Application"
  :author "Gavin Jaeger-Freeborn <gavinfreeborn@gmail.com>"
  :license "ISC"
  :version "0.0.1"
  :serial t
  :depends-on (#:chanl        ;; For go like channels
               #:bt-semaphore ;; for threading and locking
               #:portal
               #:jonathan)
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "chat")
                 (:file "main" :depends-on ("chat"))))))
