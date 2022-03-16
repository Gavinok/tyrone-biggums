;;;; lisp.asd

(asdf:defsystem #:lisp
  :description "Simple Websocket Chat Application"
  :author "Gavin Jaeger-Freeborn <gavinfreeborn@gmail.com>"
  :license  "ISC"
  :version "0.0.1"
  :serial t
  :depends-on (#:chanl ;; For go like channels
               #:bt-semaphore ;; for threading and locking
               #:portal
               #:jonathan
               #:alexandria)
  :components ((:file "package")
               (:file "lisp")
               ;; (:file "chat")
               ))
