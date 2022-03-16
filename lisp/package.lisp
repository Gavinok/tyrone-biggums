;;;; package.lisp
(defpackage #:lisp
  (:use #:cl)
  (:import-from :jonathan
		:parse
		:to-json))

(defpackage #:chat
  (:use #:cl)
  (:import-from :jonathan
		:parse
		:to-json))
