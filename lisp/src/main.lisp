;;;; lisp.lisp
(in-package #:lisp)

(defparameter *server* nil)
(defun main ()
  (let* ((this-server (make-new-server)))
    ;;     ;; TODO
    ;;     (chat.StartChat server.In, server.Out)
    (pws:define-resource "/"

      :open (lambda (socket)
	      (handle-new-connection this-server socket))

      :message (lambda (socket message)
		 (handle-new-message this-server socket message))

      :error (lambda (socket condition)
	       ;; echo error to websocket
	       (pws:send socket condition))

      :close (lambda (socket)
	       (declare (ignore socket))
	       (print "Socket leaving error server.")))
    ;; Start server at ws://127.0.0.1:5000/err
    (let ((port 5000))
      (format t "server started on ~a" port)
      (defparameter *server* (pws:server port :multi-thread)))))

;;; Messages
(defstruct Message
  (type    nil  :type (member open text message close))
  (id      0    :type fixnum)
  (message nil  :type string))

;;; Sockets
(defstruct Socket
  (id 0 :type fixnum)
  (out nil :type chanl:unbounded-channel))

;;; Server
(defstruct Server
  ;; Id of next user
  (current-id  0                                       :type Fixnum)
  (from-socket (make-instance 'chanl:bounded-channel)  :type chanl:Bounded-Channel)
  ;; Hash table of sockets connected to this server
  (sockets     (make-hash-table)                       :type Hash-Table)
  (in (make-instance 'chanl:unbounded-channel)         :type chanl:Unbounded-Channel)
  (out         (make-instance 'chanl:bounded-channel)  :type chanl:Bounded-Channel)
  (lock        (bt:make-lock)                          :type bt:lock))

(DECLAIM (ftype (function () Server) make-new-server))
(defun make-new-server ()
  (let* ((out (make-instance 'chanl:bounded-channel :size 10000))
	 (from-socket (make-instance 'chanl:bounded-channel :size 10000)))

    (make-server :from-socket from-socket
                 :out         out)))

(DECLAIM (ftype (function  (Server portal:Websocket) (Array (Unsigned-Byte 8)))
                handle-new-connection))
(defun handle-new-connection (s connection)
  (bt:with-lock-held ((server-lock s))
    (let ((id (server-current-id s)))
      (incf (server-current-id s))
      (setf (gethash connection (server-sockets s))
	    (make-socket :id id
			 :out (make-instance 'chanl:unbounded-channel)))
      (pws:send connection (format nil "Welcome to the Chat server ~a."
				   id)))))

(DECLAIM (ftype (function  (Server portal:Websocket string) (Array (Unsigned-Byte 8)))
                handle-new-message))
(defun handle-new-message (s connection message)
  (let ((brodcasted-message (format nil "from User ~a: ~a"
				    (socket-id
				     (gethash connection (server-sockets s)))
				    (the string
					 (getf (jonathan:parse message) :|again|)))))

    (type-of connection)
    (pws:send connection brodcasted-message)))


(DECLAIM (ftype (Function (Server chanl:Bounded-Channel) (values Server &optional))
		server-recv-message))
(defun server-recv-message (server out)
  ;; (chanl:pexec ()
  ;;   (loop (chanl:select
  ;;   	    ((chanl:recv out msgs)
  ;;   	     (loop
  ;;   	       :for msg :in msgs
  ;;   	       :count msg :into num-of-messages
  ;;   	       :do (chanl:send (socket-out
  ;;   				(gethash (message-id msg)
  ;;   					 (server-sockets server)))
  ;;   			       msg)
  ;;   	       :finally (format t "server sent ~a messages" num-of-messages)))
  ;;   	    ((chanl:recv (server-from-socket server) msg)
  ;;   	     (when (eql (message-type msg)
  ;;   			:close)
  ;;   	       (remhash (message-id msg)
  ;;   			(server-sockets server)))
  ;;   	     (chanl:send (server-in server) msg)))))
  server)
