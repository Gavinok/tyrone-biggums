(defstruct User
  (sent 0 :type fixnum)
  (recv 0 :type fixnum))

(defstruct Chat
  (out (make-instance 'chanl:unbounded-channel)  :type chanl:unbounded-channel) ; chan<- []*server.Message
  (users (make-hash-table)                       :type hash-table) ; map[uint]User
  (channels (make-hash-table)                    :type hash-table) ; map[string]map[uint]struct{}
  (lookup-channels (make-hash-table)             :type hash-table) ; map[uint]string
  (mu (bt:make-lock)                             :type bt:lock))

(declaim (ftype (function (Chat Fixnum) T) leave-channel))
(defun leave-channel (chat id)
  "called when a user chooses to leave a channel"
  (bt:with-lock-held ((chat-mu chat))
    (let ((val (gethash id (chat-lookup-channels chat))))
      (remhash val (chat-lookup-channels chat))
      (remhash id  (chat-channels chat)))))

(declaim (ftype (function (Chat Fixnum String) T) join-channel))
(defun join-channel (chat id channel)
  "called when a user chooses to leave a channel"
  (bt:with-lock-held ((chat-mu chat))
    (let ((found-channel (gethash channel (chat-lookup-channels chat))))
      (unless found-channel
	(setf found-channel (make-hash-table))
	(setf (gethash channel (chat-channels chat)) found-channel))
      (setf (gethash id found-channel) (list :|type| nil :|id| nil :|message| nil))
      (setf (gethash id (chat-lookup-channels chat)) channel)))

  ;; send the join message to server
  (chanl:send (chat-out chat)
	      (make-message
	       :type 'text
	       :id id
	       :message (format nil "!join successful: ~a" id))))

(declaim (ftype (function (chat message) t) process-message))
;; (defun process-message (chat message)
;;   "process an incomming MESSAGE sent to CHAT"
;;   (bt:with-lock-held ((chat-mu chat))
;;     (let ((messages (make-array 10 :element-type message :adjustable t))
;; 	  (messages (make-array 10 :element-type message :adjustable t)))
;;       (if (chat-lookup-channels  (message-id message) chat)
;; 	  (when ) ))))


;; func (c *Chat) processMessage(message *server.Message) {
;; 	c.mu.Lock()
;; 	defer c.mu.Unlock()

;;     msgs := []*server.Message{}
;; 	if val, ok := c.lookup-channels[message.Id]; ok {
;;         if user, ok := c.users[message.Id]; ok {
;;             user.recv += 1
;;         }
;; 		var content server.MessageContent
;; 		err := json.Unmarshal([]byte(message.Message), &content)
;; 		if err != nil {
;; 			fmt.Println("error:", err)
;; 		}

;; 		content.Inc += 1

;; 		channel := c.channels[val]
;; 		channel_message, err := json.Marshal(server.ChatMessage{
;; 			Channel_name:       val,
;; 			Channel_user_count: len(channel),
;; 			From:               message.Id,
;; 			Msg:                content,
;; 		})

;; 		if err != nil {
;; 			log.Fatalf("%+v\n", err)
;; 		}

;; 		channel_message_string := string(channel_message)

;; 		for socketId := range channel {
;;             if user, ok := c.users[socketId]; ok {
;;                 user.recv += 1
;;             }
;; 			msgs = append(msgs, server.NewMessage(socketId, channel_message_string))
;; 		}

;; 	} else {
;;         msgs = append(msgs, message.FromMessage("You haven't joined a channel yet.  Please execute !join <channel name> before sending messages"))
;; 	}
;;     c.out <- msgs
;; }

(declaim (ftype (function (chanl:Bounded-Channel chanl:Bounded-Channel) Chat) start-chat))
(defun start-chat (in out)
  "Start a new chat connection to pass messages back and forth."
  (let ((chat (make-chat :out out)))
    (chanl:pexec ()
	(loop :for msg = (chanl:recv in)
	      :if (eql (message-type msg) 'close)
		:do (leave-channel chat (message-id msg))
	      :else :if (not (eql (message-type msg) 'text))
		:do (print "not text")
	      :if (str:starts-with-p "!join " (message-message msg))
		:do (progn (leave-channel chat (message-id msg))
			   (join-channel chat (message-id msg)
					 (second (str:split
						  " " (message-message msg)))))
	      :else :if (equal (message-message msg) ":q")
		      :do (leave-channel chat (message-id msg))
	      :else
		:do (process-message chat msg)))
    chat))
