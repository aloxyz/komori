;; Rei <https://github.com/sovietspaceship>
(require :cl-json)
(require :drakma)
(defpackage :tl-bot
  (:use #:cl)
  (:export
   #:bot
   #:make-bot
   #:send-message
   #:get-updates))
(in-package :tl-bot)

(defparameter *telegram-api-uri* "https://api.telegram.org/bot")

(defclass bot ()
  ((id
    :documentation "Update id"
    :initform 0)
   (token
    :initarg :token
    :documentation "Bot token given by BotFather"
    :initform Nil)
   (endpoint
    :initarg :endpoint
    :documentation "HTTPS endpoint"
    :initform Nil)))

(defmethod initialize-instance :after ((b bot) &key)
  (setf (slot-value b 'endpoint)
        (concatenate 'string *telegram-api-uri* (slot-value b 'token) "/")))

(defun make-bot (token)
  "Create a new bot instance. Takes a token string."
  (make-instance 'bot :token token))

(defun get-class-slots (obj)
  "Get a list of class slots, useful to inspect Fluid classes. SBCL only."
  (mapcar #'sb-mop:slot-definition-name
          (sb-mop:class-slots
           (class-of obj))))

(defgeneric make-request (b name options))
(defmethod make-request ((b bot) name options)
  "Perform HTTP request to 'name API method with 'options JSON-encoded object."
  (drakma:http-request
   (concatenate 'string (slot-value b 'endpoint) name)
   :method :post
   :want-stream t
   :content-type "application/json"
   :content (json:encode-json-alist-to-string options)))

(defgeneric make-form-data-request (b name options))
(defmethod make-form-data-request ((b bot) name options)
  "Perform form-data HTTP request to 'name API method with' options JSON-encoded object."
  (drakma:http-request
   (concatenate 'string (slot-value b 'endpoint) name)
   :method :post
   :want-stream t
   :content-type "application/json"
   :content (json:encode-json-alist-to-string options)
   :form-data T))

(defgeneric send-document (b id doc &key caption reply))
(defmethod send-document ((b bot) id doc &key caption reply)
  "Sends a document file."
  (make-request b "sendDocument"
		(list (cons :chat_id id)
		      (cons :document doc)
		      (cons :caption caption)
		      (cons :reply_to_message_id reply))))

(defgeneric send-audio (b id audio &key duration performer title reply))
(defmethod send-audio ((b bot) id audio &key duration performer title reply)
  "Sends an audio file."
  (make-request b "sendAudio"
		(list (cons :chat_id id)
		      (cons :audio audio)
		      (cons :duration duration)
		      (cons :performer performer)
		      (cons :title title)
		      (cons :reply_to_message_id reply))))

(defgeneric send-photo (b id photo &key caption reply))
(defmethod send-photo ((b bot) id photo &key caption reply)
  "Sends an image file."
  (make-request b "sendPhoto"
		(list (cons :chat_id id)
		      (cons :photo photo)
		      (cons :caption caption)
		      (cons :reply_to_message_id reply))))

(defgeneric forward-message (b id fcid messid))
(defmethod forward-message ((b bot) id fcid messid)
  "Forwards a message."
  (make-request b "forwardMessage"
		(list (cons :chat_id id)
		      (cons :from_chat_id fcid)
		      (cons :message_id messid))))

(defgeneric send-message (b id text &key reply))
(defmethod send-message ((b bot) id text &key reply)
  "Sends a message."
  (make-request b "sendMessage"
                (list (cons :chat_id id)
                      (cons :text text)
                      (cons :reply_to_message_id reply))))

(define-condition request-error (error)
  ((what :initarg :what :reader what)))

(defun access (update &rest args)
  "Access update field. update.first.second. ... => (access update 'first 'second ...). Nil if unbound."
  (let ((current update))
    (dolist (r args)
      (unless (slot-boundp current r)
        (return-from access nil))
      (setf current (slot-value current r)))
    current))

(defun get-slot (update slot)
  "Access slot. Since fluid classes signal error on unbound slot access, this instead returns nil."
  (if (slot-boundp update slot)
      (slot-value update slot)
    nil))

(defgeneric get-updates (b))
(defmethod get-updates ((b bot))
  "Get updates using long-polling. Returns a vector of results."
  (let* ((current-id (slot-value b 'id))
         (cl-json:*json-symbols-package* :tl-bot)
         (request
          (json:with-decoder-simple-clos-semantics
           (let ((x (json:decode-json
                     (make-request b "getUpdates"
                                   (list (cons :offset current-id))))))
             (with-slots (ok result) x
                         (values x (class-of x) ok result)))))
         (results (slot-value request 'result)))
    (when (eql (slot-value request 'ok) nil)
      (error 'request-error :what request))
    (when (> (length results) 0)
      (let* ((last-update (elt results (- (length results) 1)))
             (id (slot-value last-update 'update--id)))
        (when (= current-id 0)
          (setf (slot-value b 'id) id))
        (incf (slot-value b 'id))))
    results))

(in-package :cl-user)
