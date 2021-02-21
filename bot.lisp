(ql:quickload 'cl-telegram-bot)
(ql:quickload 'drakma)
(ql:quickload 'cl-html-parse)
(load "markov/markov.lisp")
(in-package :tl-bot)

(defun match-command (regex text function)
  (multiple-value-bind (_ match)
      (cl-ppcre::scan-to-strings regex text)
    (when match 
      (funcall function match))))

(defun parse-page (content)
  (let (alke (cl-html-parse::html-parse content))
    (if (eq (caar alke) :html) (cl-ppcre:regex-replace-all "<.*?>|&.*?;" (assoc (car alke)) " "))
    (cl-ppcre:regex-replace-all "<.*?>|&.*?;" content " ")))

(let ((mkbot (make-bot "token")))
  (loop
     (loop for update across (get-updates mkbot) do
	  (let* ((message (access update 'message))
	         (text (access message 'text))
		 (message-id (access message 'message--id))
		 (chat-id (access message 'chat 'id))
		 (first-name (access message 'from 'first--name)))
	    ;;debug
	    (format t "---~%ID: ~a~%chat: ~a~%user: ~a~%text: <<~a>>~%"
		    message-id
		    chat-id
		    first-name
		    text)
	    (when text
	      (match-command "^/echo (.*)$" text
			     (lambda (args)
			       (send-message mkbot
					     chat-id
					     (elt args 0))))
	      (match-command "^/alke$" text
			     (lambda (args)
			       (send-message mkbot
					     chat-id
					     "gaffot")))
	      (match-command "^/gen (https?://.*)" text
			     (lambda (args)
			       (let* ((url (elt args 0))
				      (content (drakma::http-request url))
				      (markov (markov::make-markov-from-string (parse-page content)))
				      (msg (markov::generate-random-sentence markov)))
				 (send-message mkbot
					       chat-id
					       msg
					       :reply message-id))))

	      nil)
	    (sleep 1)))))
