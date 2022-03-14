(load "tl-bot.lisp") ; => T
(load "markov.lisp") ; => T
(ql:quickload 'cl-ppcre) ; => (CL-PPCRE)
(ql:quickload 'drakma) ; => (DRAKMA)
(ql:quickload 'cl-html-parse) ; => (CL-HTML-PARSE)
(ql:quickload 'alexandria) ; => (ALEXANDRIA)
(ql:quickload 'cl-json) ; => (CL-JSON)
(in-package :tl-bot) ; => #<PACKAGE "TL-BOT">
(defun match-command (regex text function) (multiple-value-bind (_ match) (cl-ppcre::scan-to-strings regex text)(when match (funcall function match)))) ; => MATCH-COMMAND
(defun parse-page (content)
  (let (parsed (cl-html-parse::html-parse content))
    (if (eq (caar parsed) :html) (cl-ppcre:regex-replace-all "<.*?>|&.*?;" (assoc (car parsed)) " "))
    (cl-ppcre:regex-replace-all "<.*?>|&.*?;" content " ")))
