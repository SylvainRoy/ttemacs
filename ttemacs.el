;;; ttemacs.el ---Send and receive Edifact messages from emacs.

;; Author: Sylvain Roy <sroy@amadeus.com>

(require 'bindat)
(require 'network-stream)


;;
;; High level function (these are the ones you want to use in a scenario).
;;

(defun tt-process (fun)
  "Executes the function fun"
  (setq ttemacs-sequencer-queue
	(cons `(lambda ()
		 (scenario-process ,fun))
	      ttemacs-sequencer-queue)))

(defun tt-configuration (config)
  "Sets the network configuration to use."
  (setq ttemacs-sequencer-queue
	(cons `(lambda ()
		 (scenario-set-configuration ',config))
	      ttemacs-sequencer-queue)))

(defun tt-send (query)
  "Adds a message to send in the sequencer."
  (setq ttemacs-sequencer-queue
	(cons `(lambda () (scenario-send ',query))
	      ttemacs-sequencer-queue)))

(defun tt-match (template)
  "Matches the reply with template."
  (setq ttemacs-sequencer-queue
	(cons `(lambda () (scenario-match ',template))
	      ttemacs-sequencer-queue)))

(defun tt-done ()
  "Starts the injection. To call at the very end of the scenario."
  (ttemacs-log ">> Starting")
  (ttemacs-clean-log)
  (ttemacs-clean-variables)
  (sequencer-next-action))


;;
;; Sequencer
;;

(setq ttemacs-sequencer-queue ())

(defun sequencer-next-action ()
  "Processes the next action of the sequencer"
  (let ((toeval (car (last ttemacs-sequencer-queue))))
    (if toeval
	(progn
	  ;; Dequeue and exec next action
	  (setq ttemacs-sequencer-queue (butlast ttemacs-sequencer-queue))
	  (funcall toeval))
      (progn
	(ttemacs-log ">> Closing cxn")
	(delete-process ttemacs-process)))))


;;
;; Scenario layer
;;

(defvar tt-config
  '(protocol erplv2
    ip       "127.0.0.1"
    port     40000)
  "The configuration used by tt-emacs to send messages. Use 'tt-configuration function to update it.")

(defvar tt-variables ()
  "The variables defined in queries/replies via {%varname%=*} pattern.")

(defun scenario-set-configuration (config)
  "Sets the network configuration to use."
  (ttemacs-log (format ">> Configuration:\n - ip:%s\n - port:%s\n - protocol:%s\n"
		       (plist-get tt-config 'ip)
		       (plist-get tt-config 'port)
		       (plist-get tt-config 'protocol)))
  (setq tt-config config)
  (sequencer-next-action))

(defun scenario-process (fun)
  "Process fun in the context of the scenario."
  (setq variables (funcall fun tt-variables))
  (sequencer-next-action))

(defun ttemacs-clean-variables ()
  "Resets the registered variables."
  (setq tt-variables ()))

(defvar tt-last-reply ()
  "The last reply received by the injector.")

(defun scenario-send (query)
  "Sends a message and save the response in the global var 'reply."
  (session-send (implement-query-template query tt-variables)))

(defun scenario-reply-handler (msg)
  "Called upon reception of the reply. Save the reply in 'tt-last-reply."
  (setq tt-last-reply msg)
  (sequencer-next-action))

(defun scenario-match (template)
  "Matches the last reply against template and save variables."
  (setq template (chomp template))
  (setq tt-variables (append (parse-reply template tt-last-reply)
			     tt-variables))
  (let ((l tt-variables)
	(out ""))
    (while l
      (setq out (concat out "\n    " (car l) " = " (car (cdr l))))
      (setq l (cddr l)))
    (ttemacs-log (concat ">> Variables: " out "\n")))
  (sequencer-next-action))

(defun implement-query-template (query-template variables)
  "Implement the template with the variables given in parameter."
  (defun set-variable-in-query-template (matched-region)
    (let* ((var-name matched-region)
	   (var-value (lax-plist-get variables var-name)))
      (if var-value
	  var-value
	(format "[[%s Not Found]]" var-name))))
  (replace-regexp-in-string "\\(%[^%]+%\\)"
			    'set-variable-in-query-template
			    query-template
			    t nil 0 0))

(defun parse-reply (template-reply reply)
  "Returns a plist of assoc (variable name => value), nil if no match."
  (let* ((r (build-reply-regexp template-reply))
	 (regexp-of-template (car r))
	 (variables-of-template (car (cdr r)))
	 (variables ()))
    (if (string-match regexp-of-template reply)
	(progn
	  (while variables-of-template
	    (let ((name     (car variables-of-template))
		  (position (cadr variables-of-template)))
	      (setq variables (lax-plist-put
			       variables
			       name
			       (substring reply
					  (match-beginning (+ 1 position))
					  (match-end (+ 1 position)))))
	      (setq variables-of-template (cddr variables-of-template))))
	  variables)
      nil)))

(defun build-reply-regexp (template-reply)
  "Returns:
   - a regular expression that matches instances of the template
   - a plist that gives the assoc (variable name => index of regexp group)
	where a variable is def in the template by {%varname%=*}"
  (let* ((variable-position 0)
	 (variables-strings ())
	 (variables-of-template ()))
    ;; Find all variable regions, list variables, replace regions by regexp
    (defun list-and-replace (matched-region)
      "Collects and replaces variable regions."
      (setq variables-strings (cons matched-region variables-strings))
      "\\\\([^+:'*]*\\\\)")
    (setq template-reply (replace-regexp-in-string
			  "{[ \t]*\\(%[^%]*%[ \t]*=[ \t]*\\)?\\*[ \t]*}"
			  'list-and-replace
			  template-reply
			  t nil 0 0))
    ;; Escape '+' which is a special character in regexp
    ;; todo: what about the other special characters?!
    (setq template-reply (replace-regexp-in-string
			  "\\+"
			  "\\\\+"
			  template-reply
			  t nil 0 0))
    ;; Build plist of (variable => variable position in regexp)
    (setq variables-strings (reverse variables-strings))
    (while variables-strings
      (setq variable (car variables-strings))
      (setq split-name (split-string variable "%"))
      ;; If it can be split by %, then there is a variable name in it
      ;; (e.g. {%varname%=*}) and then we want to index it.
      (if (= 3 (length split-name))
	  (setq variables-of-template
		(lax-plist-put variables-of-template
			       (concat "%" (nth 1 split-name) "%")
			       variable-position)))
      (setq variable-position (+ variable-position 1))
      (setq variables-strings (cdr variables-strings)))
    (list template-reply variables-of-template)))


;;
;; Session layer
;;

(defun session-send (msg)
  "Sends 'msg' to ip:port using. session-reply-handler will be called with reply."
  (setq msg (chomp msg))
  (setq msg (unpretty-print msg))
  (update-session-with-query msg)
  (setq msg (update-query-based-on-context msg))
  (ttemacs-log (format ">> Sent query to %s:%s (%s):\n%s\n"
		       (plist-get tt-config 'ip)
		       (plist-get tt-config 'port)
		       (plist-get tt-config 'protocol)
		       (pretty-print msg)))
  (setq msg (update-with-syntax-separators msg))
  (transport-send msg))

(defun session-reply-handler (msg)
  "Callback to handle message received at session level."
  (setq msg (update-with-display-separators msg))
  (update-session-with-reply msg)
  (setq msg (pretty-print msg))
  (ttemacs-log (format ">> Received:\n%s\n" msg))
  (scenario-reply-handler msg))

(defun unpretty-print (string)
  "Removes '\n&' at end of lines."
  (setq string (replace-regexp-in-string "^[ \t\x0a]*" "" string t nil 0 0))
  (setq string (replace-regexp-in-string "'\\(&?[ \t\x0a]*\\)" "" string t nil 1 0)))

(defun pretty-print (string)
  "Adds newline between segment of the message."
  (setq string (replace-regexp-in-string "\\('\\)." "'&\x0a" string t nil 1 0)))

(defun update-with-syntax-separators (string)
  "Detects syntax in UNB segment and changes separators accordingly."
  (unless (numberp (string-match "^UNB\\+IATA" string))
    (setq string (replace-regexp-in-string "'" "\x1c" string t nil 0 0))
    (setq string (replace-regexp-in-string "\\+" "\x1d" string t nil 0 0))
    (setq string (replace-regexp-in-string ":" "\x1f" string t nil 0 0))
    (setq string (replace-regexp-in-string "\\*" "\x19" string t nil 0 0)))
  string)

(defun update-with-display-separators (string)
  "Changes EDI separators by printable ones (the ones of IATA)."
  (setq string (replace-regexp-in-string "\x1c" "'" string t nil 0 0))
  (setq string (replace-regexp-in-string "\x1d" "+" string t nil 0 0))
  (setq string (replace-regexp-in-string "\x1f" ":" string t nil 0 0))
  (setq string (replace-regexp-in-string "\x19" "*" string t nil 0 0)))

;; Context information at session level (e.g. the conversations ID).
(setq ttemacs-session-context '(local-conv-id     nil
				local-seq-number  nil
				remote-conv-id    ""
				remote-seq-number ""))

(defun update-query-based-on-context (query)
  "Updates message according to session context."
  (setq query (update-local-part-of-message
	       query
	       (plist-get ttemacs-session-context 'local-conv-id)
	       (plist-get ttemacs-session-context 'local-seq-number)))
  (setq query (update-remote-part-of-message
	       query
	       (plist-get ttemacs-session-context 'remote-conv-id)
	       (plist-get ttemacs-session-context 'remote-seq-number)))
  query)

(defun update-session-with-query (query)
  "Updates the context with the new message to send."
  (let ((parsed-msg (parse-message query)))
    (if (not (stringp (plist-get ttemacs-session-context 'local-conv-id)))
	;; Init local ID if first message in conv
	(progn
	  (setq ttemacs-session-context
		(plist-put ttemacs-session-context 'local-conv-id
			   (plist-get parsed-msg 'local-conv-id)))
	  (setq ttemacs-session-context
		(plist-put ttemacs-session-context 'local-seq-number
			   "0000"))))
    ;; Increment sequence number
    (setq ttemacs-session-context
	  (plist-put ttemacs-session-context 'local-seq-number
		     (format "%04d" (+ 1 (string-to-number
					  (plist-get ttemacs-session-context
						     'local-seq-number))))))))

(defun update-session-with-reply (reply)
  "Updates the session context with a newly received reply."
  (let ((parsed-msg (parse-message reply)))
    (setq ttemacs-session-context
	  (plist-put ttemacs-session-context 'remote-conv-id
		     (plist-get parsed-msg 'local-conv-id)))
    (setq ttemacs-session-context
	  (plist-put ttemacs-session-context 'remote-seq-number
		     (plist-get parsed-msg 'local-seq-number)))))

(defun update-local-part-of-message (msg conv-id seq-number)
  "Returns updated message with local conv ID / seq number."
  (setq msg (replace-regexp-in-string
	     "^UNB\\+[^+]*\\+[^+]*\\+[^+]*\\+[^+]*\\+\\([^+]*\\)"
	     (concat conv-id seq-number)
	     msg t nil 1 0))
  (replace-regexp-in-string
   "UNZ\\+[^+]*\\+\\([^+']*\\)"
   (concat conv-id seq-number)
   msg t nil 1 0))

(defun update-remote-part-of-message (msg conv-id seq-number)
  "Returns updated message with remote conv ID / seq number."
  (replace-regexp-in-string
   "^UNB\\+[^+]*\\+[^+]*\\+[^+]*\\+[^+]*\\+[^+]*\\+\\([^+]*\\)"
   (concat conv-id seq-number)
   msg t nil 1 0))

(defun parse-message (msg)
  (string-match
   "^UNB\\+[^+]*\\+[^+]*\\+[^+]*\\+[^+]*\\+\\([^+]*\\)\\+\\([^+]*\\)"
   msg)
  `(local-conv-id     ,(substring msg
				  (match-beginning 1)
				  (- (match-end 1) 4))
    local-seq-number  ,(substring msg
				  (- (match-end 1) 4)
				  (match-end 1))
    remote-conv-id    ,(if (not (= (match-beginning 2) (match-end 2)))
			   (substring msg
				      (match-beginning 1)
				      (- (match-end 1) 4))
			 "")
    remote-seq-number ,(if (not (= (match-beginning 2) (match-end 2)))
			   (substring msg
				      (- (match-end 2) 4)
				      (match-end 2))
			 "")))


;;
;; Transport layer
;;

;; The process that handle the cxn
(setq ttemacs-process nil)

(defun transport-send (data)
  "Send 'data' to ip:port using ad-hoc transport encoder/decoder.
   Then, calls transport-reply-handler with the decoded reply."
  (setq ttemacs-recv-buffer "")
  (defun handle-output-flow (process output)
    "Aggregate output data, decode them and call handler."
    (setq ttemacs-recv-buffer (concat ttemacs-recv-buffer output))
    (condition-case nil
	(transport-reply-handler
	 (transport-decoder
	  (string-make-unibyte ttemacs-recv-buffer)))
      (error nil)))
  (let ((ip (plist-get tt-config 'ip))
	(port (plist-get tt-config 'port)))
    (setq ttemacs-process
	  (open-network-stream "ttemacs-process" "*Messages*" ip port))
    (set-process-filter ttemacs-process 'handle-output-flow)
    (process-send-string ttemacs-process (transport-encoder data))))

(defun transport-reply-handler (msg)
  "Callback to handle message received at transport level."
  (session-reply-handler msg))

(defun transport-encoder (data)
  "Returns encoded data"
  (let ((protocol (plist-get tt-config 'protocol)))
    (cond ((string= protocol 'erplv2) (erplv2-encoder data))
	  (t (error "protocol %s not supported" protocol)))))

(defun transport-decoder (data)
  "Returns decoded data"
  (let ((protocol (plist-get tt-config 'protocol)))
    (cond ((string= protocol 'erplv2) (erplv2-decoder data))
	  (t (error "protocol %s not supported" protocol)))))


;;
;; Transport: ERPLv2 Protocol
;;

(setq erplv2-header-spec
      '((len1          u16) ; len of the message (= #x0000)
	(len2          u16) ; len of the message (= len(data) + 12 + len(rscv))
	(checksum1     u16) ; one's complement of the len (= #xFFFF)
	(checksum2     u16) ; one's complement of the len
	(headerlen     u16) ; len of the erplv2 header (= len rscv + 4)
	(headerversion u8)  ; 0x32
	(unused        u8)  ; 0x00
	(rscv          str (eval (- (bindat-get-field struct 'headerlen) 4)))
	(message       str (eval (- (bindat-get-field struct 'len2)
				    (bindat-get-field struct 'headerlen)
				    8)))))

(defun erplv2-encoder (msg)
  "Encode a message in ERPLv2."
  (let* ((len (+ (length msg) 12))
	 (fields `((len1 . #x0000)
		   (len2 . ,len)
		   (checksum1 . #xFFFF)
		   (checksum2 . ,(logxor len #xFFFF))
		   (headerlen . 4)
		   (headerversion . #x32)
		   (unused . 0)
		   (rscv . "")
		   (message . ,msg)))
	 (data (bindat-pack erplv2-header-spec fields)))
    data))

(defun erplv2-decoder (data)
  "Decode a message in ERPLv2."
  (let* ((fields (bindat-unpack erplv2-header-spec data)))
    (bindat-get-field fields 'message)))


;;
;; Logging
;;

(defun ttemacs-log (message)
  "Log message in ttemacs-output"
  (with-current-buffer (get-buffer-create "ttemacs-output")
    (goto-char (point-max))
    (insert (concat message "\n"))
    (display-buffer "ttemacs-output")
    ))

(defun ttemacs-clean-log ()
  "Clean ttemacs-output message log buffer."
  (with-current-buffer (get-buffer-create "ttemacs-output")
    (delete-region (point-min) (point-max))))


;;
;; Misc utilities
;;

(defun chomp (string)
  "Remove heading/trailing whitespace, new lines, etc."
  (setq string (when (string-match "^[ \t\n]+" string)
		 (replace-match "" nil nil string)))
  (setq string (when (string-match "[ \t\n]+$" string)
		 (replace-match "" nil nil string)))
  string)

(defun lax-plist-update (source destination)
  "Update destination plist with content of source list."
  (while source
    (let ((key (car source))
	  (value (car (cdr source))))
      (lax-plist-put destination key value)
      (setq source (cddr source))))
  destination)
