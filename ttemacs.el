;;; ttemacs.el ---Send and receive Edifact messages from emacs.

;; Author: Sylvain Roy <sroy@amadeus.com>

(require 'bindat)
(require 'network-stream)


;;
;; High level function (these are the ones you want to use in a scenario).
;;

(defun tt-send (message)
  "Adds a message to send in the sequencer."
  (setq ttemacs-sequencer-queue (cons `(scenario-send ,message)
				     ttemacs-sequencer-queue)))

(defun tt-match (message)
  "Adds a message to match in the sequencer."
  (error "to do..."))

(defun tt-done ()
  "Starts the injection. To call at the very end of the scenario."
  (sequencer-next-action))


;;
;; Sequencer
;;

(setq ttemacs-sequencer-queue ())

(defun sequencer-next-action ()
  "Processes the next action of the sequencer"
  (eval (car (last ttemacs-sequencer-queue)))
  (setq ttemacs-sequencer-queue (butlast ttemacs-sequencer-queue)))


;;
;; Scenario layer
;;

(defvar tt-config
  '(protocol erplv2
    ip       "127.0.0.1"
    port     40000)
  "The configuration used by tt-emacs to send messages.")

(defun scenario-send (query)
  "Sends a message and save the response in the global var 'reply."
  (session-send query))

(defun scenario-reply-handler (msg)
  "Called upon reception of the reply."
  (sequencer-next-action))


;;
;; Session layer
;;

(defun session-send (msg)
  "Sends 'msg' to ip:port using. session-reply-handler will be called with reply."
  (setq msg (chomp msg))
  (setq msg (unpretty-print msg))
  (update-session-with-query msg)
  (setq msg (update-query-based-on-context msg))
  (ttemacs-log (format ">> Sending query to %s:%s (%s):\n%s"
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
  (ttemacs-log (format ">> Received:\n%s" msg))
  (scenario-reply-handler msg))

(defun unpretty-print (string)
  "Removes '\n&' at end of lines."
  (setq string (replace-regexp-in-string "^[ \t\x0a]*" "" string t nil 0 0))
  (setq string (replace-regexp-in-string "'\\(&?[ \t\x0a]*\\)" "" string t nil 1 0)))

(defun pretty-print (string)
  "Adds newline between segment of the message."
  (setq string (replace-regexp-in-string "'" "'&\x0a" string t nil 0 0)))

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
  (setq string (when (string-match "[ \t\n]*$" query)
		 (replace-match "" nil nil string)))
  (setq string (when (string-match "^[ \t\n]*" query)
		 (replace-match "" nil nil string)))
  string)
