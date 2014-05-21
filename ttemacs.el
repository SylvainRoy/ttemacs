;;; ttemacs.el ---Send and receive Edifact messages from emacs.

;; Author: Sylvain Roy <sroy@amadeus.com>

(require 'bindat)
(require 'network-stream)


;;
;; High level functions/variables (these are the ones you want to use).
;;

(defvar tt-config
  '((syntax . iatb)
    (protocol . erplv2)
    (ip . "127.0.0.1")
    (port . 40000))
  "The configuration used by tt-emacs to send messages.")

(defun tt-send (query)
  "Send a message and save the response in the global var 'reply."
  (ttemacs-log (format ">> Sending message to %s:%s (%s):\n%s\n"
		       (cdr (assoc 'ip tt-config))
		       (cdr (assoc 'port tt-config))
		       (cdr (assoc 'protocol tt-config))
		       query))
  (setq reply nil)
  (session-send query))

(defun tt-reply-handler (msg)
  "Called upon reception of the reply."
  (ttemacs-log (format ">> Received:\n%s\n" msg))
  (setq reply msg))


;;
;; Session layer
;;

(defun session-send (data)
  "Send 'data' to ip:port using.
   Then, calls session-reply-handler with the decoded reply."
  (setq data (unpretty-print data))
  (transport-send data))

(defun session-reply-handler (msg)
  "Callback to handle message received at session level."
  (tt-reply-handler (pretty-print msg)))

(defun unpretty-print (string)
  "Removes '\n&', change [+:'*] by ad-hoc separator based on syntax."
  (setq string (replace-regexp-in-string "^[ \t]*" "" string t nil 0 0))
  (setq string (replace-regexp-in-string "[ \t\x0a]*$" "" string t nil 0 0))
  (setq string (replace-regexp-in-string "'\\(&\x0a?[ \t]*\\)[A-Z][A-Z][A-Z]\\+"
					 "" string t nil 1 0))
  (setq string (replace-regexp-in-string "'\\(&\x0a?[ \t]*\\)$"
					 "" string t nil 1 0))
  (unless (numberp (string-match "^UNB\\+IATA" string))
    (setq string (replace-regexp-in-string "'" "\x1c" string t nil 0 0))
    (setq string (replace-regexp-in-string "\\+" "\x1d" string t nil 0 0))
    (setq string (replace-regexp-in-string ":" "\x1f" string t nil 0 0))
    (setq string (replace-regexp-in-string "\\*" "\x19" string t nil 0 0)))
  string)

(defun pretty-print (string)
  "Adds newline and printable separator."
  (setq string (replace-regexp-in-string "\x1c" "'" string t nil 0 0))
  (setq string (replace-regexp-in-string "\x1d" "+" string t nil 0 0))
  (setq string (replace-regexp-in-string "\x1f" ":" string t nil 0 0))
  (setq string (replace-regexp-in-string "\x19" "*" string t nil 0 0))
  (setq string (replace-regexp-in-string "'" "'&\x0a" string t nil 0 0)))


;;
;; Transport layer
;;

(defun transport-send (data)
  "Send 'data' to ip:port using ad-hoc transport encoder/decoder.
   Then, calls transport-reply-handler with the decoded reply."
  (setq buffer "")
  (defun handle-output-flow (process output)
    "Aggregate output data, decode them and call handler."
    (setq buffer (concat buffer output))
    (condition-case nil
	(transport-reply-handler (transport-decoder (string-make-unibyte buffer)))
      (error nil)))
  (let ((ip (cdr (assoc 'ip tt-config)))
	(port (cdr (assoc 'port tt-config))))
    (setq p (open-network-stream "ttemacs-process" "*Messages*" ip port))
    (set-process-filter p 'handle-output-flow)
    (process-send-string p (transport-encoder data))))

(defun transport-reply-handler (msg)
  "Callback to handle message received at transport level."
  (session-reply-handler msg))

(defun transport-encoder (data)
  "Returns encoded data"
  (let ((protocol (cdr (assoc 'protocol tt-config))))
    (cond ((string= protocol 'erplv2) (erplv2-encoder data))
	  (t (error "protocol %s not supported" protocol)))))

(defun transport-decoder (data)
  "Returns decoded data"
  (let ((protocol (cdr (assoc 'protocol tt-config))))
    (cond ((string= protocol 'erplv2) (erplv2-decoder data))
	  (t (error "protocol %s not supported" protocol)))))


;;
;; Transport: ERPLv2 Protocol
;;

(setq erplv2-header-spec
      '((len1          u16) ; total len of the message (= #x0000)
	(len2          u16) ; total len of the message (= len(data) + 12 + len(rscv))
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
