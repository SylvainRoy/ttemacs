;; Elisp library to send and receive Edifact message from emacs.

(require 'bindat)
(require 'network-stream)


;;
;; High level functions/variables (these are the ones you want to use).
;;

(defvar tt-config
  '((protocol . erplv2)
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
  (transport-send query))


;;
;; ERPLv2 Protocol
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
;; Networking - the transport layer (ERPLv2, TCIL, ...)
;;

(defun transport-send (data)
  "Send 'data' to ip:port using encoder/decoder and calls reply-hanlder with the decoded reply."
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
  "Callback to handle message received."
  (setq reply msg)
  (ttemacs-log (format ">> Received:\n%s\n" msg)))

(defun transport-encoder (data)
  (let ((protocol (cdr (assoc 'protocol configuration))))
    (cond ((string= protocol 'erplv2) (erplv2-encoder data))
	  (t (error "protocol %s not supported" protocol)))))

(defun transport-decoder (data)
  (let ((protocol (cdr (assoc 'protocol configuration))))
    (cond ((string= protocol 'erplv2) (erplv2-decoder data))
	  (t (error "protocol %s not supported" protocol)))))


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

;; =======================

;tt-send "Il etait une fois.")
;reply

