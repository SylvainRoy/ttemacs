;; Example of ttemacs injection file.

;; You need to open this file with emacs to run it.
;; Then, you just have to, eval the buffer in emacs (M-x eval-buffer).

(load-file "./ttemacs.el")

(tt-configuration '(protocol  erplv2
		    ip        "127.0.0.1"
                    port      40000))

(tt-send "
UNB+IATB:1+UNTO+FROM+140521:2133+002UOSLJ5F0001+++S'&
UNH+1+AVLREQ:01:1:UN'&
MSD+2:37'&
UNT+7+1'&
UNZ+1+002UOSLJ5F0001'
")

(tt-match "
UNB+IATB:1+FROM+UNTO+{%date%=*}:{*}+{%remoteid%=*}+002UOSLJ5F0001++E'&
UNH+1+AVLREQ:01:1:UN'&
MSD+2:37'&
UNT+7+1'&
UNZ+1+{*}'
")

(tt-send "
UNB+IATB:1+UNTO+FROM+140521:2133+002UOSLJ5F0002+++E'&
UNH+1+AVLREQ:01:1:UN'&
MSD+2:38'&
UNT+7+1'&
UNZ+1+002UOSLJ5F0002'
")

(tt-match "
UNB+IATB:1+FROM+UNTO+{*}:{*}+{*}+{*}++E'&
UNH+1+AVLREQ:01:1:UN'&
MSD+2:{%something%=*}'&
UNT+7+1'&
UNZ+1+{*}'
")

(tt-send "
UNB+IATB:1+UNTO+FROM+140521:2133+002UOSLJ5F0002+++T'&
UNH+1+AVLREQ:01:1:UN'&
MSD+2:38:%something%'&
UNT+7+1'&
UNZ+1+002UOSLJ5F0002'
")

(tt-done)
