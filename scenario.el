(load-file "./ttemacs.el")

(ttemacs-clean-log)

(setq tt-config
      '((protocol . erplv2)
	(ip . "127.0.0.1")
	(port . 40000)))

(tt-send "
UNB+IATB:1+UNTO+FROM+140521:2133+002UOSLJ5F0001+++S'&
UNH+1+AVLREQ:01:1:UN'&
MSD+2:37'&
ORG+1A:MUC+00000000:MUC1A0701+MUC+++DE:EUR:EN+BMPR'&
TFF+:::DY+24'&
TVL+040801:1000:040803:1000+CDG+SX'&
PRD+:ECMN'&
UNT+7+1'&
UNZ+1+002UOSLJ5F0001'
")

reply
