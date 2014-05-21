(load-file "./ttemacs.el")

(ttemacs-clean-log)

(setq tt-config
      '((syntax . iatb)
	(protocol . erplv2)
	(ip . "127.0.0.1")
	(port . 40000)))

(tt-send 
"UNB+IATB:1+FROM+UNTO+140521:1554+002UOSLJ5B0001+++S'&
UNH+1+RAVSRQ:03:1:1A'&
ORG+1A:MUC+00000000:MUC1A0701+MUC+++DE:EUR:GE'&
ADT++25:1'&
CPY+CAR++SX'&
LOC+176+1A:::CDG'&
LOC+DOL+1A:::CDG'&
SPR+++2004:8:1:10:0+2004:8:3:10:0'&
VHS+VT+ACR:ECMN'&
UNT+8+1'&
UNZ+1+002UOSLJ5B0001'")

reply
