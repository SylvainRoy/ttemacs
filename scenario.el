(load-file "./ttemacs.el")

(setq tt-config
      '((protocol . erplv2)
	(ip . "127.0.0.1")
	(port . 40000)))

(tt-send "Il etait une fois de plus.")

reply
