(add-to-list 'auto-mode-alist
	     '("\\.xaml$" . (lambda()
			      (nxml-mode)
			      (setq indent-tabs-mode nil)
			      (setq nxml-child-indent 4 nxml-attribute-indent 4))))

