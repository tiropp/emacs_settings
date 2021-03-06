;;
;; markdown settings
;;
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  :hook (markdown-mode . flyspell-mode)
  )

(use-package flymd
  :ensure t
  )

;; Live preview
;;
;; Since flymd has problems with firefox above version 68
;;
;; Requires multimarkdown perl script. (In debian this is contained in the
;; package libtext-markdown-perl)
;;
;; Run the markdown and open browser with command: markdown-preview-mode
;;
(use-package markdown-preview-mode
  :ensure t
  :config
  (setq markdown-preview--http-server 9322)
  (add-to-list 'markdown-preview-stylesheets
	       "<style type = text/css>html {font-family: 'Source Code Pro', sans-serif;}</style><link href='https://fonts.googleapis.com/css?family=Source Code Pro' rel='stylesheet'>")
  )

; (setq markdown-preview-stylesheets
; 	(list
;    	    "http://thomasf.github.io/solarized-css/solarized-dark.min.css"
; 	    "<style type = text/css>html {font-family: 'Source Code Pro', sans-serif;}</style><link href='https://fonts.googleapis.com/css?family=Source Code Pro' rel='stylesheet'>"
; 		))
