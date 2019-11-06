;;
;; Setup appearance of emacs
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SET DEFAULT FONT TO "Misc 7x13" ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   you can get the font currently using by M-x describe-font
;; (set-face-font 'default "-misc-fixed-medium-r-normal--13-*-*-*-c-70-iso8859-1")
;;
;; Nicer font
(if (eq system-type 'gnu/linux)
    (progn
      (cond ((find-font (font-spec :name "Source Code Pro-14"))
	     (setq default-frame-alist '((font . "Source Code Pro-14"))))
	    ((find-font (font-spec :name "Inconsolata-14"))
	     (setq default-frame-alist '((font . "Inconsolata-14"))))
	    )
      )
  )

;; NB: If font is not installed, emacs may crash. This was observed under Windwos.


;;;;;;;;;;;;;;;;;;;;;;;;;
;; REMOVE THE TOOL BAR ;;
;;;;;;;;;;;;;;;;;;;;;;;;;
(tool-bar-mode -1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CHANGE BACKGROUND OF MARKED REGION ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(set-face-attribute 'region nil :background "#6699FF" :foreground "#000000")


;;;;;;;;;;;;;;;;;;;;;;
;; DON'T DISPLAY ^M ;;
;;;;;;;;;;;;;;;;;;;;;;
;; Taken from http://stackoverflow.com/questions/730751/hiding-m-in-emacs
;; originally from Johan Bockgård
(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))



;;;;;;;;;;;;;;;;
;; BOTTOM BAR ;;
;;;;;;;;;;;;;;;;
(use-package powerline
  :ensure t
  :init (powerline-default-theme)
  )


;;;;;;;;;;;;;;;;;
;; Color-Theme ;;
;;;;;;;;;;;;;;;;;
;;;; color-theme-solarized
;(use-package emacs-color-theme-solarized
;  :config
;  (load-theme 'solarized t))
	     
;;;; color-theme-sanityinc-tomorrow
;; (use-package color-theme-sanityinc-tomorrow
;;   :ensure t
;;   :config
;;   (load-theme 'sanityinc-tomorrow-night t))

;;;; grandshell-theme
;; (use-package grandshell-theme
;;    :ensure t
;;    :config
;;    (load-theme 'grandshell t))

;;;; cobalt-theme
;;;; from: http://emacsthemes.caisah.info/cobalt-theme/
;; (use-package color-theme
;;    :ensure t
;;    :config
;;    (color-theme-initialize))
;; (use-package color-theme-cobalt
;;    :load-path "plugins/"
;;    :config
;;    (color-theme-cobalt))

;;;; spacemacs
(use-package spacemacs-theme
  :ensure t
  :defer t
  :init (load-theme 'spacemacs-dark t)
  )


