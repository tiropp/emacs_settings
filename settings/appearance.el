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
(setq default-frame-alist '((font . "Inconsolata-11")))

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


;;;;;;;;;;;;;;;;;
;; Color-Theme ;;
;;;;;;;;;;;;;;;;;
;;;; color-theme-solarized
;; (add-to-list 'custom-theme-load-path "~/.emacs.d/plugins/emacs-color-theme-solarized")
;; (load-theme 'solarized t)
;;
;;;; color-theme-sanityinc-tomorrow
(add-to-list 'load-path "~/.emacs.d/elpa/color-theme-sanityinc-tomorrow-20150803.1419")
(require 'color-theme-sanityinc-tomorrow)
(load-theme 'sanityinc-tomorrow-night t)
;;
;;;; grandshell-theme
;; (add-to-list 'custom-theme-load-path "~/.emacs.d/elpa/grandshell-theme-20150404.501")
;; (load-theme 'grandshell t)
;;
;;;; cobalt-theme
;;;; from: http://emacsthemes.caisah.info/cobalt-theme/
;; (add-to-list 'load-path "~/.emacs.d/elpa/color-theme-20080305.34")
;; (require 'color-theme)
;; (color-theme-initialize)
;; (load-file "~/.emacs.d/plugins/cobalt-theme/color-theme-cobalt.el")
;; (color-theme-cobalt)
