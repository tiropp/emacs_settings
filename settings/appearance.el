;;
;; Setup appearance of emacs
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SET DEFAULT FONT TO "Misc 7x13" ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   you can get the font currently using by M-x describe-font
;; (set-face-font 'default "-misc-fixed-medium-r-normal--13-*-*-*-c-70-iso8859-1")


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
