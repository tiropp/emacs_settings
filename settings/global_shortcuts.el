;;
;; Set up global shortcuts
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SHORT-CUT FOR GOT0-LINE COMMAND ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; only for c-mode
;(add-hook 'c-mode-common-hook
;  (lambda()
;     (local-set-key [f5] 'goto-line)))
;; now in ervery mode f5 is goto-line!
(global-set-key [f5] 'goto-line)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use "f6" to jump to the matching parenthesis. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun goto-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis, otherwise insert
  the character typed."
    (interactive "p")
	  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
	      ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
		      (t                    (self-insert-command (or arg 1))) ))
(global-set-key [f6] `goto-match-paren)


;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INSERT DATE AND TIME ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(defvar current-date-time-format "%a %b %d %H:%M:%S %Z %Y"
(defvar current-date-time-format "$Date: %Y/%m/%e %T %Z$"
  "Format of date to insert with `insert-current-date-time' func
See help of `format-time-string' for possible replacements")

(defvar current-time-format "%a %H:%M:%S"
  "Format of date to insert with `insert-current-time' func.
Note the weekly scope of the command's precision.")

(defun insert-current-date-time ()
  "insert the current date and time into current buffer.
Uses `current-date-time-format' for the formatting the date/time."
       (interactive)
;       (insert "==========\n")
;       (insert (let () (comment-start)))
       (insert (format-time-string current-date-time-format (current-time)))
       (insert "\n")
       )

(defun insert-current-time ()
  "insert the current time (1-week scope) into the current buffer."
       (interactive)
       (insert (format-time-string current-time-format (current-time)))
       (insert "\n")
       )

(global-set-key [f8] 'insert-current-date-time)
(global-set-key "\C-c\C-t" 'insert-current-time)


;;;;;;;;;;;;;;;;;;;;;;;;
;; HIGHLITHING SYMBOL ;;
;;;;;;;;;;;;;;;;;;;;;;;;
;; highlight-symbol taken from: http://nschum.de/src/emacs/highlight-symbol/
(use-package highlight-symbol
   :defer t
   :init
   (load-file "~/.emacs.d/settings/highlight-symbol.el")
   :bind (([f7]           . highlight-symbol-at-point)
	  ([(shift f7)]   . highlight-symbol-next)
	  ([(control f7)] . highlight-symbol-prev))
   :config
   (put 'narrow-to-region 'disabled nil)
   )
