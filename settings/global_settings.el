;;
;; Define global settings
;;

;;;;;;;;;;;;;;;;;;;;;;;;;
;; TURN WHEEL MOUSE ON ;;
;;;;;;;;;;;;;;;;;;;;;;;;;
(mouse-wheel-mode t) 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TURN LINE AND COLUMN NUMBER MODE ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq line-number-mode t)
(setq column-number-mode t)


;;;;;;;;;;;;;;;;;;;;
;; TRUNCATE LINES ;;
;;;;;;;;;;;;;;;;;;;;
;; truncate lines if they are too long
(setq-default truncate-lines t)
;; trucate even even when screen is split into multiple windows
(setq-default truncate-partial-width-windows nil)
;;; AUTO-SHOW SEAMS TO BE OBSOLET NOW
;; load auto-show (shows lines when cursor moves to right of long line)
;; (require 'auto-show)
;; (auto-show-mode 1)
;;(setq-default auto-show-mode t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C-k kills whole line and newline if at beginning of line ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq kill-whole-line t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set up WindMove to be able to move between windows with shift-arrow ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

