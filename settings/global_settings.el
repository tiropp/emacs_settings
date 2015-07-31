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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; USE IDO FOR BUFFER CHANGE ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'ido)
(ido-mode t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COPY TO AND FROM REGISTER ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "\C-x x") 'copy-to-register)
(global-set-key (kbd "\C-x g") 'insert-register)


;;;;;;;;;;;;;;;;;;;;;;;;
;; Volatile-Highlight ;;
;;;;;;;;;;;;;;;;;;;;;;;;
;; From the original documentation:
;;
;; This library provides minor mode `volatile-highlight-mode', which
;; brings visual feedback to some operations by highlighting portions
;; relating to the operations.
;;
;; All of highlights made by this library will be removed
;; when any new command is executed.
;;
(add-to-list 'load-path "~/.emacs.d/elpa/volatile-highlights-20141004.2240")
(require 'volatile-highlights)
(volatile-highlights-mode t)


;;;;;;;;;;;;;;;
;; Undo-Tree ;;
;;;;;;;;;;;;;;;
;; Use undo-tree-visualize to see the undo tree in a separated buffer.
(add-to-list 'load-path "~/.emacs.d/elpa/undo-tree-20140509.522")
(require 'undo-tree)


;;;;;;;;;;;;;;;
;; Yasnippet ;;
;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/.emacs.d/elpa/yasnippet-20150415.244")
(require 'yasnippet)
(yas-global-mode 1)

