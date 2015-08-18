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


;;;;;;;;;;;;;;;;;;;
;; CODING SYSTEM ;;
;;;;;;;;;;;;;;;;;;;
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

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
;; Use helm instead of ido now
;(require 'ido)
;(ido-mode t)



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



;;;;;;;;;;;;;;;;;;;
;; Expand-Region ;;
;;;;;;;;;;;;;;;;;;;
;; Expand region increases the selected region by semantic units. Just keep
;; pressing the key until it selects what you want.
;;
(add-to-list 'load-path "~/.emacs.d/elpa/expand-region-20150718.18")
(require 'expand-region)
(global-set-key (kbd "M-m") 'er/expand-region)


;;;;;;;;;;
;; Nyan ;;
;;;;;;;;;;
;; Nyan Mode is an analog indicator of your position in the buffer. The Cat
;; should go from left to right in your mode-line, as you move your point from
;; 0% to 100%.
;;
;; only turn on if a window system is available
;; this prevents error under terminal that does not support X
(add-to-list 'load-path "~/.emacs.d/elpa/nyan-mode-20150128.1218")
(require 'nyan-mode)
(case window-system
  ((x w32) (nyan-mode)))


;;;;;;;;;;;;;;;;;;
;; Golden-Ratio ;;
;;;;;;;;;;;;;;;;;;
;; Automatic resizing of Emacs windows to the golden ratio
;;
;; The window that has the main focus will have the perfect size for editing,
;; while the ones that are not being actively edited will be re-sized to a
;; smaller size that doesn't get in the way, but at the same time will be
;; readable enough to know it's content.
;;
;; (add-to-list 'load-path "~/.emacs.d/elpa/golden-ratio-20150526.1200")
;; (require 'golden-ratio)

;; (add-to-list 'golden-ratio-exclude-modes "ediff-mode")
;; (add-to-list 'golden-ratio-exclude-modes "helm-mode")
;; (add-to-list 'golden-ratio-exclude-modes "dired-mode")
;; (add-to-list 'golden-ratio-inhibit-functions 'pl/helm-alive-p)

;; (defun pl/helm-alive-p ()
;;   (if (boundp 'helm-alive-p)
;;       (symbol-value 'helm-alive-p)))

;; ;; do not enable golden-raio in thses modes
;; (setq golden-ratio-exclude-modes '("ediff-mode"
;;                                    "gud-mode"
;;                                    "gdb-locals-mode"
;;                                    "gdb-registers-mode"
;;                                    "gdb-breakpoints-mode"
;;                                    "gdb-threads-mode"
;;                                    "gdb-frames-mode"
;;                                    "gdb-inferior-io-mode"
;;                                    "gud-mode"
;;                                    "gdb-inferior-io-mode"
;;                                    "gdb-disassembly-mode"
;;                                    "gdb-memory-mode"
;;                                    "magit-log-mode"
;;                                    "magit-reflog-mode"
;;                                    "magit-status-mode"
;;                                    "IELM"
;;                                    "eshell-mode" "dired-mode"))

;; (golden-ratio-mode)


;;;;;;;;;;;;;;;;;;;;;;;
;; Discover-My-Major ;;
;;;;;;;;;;;;;;;;;;;;;;;
;; Discover key bindings and their meaning for the current Emacs major mode
(add-to-list 'load-path "~/.emacs.d/elpa/discover-my-major-20140510.1007")
(add-to-list 'load-path "~/.emacs.d/elpa/makey-20131231.630")
(require 'discover-my-major)
(global-unset-key (kbd "C-h h"))        ; original "C-h h" displays "hello world" in different languages
(define-key 'help-command (kbd "h m") 'discover-my-major)


