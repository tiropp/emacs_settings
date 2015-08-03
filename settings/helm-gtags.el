;;
;; Initial version taken from
;;    https://github.com/tuhdo/emacs-c-ide-demo
;;

;; this variables must be set before load helm-gtags
;; you can change to any prefix key of your choice
(setq helm-gtags-prefix-key "\C-cg")

(add-to-list 'load-path "~/.emacs.d/elpa/helm-gtags-20150617.1931")
(require 'helm-gtags)

(setq
 helm-gtags-ignore-case t
 helm-gtags-auto-update t
 helm-gtags-use-input-at-cursor t
 helm-gtags-pulse-at-cursor t
 helm-gtags-prefix-key "\C-cg"
 helm-gtags-suggested-key-mapping t
 )

;; Enable helm-gtags-mode in Dired so you can jump to any tag
;; when navigate project tree with Dired
(add-hook 'dired-mode-hook 'helm-gtags-mode)

;; Enable helm-gtags-mode in Eshell for the same reason as above
(add-hook 'eshell-mode-hook 'helm-gtags-mode)

;; Enable helm-gtags-mode in languages that GNU Global supports
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'java-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)

;; key bindings
(define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
(define-key helm-gtags-mode-map (kbd "C-j") 'helm-gtags-select)
(define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
(define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
(define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
(define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)

(define-key helm-gtags-mode-map (kbd "C-M-m") 'mark-defun)
(define-key helm-gtags-mode-map (kbd "C-c g r") 'helm-gtags-find-rtag)


;; Key bindings:
;; C-M-f   forward-sexp        Move forward to the next expression
;; C-M-b   backward-sexp
;; C-M-k   kill-sexp           Kill next expression
;; C-M-SPC mark-sexp
;; C-M-a   beginning-of-defun  Jump to begin of function
;; C-M-e   end-of-defun        Jump to end of function
;; C-M-m   mark-defun          Mark current function
;;
;; M-.     helm-gtags-dwin                    Find definition
;; M-,     helm-gtags-pop-stack               Jump back to prior location
;; C-j     helm-tags-select                   Interactive selection of symbols
;; C-c g r helm-gtags-find-rtag               Find reference
;; C-c g a helm-gtags-tags-in-this-function   Gives lits of functions called by the current function

