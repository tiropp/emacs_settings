;;
;; C# configuration
;;
;; ;; Add file
;; (add-to-list 'load-path "~/.emacs.d")
;; (require 'csharp-mode
;; )

;; ;; Set mode
;; (autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
;; (setq auto-mode-alist
;;       (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))

;; ;; Custom code to use a default compiler string for all C# files
;; ;; This is for flymake which per default expects the compiler to be csc.exe
;; (defvar my-csharp-default-compiler nil)
;; (setq my-csharp-default-compiler "gmcs @@FILE@@")
;; ;; Still ensure that it is possible to give the compiler line in the c# source 
;; ;; file
;; (defun my-csharp-get-value-from-comments (marker-string line-limit)
;;   my-csharp-default-compiler)

;; ;; Define special settings for c# mode
;; (defun my-csharp-mode-fn ()
;;   "function that runs when csharp-mode is initialized for a buffer."
;;   (turn-on-auto-revert-mode)
;;   (setq indent-tabs-mode nil)
;;   (require 'flymake)
;;   lambda ()(
;; 	    if my-csharp-default-compiler
;; 	       (progn
;; 		 (fset 'orig-csharp-get-value-from-comments
;; 		       (symbol-function 'csharp-get-value-from-comments))
;; 		 (fset 'csharp-get-value-from-comments
;; 		       (symbol-function 'my-csharp-get-value-from-comments)))
;; 	     )
;;   (flymake-mode 1)
;;   (require 'yasnippet)
;;   (yas/minor-mode-on)
;;   (require 'rfringe)
;;   )
;; (add-hook  'csharp-mode-hook 'my-csharp-mode-fn t)

