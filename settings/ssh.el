;;
;; SSH via tramp
;;
;; (add-to-list 'load-path "~/.emacs.d/tramp/lisp/")
;; (require 'tramp)
;; (setq tramp-default-method "ssh")
(use-package tramp
  :config
  (setq tramp-default-method "ssh")

  ;; Turn off company-mode when openning c/c++ file via tramp, because company
  ;; will freeze emacs in this case.
  (add-hook 'c-mode-hook
	    (lambda () (when (file-remote-p default-directory) (company-mode -1))))
  )
