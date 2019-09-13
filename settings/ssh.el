;;
;; SSH via tramp
;;
;; (add-to-list 'load-path "~/.emacs.d/tramp/lisp/")
;; (require 'tramp)
;; (setq tramp-default-method "ssh")
(use-package tramp
  :defer t
  :config
  (setq tramp-default-method "ssh"))
