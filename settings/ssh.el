;;
;; SSH via tramp
;;
(add-to-list 'load-path "~/.emacs.d/tramp/lisp/")
(require 'tramp)
(setq tramp-default-method "ssh")

