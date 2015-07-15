;;
;; The emacs root configuration file 
;;
;;

;; this is intended for manually installed libraries
(add-to-list 'load-path "~/.emacs.d/site-lisp/")

;; load the package system and add some repositories
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; Install a hook running post-init.el *after* initialization took place
(add-hook 'after-init-hook (lambda () (load "~/.emacs.d/post-init.el")))

;; Do here basic initialization, (require) non-ELPA packages, etc.
(load-file "~/.emacs.d/init_default.el")

;; disable automatic loading of packages after init.el is done
(setq package-enable-at-startup nil)
;; and force it to happen now
(package-initialize)

;; NOW you can (require) your ELPA packages and configure them as normal
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(case-fold-search t)
 '(current-language-environment "UTF-8")
 '(default-input-method "rfc1345")
 '(ecb-options-version "2.40")
 '(global-font-lock-mode t nil (font-lock))
 '(newsticker-url-list (quote (("Mozilla news" "http://fxfeeds.mozilla.com/en-US/firefox/headlines.xml" nil nil nil))))
 '(show-paren-mode t nil (paren))
 '(tex-close-quote "\"")
 '(tex-open-quote "\"")
 '(transient-mark-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
