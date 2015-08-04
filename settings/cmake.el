;;
;; CMAKE settings
;;
(add-to-list 'load-path "~/.emacs.d/cmake")
(require 'cmake-mode)
(add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-mode))
(add-hook 'cmake-mode-hook
	  ;; Do not use tabs, but use white spaces instead 
	  (lambda() (setq indent-tabs-mode nil))
)
