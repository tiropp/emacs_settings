;;
;; CMAKE settings
;;
(use-package cmake-mode
   :defer t
   :ensure t
   :mode("CMakeLists\\.txt\\'" "\\.cmake\\'")
   
   :config
   (add-hook 'cmake-mode-hook
	     ;; Do not use tabs, but use white spaces instead 
	     (lambda() (setq indent-tabs-mode nil))
	     )
   )
