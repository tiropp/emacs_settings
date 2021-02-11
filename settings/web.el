;;;;;;;;;;;;;;;;;;;
;; atomic-chrome ;;
;;;;;;;;;;;;;;;;;;;
(use-package atomic-chrome
  :ensure t
  :config
  (setq atomic-chrome-default-major-mode 'markdown-mode)
  (setq atomic-chrome-buffer-open-style 'frame)
  (atomic-chrome-start-server)
  )
