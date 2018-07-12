;;
;; Bash configuration
;;
(add-hook 'sh-mode-hook 
    (lambda()
      (local-set-key (kbd "\C-c\C-c") 'comment-region)
      (local-set-key (kbd "\C-c\C-v") 'uncomment-region)
      (setq indent-tabs-mode nil)))
