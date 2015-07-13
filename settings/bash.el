;;
;; Bash configuration
;;
(add-hook 'sh-mode-hook 
    (lambda()
      (local-set-key (kbd "\C-c\C-c") 'comment-region)))
(add-hook 'sh-mode-hook 
    (lambda()
      (local-set-key (kbd "\C-c\C-v") 'uncomment-region)))
