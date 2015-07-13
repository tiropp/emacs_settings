;;
;; Define configuration for C++
;;


;; C default programming style
(setq c-default-style
      '((other . "cc-mode")))

;; Define my common c mode 
(defun my-c-mode-config()
  (define-key c-mode-base-map (kbd "\C-c\C-v")   'uncomment-region)
  ;; backtab = shift-tab
  (define-key c-mode-base-map (kbd "<backtab>")  'complete-symbol)
  )
;;
;; (add-hook 'c-mode-common-hook 'c-mode-config)
;;
;; NB: The initialization-hook only runs once per emacs session!
(add-hook 'c-initialization-hook 'my-c-mode-config)


;; Quickly switching between header and implementation file
;;   easy switching between *.h and *.C C++ files with key F4
(add-hook 'c-mode-common-hook
  (lambda() 
;;    (local-set-key  (kbd "C-c o") 'ff-find-other-file)))
    (local-set-key  [f4] 'ff-find-other-file)))
