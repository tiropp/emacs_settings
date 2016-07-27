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
  ;; (define-key c-mode-base-map (kbd "<backtab>")  'complete-symbol)
  )

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


;; Add column 80 marker
;;
;; Use C-c m to set a new marker position
;; (global-set-key [?\C-c ?m] 'column-marker-3)
(use-package column-marker
   :ensure t
   :config
   (add-hook 'c-mode-common-hook (lambda () (interactive) (column-marker-3 80))))
  

;; Activate spell checking within comments
(add-hook 'c-mode-common-hook 'flyspell-prog-mode)
(add-hook 'c-mode-common
	  (lambda() (seqq flyspell-issue-message-flag nil)))


;; Do not use tabs, but use white spaces instead 
(add-hook 'c-mode-common-hook
	  (lambda() (setq indent-tabs-mode nil))
)

;; Open .h file in c++ mode
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; The access lables (private, protected, public) shall be indented with
;; (4-2 = 2) spaces.
(c-set-offset 'access-label -2)
