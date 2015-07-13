;;
;; Tex, LaTeX configuration
;;

;; (defun ke-latex-mode-hook ()
;;   (setq LaTeX-label-function 'reftex-label
;;         local-abbrev-table LaTeX-mode-abbrev-table)
;;   (setq fill-column 79)
;;   (require 'font-latex))

;; (add-hook 'LaTeX-mode-hook 'ke-latex-mode-hook)
;; (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
;; (add-hook 'LaTeX-mode-hook 'flyspell-mode)

;; (add-hook 'LaTeX-mode-hook
;;    (lambda()
;;      (local-set-key (kbd "\C-c\C-c") 'comment-region)))
;; (add-hook 'LaTeX-mode-hook
;;    (lambda()
;;      (local-set-key (kbd "\C-c\C-v") 'uncomment-region)))

(setq TeX-auto-save t) 
(setq TeX-parse-self t) 
(setq TeX-save-query nil)
; (setq TeX-PDF-mode t)


;;(require 'tex-site)
;;(autoload 'texinfo-mode "texinfo" no-doc t)


;; (define-abbrev-table 'LaTeX-mode-abbrev-table (make-abbrev-table))
;;   (add-hook 'LaTeX-mode-hook (lambda ()
;;     (setq abbrev-mode t)
;;     (setq local-abbrev-table TeX-mode-abbrev-table)))

;;(defun ke-latex-mode-hook ()
;;  (setq LaTeX-label-function 'reftex-label
;;        local-abbrev-table LaTeX-mode-abbrev-table)
;;  (setq fill-column 79)
;;  (require 'font-latex)
;;)
(defun ke-latex-mode-hook ()
  (setq LaTeX-label-function 'reftex-label
        local-abbrev-table)
  (setq fill-column 79)
  (require 'font-latex)
)

;; could be ispell as well, depending on your preferences 
(setq ispell-program-name "aspell") 

;; this can obviously be set to any language your spell-checking program supports   
(setq ispell-dictionary "english") 

(defun latex-config() 
  ;;(ke-latex-mode-hook)
  (turn-on-reftex)
  (flyspell-mode)
  (flyspell-buffer)
  (lambda() (local-set-key (kbd "\C-c\C-c") 'comment-region))
  (lambda() (local-set-key (kbd "\C-c\C-v") 'uncomment-region))
  (turn-on-visual-line-mode)  ;; use the visual-line-mode for line wrapping
  (load "auctex.el" nil t t)          ;; load auctex
  (load "preview-latex.el" nil t t)

)
(add-hook 'LaTeX-mode-hook 'latex-config)
(add-hook 'latex-mode-hook 'latex-config)


;; Define new minor latex mode "my-keys" which redefines some keys
;; NB: Per default C-c C-c is bound to tex-compile function. Which we don't
;;     need. I prefere to run latex/pdflatex in the shell myself, especially
;;     for large projects, which anyway use a Makefile.
(defvar my-keys-latex-mode-map (make-keymap) "my-keys-latex-mode keymap.")
(define-key my-keys-latex-mode-map (kbd "\C-c\C-c") 'comment-region)
(define-key my-keys-latex-mode-map (kbd "\C-c\C-v") 'uncomment-region)
(define-minor-mode my-keys-latex-mode
  "My latex mode, redefining some keys"
  t " my-keys" 'my-keys-latex-mode-map)
(my-keys-latex-mode 1)
;; Turn of the my-keys minor mode in mini buffer
(defun my-minibuffer-setup-hook ()
  (my-keys-latex-mode 0))
(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup-hook)

;; outline-mode
;;  --> hiding parts of the document
(defun turn-on-outline-minor-mode () (outline-minor-mode 1))
(add-hook 'LaTeX-mode-hook 'turn-on-outline-minor-mode) 
(add-hook 'latex-mode-hook 'turn-on-outline-minor-mode) 
(setq outline-minor-mode-prefix "\C-c\C-o")
;; use C-c C-o C-l to map in a section 
;;     C-c C-o C-a to map out a section


;; Change font-look mode for latex
(add-hook 'latex-mode-hook
  (lambda()
     (font-lock-add-keywords nil
        '(("\\(\\\\fotenote\\)\\>" 1 font-lock-warning-face t)))
))

