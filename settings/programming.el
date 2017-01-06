;;;;;;;;;;
;; HELM ;;
;;;;;;;;;;
;; (load-file "~/.emacs.d/settings/cedet_helm.el")
(load-file "~/.emacs.d/settings/helm.el")
;; Use ggtags via helm instead of directly
;; (load-file "~/.emacs.d/settings/ggtags.el")
(load-file "~/.emacs.d/settings/helm-gtags.el")


;;;;;;;;;;;;;;;;
;; ibuffer-vc ;;
;;;;;;;;;;;;;;;;
;; Adds functionality to ibuffer for grouping buffers by their parent
;; version control (vc) root directory, and for displaying and/or sorting
;; by the vc status of listed files.
;;
(add-hook 'ibuffer-hook
          (lambda ()
            (ibuffer-vc-set-filter-groups-by-vc-root)
            (unless (eq ibuffer-sorting-mode 'alphabetic)
              (ibuffer-do-sort-by-alphabetic))))

(setq ibuffer-formats
      '((mark modified read-only vc-status-mini " "
              (name 18 18 :left :elide)
              " "
              (size 9 -1 :right)
              " "
              (mode 16 16 :left :elide)
              " "
              (vc-status 16 16 :left)
              " "
              filename-and-process)))


;;;;;;;;;;;;;
;; diff-hl ;;
;;;;;;;;;;;;;
;; `diff-hl-mode' highlights uncommitted changes on the side of the
;; window (using the fringe, by default), allows you to jump between
;; the hunks and revert them selectively.
;; Provided commands:
;; `diff-hl-diff-goto-hunk'  C-x v =
;; `diff-hl-revert-hunk'     C-x v n
;; `diff-hl-previous-hunk'   C-x v [
;; `diff-hl-next-hunk'       C-x v ]
;;
(use-package diff-hl
   :ensure t
   :config
   (global-diff-hl-mode)
   (add-hook 'dired-mode-hook 'diff-hl-dired-mode))


;;;;;;;;;;;
;; Magit ;;
;;;;;;;;;;;
;; Magit is an interface to the version control system Git,
;; implemented as an Emacs package.  Magit aspires to be a complete
;; Git porcelain.
(use-package magit
   :ensure t

   :commands(magit-status)
   
   :init
   (global-unset-key (kbd "C-x t"))

   :config
   (set-default 'magit-stage-all-confirm nil)
   (add-hook 'magit-mode-hook 'magit-load-config-extensions)
   
   ;; full screen magit-status
   (defadvice magit-status (around magit-fullscreen activate)
     (window-configuration-to-register :magit-fullscreen)
     ad-do-it
     (delete-other-windows))

   :bind(("C-x t h" . magit-log)
	 ("C-x t f" . magit-file-log)
	 ("C-x t b" . magit-blame-mode)
	 ("C-x t m" . magit-branch-manager)
	 ("C-x t c" . magit-branch)
	 ("C-x t s" . magit-status)
	 ("C-x t r" . magit-reflog)
	 ("C-x t t" . magit-tag))
   )
(use-package dash :ensure t)
(use-package with-editor :ensure t)
(use-package git-commit :ensure t)
(use-package magit-popup :ensure t)


;;;;;;;;;;;;;;
;; Flycheck ;;
;;;;;;;;;;;;;;
;; On-the-fly syntax checking for GNU Emacs 24.
;;
;; NB: Flycheck and/or popup causes emacs to crash on Win32
;;
; (add-to-list 'load-path "~/.emacs.d/elpa/flycheck-20150802.212")
; (require 'flycheck)
; (add-hook 'after-init-hook #'global-flycheck-mode)


;; The errors are now displayed by popup, instead of printing into the echo
;; area.
; (add-to-list 'load-path "~/.emacs.d/elpa/flycheck-tip-20150726.156")
; (add-to-list 'load-path "~/.emacs.d/elpa/popup-20150626.711")
; (require 'flycheck-tip)
; (flycheck-tip-use-timer 'verbose)


;;;;;;;;;;;;;;;;;;;;;;;
;; Highlight-Numbers ;;
;;;;;;;;;;;;;;;;;;;;;;;
;; This minor mode provides syntax highlighting of numeric literals
;; in source code, like what many editors provide by default.
(use-package highlight-numbers
   :ensure t
   :config
   (add-hook 'prog-mode-hook 'highlight-numbers-mode)
   )


;;;;;;;;;;;;;;;;;;
;; Function-Arg ;;
;;;;;;;;;;;;;;;;;;
;; The traditional way of showing function arguments in Emacs is to show them in
;; the minibuffer. This approach isn't optimal, since I have to traverse the
;; whole screen just to see the hint. After that traverse the whole screen back
;; to find the cursor.
;;
;; Other environments such as Qt Creator and Eclipse implement the hint as a
;; popup located exactly where the function call is. This is the behavior that
;; function-args implements for Emacs.
;;
;; Somehow the function-arg loads the semantic-db stuff, which is annoying, so
;; lets disable it for the moment.
;;
;(add-to-list 'load-path "~/.emacs.d/elpa/function-args-20150731.646")
;(require 'function-args)
;(fa-config-default)
;(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
;(set-default 'semantic-case-fold t)


;;;;;;;;;;;;;;;
;; Yasnippet ;;
;;;;;;;;;;;;;;;
(use-package yasnippet
   :ensure t
   :config
   (yas-global-mode 1))   


;;;;;;;;;;;;;
;; Company ;;
;;;;;;;;;;;;;
;; Company is a modular completion mechanism.  Modules for retrieving completion
;; candidates are called back-ends, modules for displaying them are front-ends.
(use-package company
   :ensure t
	     
   :config
   (add-hook 'after-init-hook 'global-company-mode)
   
   ;; Use clang backend for company
   ;;
   ;; In case company-semantic via CEDET was activated remove it here again,
   ;; s.t. clang backend can be used.
   (setq company-backends (delete 'company-semantic company-backends))
   
   ;; Use tab for completation for C(++)
   (add-hook 'c-mode-common-hook
	     (lambda()
	       ;; backtab = shift-tab
	       ;; (define-key c-mode-base-map [(backtab)] 'company-complete)
	       (define-key c-mode-base-map (kbd "<C-tab>") 'company-complete)
	    ))
   )


;;;;;;;;;;;;;;;;;;;;;;;
;; Company-C-Headers ;;
;;;;;;;;;;;;;;;;;;;;;;;
(use-package company-c-headers
   :ensure t

   :config
   (add-to-list 'company-backends 'company-c-headers)

   ;; Add additional c++ pathes
   (add-hook 'c-mode-common-hook
	     (lambda()
	       (if (eq system-type 'gnu/linux)
		   (progn 
		     (add-to-list 'company-c-headers-path-system "/usr/include/c++/6")      
		     ))))
   )
   

;;;;;;;;;;;
;; Irony ;;
;;;;;;;;;;;
;;
;; After first set up run
;;   M-x irony-install-server
;; this will will build the irony-server application
;;
;; Setup a project with cmake
;;   o Add the argument
;;       -DCMAKE_EXPORT_COMPILE_COMMANDS=ON
;;     to cmake. It will create the command_commands.json file in the build
;;     directory.
;;
(use-package irony
   :ensure t
	     
   :config
   (add-hook 'c++-mode-hook 'irony-mode)
   (add-hook 'c-mode-hook 'irony-mode)
   (add-hook 'objc-mode-hook 'irony-mode)

   ;; replace the `completion-at-point' and `complete-symbol' bindings in
   ;; irony-mode's buffers by irony-mode's function
   (defun my-irony-mode-hook ()
     (define-key irony-mode-map [remap completion-at-point]
       'irony-completion-at-point-async)
     (define-key irony-mode-map [remap complete-symbol]
       'irony-completion-at-point-async))
   (add-hook 'irony-mode-hook 'my-irony-mode-hook)
   (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

   ;; Only needed on Windows
   (when (eq system-type 'windows-nt)
     (setq w32-pipe-read-delay 0))
   
   )


;;;;;;;;;;;;;;;;;;;
;; Company-Irony ;;
;;;;;;;;;;;;;;;;;;;
;; Irony backend for company
(use-package company-irony
   :ensure t
   :config
   (eval-after-load 'company
     '(add-to-list 'company-backends 'company-irony))
   (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
   )    


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Company-Irony-C-Headers ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (add-to-list 'load-path "~/.emacs.d/elpa/company-irony-c-headers-20150728.2335")
;; (require 'company-irony-c-headers)
;; ;; Load with `irony-mode` as a grouped backend
;; (eval-after-load 'company
;;   '(add-to-list
;;     'company-backends '(company-irony-c-headers company-irony)))



;;;;;;;;;;;;;;;;;;;;;;;;
;; clean-aindent-mode ;;
;;;;;;;;;;;;;;;;;;;;;;;;
;; Clean up unnecessary white spaces
(use-package clean-aindent-mode
   :ensure t
   :config
   (add-hook 'prog-mode-hook 'clean-aindent-mode)
   )


;;;;;;;;;;;;;;;;;
;; dtrt-indent ;;
;;;;;;;;;;;;;;;;;
;; A minor mode that guesses the indentation offset and
;; `indent-tabs-mode' originally used for creating source code files and
;; transparently adjusts the corresponding settings in Emacs, making it
;; more convenient to edit foreign files.
;;
;; It skrews up the indentation size sometimes, hence deacitave it.
;(use-package dtrt-indent
;   :ensure t
;   :config
;   (dtrt-indent-mode 1)
;   )
;   (add-to-list 'load-path "~/.emacs.d/elpa/dtrt-indent-20150413.243")
;;
;; It skrews up the indentation size sometimes, hence deacitave it.
; (require 'dtrt-indent)
; (dtrt-indent-mode 1)


;;;;;;;;;;;;;;;
;; ws-butler ;;
;;;;;;;;;;;;;;;
;; ws-butler helps managing whitespace on every line of code written or edited,
;; in an unobtrusive, help you write clean code without noisy whitespace
;; effortlessly
(use-package ws-butler
   :ensure t
   :config
   (add-hook 'c-mode-common-hook 'ws-butler-mode))



;;;;;;;;;;;
;; rtags ;;
;;;;;;;;;;;
(use-package rtags
  :ensure t
  :config
  (add-hook 'c-mode-common-hook 'rtags-start-process-unless-running)
  (add-hook 'c++-mode-common-hook 'rtags-start-process-unless-running)

  ;; Keybindings
  (rtags-enable-standard-keybindings)

  (setq rtags-autostart-diagnostics t)
  (rtags-diagnostics)
  
  (setq rtags-use-helm t)

  ;; Use rtags as company backend
  (setq rtags-completions-enabled t)    
  (require 'company)
  (push 'company-rtags company-backends)
  )
