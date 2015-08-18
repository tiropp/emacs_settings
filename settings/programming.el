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
(add-to-list 'load-path "~/.emacs.d/elpa/diff-hl-20150606.643")
(require 'diff-hl)
(global-diff-hl-mode)
(add-hook 'dired-mode-hook 'diff-hl-dired-mode)


;;;;;;;;;;;
;; Magit ;;
;;;;;;;;;;;
;; Magit is an interface to the version control system Git,
;; implemented as an Emacs package.  Magit aspires to be a complete
;; Git porcelain.
(add-to-list 'load-path "~/.emacs.d/elpa/magit-20150803.337")
(add-to-list 'load-path "~/.emacs.d/elpa/dash-20150717.1321")
(add-to-list 'load-path "~/.emacs.d/elpa/with-editor-20150710.252")
(add-to-list 'load-path "~/.emacs.d/elpa/git-commit-20150727.1401")
(add-to-list 'load-path "~/.emacs.d/elpa/magit-popup-20150730.1344")
(require 'magit)
(set-default 'magit-stage-all-confirm nil)
(add-hook 'magit-mode-hook 'magit-load-config-extensions)

;; full screen magit-status
(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(global-unset-key (kbd "C-x t"))
(global-set-key (kbd "C-x t h") 'magit-log)
(global-set-key (kbd "C-x t f") 'magit-file-log)
(global-set-key (kbd "C-x t b") 'magit-blame-mode)
(global-set-key (kbd "C-x t m") 'magit-branch-manager)
(global-set-key (kbd "C-x t c") 'magit-branch)
(global-set-key (kbd "C-x t s") 'magit-status)
(global-set-key (kbd "C-x t r") 'magit-reflog)
(global-set-key (kbd "C-x t t") 'magit-tag)


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
(add-to-list 'load-path "~/.emacs.d/elpa/flycheck-tip-20150726.156")
(add-to-list 'load-path "~/.emacs.d/elpa/popup-20150626.711")
(require 'flycheck-tip)
(flycheck-tip-use-timer 'verbose)


;;;;;;;;;;;;;;;;;;;;;;;
;; Highlight-Numbers ;;
;;;;;;;;;;;;;;;;;;;;;;;
;; This minor mode provides syntax highlighting of numeric literals
;; in source code, like what many editors provide by default.
(add-to-list 'load-path "~/.emacs.d/elpa/highlight-numbers-20150531.607")
(add-hook 'prog-mode-hook 'highlight-numbers-mode)


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
(add-to-list 'load-path "~/.emacs.d/elpa/function-args-20150731.646")
(require 'function-args)
(fa-config-default)
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(set-default 'semantic-case-fold t)


;;;;;;;;;;;;;;;
;; Yasnippet ;;
;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/.emacs.d/elpa/yasnippet-20150803.1124")
(require 'yasnippet)
(yas-global-mode 1)

;;;;;;;;;;;;;
;; Company ;;
;;;;;;;;;;;;;
;; Company is a modular completion mechanism.  Modules for retrieving completion
;; candidates are called back-ends, modules for displaying them are front-ends.
(add-to-list 'load-path "~/.emacs.d/elpa/company-20150727.1415")
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

;; Use clang backend for company
;;
;; In case company-semantic via CEDET was activated remove it here again,
;; s.t. clang backend can be used.
;; (setq company-backends (delete 'company-semantic company-backends))
;;
;; Use tab for completation for C(++)
(add-hook 'c-mode-common-hook
	  (lambda()
	    (define-key c-mode-map  [(tab)] 'company-complete)
	    (define-key c++-mode-map  [(tab)] 'company-complete)
	    ))

;;;;;;;;;;;;;;;;;;;;;;;
;; Company-C-Headers ;;
;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/.emacs.d/elpa/company-c-headers-20150801.901")
(add-to-list 'company-backends 'company-c-headers)
;;
;; Add additional c++ pathes
(add-hook 'c-mode-common-hook
	  (lambda()
	    (if (eq system-type 'gnu/linux)
		(progn 
		  (add-to-list 'company-c-headers-path-system "/usr/include/c++/4.9/")      
		  ))))



;; One could also use irony for code completion, but I never really got it to
;; work properly.
;;
;; ;;;;;;;;;;;
;; ;; Irony ;;
;; ;;;;;;;;;;;
;; ;;
;; ;; After first set up run
;; ;;   M-x irony-install-server
;; ;; this will will build the irony-server application
;; ;;
;; ;; Setup a project with cmake
;; ;;   o Add the argument
;; ;;       -DCMAKE_EXPORT_COMPILE_COMMANDS=ON
;; ;;     to cmake. It will create the command_commands.json file in the build
;; ;;     directory.
;; ;;
;; (add-to-list 'load-path "~/.emacs.d/elpa/irony-20150802.1203")
;; (add-hook 'c++-mode-hook 'irony-mode)
;; (add-hook 'c-mode-hook 'irony-mode)
;; (add-hook 'objc-mode-hook 'irony-mode)

;; ;; replace the `completion-at-point' and `complete-symbol' bindings in
;; ;; irony-mode's buffers by irony-mode's function
;; (defun my-irony-mode-hook ()
;;   (define-key irony-mode-map [remap completion-at-point]
;;     'irony-completion-at-point-async)
;;   (define-key irony-mode-map [remap complete-symbol]
;;     'irony-completion-at-point-async))
;; (add-hook 'irony-mode-hook 'my-irony-mode-hook)
;; (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; ;; Only needed on Windows
;; (when (eq system-type 'windows-nt)
;;   (setq w32-pipe-read-delay 0))

;; ;;;;;;;;;;;;;;;;;;;
;; ;; Company-Irony ;;
;; ;;;;;;;;;;;;;;;;;;;
;; ;; Irony backend for company
;; ;;
;; (add-to-list 'load-path "~/.emacs.d/elpa/company-irony-20140629.1118")
;; (eval-after-load 'company
;;   '(add-to-list 'company-backends 'company-irony))
;; (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Company-Irony-C-Headers ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; (add-to-list 'load-path "~/.emacs.d/elpa/company-irony-c-headers-20150728.2335")
;; ;; (require 'company-irony-c-headers)
;; ;; ;; Load with `irony-mode` as a grouped backend
;; ;; (eval-after-load 'company
;; ;;   '(add-to-list
;; ;;     'company-backends '(company-irony-c-headers company-irony)))



;;;;;;;;;;;;;;;;;;;;;;;;
;; clean-aindent-mode ;;
;;;;;;;;;;;;;;;;;;;;;;;;
;; Clean up unnecessary white spaces
(add-to-list 'load-path "~/.emacs.d/elpa/clean-aindent-mode-20150618.948")
(require 'clean-aindent-mode)
(add-hook 'prog-mode-hook 'clean-aindent-mode)

;;;;;;;;;;;;;;;;;
;; dtrt-indent ;;
;;;;;;;;;;;;;;;;;
;; A minor mode that guesses the indentation offset and
;; `indent-tabs-mode' originally used for creating source code files and
;; transparently adjusts the corresponding settings in Emacs, making it
;; more convenient to edit foreign files.
(add-to-list 'load-path "~/.emacs.d/elpa/dtrt-indent-20150413.243")
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
(add-to-list 'load-path "~/.emacs.d/elpa/ws-butler-20150126.759")
(require 'ws-butler)
(add-hook 'c-mode-common-hook 'ws-butler-mode)
