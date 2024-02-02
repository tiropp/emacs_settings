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
(use-package dash
  :ensure t)
(use-package with-editor
  :ensure t)
(use-package git-commit
  :ensure t)
(use-package magit-popup
  :ensure t)
(use-package magit-lfs
  :ensure t
  :pin melpa)

;;;;;;;;;;
;; dsvn ;;
;;;;;;;;;;
(use-package dsvn
  :ensure t

  :bind(("C-x v s" . svn-status)
	)
  )


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
(use-package yasnippet-snippets
  :ensure t)


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
   (add-hook 'c-mode-common-hook 'ws-butler-mode)
   (add-hook 'elm-mode-hook 'ws-butler-mode)
   (add-hook 'prog-mode-hook 'ws-butler-mode)
   (add-hook 'gfm-mode-hook 'ws-butler-mode)
   )


;;;;;;;;
;; HS ;;
;;;;;;;;
;; hs-minor-mode can be used to hide blocks of code
;;
;; E.g. a block is everything within a '{..}'
;;
;; Default key bindings:
;; Key 	        Command             Description
;; C-c @ C-e 	hs-toggle-hiding    Toggle hiding/showing of a block
;; C-c @ C-h 	hs-hide-block       Select current block at point and hide it
;; C-c @ C-l 	hs-hide-level       Hide all block with indentation levels below this block
;; C-c @ C-s 	hs-show-block       Select current block at point and show it.
;; C-c @ C-M-h 	hs-hide-all         Hide all top level blocks, displaying only first and last lines.
;; C-c @ C-M-s 	hs-show-all         Show everything
;;
(add-hook 'c-mode-common-hook 'hs-minor-mode)


;;;;;;;;;;;;;;;;;;;;;;
;; MULTIPLE-CURSORS ;;
;;;;;;;;;;;;;;;;;;;;;;
(use-package multiple-cursors
  :ensure t
  :bind(("C->" .     mc/mark-next-like-this)
	("C-<" .     mc/mark-previous-like-this)
	("C-c C-<" . mc/mark-all-like-this))
  )


;;;;;;;;;;;;;;;
;; helm-dash ;;
;;;;;;;;;;;;;;;
;; Dash docu system with helm support
(use-package helm-dash
  :ensure t
  :bind(("C-c d ." . helm-dash-at-point)
	("C-c d l" . helm-dash))
  :config
  (defun cpp-doc ()
    (setq-local helm-dash-docsets '("C++")))
  (add-hook 'c-mode-hook 'cpp-doc)
  (add-hook 'c++-mode-hook 'cpp-doc)
  )


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
(use-package flycheck
  :ensure t
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COMPLETION FRAMEWORKS ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The following part will add completion frameworks. There are quite a few
;; around. Since some of them can be combined, and some cannot, below defines
;; first some configuration setting variables. When the variables are set to
;; true, then the framwork is activated.
;;
(setq use-irony nil)
(setq use-company t)
(setq use-rtags nil)
(setq use-lsp t)



(when (eq use-irony t)
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
)


(when (eq use-company t)
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

  ;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Company-Quitckhelp ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Shows doxygen help as overlay for company-compeltion listing
  ;;
  ;; I.e. the same information is displayed as can be accessed by pressing <f1> if
  ;; company-completion listing is shown.
  (use-package company-quickhelp
    :ensure t
    :config
    (company-quickhelp-mode 1)
    )

  (when (eq use-irony t)
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
    )
)


(when (eq use-rtags t)
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

    ;; set different bin names
    ;; these are default not when installing rtags with apt
    (setq rtags-rc-binary-name "rtags-rc")
    (setq rtags-rdm-binary-name "rtags-rdm")
    )
)


(when (eq use-lsp t)
  ;; You still need to install language servers with M-x lsp-install-server
  ;; then select the language.
  ;;
  ;; For language servers installation see https://emacs-lsp.github.io/lsp-mode/page/languages/
  ;;
  ;; C/C++:
  ;;   Install clangd language server with with
  ;;     > apt isntall clangd
  ;;
  ;; C#:
  ;;  Install language server with M-x lsp-install-server and select server
  ;;  'csharp'
  (use-package lsp-mode
    :ensure t
    :hook ((c-mode . lsp)
	   (c++-mode . lsp)
	   (python-mode . lsp)
	   (csharp-mode . lsp)

	   ;; if you want which-key integration
	   (lsp-mode . (lambda ()
			 (let ((lsp-keymap-prefix "C-c p"))
                           (lsp-enable-which-key-integration)))))
    :init
    ;; Bernina default settings shall use flake not lsp for flycheck.
    ;; Turning of diagnostic for LSP must happend before package is loaded
    ;; otherwise its too late and will not be properly processed, i.e. LSP
    ;; remains to be the default checker.
    (when (eq use-bernina-settings t)
      (setq lsp-diagnostic-package :none))

    :config
    ;; Set prefix for lsp-command's, default "s-l", few alternatives - "C-l", "C-c l"
    ;; Don't use '(setq lsp-keymap-prefix "C-c p")' it's not working anymore
    (define-key lsp-mode-map (kbd "C-c p") lsp-command-map)

    (add-to-list 'lsp-enabled-clients 'clangd)
    (add-to-list 'lsp-enabled-clients 'omnisharp)
    (add-to-list 'lsp-enabled-clients 'rust-analyzer)

    ;; Change some lsp performance factors, see lsp-doctor and page [1]
    ;; [1]     ;; See recommendation on page https://emacs-lsp.github.io/lsp-mode/page/performance
    ;; Set garbage collector size
    (setq gc-cons-threshold 100000000)
    ;; Change data read size to 1 MB
    (setq read-process-output-max (* 1024 1024))

    ;; C#
    ;; Use csharp-ls lang server instaed of omnisharp
    (add-to-list 'lsp-disabled-clients 'omnisharp)
    (add-to-list 'lsp-enabled-clients 'csharp-ls)

    ;; python
    (add-to-list 'lsp-disabled-clients 'pyls)
    (add-to-list 'lsp-enabled-clients 'jedi)
    (add-to-list 'lsp-disabled-clients 'pylsp)

    :commands lsp lsp-deferred
    )
  (use-package lsp-ui
    :ensure t
    :commands lsp-ui-mode
    )
  (use-package helm-lsp
    :ensure t
    :commands helm-lsp-workspace-symbol
    )
  (use-package helm-xref
    :ensure t
  )
  (use-package which-key
    :ensure t
    :config
    (which-key-mode)
    )

  ;; use debugger
  ;; (use-package dap-mode
  ;;   :ensure t
  ;; )
  ;; (use-package dap-LANGUAGE)

  ;; c/c++ language server
  ;; (use-package ccls
  ;;   :ensure t
  ;;   :hook ((c-mode c++-mode) .
  ;; 	   (lambda() (require 'ccls) (lsp)))
  ;;   :config
  ;;   (with-eval-after-load "lsp-mode"
  ;;     (add-to-list 'lsp-enabled-clients 'ccls))
  ;;   )

  ;; python language server
  ;; install jedi language server with
  ;;   > pip3 install -U jedi-language-server
  (use-package lsp-jedi
    :ensure t
    :config
    (with-eval-after-load "lsp-mode"
      (add-to-list 'lsp-disabled-clients 'pyls)
      (add-to-list 'lsp-enabled-clients 'jedi)))

  ;; C# language server
  ;;;; omnisharp
  ;;;; use lsp-install-server then choose omnisharp
  ;;;;
  ;;;; csharp-ls
  ;;;; Install package with:
  ;;;;   > dotnet tool install --global csharp-ls
  ;;;; In emacs install lang server with M+x lsp-install-server then choose csharp-ls
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DAP (Debug Adapter Protocol ) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package dap-mode
  :ensure t
  )

;; DAP adapter for c++/vscode
;; see documentation https://emacs-lsp.github.io/lsp-mode/tutorials/CPP-guide/
;;
;; NB: Run M-x dap-cpptools-setup install adapter
;;
;; Run dap-debug-edit-template and finally dap-debug to start debugging.
(use-package dap-cpptools
  ;; is part of dap-mode package, hence ensure nil.
  :ensure nil
  )


;;;;;;;;;;;;;;;;
;; TYPESCRIPT ;;
;;;;;;;;;;;;;;;;
;; The default indent for most typescripts seems to be 2, so let's stick to it.
(use-package typescript-mode
  :ensure t
  :config
  (setq-default typescript-indent-level 2)
  :hook ((typescript-mode) .
	 (lambda() (setq indent-tabs-mode nil)))
)


;;;;;;;;;;;;;;;;
;; JAVASCRIPT ;;
;;;;;;;;;;;;;;;;
(setq js-indent-level 2)

(use-package ac-js2
  :ensure t
  )
(use-package js2-mode
  :ensure t
  :hook ((js-mode . js2-minor-mode)
	 (js2-mode . ac-js2-mode))
  )

;; You need to install js-beautify npm package with
;;  > npm i -g js-beautify
;; Use e.g. then function web-beautify-js to beautify JS code
(use-package web-beautify
  :ensure t
  )

;; web-beautify extension to minify JS code
;;
;; In order to use the command of this extension the uglifyjs NPM package must
;; be installed:
;;   > npm i -g uglify
;;
(load-file "~/.emacs.d/packages/web-uglifyjs/web-uglifyjs.el")


;;;;;;;;;;;;;;;;
;; POWERSHELL ;;
;;;;;;;;;;;;;;;;
(use-package powershell
  :ensure t
  )


;;;;;;;;;;;;
;; PYTHON ;;
;;;;;;;;;;;;
;; somehow when used within use-package anaconda-mode, the variable is set but
;; not correctly picked-up by anconda. Hence set it before anaconda-mode setup
(setq python-shell-interpreter "python3")

;; See https://github.com/pythonic-emacs/anaconda-mode
(use-package anaconda-mode
  :ensure t
  :config
  (add-hook 'python-mode-hook 'anaconda-mode)
  )

;; Code formatting
;; Need to install black with
;;   > pip install black
(use-package python-black

;; Bernina settings disable diagnostic mode for lsp, such that lsp isn't used
;; as default checker, therefore have to enable flycheck for python by hand
;; here.
(when (eq use-bernina-settings t)
  (add-hook 'python-mode-hook 'flycheck-mode))


  :ensure t
  :after python
  :hook (python-mode . python-black-on-save-mode-enable-dwim))


;;;;;;;;;;;;;;;
;; TERRAFORM ;;
;;;;;;;;;;;;;;;
(use-package terraform-mode
  :ensure t
  )


;;;;;;;;;;
;; HTML ;;
;;;;;;;;;;
(add-hook 'html-mode-hook
	  (lambda ()
	    (set (make-local-variable 'sgml-basic-offset) 4)))

;;;;;;;;;;
;; JAVA ;;
;;;;;;;;;;
;;
;; see meghandada documentation https://github.com/mopemope/meghanada-emacs
;;
(use-package meghanada
  :ensure t
  :hook ((java-mode) .
	 (lambda ()
	   ;; meghanada-mode on
	   (meghanada-mode t)
	   (flycheck-mode +1)
	   (setq c-basic-offset 4)
	   ;; use code format
	   (add-hook 'before-save-hook 'meghanada-code-beautify-before-save)))
  :config
  (cond
   ((eq system-type 'windows-nt)
    (setq meghanada-java-path (expandd-file-name "bin/java.exe" (getenv "JAVA_HOME")))
    (setq meghanada-maven-path "mvn.cmd"))
   (t
    (setq meghanada-java-path "java")
    (setq meghanada-maven-path "mvn")))
  )


;;;;;;;;;;
;; JSON ;;
;;;;;;;;;;
;; flymake-json requires jsonlint. Install jsonlint with
;;  > npm install -g jsonlint
;;
(use-package json-mode
  :ensure t)
(use-package flymake-json
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))
  :hook
  ((json-mode) . (lambda () (flymake-json-load))))


;;;;;;;;;;;;;
;; JENKINS ;;
;;;;;;;;;;;;;
;; Jenkins file mode
(use-package jenkinsfile-mode
  :ensure t)


;;;;;;;;;;;;
;; Qt QML ;;
;;;;;;;;;;;;
(use-package qml-mode
  :ensure t
  :hook
  ((js-mode) .
   (lambda()
    (setq indent-tabs-mode t
	  tab-width 4
	  js-indent-level 4))))


;;;;;;;;;;;;
;; GROOVY ;;
;;;;;;;;;;;;
(use-package groovy-mode
  :ensure t
  :hook
  ((groovy-mode) .
   (lambda()
     ;; BERNINA
     (setq tab-width 4))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OpenGL Shader Language ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package glsl-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.gsl\\'" . glsl-mode)))


;;;;;;;;;;
;; RUST ;;
;;;;;;;;;;
(use-package rustic
  :ensure t

  :config
  ;; Run rustfmt on save
  (setq rustic-format-on-save t)

  :custom
  ;; rust-analyzer taken from rustup apt package
  (rustic-analyzer-command '("rustup" "run" "stable" "rust-analyzer")))
