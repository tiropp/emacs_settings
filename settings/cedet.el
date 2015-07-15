;; Initial version based on 
;;    https://github.com/alexott/emacs-configs/blob/master/rc/emacs-rc-cedet.el
;;
;; See also Alex's article
;;    http://alexott.net/en/writings/emacs-devenv/EmacsCedet.html
;;
;; 
;; DEBUGGING:
;;   For debugging of semantic do:
;;     1. Load the library: M-x load-library semantic/analyze/debug
;;     2. Turn on debugging for semantic: M-x semantic-analyze-debug-assit
;;
;;


;; The cedet bzr version is stored in the .emacs.d/cedet-bzr. If you don't want 
;; to use the bazaar version of cedet, but the build in version which comes with
;; emacs comment out the following few lines.
(load-file "~/.emacs.d/cedet-bzr/trunk/cedet-devel-load.el")
(load-file "~/.emacs.d/cedet-bzr/trunk/contrib/cedet-contrib-load.el")
(add-to-list 'load-path "~/.emacs.d/cedet-bzr/trunk/contrib/")
(add-to-list 'Info-directory-list "~/.emacs.d/cedet-bzr/trunk/doc/info")


;;;;;;;;;;;;;;;;;;;;;;;
;; CEDET minor modes ;;
;;;;;;;;;;;;;;;;;;;;;;;
;; Enables global support for Semanticdb
(add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)

;; Enables automatic bookmarking of tags that you edited, so you can return to 
;; them later with the semantic-mrub-switch-tags (M-x B) command, e.g. after a
;; jump (semantic-ia-fast-jump)
(add-to-list 'semantic-default-submodes 'global-semantic-mru-bookmark-mode)

;; activates CEDET's context menu that is bound to right mouse button
;;(add-to-list 'semantic-default-submodes 'global-cedet-m3-minor-mode)

;; Activates automatic parsing of source code in the idle time
(add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)


;; Activates displaying of possible name completions in the idle time. Requires
;;  that global-semantic-idle-scheduler-mode was enabled;
(add-to-list 'semantic-default-submodes 'global-semantic-idle-completions-mode)

;; Activates displaying of information about current tag in the idle time. 
;; Requires that global-semantic-idle-scheduler-mode was enabled.
(add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode)


;; Activates mode when name of current tag will be shown in top line of buffer;
(add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)

;; Activates highlighting of first line for current tag (function, class, 
;; etc.);
(add-to-list 'semantic-default-submodes 'global-semantic-highlight-func-mode)

;; Activates use of separate styles for tags decoration (depending on tag's
;; class). These styles are defined in the semantic-decoration-styles list.
;(add-to-list 'semantic-default-submodes 'global-semantic-decoration-mode)

;; Activates highlighting of local names that are the same as name of tag under
;; cursor.
(add-to-list 'semantic-default-submodes 'global-semantic-idle-local-symbol-highlight-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CEDET minor modes useful for debugging ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(add-to-list 'semantic-default-submodes 'global-semantic-show-unmatched-syntax-mode)
;;(add-to-list 'semantic-default-submodes 'global-semantic-highlight-edits-mode)
;;(add-to-list 'semantic-default-submodes 'global-semantic-show-parser-state-mode)



;;;;;;;;;;;;;;;;;;;;;;;
;; ACTIVATE PACKAGES ;;
;;;;;;;;;;;;;;;;;;;;;;;
;; Activate semantic
(semantic-mode 1)

(require 'semantic/bovine/c)
(require 'semantic/bovine/clang)

(require 'cedet-files)
(require 'srecode)

;; loading contrib packages
(require 'eassist)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CUSTOMISATION OF MODES ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun alexott/cedet-hook ()
  (local-set-key [(control return)]  'semantic-ia-complete-symbol)
  (local-set-key "\C-c?"             'semantic-ia-complete-symbol-menu)
  ;;
  (local-set-key "\C-c>"             'semantic-complete-analyze-inline)
  (local-set-key "\C-c="             'semantic-decoration-include-visit)

  ;; Use C-x B to jump back
  (local-set-key "\C-cj"             'semantic-ia-fast-jump)
  (local-set-key "\C-cq"             'semantic-ia-show-doc)
  (local-set-key "\C-cs"             'semantic-ia-show-summary)
  (local-set-key "\C-cp"             'semantic-analyze-proto-impl-toggle)
;; (local-set-key (kbd "C-c <left>") 'semantic-tag-folding-fold-block)
;; (local-set-key (kbd "C-c <right>") 'semantic-tag-folding-show-block)

  ;**; (add-to-list 'ac-sources 'ac-source-semantic)
  )
;; (add-hook 'semantic-init-hooks 'alexott/cedet-hook)
(add-hook 'c-mode-common-hook   'alexott/cedet-hook)
(add-hook 'lisp-mode-hook       'alexott/cedet-hook)
(add-hook 'scheme-mode-hook     'alexott/cedet-hook)
(add-hook 'emacs-lisp-mode-hook 'alexott/cedet-hook)
(add-hook 'erlang-mode-hook     'alexott/cedet-hook)

(defun alexott/c-mode-cedet-hook ()
  (local-set-key "." 'semantic-complete-self-insert)
  (local-set-key ">" 'semantic-complete-self-insert)
  (local-set-key "\C-ct" 'eassist-switch-h-cpp)
  (local-set-key "\C-xt" 'eassist-switch-h-cpp)
  (local-set-key "\C-ce" 'eassist-list-methods)
  (local-set-key "\C-c\C-r" 'semantic-symref)

  ;**; (add-to-list 'ac-sources 'ac-source-gtags)
  )
(add-hook 'c-mode-common-hook 'alexott/c-mode-cedet-hook)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CUSTOMIZATION OF SEMANTIC DB ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;**; (when (cedet-gnu-global-version-check t)
;**;   (semanticdb-enable-gnu-global-databases 'c-mode t)
;**;   (semanticdb-enable-gnu-global-databases 'c++-mode t))

;**; (when (cedet-ectag-version-check t)
;**;   (semantic-load-enable-primary-ectags-support))

;; SRecode
(global-srecode-minor-mode 1)

;; Add semantics to imenus
(defun my-semantic-hook ()
  (imenu-add-to-menubar "TAGS"))
(add-hook 'semantic-init-hooks 'my-semantic-hook)



;;;;;;;;;
;; EDE ;;
;;;;;;;;;
(global-ede-mode 1)                      ; Enable the Project management system
(semantic-load-enable-code-helpers)      ; Enable prototype help and smart completion 

;; Use ede-new to create a new project



;; helper for boost setup...
(defun c++-setup-boost (boost-root)
  (when (file-accessible-directory-p boost-root)
    (let ((cfiles (cedet-files-list-recursively boost-root "\\(config\\|user\\)\\.hpp")))
      (dolist (file cfiles)
        (add-to-list 'semantic-lex-c-preprocessor-symbol-file file)))))


;; my functions for EDE
(defun alexott/ede-get-local-var (fname var)
  "fetch given variable var from :local-variables of project of file fname"
  (let* ((current-dir (file-name-directory fname))
         (prj (ede-current-project current-dir)))
    (when prj
      (let* ((ov (oref prj local-variables))
            (lst (assoc var ov)))
        (when lst
          (cdr lst))))))

;; setup compile package
(require 'compile)
(setq compilation-disable-input nil)
(setq compilation-scroll-output t)
(setq mode-compile-always-save-buffer-p t)

(defun alexott/compile ()
  "Saves all unsaved buffers, and runs 'compile'."
  (interactive)
  (save-some-buffers t)
  (let* ((fname (or (buffer-file-name (current-buffer)) default-directory))
(current-dir (file-name-directory fname))
         (prj (ede-current-project current-dir)))
    (if prj
(project-compile-project prj)
(compile compile-command))))
(global-set-key [f9] 'alexott/compile)

;;
(defun alexott/gen-std-compile-string ()
  "Generates compile string for compiling CMake project in debug mode"
  (let* ((current-dir (file-name-directory
                       (or (buffer-file-name (current-buffer)) default-directory)))
         (prj (ede-current-project current-dir))
         (root-dir (ede-project-root-directory prj)))
    (concat "cd " root-dir "; make -j2")))

;;
(defun alexott/gen-cmake-debug-compile-string ()
  "Generates compile string for compiling CMake project in debug mode"
  (let* ((current-dir (file-name-directory
                       (or (buffer-file-name (current-buffer)) default-directory)))
         (prj (ede-current-project current-dir))
         (root-dir (ede-project-root-directory prj))
         (subdir "")
         )
    (when (string-match root-dir current-dir)
      (setf subdir (substring current-dir (match-end 0))))
    (concat "cd " root-dir "Debug/" "; make -j3")))



;;;;;;;;;;;;;;;;;;
;; Autocomplete ;;
;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/.emacs.d/elpa/auto-complete-20150618.1949")
(add-to-list 'load-path "~/.emacs.d/elpa/popup-20150626.711")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories (expand-file-name
             "~/.emacs.d/elpa/auto-complete-20150618.1949/dict"))
(setq ac-comphist-file (expand-file-name
             "~/.emacs.d/ac-comphist.dat"))
(ac-config-default)


;;; Projects
;;
;; EXAMPLES
;;
;; cpp-tests project definition
;; (when (file-exists-p "~/projects/lang-exp/cpp/CMakeLists.txt")
;;   (setq cpp-tests-project
;; 	(ede-cpp-root-project "cpp-tests"
;; 			      :file "~/projects/lang-exp/cpp/CMakeLists.txt"
;; 			      :system-include-path '("/home/ott/exp/include"
;; 						     boost-base-directory)
;; 			      :compile-command "cd Debug && make -j2"
;; 			      )))
;;
;; (when (file-exists-p "~/projects/squid-gsb/README")
;;   (setq squid-gsb-project
;; 	(ede-cpp-root-project "squid-gsb"
;; 			      :file "~/projects/squid-gsb/README"
;; 			      :system-include-path '("/home/ott/exp/include"
;; 						     boost-base-directory)
;; 			      :compile-command "cd Debug && make -j2"
;; 			      )))
;;

