;; Report startup time and garbage collections
;; Use a hook so the message doesn't get clobbered by other messages.
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; Load first use-packge, it is needed by almost all other packages
(load-file "~/.emacs.d/settings/usepackage.el")

;;;;;;;;;;;
;; CEDET ;;
;;;;;;;;;;;
;; Should be first, in order to ensure that not an other package loads a package
;; cedet needs with a different version. It is quite picky!
;;
;; Use gtags instead of cedet for code completion
;; (load-file "~/.emacs.d/settings/cedet.el")

;;;;;;;;;;;;;;;;;;;;;;;
;; GLOBAL SHORT CUTS ;;
;;;;;;;;;;;;;;;;;;;;;;;
(load-file "~/.emacs.d/settings/global_shortcuts.el")

;;;;;;;;;;;;;;;;
;; APPEARANCE ;;
;;;;;;;;;;;;;;;;
(load-file "~/.emacs.d/settings/appearance.el")

;;;;;;;;;;;;;;;;;;;;;
;; GLOBAL SETTINGS ;;
;;;;;;;;;;;;;;;;;;;;;
(load-file "~/.emacs.d/settings/global_settings.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GENERAL PROGRAMMING SETTINGS ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-file "~/.emacs.d/settings/programming.el")

;;;;;;;;;
;; C++ ;;
;;;;;;;;;
(load-file "~/.emacs.d/settings/cpp.el")
(load-file "~/.emacs.d/settings/cpp_macros.el");
(load-file "~/.emacs.d/settings/gud.el");

;;;;;;;;;;;;;;;;
;; TEX, LATEX ;;
;;;;;;;;;;;;;;;;
(load-file "~/.emacs.d/settings/tex.el")

;;;;;;;;;;
;; BASH ;;
;;;;;;;;;;
(load-file "~/.emacs.d/settings/bash.el")

;;;;;;;;
;; C# ;;
;;;;;;;;
(load-file "~/.emacs.d/settings/csharp.el")

;;;;;;;;;
;; ELM ;;
;;;;;;;;;
(load-file "~/.emacs.d/settings/elm.el")

;;;;;;;;;;;;;
;; HASKELL ;;
;;;;;;;;;;;;;
(load-file "~/.emacs.d/settings/haskell.el")

;;;;;;;;;
;; SSH ;;
;;;;;;;;;
(load-file "~/.emacs.d/settings/ssh.el")

;;;;;;;;;;;
;; CMAKE ;;
;;;;;;;;;;;
(load-file "~/.emacs.d/settings/cmake.el")

;;;;;;;;;;;;;;
;; MARKDOWN ;;
;;;;;;;;;;;;;;
(load-file "~/.emacs.d/settings/markdown.el")

;;;;;;;;;
;; ORG ;;
;;;;;;;;;
(load-file "~/.emacs.d/settings/org.el")

;;;;;;;;;;
;; XAML ;;
;;;;;;;;;;
(load-file "~/.emacs.d/settings/xaml.el")

;;;;;;;;;;
;; YAML ;;
;;;;;;;;;;
(load-file "~/.emacs.d/settings/yaml.el")

;;;;;;;;;;;;;;;;;
;; DOCKER FILE ;;
;;;;;;;;;;;;;;;;;
(load-file "~/.emacs.d/settings/dockerfile.el")
