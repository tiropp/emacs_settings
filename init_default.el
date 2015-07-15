;;;;;;;;;;;
;; CEDET ;;
;;;;;;;;;;;
;; Should be first, in order to ensure that not an other package loads a package
;; cedet needs with a different version. It is quite picky!
(load-file "~/.emacs.d/settings/cedet.el")

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

;;;;;;;;;
;; C++ ;;
;;;;;;;;;
(load-file "~/.emacs.d/settings/cpp.el")
(load-file "~/.emacs.d/settings/cpp_macros.el");

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
;; SSH ;;
;;;;;;;;;
(load-file "~/.emacs.d/settings/ssh.el")

;;;;;;;;;;;
;; CMAKE ;;
;;;;;;;;;;;
(load-file "~/.emacs.d/settings/cmake.el")


