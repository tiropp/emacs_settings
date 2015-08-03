;;
;; Initial version taken from
;;    https://github.com/tuhdo/emacs-c-ide-demo
;;


;; The cedet bzr version is stored in the .emacs.d/cedet-bzr. If you don't want 
;; to use the bazaar version of cedet, but the build in version which comes with
;; emacs comment out the following few lines.
(load-file "~/.emacs.d/cedet-bzr/cedet/cedet-devel-load.el")
(load-file "~/.emacs.d/cedet-bzr/cedet/contrib/cedet-contrib-load.el")
(add-to-list 'load-path "~/.emacs.d/cedet-bzr/cedet/contrib/")
(add-to-list 'Info-directory-list "~/.emacs.d/cedet-bzr/cedet/doc/info")


(require 'cc-mode)
(require 'semantic)

(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)
(global-semantic-stickyfunc-mode 1)

(semantic-mode 1)

(defun alexott/cedet-hook ()
  (local-set-key "\C-c\C-j" 'semantic-ia-fast-jump)
  (local-set-key "\C-c\C-s" 'semantic-ia-show-summary))

(add-hook 'c-mode-common-hook 'alexott/cedet-hook)
(add-hook 'c-mode-hook 'alexott/cedet-hook)
(add-hook 'c++-mode-hook 'alexott/cedet-hook)

;; Enable EDE only in C/C++
(require 'ede)
(global-ede-mode)
