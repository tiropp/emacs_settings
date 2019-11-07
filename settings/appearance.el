;;
;; Setup appearance of emacs
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SET DEFAULT FONT TO "Misc 7x13" ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   you can get the font currently using by M-x describe-font
;; (set-face-font 'default "-misc-fixed-medium-r-normal--13-*-*-*-c-70-iso8859-1")
;;
;; Nicer font
(if (eq system-type 'gnu/linux)
    (progn
      (cond ((find-font (font-spec :name "Source Code Pro-12"))
	     (setq default-frame-alist '((font . "Source Code Pro-12"))))
	    ((find-font (font-spec :name "Inconsolata-12"))
	     (setq default-frame-alist '((font . "Inconsolata-12"))))
	    )
      )
  )

;; NB: If font is not installed, emacs may crash. This was observed under Windwos.


;;;;;;;;;;;;;;;;;;;;;;;;;
;; REMOVE THE TOOL BAR ;;
;;;;;;;;;;;;;;;;;;;;;;;;;
(tool-bar-mode -1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CHANGE BACKGROUND OF MARKED REGION ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(set-face-attribute 'region nil :background "#6699FF" :foreground "#000000")


;;;;;;;;;;;;;;;;;;;;;;
;; DON'T DISPLAY ^M ;;
;;;;;;;;;;;;;;;;;;;;;;
;; Taken from http://stackoverflow.com/questions/730751/hiding-m-in-emacs
;; originally from Johan Bockgård
(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))



;;;;;;;;;;;;;;
;; MODELINE ;;
;;;;;;;;;;;;;;
;;
;; POWERLINE
;;
;; This theme is based on powerline-default-them
;;
;; The only change basically is that the line/column numbers are moved to the
;; left side, since when the window is too small one cannot see them anymore.
;;
(defun powerline-my-theme ()
  "Setup the default mode-line."
  (interactive)
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (mode-line-buffer-id (if active 'mode-line-buffer-id 'mode-line-buffer-id-inactive))
                          (mode-line (if active 'mode-line 'mode-line-inactive))
                          (face0 (if active 'powerline-active0 'powerline-inactive0))
                          (face1 (if active 'powerline-active1 'powerline-inactive1))
                          (face2 (if active 'powerline-active2 'powerline-inactive2))
                          (separator-left (intern (format "powerline-%s-%s"
                                                          (powerline-current-separator)
                                                          (car powerline-default-separator-dir))))
                          (separator-right (intern (format "powerline-%s-%s"
                                                           (powerline-current-separator)
                                                           (cdr powerline-default-separator-dir))))
                          (lhs (list (powerline-raw "%*" face0 'l)
                                     (when powerline-display-buffer-size
                                       (powerline-buffer-size face0 'l))
                                     (when powerline-display-mule-info
                                       (powerline-raw mode-line-mule-info face0 'l))
                                     (powerline-buffer-id `(mode-line-buffer-id ,face0) 'l)

				     ;; line/column numbers
                                     (unless window-system
                                       (powerline-raw (char-to-string #xe0a1) face0 'l))
                                     (powerline-raw "%l" face0 'l)
                                     (powerline-raw ":" face0)
                                     (powerline-raw "%c" face0)

				     ;; "arrow"
                                     (when (and (boundp 'which-func-mode) which-func-mode)
                                       (powerline-raw which-func-format face0 'l))
                                     (powerline-raw " " face0)
				     (funcall separator-left face0 face1)
                                     (when (and (boundp 'erc-track-minor-mode) erc-track-minor-mode)
                                       (powerline-raw erc-modified-channels-object face1 'l))

				     ;; modes
                                     (powerline-major-mode face1 'l)
                                     (powerline-process face1)
                                     (powerline-minor-modes face1 'l)
                                     (powerline-narrow face1 'l)
                                     (powerline-raw " " face1)
                                     (funcall separator-left face1 face2)
                                     (powerline-vc face2 'r)
                                     (when (bound-and-true-p nyan-mode)
                                       (powerline-raw (list (nyan-create)) face2 'l))))
                          (rhs (list (powerline-raw global-mode-string face2 'r)
                                     (funcall separator-right face2 face1)
                                     (funcall separator-right face1 face0)
                                     (powerline-raw " " face0)
                                     (powerline-raw "%6p" face0 'r)
                                     (when powerline-display-hud
                                       (powerline-hud face0 face2))
                                     (powerline-fill face0 0)
                                     )))
                     (concat (powerline-render lhs)
                             (powerline-fill face2 (powerline-width rhs))
                             (powerline-render rhs)))))))

(use-package powerline
  :ensure t
  :init (powerline-my-theme)
  )


;; Change the default git string in mode line from
;;   Git:master
:: to
::   git-branch-symbol(f126 in awsome-font):master
;;
(defun my-vc-git-mode-line-string (orig-fn &rest args)
  "Replace Git in modeline with font-awesome git icon via ORIG-FN and ARGS."
  (let ((str (apply orig-fn args)))
    (concat [#xF126] ":" (substring-no-properties str 4))))
(advice-add #'vc-git-mode-line-string :around #'my-vc-git-mode-line-string)



;;;;;;;;;;;;;;;;;
;; Color-Theme ;;
;;;;;;;;;;;;;;;;;
;;;; color-theme-solarized
;(use-package emacs-color-theme-solarized
;  :config
;  (load-theme 'solarized t))
	     
;;;; color-theme-sanityinc-tomorrow
;; (use-package color-theme-sanityinc-tomorrow
;;   :ensure t
;;   :config
;;   (load-theme 'sanityinc-tomorrow-night t))

;;;; grandshell-theme
;; (use-package grandshell-theme
;;    :ensure t
;;    :config
;;    (load-theme 'grandshell t))

;;;; cobalt-theme
;;;; from: http://emacsthemes.caisah.info/cobalt-theme/
;; (use-package color-theme
;;    :ensure t
;;    :config
;;    (color-theme-initialize))
;; (use-package color-theme-cobalt
;;    :load-path "plugins/"
;;    :config
;;    (color-theme-cobalt))

;;;; spacemacs
(use-package spacemacs-theme
  :ensure t
  :defer t
  :init (load-theme 'spacemacs-dark t)
  )


