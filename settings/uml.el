(use-package plantuml-mode
  :ensure t
  :mode ("\\.puml\\'" . plantuml-mode)

  :config
  ;; Use 'plantuml' executable to convert text -> image
  ;; Note: flycheck-plantuml can only handle jar mode, hence this mode is used here.
  (setq plantuml-default-exec-mode 'jar)
  (setq plantuml-jar-path (getenv "PLANTUML_JAR"))

  ;; Export per default as SVG
  (setq plantuml-output-type "svg")

  ;; Set indentation
  (setq plantuml-indent-level 2)

  ;; Set preview to C-C C-p shortcut (per default it is set to C-c C-c which is already used)
  (define-key plantuml-mode-map (kbd "C-c C-p") #'plantuml-preview)
  )

(use-package flycheck-plantuml
  :ensure t
  :config
  ;; Load flyckec with plantuml-mode
  (add-hook 'plantuml-mode-hook
	    (lambda ()
	      (flycheck-plantuml-setup)
	      (flycheck-mode 1)))
  )
