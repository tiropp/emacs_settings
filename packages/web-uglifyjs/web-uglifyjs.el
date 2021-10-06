;;; web-uglifyjs.el --- Minimize JavaScript code
;;
;; Most code is copied from web-beautify package
(defvar web-uglifyjs-program "uglifyjs"
  "The executable to use for un-formatting JavaScript.")

(defun web-uglifyjs ()
  "Format regino if active, otherwise the current buffer.

Formatting is done according to the uglifyjs commandl"
  (interactive)
  (if (use-region-p)
      (web-uglifyjs-format-region
       web-uglifyjs-program
       (region-beginning) (region-end))
    (web-uglifyjs-buffer)))

(defun web-uglifyjs-buffer ()
  "Format the current buffer according to the uglifyjs command"
  (web-uglifyjs-format-buffer web-uglifyjs-program))

(defun web-uglifyjs-format-buffer (program)
  "By PROGRAM, format current buffer with EXTENSTION."
  (web-uglifyjs-format-region program (point-min) (point-max)))

(defun web-uglifyjs-format-region (program beginning end)
  "By PROGRAM, format each line in the BEGINNING .. END region."
  ;; Check that js-beautify is installed.
  (if (executable-find program)
      (let* ((output-buffer-name "*Web Uglify Errors*")
             (output-buffer (get-buffer-create output-buffer-name))
             ;; Stash the previous point/window positions so they can be
             ;; reclaimed after the buffer is replaced. Otherwise there is a
             ;; disturbing "jump" to vertically-center point after being
             ;; momentarily bounced to the top of the file.
             (previous-point (point))
             (previous-window-start (window-start))
             (shell-command (web-uglifyjs-get-shell-command program)))
        ;; Run the command.
        (if (zerop (shell-command-on-region beginning end shell-command (current-buffer) t output-buffer t))
            (progn
              ;; Reclaim position for a smooth transition.
              (goto-char previous-point)
              (set-window-start nil previous-window-start)
              (message "Applied web-beautify.")
              (web-beautify-reload)
              (kill-buffer output-buffer))
          ;; Unfortunately an error causes the buffer to be replaced with
          ;; emptiness... so undo that. Kind of an ugly hack. But a
          ;; properly-configured web-beautify shouldn't encounter this much, if
          ;; ever.
          (undo)
          (message (web-beautify-format-error-message output-buffer-name))))
    (message (web-beautify-command-not-found-message program))))

(defun web-uglifyjs-get-shell-command (program)
  "Join PROGRAM with the constant uglifyjs args."
  (mapconcat 'identity (append (list program) web-uglify-args) " "))

(defconst web-uglify-args '())


(provide 'web-uglifyjs)


;; Local Variables:
;; coding: utf-8
;; eval: (checkdoc-minor-mode 1)
;; End:

;;; web-uglifyjs.el ends here
