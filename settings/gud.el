;;
;; Set up grand unified debugger (GUD)
;;
;; Org code found at
;;   https://groups.google.com/forum/#!topic/gnu.emacs.help/kYjRU9NmDOI
;;


;; 
;; Define a keymap with commonly used Gud commands.
;; This is likely to evolve.
(defvar yk-gud-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "b" 'gud-break)
    (define-key map "c" 'gud-cont)
    (define-key map "d" 'gud-down)
    (define-key map "f" 'gud-finish)
    (define-key map "n" 'gud-next)
    (define-key map "s" 'gud-step)
    (define-key map "u" 'gud-up)
    (define-key map "v" 'gdb-display-locals-buffer)
    (define-key map "j" 'gud-jump)
    (define-key map "l" 'gud-refresh)
    (define-key map "p" 'gud-print)
    (define-key map "." 'gud-until)
    (define-key map "<" 'gud-up)
    (define-key map ">" 'gud-down)
    (define-key map "t" 'gud-tbreak)
    map))

;; Define a buffer-local minor mode using the above keymap.
(define-minor-mode yk-gud-mode nil
  :lighter " Gud"
  :keymap yk-gud-mode-map
  (if yk-gud-mode (yk-gud-mode--enable) (yk-gud-mode--disable)))

;; When the mode is enabled, remember the read-only status
;; of the buffer, and lock it down.
(defun yk-gud-mode--enable ()
  (set (make-local-variable 'yk-gud-read-only) buffer-read-only)
  (setq buffer-read-only t))

;; Restore the read-only status when disabling. (Can’t unlock it
;; unconditionally as some sources come from /usr/include and other
;; read-only locations.)
(defun yk-gud-mode--disable ()
  (when (local-variable-p 'yk-gud-read-only)
    (setq buffer-read-only yk-gud-read-only)))

;; This solves the immediate problem of uncomfortable keybindings.
;; Now we want it to be automatically enable for all source buffers.
;; Gud has a hook that runs when debugging is started.
(add-hook 'gdb-mode-hook 'yk-gud--started)

;; In this hook, we set up “find-file-hook” so that buffers
;; opened while debugging start out with our mode enabled…
(defun yk-gud--started ()
  (add-hook 'find-file-hook 'yk-gud--find-file))

;; … if needed. The variable “gdb-source-file-list” contains
;; the list of file paths that Gud considers to be sources of
;; the program. We use it to determine whether to enable our mode.
(defun yk-gud--find-file ()
  (when (member (buffer-file-name) gdb-source-file-list)
    (yk-gud-mode)))

;; The other half of the startup issue is enabling the mode
;; for buffers that are already open by the time Gud is started.
;; Unfortunately, “gdb-mode-hook” runs before “gdb-source-file-list”
;; is populated, so we have to resort to an advice.
;; “gdb-init-buffer” sets up source buffers so that clicking
;; in the fringe sets and deletes breakpoints, so it’s a good place
;; for us too.
(defadvice gdb-init-buffer (after yk-gdb-init-buffer ())
  (yk-gud-mode))
(ad-activate 'gdb-init-buffer)

;; Now, there’s another problem. When we exit the debugger, files
;; remain locked. We need to disable our mode. Gud does not have
;; a suitable hook, so we advice “gdb-reset”.
(defadvice gdb-reset (after yk-gdb-reset ())
  (yk-gud--disable-all))
(ad-activate 'gdb-reset)

(defun yk-gud--disable-all ()
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (yk-gud-mode 0))))

;; And a further annoyance: the state of our minor mode is stored
;; in the desktop file, while the state of Gud is not. So if we exit
;; Emacs in the middle of debugging and then restart it, the source
;; buffers end up read-only for no valid reason.
;; It was easiest to just disable the mode for all buffers after
;; the desktop is loaded.
(add-hook 'desktop-after-read-hook 'yk-gud--disable-all) 
