;;
;; Define macros for C++
;;

;;
;; cpp-insert-section
;;
(defun cpp-insert-section-header(name)
  "Insert a code section header into c++ code"
  (interactive
   (list (read-from-minibuffer "Section: " nil nil nil 'cpp-insert-section-history)))
  (let ((n (length name)))
    (let (( s1 (concat  "/****" (make-string n ?*) "****/\n") ))
	 (indent-according-to-mode)(insert s1)
	 (indent-according-to-mode)(insert "/*** " name " ***/\n")
	 (indent-according-to-mode)(insert s1)
	 )
    )
  )


;;
;; cpp-insert-namespace
;;
(defun cpp-do-insert-one-namespace(name)
  "Insert namespace snippet"
  (insert "namespace " name " {\n")
  ;; No indentation done here, because namespace "should" not be indented anyway
  ;; at least not in the current style I like.
  ;;   (indent-according-to-mode)
  (let ((pos (point)))
    (insert "\n} // End namespace " name)
    (goto-char pos)))
(defun cpp-do-insert-namespaces(names)
  "Snippet to insert several namespace"
  (let ((name-tokens (split-string names "\\.")))
    (let ((name (car name-tokens)))
      (while name-tokens
	(let ((name (car name-tokens)))
	  (cpp-do-insert-one-namespace name))
	(setq name-tokens (cdr name-tokens))))))
(defun cpp-do-insert-unnamed-namespace()
  "Snippet to insert an unnamed namespace"
  (insert "namespace {\n")
  (let ((pos (point)))
    (insert "\n} // End unnamed namespace ")
    (goto-char pos)
    ))
(defun cpp-insert-namespace(names)
  "Interactively insert namespace snippet

It is possible to specify several namespace wihtin one command by separating
namespace by a '.'. E.g. using NAME equal to 'abc.def' would result in
  namespace abc {
  namespace def {
  } // End namespace def
  } // End namespace abc
"
  (interactive
   (list (read-from-minibuffer "Namespace: " nil nil nil 'cpp-insert-namespace-history)))
  (if (> (length names) 0)
      (cpp-do-insert-namespaces names)
    (cpp-do-insert-unnamed-namespace)
    ))

;;
;; cpp-insert-header-guard
;;
(defun cpp-insert-header-guard(name)
  "Insert header guard"
  (interactive
   (list (read-from-minibuffer "Header guard: " nil nil nil 'cpp-insert-header-guard-history)))
  (beginning-of-line)(insert "#ifndef " name "\n#define " name "\n\n")
  (let ((pos (point)))
    (insert "\n\n#endif  // " name "\n")
    (goto-char pos)
    )
  )

;;
;; insert-file-name-without-ending
;;
(defun cpp-insert-file-name-without-ending()
  "Insert file name of current buffer without file ending"
  (interactive
   (insert (file-name-base (buffer-file-name))))
  )


;;
;; cpp-macro-keys
;;
(defun cpp-macro-keys()
  "Setup C++ macro keys and menus"
  ;; Key map
  (define-key c-mode-base-map [(control n) (h)] 'cpp-insert-section-header)
  (define-key c-mode-base-map [(control n) (n)] 'cpp-insert-namespace)
  (define-key c-mode-base-map [(control n) (g)] 'cpp-insert-header-guard)
  (define-key c-mode-base-map [(control n) (f)] 'cpp-insert-file-name-without-ending)

  ;; Creating a new menu pane 'C++2' in the menu bar
  (let ((menuMap (make-sparse-keymap "C++2")))  
    (define-key c-mode-base-map [menu-bar cpp_2] (cons "C++2" menuMap))
    (define-key menuMap [ih]
      '("Insert Section Header" . cpp-insert-section-header))
    (define-key menuMap [in]
      '("Insert Namespace" . cpp-insert-namespace))
    (define-key menuMap [ig]
      '("Insert Header Guard" . cpp-insert-header-guard))
    (define-key menuMap [if]
      '("Insert Filename". cpp-insert-file-name-without-ending))))

(add-hook 'c-mode-common-hook
	  ;; Set the tab size to 4, same as indent size
	  (lambda()
	    (setq tab-width 4)
	    (cpp-macro-keys)
	    )
	  )

;;;;;;;;;
;; OLD ;;
;;;;;;;;;
;; ;;;; here are the headers to be inserted:
;; (defvar header1 
;;     "/****************************************************************************/"
;;     "/****************************************************************************/"
;; )
;; ;;;; here are auxiliary functions to really insert the variables from above
;; (defun insert-header1()
;;     (interactive)
;;     (insert  header1))
;; ;;;; here are the key bindings
;; (global-set-key "\C-h1" 'insert-header1)

