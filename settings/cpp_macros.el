;;
;; Define macros for C++
;;

(defun insert-header(name)
  "Insert a code section header into c++ code"
  (interactive "sName: ")
  (let ((n (length name)))
    (let (( s1 (concat  "/****" (make-string n ?*) "****/\n") ))
      (indent-according-to-mode)(insert s1)
      (indent-according-to-mode)(insert "/*** " name " ***/\n")
      (indent-according-to-mode)(insert s1)
      )
    )
  )

(defun insert-namespace(name)
  "Insert namespace snippet"
  (interactive "sName: ")
  (insert "namespace " name " {\n")
  (indent-according-to-mode)
  (let ((pos (point)))
    (insert "\n} // End namespace " name)
    (goto-char pos)
    )
  )

(defun insert-guard(name)
  "Insert header guard"
  (interactive "sName: ")
  (beginning-of-line)(insert "#ifndef " name "\n#define " name "\n\n")
  (let ((pos (point)))
    (insert "\n\n#endif  // " name "\n")
    (goto-char pos)
    )
  )


;; Key map
(define-key global-map [(control c) (control i) (h)] 'insert-header)
(define-key global-map [(control c) (control i) (n)] 'insert-namespace)
(define-key global-map [(control c) (control i) (g)] 'insert-guard)
  

;; Creating a new menu pane in the menu bar to the right of “Tools” menu
(define-key-after
  global-map
  [menu-bar cpp_2]
  (cons "C++2" (make-sparse-keymap "hoot hoot"))
  'tools )
;; Creating a menu item, under the menu by the id “[menu-bar mymenu]”
(define-key
  global-map
  [menu-bar cpp_2 ih]
  '("Insert Header" . insert-header))



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
