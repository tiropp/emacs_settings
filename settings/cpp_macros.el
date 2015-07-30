;;
;; Define macros for C++
;;
(defun cpp-insert-section-header(name)
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

(defun cpp-insert-namespace(name)
  "Insert namespace snippet"
  (interactive "sName: ")
  (insert "namespace " name " {\n")
  (indent-according-to-mode)
  (let ((pos (point)))
    (insert "\n} // End namespace " name)
    (goto-char pos)
    )
  )

(defun cpp-insert-header-guard(name)
  "Insert header guard"
  (interactive "sName: ")
  (beginning-of-line)(insert "#ifndef " name "\n#define " name "\n\n")
  (let ((pos (point)))
    (insert "\n\n#endif  // " name "\n")
    (goto-char pos)
    )
  )

;; Key map
(define-key c-mode-base-map [(control n) (h)] 'cpp-insert-section-header)
(define-key c-mode-base-map [(control n) (n)] 'cpp-insert-namespace)
(define-key c-mode-base-map [(control n) (g)] 'cpp-insert-header-guard)


;; Creating a new menu pane 'C++2' in the menu bar
(let ((menuMap (make-sparse-keymap "C++2")))  
  (define-key c-mode-base-map [menu-bar cpp_2] (cons "C++2" menuMap))
  (define-key menuMap [ih]
    '("Insert Section Header" . cpp-insert-section-header))
  (define-key menuMap [in]
    '("Insert Namespace" . cpp-insert-namespace))
  (define-key menuMap [ig]
    '("Insert Header Guard" . cpp-insert-header-guard)))
    

(add-hook 'c-mode-common-hook
	  ;; Set the tab size to 4, same as indent size
	  (lambda()
	    (setq tab-width 4)
	    ))

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
