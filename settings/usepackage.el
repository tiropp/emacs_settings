;; Bootstrap `use-package'
;;
;; The old way, requires use-package.el to be araound
;(unless (package-installed-p 'use-package)
;  (package-refresh-contents)
;  (package-install 'use-package))
;;
;;
;; This sould work even if use-package.el is not around at runtime
(eval-when-compile
  (require 'use-package))
; (use-package diminish)                ;; if you use :diminish
(use-package bind-key)                ;; if you use any :bind variant
