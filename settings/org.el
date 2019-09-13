(use-package org
  :defer t
  :ensure t
  :bind(("C-c l" . org-store-link)
	("C-c a" . org-agenda)
	("C-c c" . org-capture)
	("C-c b" . org-switchb))
  )
