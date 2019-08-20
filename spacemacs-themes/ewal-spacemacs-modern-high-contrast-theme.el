;; ewal-spacemacs-modern-high-contrast-theme.el --- A modern, high-contrast, `ewal'-colored take on `spacemacs-theme'.

(require 'ewal-spacemacs-themes)
;; has to be run before loading `spacemacs-common'
(setq spacemacs-theme-org-highlight t)
(let ((ewal-high-contrast-p t)
      (spacemacs-theme-custom-colors
       (ewal-spacemacs-themes-get-colors)))
  (require 'spacemacs-common)
  (deftheme ewal-spacemacs-modern-high-contrast)
  ;; must be run before `create-spacemacs-theme'
  (ewal-spacemacs-themes-modernize-theme)
  (create-spacemacs-theme 'dark 'ewal-spacemacs-modern-high-contrast))

(provide-theme 'ewal-spacemacs-modern-high-contrast)
;; ewal-spacemacs-modern-high-contrast-theme.el ends here
