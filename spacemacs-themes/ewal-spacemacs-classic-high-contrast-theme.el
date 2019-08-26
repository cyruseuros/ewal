;; ewal-spacemacs-classic-high-contrast-theme.el --- A classic, high-contrast, `ewal'-colored take on `spacemacs-theme'.

(require 'ewal-spacemacs-themes)
;; has to be run before loading spacemacs-common
(let ((ewal-pct-shade 10)
      (spacemacs-theme-custom-colors
       (ewal-spacemacs-themes-get-colors t)))
  (require 'spacemacs-common)
  (deftheme ewal-spacemacs-classic-high-contrast)
  (create-spacemacs-theme 'dark 'ewal-spacemacs-classic-high-contrast))

(provide-theme 'ewal-spacemacs-classic-high-contrast)
;; ewal-spacemacs-classic-high-contrast-theme.el ends here
