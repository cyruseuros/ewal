(require 'ewal-spacemacs-common)

(deftheme ewal-spacemacs)
(ewal-spacemacs-theme-get-colors :apply t)
(let ((spacemacs-theme-custom-colors (ewal-spacemacs-theme-get-colors)))
  (create-spacemacs-theme 'dark 'ewal-spacemacs))
(provide-theme 'ewal-spacemacs)
