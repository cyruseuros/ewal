(require 'ewal-spacemacs-themes)
;; has to be run before loading spacemacs-common
(ewal-spacemacs-themes-get-colors :apply t :borders t :high-contrast t)
(require 'spacemacs-common)

(deftheme ewal-spacemacs-classic-high-contrast)

(create-spacemacs-theme 'dark 'ewal-spacemacs-classic-high-contrast)

(provide-theme 'ewal-spacemacs-classic-high-contrast)
