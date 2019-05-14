(require 'ewal-spacemacs-themes)
;; has to be run before loading spacemacs-common
(ewal-spacemacs-themes-get-colors :apply t :borders t)
(require 'spacemacs-common)

(deftheme ewal-spacemacs-classic)

(create-spacemacs-theme 'dark 'ewal-spacemacs-classic)

(provide-theme 'ewal-spacemacs-classic)
