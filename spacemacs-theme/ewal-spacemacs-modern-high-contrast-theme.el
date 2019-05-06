(require 'ewal-spacemacs)
;; has to be run before loading spacemacs-common
(ewal-spacemacs-get-colors :apply t :high-contrast t)
(require 'spacemacs-common)

(deftheme ewal-spacemacs-modern-high-contrast)

(create-spacemacs-theme 'dark 'ewal-spacemacs-modern-high-contrast)
(with-eval-after-load 'ewal-spacemacs-modern-high-contrast-theme
  (custom-theme-set-faces
   'ewal-spacemacs-modern-high-contrast
   (let ((class '((class color) (min-colors 89))))
     `(line-number
       ((,class (:background ,(ewal-get-color 'background 0))))))))

(provide-theme 'ewal-spacemacs-modern-high-contrast)
