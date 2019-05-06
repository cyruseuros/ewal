(require 'ewal-spacemacs)
;; has to be run before loading spacemacs-common
(ewal-spacemacs-get-colors :apply t)
(require 'spacemacs-common)

(deftheme ewal-spacemacs-modern)

(create-spacemacs-theme 'dark 'ewal-spacemacs-modern)

(with-eval-after-load 'ewal-spacemacs-modern-theme
  (custom-theme-set-faces
   'ewal-spacemacs-modern
   (let ((class '((class color) (min-colors 89))))
     `(line-number
       ((,class (:background ,(ewal-get-color 'background 0)))))
     `(page-break-lines
       ((,class (:background ,(ewal-get-color 'background -2))))))))

(provide-theme 'ewal-spacemacs-modern)
