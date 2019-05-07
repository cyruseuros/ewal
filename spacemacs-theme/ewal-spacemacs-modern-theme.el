(require 'ewal-spacemacs)
;; has to be run before loading `spacemacs-common'
(ewal-spacemacs-get-colors :apply t)
(require 'spacemacs-common)

(deftheme ewal-spacemacs-modern)

;; must be run before `create-spacemacs-theme'
(custom-theme-set-faces
 'ewal-spacemacs-modern
 (let ((class '((class color) (min-colors 89))))
   `(line-number ((,class
                   (:foreground ,(alist-get 'lnum spacemacs-theme-custom-colors)
                    :background ,(alist-get 'bg1 spacemacs-theme-custom-colors)
                    :inherit default))))))
(create-spacemacs-theme 'dark 'ewal-spacemacs-modern)

(provide-theme 'ewal-spacemacs-modern)
