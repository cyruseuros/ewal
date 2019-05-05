(require 'ewal-spacemacs)
;; has to be run before loading spacemacs-common
(ewal-spacemacs-get-colors :apply t)
(require 'spacemacs-common)

(deftheme ewal-spacemacs-modern)

(create-spacemacs-theme 'dark 'ewal-spacemacs-modern)

;; (custom-theme-set-faces
;;  'ewal-spacemacs-modern
;;  (let ((class '((class color) (min-colors 89))))
;;    `(page-break-lines ((,class (:background ,(ewal-get-color 'background -2)))) t)
;;    `(line-number ((,class (:background ,(ewal-get-color 'background 0)))) t)))

(provide-theme 'ewal-spacemacs-modern)
