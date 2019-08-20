;; ewal-spacemacs-modern-theme.el --- A modern, `ewal'-colored take on `spacemacs-theme'.

(require 'ewal-spacemacs-themes)
;; has to be run before loading `spacemacs-common'
(let ((spacemacs-theme-org-highlight t)
      (spacemacs-theme-custom-colors
       (ewal-spacemacs-themes-get-colors)))
  (require 'spacemacs-common)

  (deftheme ewal-spacemacs-modern)

  ;; must be run before `create-spacemacs-theme'
  (custom-theme-set-faces
  'ewal-spacemacs-modern
  (let ((class '((class color) (min-colors 89))))
    `(line-number
      ((,class
        (:foreground ,(ewal--get-color 'comment 0)
          :background ,(ewal--get-color 'background 0)
          :inherit default))))
    `(page-break-lines
      ((,class
        ;; depends on custom variable
        (:foreground ,(alist-get 'act2 spacemacs-theme-custom-colors)
          :background ,(alist-get 'act1 spacemacs-theme-custom-colors)))))))
  (create-spacemacs-theme 'dark 'ewal-spacemacs-modern))

(provide-theme 'ewal-spacemacs-modern)
;; ewal-spacemacs-modern-theme.el ends here
