;; ewal-spacemacs-modern-high-contrast-theme.el --- A modern, high-contrast, `ewal'-colored take on `spacemacs-theme'.

(require 'ewal-spacemacs-themes)
;; has to be run before loading `spacemacs-common'
(setq spacemacs-theme-org-highlight t)
(ewal-spacemacs-themes-get-colors :apply t :high-contrast t)
(require 'spacemacs-common)

(deftheme ewal-spacemacs-modern-high-contrast)

;; must be run before `create-spacemacs-theme'
(custom-theme-set-faces
 'ewal-spacemacs-modern-high-contrast
 (let ((class '((class color) (min-colors 89))))
   `(line-number ((,class
                   (:foreground
                    ,(alist-get 'lnum spacemacs-theme-custom-colors)
                    :background
                    ,(alist-get 'bg1 spacemacs-theme-custom-colors)
                    :inherit default))))
   `(page-break-lines ((,class
                        (:foreground
                         ,(alist-get 'act2 spacemacs-theme-custom-colors)
                         :background
                         ,(alist-get 'act1 spacemacs-theme-custom-colors)))))))
(create-spacemacs-theme 'dark 'ewal-spacemacs-modern-high-contrast)

(provide-theme 'ewal-spacemacs-modern-high-contrast)
;; ewal-spacemacs-modern-high-contrast-theme.el ends here
