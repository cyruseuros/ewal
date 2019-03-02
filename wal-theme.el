;;; wal-theme --- A `wal'-based automatic, terminal aware generator.
;;; Commentary: We can't let `Vim' be the only one to have it.
;;; Code:
(defvar wal-theme-own-cache-dir "./cache"
  "Location of cached wal theme in json format.")
(defvar wal-theme-wal-cache-json "~/.cache/wal/colors.json"
  "Location of cached wal theme in json format.")

(defvar wal-theme-base-palette nil "Unmodified colors extracted directly from pywal, stored in a flat alist.")
(defvar wal-theme-full-gui-colors nil "Full `wal-theme', gui-enabled palette, stored in a flat alist.")
(defvar wal-theme-full-tty-colors nil "Full `wal-theme' tty-compatible palette, stored in a flat alist.")

(defun wal-theme--wal-cache-json-load (&optional json)
  "Read JSON as the most complete of the stored files."
  (let ((json (or json wal-theme-wal-cache-json))
        (json-object-type 'alist)
        (colors (json-read-file wal-theme-wal-cache-json))
        (canonical-color-names '(black red green yellow blue magenta cyan
                                       white)))
    (let ((special-colors (alist-get 'special colors))
          (regular-colors (alist-get 'colors colors)))
      (let ((regular-color-values (cl-loop for
                                           (key . value)
                                           in
                                           regular-colors
                                           collect
                                           value)))
        (let ((cannonical-colors (pairlis canonical-color-names regular-color-values)))
          (append special-colors cannonical-colors))))))

(setq wal-theme-base-palette (wal-theme--wal-cache-json-load))
;; (insert (format "%s" wal-theme-base-colors))
;; ((background . #263238)
;;  (foreground . #eceff1)
;;  (cursor . #ffa74d)
;;  (black . #263238)
;;  (red . #ff9800)
;;  (green . #8bc34a)
;;  (yellow . #ffc107)
;;  (blue . #03a9f4)
;;  (magenta . #e91e63)
;;  (cyan . #009688)
;;  (white . #cfd8dc))

(defun wal-theme--extend-base-color (color num-degrees degree-size)
  "Extends darkens (-) or lightens (+) COLOR.
Does so by NUM-DEGREES, in increments of DEGREE-SIZE percentage
points. Returns list of extended colors"
  (let ((extended-color-list ()))
    (dotimes (i num-degrees extended-color-list)
      (add-to-list 'extended-color-list
                   (color-darken-name color
                                      (* i degree-size))))
    (add-to-list 'extended-color-list color t)
    (dotimes (i num-degrees extended-color-list)
      (add-to-list 'extended-color-list
                   (color-lighten-name color
                                      (* i degree-size))
                   t))))


(defun wal-theme--extend-base-palette (num-degrees degree-size &optional palette)
  "Extends darkens (-) or lightens (+) PALETTE.
Does so by NUM-DEGREES, in increments of DEGREE-SIZE percentage
points. Returns extended palette as alist."
  (let ((palette (or palette wal-theme-base-palette)))
    (cl-loop for
             (key . value)
             in
             palette
             collect
             `(,key
               . ,(wal-theme--extend-base-color value num-degrees
                                                degree-size)))))
