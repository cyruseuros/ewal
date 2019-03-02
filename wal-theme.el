;;; wal-theme --- A `wal'-based automatic, terminal aware generator.
;;; Commentary: We can't let `Vim' be the only one to have it.
;;; Code:
(defvar wal-theme-wal-cache-json
  (substitute-in-file-name "$HOME/.cache/wal/colors.json")
  "Location of cached `wal' theme in json format.")
(defvar wal-theme-own-cache-dir "cache"
  "Location of cached `wal' theme in json format.")
(defvar wal-theme-own-cache-json-hashfile nil
  "Hash value base filename of current `wal' theme.
Used for caching results of palette extension.")
(defvar wal-theme-base-palette nil
  "Unmodified colors extracted directly from `wal'.
Stored in a flat alist.")
(defvar wal-theme-extended-pallete nil
  "Extended color palette stored as flat alist.")

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
(setq wal-theme-own-cache-json-hashfile
      (concat (sha1 (format "%s" wal-theme-base-palette)) ".json"))

(defun wal-theme--extend-base-color (color num-degrees degree-size)
  "Extends (darkens (-) and lightens (+)) COLOR.
Does so by 2 * NUM-DEGREES, in increments of DEGREE-SIZE percentage
points. Returns list of extended colors"
  (let ((extended-color-list ()))
    (dotimes (i num-degrees extended-color-list)
      (add-to-list 'extended-color-list
                   (color-darken-name color
                                      (* i degree-size))))
    (add-to-list 'extended-color-list color t)
    (dotimes (i (+ 1 num-degrees) extended-color-list)
      (add-to-list 'extended-color-list
                   (color-lighten-name color
                                      (* i degree-size))
                   t))))


(defun wal-theme--extend-base-palette (num-degrees degree-size &optional palette)
  "Extends (darkens (-) and lightens (+)) PALETTE.
Does so by 2 * NUM-DEGREES, in increments of DEGREE-SIZE percentage
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

(setq wal-theme-extended-pallete (wal-theme--extend-base-palette 4 5))

(defun wal-theme-get-color (color &optional shade palette)
  "Return SHADE of COLOR from current `wal-theme' pallete.
Darken (-) or lightened (+) COLOR by SHADE. SHADE defaults to 0,
returning unmodified `wal' COLOR. If SHADE exceeds number of
available shades, the darkest/lightest shade is returned.
If `(display-graphic-p)' is nil, the default `wal' color is
returned."
  (let ((palette (or palette wal-theme-extended-pallete))
        (middle (/ (- (length (car wal-theme-extended-pallete)) 1) 2))
        (shade (if (display-graphic-p) (or shade 0) 0)))
    (let ((return-color (nth (+ middle shade) (alist-get color palette))))
      (if return-color
          return-color
        (car (last (alist-get color palette)))))))

(defun wal-theme--cache-extended-palette (&optional palette)
  "Serializes extended PALETTE in json format.
Defaults to current pallete."
  (let ((palette (or palette wal-theme-extended-pallete))
        (hashfile (concat (file-name-as-directory wal-theme-own-cache-dir)
                          wal-theme-own-cache-json-hashfile)))
    (if (null (file-exists-p hashfile))
        (progn
          (if (null (file-directory-p wal-theme-own-cache-dir))
              (make-directory wal-theme-own-cache-dir))
          (with-temp-file hashfile
            (insert (json-encode-list palette)))))))

(wal-theme--cache-extended-palette)

(defun wal-theme--load-extended-palette (&optional hashfile)
  "Load one of the cached extended palettes.
Defaults to palette currently being loaded."
  (let ((hashfile (or hashfile wal-theme-own-cache-json-hashfile))
        (json-array-type 'list))
    (let ((hashfile-path (concat (file-name-as-directory wal-theme-own-cache-dir)
                                 hashfile)))
      (json-read-file hashfile-path))))
