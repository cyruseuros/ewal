;;; wal-theme --- A `wal'-based automatic, terminal aware generator.
;;; Commentary: We can't let `Vim' be the only one to have it.
;;; Code:
(defvar wal-theme-wal-cache-json
  (substitute-in-file-name "$HOME/.cache/wal/colors.json")
  "Location of cached `wal' theme in json format.")
(defvar wal-theme-own-cache-dir "cache"
  "Location of cached `wal' theme in json format.")
(defvar wal-theme-canonical-tty-color-names
  '(black red green yellow blue magenta cyan white)
  "The 8 most universaly supported tty color names.
Their look-alikes will be extracted from the `wal' cache, and
with the right escape sequences---i.e. (cat
~/.cache/wal/sequences &)---should be viewable even in the Linux
console. NOTE: Order matters.")
(defvar wal-theme-base-palette nil
  "Unmodified colors extracted directly from `wal'.
Stored in a flat alist.")
(defvar wal-theme-extended-palette nil
  "Extended color palette stored as flat alist.")
(defvar wal-theme-full-theme-colors nil
  "Colors in use in current wal-theme.
Generated from `wal-theme-base-palette'")
(defvar wal-theme-tty-theme-colors nil
  "Colors in use in current wal-theme.
Extracted from `wal-theme-base-palette'")

(defun wal-theme--wal-cache-json-load (&optional json color-names)
  "Read JSON as the most complete of the stored files.
COLOR-NAMES will be associated with the first 8 colors of regular
wal colors."
  (let ((json (or json wal-theme-wal-cache-json))
        (json-object-type 'alist)
        (color-names (or color-names wal-theme-canonical-tty-color-names)))
    (let ((colors (json-read-file json)))
      (let ((special-colors (alist-get 'special colors))
            (regular-colors (alist-get 'colors colors)))
        (let ((regular-color-values (cl-loop for
                                             (key . value)
                                             in
                                             regular-colors
                                             collect
                                             value)))
          (let ((cannonical-colors (pairlis color-names regular-color-values)))
            (append special-colors cannonical-colors)))))))

(setq wal-theme-base-palette (wal-theme--wal-cache-json-load))

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

(setq wal-theme-extended-palette (wal-theme--extend-base-palette 4 5))

(defun wal-theme-get-color (color &optional shade tty palette)
  "Return SHADE of COLOR from current `wal-theme' palette.
Darken (-) or lightened (+) COLOR by SHADE. SHADE defaults to 0,
returning unmodified `wal' COLOR. If SHADE exceeds number of
available shades, the darkest/lightest shade is returned.
If TTY is t, the default, tty compatible `wal' color is
returned."
  (let ((palette (or palette wal-theme-extended-palette))
        (tty (or tty nil))
        (middle (/ (- (length (car wal-theme-extended-palette)) 1) 2))
        (shade (if tty 0 (or shade 0))))
    (let ((return-color (nth (+ middle shade) (alist-get color palette))))
      (if return-color
          return-color
        (car (last (alist-get color palette)))))))

(defun wal-theme--generate-theme-colors (&optional tty accent-color)
  "Make theme colorscheme from theme palettes.
If TTY is t colorscheme is reduced to basic tty supported colors.
ACCENT-COLOR sets the main theme color---defaults to `magenta'"
  (let ((accent-color (or accent-color 'magenta)))
    (let ((theme-colors
          `((act1          . ,(wal-theme-get-color 'background -4 tty))
            (act2          . ,(wal-theme-get-color accent-color -4 tty))
            (base          . ,(wal-theme-get-color 'background 4 tty))
            (base-dim      . ,(wal-theme-get-color 'background 3 tty))
            (bg1           . ,(wal-theme-get-color 'background 0 tty))
            (bg2           . ,(wal-theme-get-color 'background -1 tty))
            (bg3           . ,(wal-theme-get-color 'background -2 tty))
            (bg4           . ,(wal-theme-get-color 'background -3 tty))
            (border        . ,(wal-theme-get-color accent-color -4 tty))
            (cblk          . ,(wal-theme-get-color 'background 4 tty))
            (cblk-bg       . ,(wal-theme-get-color 'background -4 tty))
            (cblk-ln       . ,(wal-theme-get-color accent-color 1 tty))
            (cblk-ln-bg    . ,(wal-theme-get-color accent-color -2 tty))
            (cursor        . ,(wal-theme-get-color 'foreground -2 tty))
            (const         . ,(wal-theme-get-color accent-color 3 tty))
            (comment       . ,(wal-theme-get-color 'cyan 0 tty))
            (comment-light . ,(wal-theme-get-color 'cyan 2 tty))
            (comment-bg    . ,(wal-theme-get-color 'background 0 tty))
            (comp          . ,(wal-theme-get-color accent-color 2 tty))
            (err           . ,(wal-theme-get-color 'red 4 tty))
            (func          . ,(wal-theme-get-color accent-color 2 tty))
            (head1         . ,(wal-theme-get-color 'blue 0 tty))
            (head1-bg      . ,(wal-theme-get-color 'background 0 tty))
            (head2         . ,(wal-theme-get-color 'cyan 0 tty))
            (head2-bg      . ,(wal-theme-get-color 'background 0 tty))
            (head3         . ,(wal-theme-get-color 'green 0 tty))
            (head3-bg      . ,(wal-theme-get-color 'background 0 tty))
            (head4         . ,(wal-theme-get-color 'yellow 4 tty))
            (head4-bg      . ,(wal-theme-get-color 'background 0 tty))
            (highlight     . ,(wal-theme-get-color 'background 4 tty))
            (highlight-dim . ,(wal-theme-get-color 'background 3 tty))
            (keyword       . ,(wal-theme-get-color 'blue 2 tty))
            (lnum          . ,(wal-theme-get-color 'background 2 tty))
            (mat           . ,(wal-theme-get-color 'green 1 tty))
            (meta          . ,(wal-theme-get-color 'yellow 4 tty))
            (str           . ,(wal-theme-get-color 'cyan -1 tty))
            (suc           . ,(wal-theme-get-color 'green 1 tty))
            (ttip          . ,(wal-theme-get-color 'background 2 tty))
            (ttip-sl       . ,(wal-theme-get-color 'background 3 tty))
            (ttip-bg       . ,(wal-theme-get-color 'background 4 tty))
            (type          . ,(wal-theme-get-color 'red 2 tty))
            (var           . ,(wal-theme-get-color 'blue 4 tty))
            (war           . ,(wal-theme-get-color 'red 1 tty))

            ;; colors
            (aqua          . ,(wal-theme-get-color 'cyan 0 tty))
            (aqua-bg       . ,(wal-theme-get-color 'cyan -4 tty))
            (green         . ,(wal-theme-get-color 'green 0 tty))
            (green-bg      . ,(wal-theme-get-color 'green -4 tty))
            (green-bg-s    . ,(wal-theme-get-color 'green -3 tty))
            (cyan          . ,(wal-theme-get-color 'cyan 4 tty))
            (red           . ,(wal-theme-get-color 'red 0 tty))
            (red-bg        . ,(wal-theme-get-color 'red -4 tty))
            (red-bg-s      . ,(wal-theme-get-color 'red -3 tty))
            (blue          . ,(wal-theme-get-color 'blue 0 tty))
            (blue-bg       . ,(wal-theme-get-color 'blue -4 tty))
            (blue-bg-s     . ,(wal-theme-get-color 'blue -3 tty))
            (magenta       . ,(wal-theme-get-color 'magenta 0 tty))
            (yellow        . ,(wal-theme-get-color 'yellow 0 tty))
            (yellow-bg     . ,(wal-theme-get-color 'yellow -4 tty)))))
          theme-colors)))

(setq wal-theme-full-theme-colors (wal-theme--generate-theme-colors nil))
(setq wal-theme-tty-theme-colors (wal-theme--generate-theme-colors t))

(defun wal-theme--cache-current-theme (&optional base-palette extended-palette tty-theme-colors
                                                 full-theme-colors)
  "Serializes all palettes and colors in json format.
BASE-PALETTE, EXTENDED-PALETTE, TTY-THEME-COLORS, and
FULL-THEME-COLORS all refer to the variables provided by
wal-theme by default, prefixed with the package name."
  (let ((cache-dir (file-name-as-directory wal-theme-own-cache-dir))
        (base-palette (or base-palette wal-theme-base-palette))
        (extended-palette (or extended-palette wal-theme-extended-palette))
        (tty-theme-colors (or base-palette wal-theme-tty-theme-colors))
        (full-theme-colors (or base-palette wal-theme-full-theme-colors))
        )
    (let ((base-palette-file (concat cache-dir "base-palette.json"))
          (extended-palette-file (concat cache-dir "extended-palette.json"))
          (tty-theme-colors-file (concat cache-dir "tty-theme-colors.json"))
          (full-theme-colors-file (concat cache-dir "full-theme-colors.json"))
          )
      (if (null (and
                 (file-exists-p base-palette-file)
                 (file-exists-p extended-palette-file)
                 (file-exists-p tty-theme-colors-file)
                 (file-exists-p full-theme-colors-file)
                 ))
          (progn
            (if (null (file-directory-p wal-theme-own-cache-dir))
                (make-directory wal-theme-own-cache-dir))
            (with-temp-file base-palette-file
              (insert (json-encode-list wal-theme-base-palette)))
            (with-temp-file extended-palette-file
              (insert (json-encode-list wal-theme-extended-palette)))
            (with-temp-file tty-theme-colors-file
              (insert (json-encode-list wal-theme-tty-theme-colors)))
            (with-temp-file full-theme-colors-file
              (insert (json-encode-list wal-theme-full-theme-colors)))
            )))))

(wal-theme--cache-current-theme)

(defun wal-theme--load-current-theme ()
  "Load most recently cached theme files and set related global variables."
  (let ((cache-dir (file-name-as-directory wal-theme-own-cache-dir))
        (json-array-type 'list))
    (let ((base-palette-file (concat cache-dir "base-palette.json"))
          (extended-palette-file (concat cache-dir "extended-palette.json"))
          (tty-theme-colors-file (concat cache-dir "tty-theme-colors.json"))
          (full-theme-colors-file (concat cache-dir "full-theme-colors.json")))
      (setq wal-theme-base-palette (json-read-file base-palette-file))
      (setq wal-theme-extended-palette (json-read-file extended-palette-file))
      (setq wal-theme-tty-theme-colors (json-read-file tty-theme-colors-file))
      (setq wal-theme-full-theme-colors (json-read-file full-theme-colors-file)))))
