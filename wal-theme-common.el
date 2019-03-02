;;; wal-theme-common.el --- A `wal'-based automatic, terminal aware generator.

;; Copyright (C) 2015-2018 Nasser Alshammari
;; Copyright (C) 2019 Uros Perisic

;; Author: Nasser Alshammari
;; URL: <https://github.com/nashamri/spacemacs-theme>
;;
;; Version: 0.1
;; Keywords: color, theme
;; Package-Requires: ((emacs "24") (cl-lib) (json))

;; Initially created with the help of emacs-theme-generator, <https://github.com/mswift42/theme-creator>.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of Emacs.

;;; Commentary:

;; This is a color theme for Emacs with an eye towards Spacemacs
;; <https://github.com/syl20bnr/spacemacs>. It comes with two versions, tty and
;; gui and should work well in both.
;;; Code:
(require 'json)
(require 'cl-lib)

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
(defvar wal-theme-force-tty-colors nil
  "Whether to use a non-extended version of wall theme.
Meant for setting tty theme regardless of gui support.")
(defvar wal-theme-accent-color nil
  "Whether to use a non-extended version of wall theme.
Meant for setting tty theme regardless of gui support.")
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

;; stolen from spacemacs
(defmacro dyn-let (varlist fn setfaces setvars)
  (list 'let (append varlist (funcall fn)) setfaces setvars))

(defgroup wal-theme nil
  "Wal-theme options."
  :group 'faces)

(defcustom wal-theme-comment-bg t
  "Use a background for comment lines."
  :type 'boolean
  :group 'wal-theme)

(defcustom wal-theme-comment-italic nil
  "Enable italics for comments and also disable background."
  :type 'boolean
  :group 'wal-theme)

(defcustom wal-theme-keyword-italic nil
  "Enable italics for keywords."
  :type 'boolean
  :group 'wal-theme)

(defcustom wal-theme-org-agenda-height nil
  "If non-nil, use varying text heights for agenda items.

Note that if you change this to a non-nil value, you may want to
also adjust the value of `org-agenda-tags-column'. If that is set
to 'auto, tags may not be properly aligned. "
  :type 'boolean
  :group 'wal-theme)

(defcustom wal-theme-org-height t
  "Use varying text heights for org headings."
  :type 'boolean
  :group 'wal-theme)

(defcustom wal-theme-org-bold t
  "Inherit text bold for org headings"
  :type 'boolean
  :group 'wal-theme)

(defcustom wal-theme-org-priority-bold t
  "Inherit text bold for priority items in agenda view"
  :type 'boolean
  :group 'wal-theme)

(defcustom wal-theme-org-highlight nil
  "Highlight org headings."
  :type 'boolean
  :group 'wal-theme)

(defcustom wal-theme-custom-colors nil
  "Specify a list of custom colors."
  :type 'alist
  :group 'wal-theme)

(defcustom wal-theme-underline-parens t
  "If non-nil, underline matching parens when using `show-paren-mode' or similar."
  :type 'boolean
  :group 'wal-theme)

(defun custom-colors-override ()
  (mapcar (lambda (x) (list (car x) (cdr x)))
          wal-theme-custom-colors))


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

(defun wal-theme--load-cached-theme-vars ()
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

(defun wal-theme--load-current-theme-vars ()
  "Load theme from scratch when cache is out of date.
Also cache the results."
  (setq wal-theme-base-palette (wal-theme--wal-cache-json-load))
  (setq wal-theme-extended-palette (wal-theme--extend-base-palette 4 5))
  (setq wal-theme-full-theme-colors (wal-theme--generate-theme-colors nil))
  (setq wal-theme-tty-theme-colors (wal-theme--generate-theme-colors t))
  (wal-theme--cache-current-theme))

(defun wal-theme-create-theme (&optional tty accent-color)
  "Create new wal-theme---either from wal-theme cache or wal cache.
TTY deafults to (display-graphic-p) unless overridden by `wal-theme-force-tty-colors',
while ACCENT-COLOR defaults to `wal-theme-accent-color' if set, 'magenta otherwise."
  (if (file-newer-than-file-p
       wal-theme-wal-cache-json
       (concat (file-name-as-directory wal-theme-own-cache-dir) "full-theme-colors.json"))
      (wal-theme--load-current-theme-vars)
    (wal-theme--load-cached-theme-vars))
  (let ((tty (or tty wal-theme-force-tty-colors (display-graphic-p)))
        (accent-color (or accent-color wal-theme-accent-color 'magenta)))
    (let (colors (if tty wal-theme-tty-theme-colors wal-theme-full-theme-colors))

    (custom-theme-set-faces
     wal-theme

;;;;; basics
     `(cursor (((alist-get colors 'class) (:background (alist-get colors 'cursor)))))
     `(custom-button (((alist-get colors 'class) :background (alist-get colors 'bg2) :foreground (alist-get colors 'base) :box (:line-width 2 :style released-button))))
     `(default (((alist-get colors 'class) (:background (alist-get colors 'bg1) :foreground (alist-get colors 'base)))))
     `(default-italic (((alist-get colors 'class) (:italic t))))
     `(error (((alist-get colors 'class) (:foreground (alist-get colors 'err)))))
     `(eval-sexp-fu-flash (((alist-get colors 'class) (:background (alist-get colors 'suc) :foreground (alist-get colors 'bg1)))))
     `(eval-sexp-fu-flash-error (((alist-get colors 'class) (:background (alist-get colors 'err) :foreground (alist-get colors 'bg1)))))
     `(font-lock-builtin-face (((alist-get colors 'class) (:foreground (alist-get colors 'keyword)))))
     `(font-lock-comment-face (((alist-get colors 'class) (:foreground (alist-get colors ')(if spacemacs-theme-comment-italic comment-light comment) :background (alist-get colors ')(when spacemacs-theme-comment-bg comment-bg) :slant (alist-get colors ')(if spacemacs-theme-comment-italic 'italic 'normal)))))
     `(font-lock-constant-face (((alist-get colors 'class) (:foreground (alist-get colors 'const)))))
     `(font-lock-doc-face (((alist-get colors 'class) (:foreground (alist-get colors 'meta)))))
     `(font-lock-function-name-face (((alist-get colors 'class) (:foreground (alist-get colors 'func) :inherit bold))))
     `(font-lock-keyword-face (((alist-get colors 'class) (:inherit bold :foreground (alist-get colors 'keyword) :slant (alist-get colors ')(if spacemacs-theme-keyword-italic 'italic 'normal)))))
     `(font-lock-negation-char-face (((alist-get colors 'class) (:foreground (alist-get colors 'const)))))
     `(font-lock-preprocessor-face (((alist-get colors 'class) (:foreground (alist-get colors 'func)))))
     `(font-lock-reference-face (((alist-get colors 'class) (:foreground (alist-get colors 'const)))))
     `(font-lock-string-face (((alist-get colors 'class) (:foreground (alist-get colors 'str)))))
     `(font-lock-type-face (((alist-get colors 'class) (:foreground (alist-get colors 'type) :inherit bold))))
     `(font-lock-variable-name-face (((alist-get colors 'class) (:foreground (alist-get colors 'var)))))
     `(font-lock-warning-face (((alist-get colors 'class) (:foreground (alist-get colors 'war) :background (alist-get colors 'bg1)))))
     `(fringe (((alist-get colors 'class) (:background (alist-get colors 'bg1) :foreground (alist-get colors 'base)))))
     `(header-line (((alist-get colors 'class) :background (alist-get colors 'bg4))))
     `(highlight (((alist-get colors 'class) (:foreground (alist-get colors 'base) :background (alist-get colors 'highlight)))))
     `(hl-line (((alist-get colors 'class) (:background (alist-get colors 'bg2)))))
     `(isearch (((alist-get colors 'class) (:foreground (alist-get colors 'bg1) :background (alist-get colors 'mat)))))
     `(lazy-highlight (((alist-get colors 'class) (:background (alist-get colors 'green)-bg-s :weight normal))))
     `(link (((alist-get colors 'class) (:foreground (alist-get colors 'comment) :underline t))))
     `(link-visited (((alist-get colors 'class) (:foreground (alist-get colors 'comp) :underline t))))
     `(match (((alist-get colors 'class) (:background (alist-get colors 'highlight) :foreground (alist-get colors 'mat)))))
     `(minibuffer-prompt (((alist-get colors 'class) (:inherit bold :foreground (alist-get colors 'keyword)))))
     `(page-break-lines (((alist-get colors 'class) (:foreground (alist-get colors 'act2)))))
     `(region (((alist-get colors 'class) (:background (alist-get colors 'highlight)))))
     `(secondary-selection (((alist-get colors 'class) (:background (alist-get colors 'bg3)))))
     `(shadow (((alist-get colors 'class) (:foreground (alist-get colors 'base)-dim))))
     `(success (((alist-get colors 'class) (:foreground (alist-get colors 'suc)))))
     `(tooltip (((alist-get colors 'class) (:background (alist-get colors 'ttip)-sl :foreground (alist-get colors 'base) :bold nil :italic nil :underline nil))))
     `(vertical-border (((alist-get colors 'class) (:foreground (alist-get colors 'border)))))
     `(warning (((alist-get colors 'class) (:foreground (alist-get colors 'war)))))

;;;;; ace-window
     `(aw-leading-char-face (((alist-get colors 'class) (:foreground (alist-get colors 'func) :weight bold :height 2.0 :box (:line-width 1 :color (alist-get colors 'keyword) :style released-button)))))

;;;;; ahs
     `(ahs-face (((alist-get colors 'class) (:background (alist-get colors 'highlight)))))
     `(ahs-plugin-whole-buffer-face (((alist-get colors 'class) (:background (alist-get colors 'mat) :foreground (alist-get colors 'bg1)))))

;;;;; anzu-mode
     `(anzu-mode-line (((alist-get colors 'class) (:foreground (alist-get colors 'yellow) :inherit bold))))

;;;;; auto-complete
     `(ac-completion-face (((alist-get colors 'class) (:background (alist-get colors 'ttip)-bg :foreground (alist-get colors 'ttip)))))

;;;;; avy
     `(avy-lead-face   (((alist-get colors 'class) (:background (alist-get colors 'green)-bg :foreground (alist-get colors 'green)))))
     `(avy-lead-face-0 (((alist-get colors 'class) (:background (alist-get colors 'green)-bg :foreground (alist-get colors 'yellow)))))
     `(avy-lead-face-1 (((alist-get colors 'class) (:background (alist-get colors 'green)-bg :foreground (alist-get colors 'magenta)))))
     `(avy-lead-face-2 (((alist-get colors 'class) (:background (alist-get colors 'green)-bg :foreground (alist-get colors 'blue)))))

;;;;; calfw
     `(cfw:face-title               (((alist-get colors 'class) (:foreground (alist-get colors 'head1) :height 2.0 :weight bold :inherit variable-pitch))))
     `(cfw:face-header              (((alist-get colors 'class) (:foreground (alist-get colors 'base) :weight bold))))
     `(cfw:face-saturday            (((alist-get colors 'class) (:foreground (alist-get colors 'base) :weight bold))))
     `(cfw:face-sunday              (((alist-get colors 'class) (:foreground (alist-get colors 'base) :weight bold))))
     `(cfw:face-holiday             (((alist-get colors 'class) (:foreground (alist-get colors 'head1) :weight bold))))
     `(cfw:face-grid                (((alist-get colors 'class) (:foreground (alist-get colors 'border)))))
     `(cfw:face-default-content     (((alist-get colors 'class) (:foreground (alist-get colors 'green)))))
     `(cfw:face-periods             (((alist-get colors 'class) (:foreground (alist-get colors 'cyan)))))
     `(cfw:face-day-title           (((alist-get colors 'class) (:background (alist-get colors 'head1)-bg))))
     `(cfw:face-default-day         (((alist-get colors 'class) (:foreground (alist-get colors 'base) :weight bold))))
     `(cfw:face-annotation          (((alist-get colors 'class) (:foreground (alist-get colors 'aqua)))))
     `(cfw:face-disable             (((alist-get colors 'class) (:foreground (alist-get colors 'base)-dim))))
     `(cfw:face-today-title         (((alist-get colors 'class) (:background (alist-get colors 'blue) :weight bold))))
     `(cfw:face-today               (((alist-get colors 'class) (:background (alist-get colors 'head1)-bg :weight bold))))
     `(cfw:face-select              (((alist-get colors 'class) (:background (alist-get colors 'magenta) :weight bold))))
     `(cfw:face-toolbar             (((alist-get colors 'class) (:foreground (alist-get colors 'base) :background (alist-get colors 'bg1)))))
     `(cfw:face-toolbar-button-off  (((alist-get colors 'class) (:foreground (alist-get colors 'base) :weight bold))))
     `(cfw:face-toolbar-button-on   (((alist-get colors 'class) (:foreground (alist-get colors 'base) :weight bold))))

;;;;; cider
     `(cider-enlightened (((alist-get colors 'class) (:background nil :box (:color (alist-get colors 'yellow) :line-width -1 :style nil) :foreground (alist-get colors 'yellow)))))
     `(cider-enlightened-local (((alist-get colors 'class) (:foreground (alist-get colors 'yellow)))))
     `(cider-instrumented-face (((alist-get colors 'class) (:background nil :box (:color (alist-get colors 'red) :line-width -1 :style nil) :foreground (alist-get colors 'red)))))
     `(cider-result-overlay-face (((alist-get colors 'class) (:background nil :box (:color (alist-get colors 'blue) :line-width -1 :style nil) :foreground (alist-get colors 'blue)))))
     `(cider-test-error-face (((alist-get colors 'class) (:background (alist-get colors 'war) :foreground (alist-get colors 'bg1)))))
     `(cider-test-failure-face (((alist-get colors 'class) (:background (alist-get colors 'err) :foreground (alist-get colors 'bg1)))))
     `(cider-test-success-face (((alist-get colors 'class) (:background (alist-get colors 'suc) :foreground (alist-get colors 'bg1)))))
     `(cider-traced-face (((alist-get colors 'class) :box (:color (alist-get colors 'cyan) :line-width -1 :style nil))))

;;;;; company
     `(company-echo-common (((alist-get colors 'class) (:background (alist-get colors 'base) :foreground (alist-get colors 'bg1)))))
     `(company-preview (((alist-get colors 'class) (:background (alist-get colors 'ttip)-bg :foreground (alist-get colors 'ttip)))))
     `(company-preview-common (((alist-get colors 'class) (:background (alist-get colors 'ttip)-bg :foreground (alist-get colors 'base)))))
     `(company-preview-search (((alist-get colors 'class) (:inherit match))))
     `(company-scrollbar-bg (((alist-get colors 'class) (:background (alist-get colors 'bg2)))))
     `(company-scrollbar-fg (((alist-get colors 'class) (:background (alist-get colors 'act2)))))
     `(company-template-field (((alist-get colors 'class) (:inherit region))))
     `(company-tooltip (((alist-get colors 'class) (:background (alist-get colors 'ttip)-bg :foreground (alist-get colors 'ttip)))))
     `(company-tooltip-annotation (((alist-get colors 'class) (:foreground (alist-get colors 'type)))))
     `(company-tooltip-common (((alist-get colors 'class) (:background (alist-get colors 'ttip)-bg :foreground (alist-get colors 'keyword)))))
     `(company-tooltip-common-selection (((alist-get colors 'class) (:foreground (alist-get colors 'base)))))
     `(company-tooltip-mouse (((alist-get colors 'class) (:inherit highlight))))
     `(company-tooltip-search (((alist-get colors 'class) (:inherit match))))
     `(company-tooltip-selection (((alist-get colors 'class) (:background (alist-get colors 'ttip)-sl :foreground (alist-get colors 'base)))))

;;;;; diff
     `(diff-added             (((alist-get colors 'class) :background nil :foreground (alist-get colors 'green))))
     `(diff-changed           (((alist-get colors 'class) :background nil :foreground (alist-get colors 'blue))))
     `(diff-header            (((alist-get colors 'class) :background (alist-get colors 'cblk)-ln-bg :foreground (alist-get colors 'func))))
     `(diff-file-header       (((alist-get colors 'class) :background (alist-get colors 'cblk)-ln-bg :foreground (alist-get colors 'cblk))))
     `(diff-indicator-added   (((alist-get colors 'class) :background nil :foreground (alist-get colors 'green))))
     `(diff-indicator-changed (((alist-get colors 'class) :background nil :foreground (alist-get colors 'blue))))
     `(diff-indicator-removed (((alist-get colors 'class) :background nil :foreground (alist-get colors 'red))))
     `(diff-refine-added      (((alist-get colors 'class) :background (alist-get colors 'green) :foreground (alist-get colors 'bg1))))
     `(diff-refine-changed    (((alist-get colors 'class) :background (alist-get colors 'blue) :foreground (alist-get colors 'bg1))))
     `(diff-refine-removed    (((alist-get colors 'class) :background (alist-get colors 'red) :foreground (alist-get colors 'bg1))))
     `(diff-removed           (((alist-get colors 'class) :background nil :foreground (alist-get colors 'red))))

;;;;; diff-hl
     `(diff-hl-change (((alist-get colors 'class) :background (alist-get colors 'blue)-bg-s :foreground (alist-get colors 'blue))))
     `(diff-hl-delete (((alist-get colors 'class) :background (alist-get colors 'red)-bg-s :foreground (alist-get colors 'red))))
     `(diff-hl-insert (((alist-get colors 'class) :background (alist-get colors 'green)-bg-s :foreground (alist-get colors 'green))))

;;;;; dired
     `(dired-directory (((alist-get colors 'class) (:foreground (alist-get colors 'keyword) :background (alist-get colors 'bg1) :inherit bold))))
     `(dired-flagged (((alist-get colors 'class) (:foreground (alist-get colors 'red)))))
     `(dired-header (((alist-get colors 'class) (:foreground (alist-get colors 'comp) :inherit bold))))
     `(dired-ignored (((alist-get colors 'class) (:inherit shadow))))
     `(dired-mark (((alist-get colors 'class) (:foreground (alist-get colors 'comp) :inherit bold))))
     `(dired-marked (((alist-get colors 'class) (:foreground (alist-get colors 'magenta) :inherit bold))))
     `(dired-perm-write (((alist-get colors 'class) (:foreground (alist-get colors 'base) :underline t))))
     `(dired-symlink (((alist-get colors 'class) (:foreground (alist-get colors 'cyan) :background (alist-get colors 'bg1) :inherit bold))))
     `(dired-warning (((alist-get colors 'class) (:foreground (alist-get colors 'war)))))

;;;;; ediff
     `(ediff-current-diff-A (((alist-get colors 'class)(:background (alist-get colors 'red)-bg :foreground (alist-get colors 'red)))))
     `(ediff-current-diff-Ancestor (((alist-get colors 'class)(:background (alist-get colors 'aqua)-bg :foreground (alist-get colors 'aqua)))))
     `(ediff-current-diff-B (((alist-get colors 'class)(:background (alist-get colors 'green)-bg :foreground (alist-get colors 'green)))))
     `(ediff-current-diff-C (((alist-get colors 'class)(:background (alist-get colors 'blue)-bg :foreground (alist-get colors 'blue)))))
     `(ediff-even-diff-A (((alist-get colors 'class)(:background (alist-get colors 'bg3)))))
     `(ediff-even-diff-Ancestor (((alist-get colors 'class)(:background (alist-get colors 'bg3)))))
     `(ediff-even-diff-B (((alist-get colors 'class)(:background (alist-get colors 'bg3)))))
     `(ediff-even-diff-C (((alist-get colors 'class)(:background (alist-get colors 'bg3)))))
     `(ediff-fine-diff-A (((alist-get colors 'class)(:background (alist-get colors 'red) :foreground (alist-get colors 'bg1)))))
     `(ediff-fine-diff-Ancestor (((alist-get colors 'class)(:background nil :inherit bold))))
     `(ediff-fine-diff-B (((alist-get colors 'class)(:background (alist-get colors 'green) :foreground (alist-get colors 'bg1)))))
     `(ediff-fine-diff-C (((alist-get colors 'class)(:background (alist-get colors 'blue) :foreground (alist-get colors 'bg1)))))
     `(ediff-odd-diff-A (((alist-get colors 'class)(:background (alist-get colors 'bg4)))))
     `(ediff-odd-diff-Ancestor (((alist-get colors 'class)(:background (alist-get colors 'bg4)))))
     `(ediff-odd-diff-B (((alist-get colors 'class)(:background (alist-get colors 'bg4)))))
     `(ediff-odd-diff-C (((alist-get colors 'class)(:background (alist-get colors 'bg4)))))

;;;;; ein
     `(ein:cell-input-area(((alist-get colors 'class) (:background (alist-get colors 'bg2)))))
     `(ein:cell-input-prompt (((alist-get colors 'class) (:foreground (alist-get colors 'suc)))))
     `(ein:cell-output-prompt (((alist-get colors 'class) (:foreground (alist-get colors 'err)))))
     `(ein:notification-tab-normal (((alist-get colors 'class) (:foreground (alist-get colors 'keyword)))))
     `(ein:notification-tab-selected (((alist-get colors 'class) (:foreground (alist-get colors 'suc) :inherit bold))))

;;;;; eldoc
     `(eldoc-highlight-function-argument (((alist-get colors 'class) (:foreground (alist-get colors 'mat) :inherit bold))))

;;;;; elfeed
     `(elfeed-search-date-face (((alist-get colors 'class) (:foreground (alist-get colors 'head2)))))
     `(elfeed-search-feed-face (((alist-get colors 'class) (:foreground (alist-get colors 'blue)))))
     `(elfeed-search-tag-face (((alist-get colors 'class) (:foreground (alist-get colors 'func)))))
     `(elfeed-search-title-face (((alist-get colors 'class) (:foreground (alist-get colors 'var)))))
     `(elfeed-search-unread-title-face (((alist-get colors 'class) (:foreground (alist-get colors 'base)))))

;;;;; enh-ruby
     `(enh-ruby-op-face (((alist-get colors 'class) (:background (alist-get colors 'bg1) :foreground (alist-get colors 'base)))))
     `(enh-ruby-string-delimiter-face (((alist-get colors 'class) (:foreground (alist-get colors 'str)))))

;;;;; erc
     `(erc-input-face (((alist-get colors 'class) (:foreground (alist-get colors 'func)))))
     `(erc-my-nick-face (((alist-get colors 'class) (:foreground (alist-get colors 'keyword)))))
     `(erc-nick-default-face (((alist-get colors 'class) (:foreground (alist-get colors 'keyword)))))
     `(erc-nick-prefix-face (((alist-get colors 'class) (:foreground (alist-get colors 'yellow)))))
     `(erc-notice-face (((alist-get colors 'class) (:foreground (alist-get colors 'str)))))
     `(erc-prompt-face (((alist-get colors 'class) (:foreground (alist-get colors 'mat) :inherit bold))))
     `(erc-timestamp-face (((alist-get colors 'class) (:foreground (alist-get colors 'keyword)))))

;;;;; eshell
     `(eshell-ls-archive (((alist-get colors 'class) (:foreground (alist-get colors 'red) :inherit bold))))
     `(eshell-ls-backup (((alist-get colors 'class) (:inherit font-lock-comment-face))))
     `(eshell-ls-clutter (((alist-get colors 'class) (:inherit font-lock-comment-face))))
     `(eshell-ls-directory (((alist-get colors 'class) (:foreground (alist-get colors 'keyword) :inherit bold))))
     `(eshell-ls-executable (((alist-get colors 'class) (:foreground (alist-get colors 'suc) :inherit bold))))
     `(eshell-ls-missing (((alist-get colors 'class) (:inherit font-lock-warning-face))))
     `(eshell-ls-product (((alist-get colors 'class) (:inherit font-lock-doc-face))))
     `(eshell-ls-special (((alist-get colors 'class) (:foreground (alist-get colors 'yellow) :inherit bold))))
     `(eshell-ls-symlink (((alist-get colors 'class) (:foreground (alist-get colors 'cyan) :inherit bold))))
     `(eshell-ls-unreadable (((alist-get colors 'class) (:foreground (alist-get colors 'base)))))
     `(eshell-prompt (((alist-get colors 'class) (:foreground (alist-get colors 'keyword) :inherit bold))))

;;;;; ESS
     `(ess-assignment-face (((alist-get colors 'class) (:foreground (alist-get colors 'type) :inherit bold))))
     `(ess-backquoted-face (((alist-get colors 'class) (:foreground (alist-get colors 'var)))))
     `(ess-constant-face (((alist-get colors 'class) (:inherit font-lock-constant-face))))
     `(ess-f-t-face (((alist-get colors 'class) (:inherit font-lock-constant-face))))
     `(ess-function-call-face (((alist-get colors 'class) (:foreground (alist-get colors 'func)))))
     `(ess-keyword-face (((alist-get colors 'class) (:inherit font-lock-keyword-face))))
     `(ess-matrix-face (((alist-get colors 'class) (:foreground (alist-get colors 'base)-dim))))
     `(ess-modifiers-face (((alist-get colors 'class) (:foreground (alist-get colors 'keyword)))))
     `(ess-numbers-face (((alist-get colors 'class) (:inherit font-lock-constant-face))))
     `(ess-operator-face (((alist-get colors 'class) (:foreground (alist-get colors 'var)))))
     `(ess-paren-face (((alist-get colors 'class) (:foreground (alist-get colors 'blue)))))
     `(ess-r-control-flow-keyword-face (((alist-get colors 'class) (:foreground (alist-get colors 'keyword)))))
     `(ess-r-signal-keyword-face (((alist-get colors 'class) (:foreground (alist-get colors 'war)))))

;;;;; evil
     `(evil-ex-substitute-matches (((alist-get colors 'class) (:background (alist-get colors 'red)-bg :foreground (alist-get colors 'red)))))
     `(evil-ex-substitute-replacement (((alist-get colors 'class) (:background (alist-get colors 'green)-bg :foreground (alist-get colors 'green)))))

;;;;; evil-goggles
      `(evil-goggles--pulse-face (((alist-get colors 'class) (:background (alist-get colors 'yellow)-bg :foreground (alist-get colors 'yellow)))))
      `(evil-goggles-change-face (((alist-get colors 'class) (:background (alist-get colors 'blue)-bg-s :foreground (alist-get colors 'blue)))))
      `(evil-goggles-commentary-face (((alist-get colors 'class) (:background (alist-get colors 'aqua)-bg :foreground (alist-get colors 'aqua)))))
      `(evil-goggles-delete-face (((alist-get colors 'class) (:background (alist-get colors 'red)-bg-s :foreground (alist-get colors 'red)))))
      `(evil-goggles-fill-and-move-face (((alist-get colors 'class) (:background (alist-get colors 'green)-bg-s :foreground (alist-get colors 'green)))))
      `(evil-goggles-indent-face (((alist-get colors 'class) (:background (alist-get colors 'green)-bg-s :foreground (alist-get colors 'green)))))
      `(evil-goggles-join-face (((alist-get colors 'class) (:background (alist-get colors 'green)-bg-s :foreground (alist-get colors 'green)))))
      `(evil-goggles-nerd-commenter-face (((alist-get colors 'class) (:background (alist-get colors 'aqua)-bg :foreground (alist-get colors 'aqua)))))
      `(evil-goggles-paste-face (((alist-get colors 'class) (:background (alist-get colors 'green)-bg-s :foreground (alist-get colors 'green)))))
      `(evil-goggles-record-macro-face (((alist-get colors 'class) (:background (alist-get colors 'blue)-bg-s :foreground (alist-get colors 'blue)))))
      `(evil-goggles-replace-with-register-face (((alist-get colors 'class) (:background (alist-get colors 'yellow)-bg :foreground (alist-get colors 'yellow)))))
      `(evil-goggles-set-marker-face (((alist-get colors 'class) (:background (alist-get colors 'blue)-bg-s :foreground (alist-get colors 'blue)))))
      `(evil-goggles-shift-face (((alist-get colors 'class) (:background (alist-get colors 'blue)-bg-s :foreground (alist-get colors 'blue)))))
      `(evil-goggles-surround-face (((alist-get colors 'class) (:background (alist-get colors 'blue)-bg-s :foreground (alist-get colors 'blue)))))
      `(evil-goggles-yank-face (((alist-get colors 'class) (:background (alist-get colors 'blue)-bg-s :foreground (alist-get colors 'blue)))))
      `(evil-goggles-undo-redo-add-face (((alist-get colors 'class) (:background (alist-get colors 'green)-bg-s :foreground (alist-get colors 'green)))))
      `(evil-goggles-undo-redo-change-face (((alist-get colors 'class) (:background (alist-get colors 'blue)-bg-s :foreground (alist-get colors 'blue)))))
      `(evil-goggles-undo-redo-remove-face (((alist-get colors 'class) (:background (alist-get colors 'red)-bg-s :foreground (alist-get colors 'red)))))

;;;;; flycheck
     `(flycheck-error
       (((alist-get colors ')(append '((supports :underline (:style line))) class)
         (:underline (:style line :color (alist-get colors 'err))))
        ((alist-get colors 'class) (:foreground (alist-get colors 'base) :background (alist-get colors 'err) :inherit bold :underline t))))
     `(flycheck-error-list-checker-name (((alist-get colors 'class) (:foreground (alist-get colors 'keyword)))))
     `(flycheck-fringe-error (((alist-get colors 'class) (:foreground (alist-get colors 'err) :inherit bold))))
     `(flycheck-fringe-info (((alist-get colors 'class) (:foreground (alist-get colors 'keyword) :inherit bold))))
     `(flycheck-fringe-warning (((alist-get colors 'class) (:foreground (alist-get colors 'war) :inherit bold))))
     `(flycheck-info
       (((alist-get colors ')(append '((supports :underline (:style line))) class)
         (:underline (:style line :color (alist-get colors 'keyword))))
        ((alist-get colors 'class) (:foreground (alist-get colors 'base) :background (alist-get colors 'keyword) :inherit bold :underline t))))
     `(flycheck-warning
       (((alist-get colors ')(append '((supports :underline (:style line))) class)
         (:underline (:style line :color (alist-get colors 'war))))
        ((alist-get colors 'class) (:foreground (alist-get colors 'base) :background (alist-get colors 'war) :inherit bold :underline t))))

;;;;; flymake
     `(flymake-error (((alist-get colors ')(append '((supports :underline (:style line))) class)
                       (:underline (:style line :color (alist-get colors 'err))))
                      ((alist-get colors 'class) (:foreground (alist-get colors 'base) :background (alist-get colors 'err) :inherit bold :underline t))))
     `(flymake-note (((alist-get colors ')(append '((supports :underline (:style line))) class)
                      (:underline (:style wave :color (alist-get colors 'keyword))))
                     ((alist-get colors 'class) (:foreground (alist-get colors 'base) :background (alist-get colors 'keyword) :inherit bold :underline t))))
     `(flymake-warning (((alist-get colors ')(append '((supports :underline (:style line))) class)
                         (:underline (:style line :color (alist-get colors 'war))))
                        ((alist-get colors 'class) (:foreground (alist-get colors 'base) :background (alist-get colors 'war) :inherit bold :underline t))))

;;;;; flyspell
     `(flyspell-incorrect (((alist-get colors ')(append '((supports :underline (:style line))) class)
                            (:underline (:style wave :color (alist-get colors 'war))))
                           ((alist-get colors 'class) (:foreground (alist-get colors 'base) :background (alist-get colors 'war) :inherit bold :underline t))))
     `(flyspell-duplicate (((alist-get colors ')(append '((supports :underline (:style line))) class)
                            (:underline (:style wave :color (alist-get colors 'keyword))))
                           ((alist-get colors 'class) (:foreground (alist-get colors 'base) :background (alist-get colors 'keyword) :inherit bold :underline t))))

;;;;; jabber
     `(jabber-activity-face (((alist-get colors 'class) (:inherit bold :foreground (alist-get colors 'red)))))
     `(jabber-activity-personal-face (((alist-get colors 'class) (:inherit bold :foreground (alist-get colors 'blue)))))
     `(jabber-chat-error (((alist-get colors 'class) (:inherit bold :foreground (alist-get colors 'red)))))
     `(jabber-chat-prompt-foreign (((alist-get colors 'class) (:inherit bold :foreground (alist-get colors 'red)))))
     `(jabber-chat-prompt-local (((alist-get colors 'class) (:inherit bold :foreground (alist-get colors 'blue)))))
     `(jabber-chat-prompt-system (((alist-get colors 'class) (:inherit bold :foreground (alist-get colors 'green)))))
     `(jabber-chat-text-foreign (((alist-get colors 'class) (:foreground (alist-get colors 'base)))))
     `(jabber-chat-text-local (((alist-get colors 'class) (:foreground (alist-get colors 'base)))))
     `(jabber-rare-time-face (((alist-get colors 'class) (:foreground (alist-get colors 'green)))))
     `(jabber-roster-user-away (((alist-get colors 'class) (:foreground (alist-get colors 'yellow)))))
     `(jabber-roster-user-chatty (((alist-get colors 'class) (:inherit bold :foreground (alist-get colors 'green)))))
     `(jabber-roster-user-dnd (((alist-get colors 'class) (:foreground (alist-get colors 'red)))))
     `(jabber-roster-user-error (((alist-get colors 'class) (:foreground (alist-get colors 'err)))))
     `(jabber-roster-user-offline (((alist-get colors 'class) (:foreground (alist-get colors 'base)))))
     `(jabber-roster-user-online (((alist-get colors 'class) (:inherit bold :foreground (alist-get colors 'green)))))
     `(jabber-roster-user-xa (((alist-get colors 'class) (:foreground (alist-get colors 'aqua)))))

;;;;; git-gutter-fr
     `(git-gutter-fr:added (((alist-get colors 'class) (:foreground (alist-get colors 'green) :inherit bold))))
     `(git-gutter-fr:deleted (((alist-get colors 'class) (:foreground (alist-get colors 'war) :inherit bold))))
     `(git-gutter-fr:modified (((alist-get colors 'class) (:foreground (alist-get colors 'keyword) :inherit bold))))

;;;;; git-timemachine
     `(git-timemachine-minibuffer-detail-face (((alist-get colors 'class) (:foreground (alist-get colors 'blue) :inherit bold :background (alist-get colors 'blue)-bg))))

;;;;; gnus
     `(gnus-emphasis-highlight-words (((alist-get colors 'class) (:background (alist-get colors 'suc) :foreground (alist-get colors 'bg1)))))
     `(gnus-header-content (((alist-get colors 'class) (:foreground (alist-get colors 'keyword)))))
     `(gnus-header-from (((alist-get colors 'class) (:foreground (alist-get colors 'var)))))
     `(gnus-header-name (((alist-get colors 'class) (:foreground (alist-get colors 'comp)))))
     `(gnus-header-subject (((alist-get colors 'class) (:foreground (alist-get colors 'func) :inherit bold))))
     `(gnus-summary-cancelled (((alist-get colors 'class) (:background (alist-get colors 'war) :foreground (alist-get colors 'bg1)))))

;;;;; guide-key
     `(guide-key/highlight-command-face (((alist-get colors 'class) (:foreground (alist-get colors 'base)))))
     `(guide-key/key-face (((alist-get colors 'class) (:foreground (alist-get colors 'keyword)))))
     `(guide-key/prefix-command-face (((alist-get colors 'class) (:foreground (alist-get colors 'keyword) :inherit bold))))

;;;;; helm
     `(helm-bookmark-directory (((alist-get colors 'class) (:inherit helm-ff-directory))))
     `(helm-bookmark-file (((alist-get colors 'class) (:foreground (alist-get colors 'base)))))
     `(helm-bookmark-gnus (((alist-get colors 'class) (:foreground (alist-get colors 'comp)))))
     `(helm-bookmark-info (((alist-get colors 'class) (:foreground (alist-get colors 'comp)))))
     `(helm-bookmark-man (((alist-get colors 'class) (:foreground (alist-get colors 'comp)))))
     `(helm-bookmark-w3m (((alist-get colors 'class) (:foreground (alist-get colors 'comp)))))
     `(helm-buffer-directory (((alist-get colors 'class) (:foreground (alist-get colors 'base) :background (alist-get colors 'bg1)))))
     `(helm-buffer-file (((alist-get colors 'class) (:foreground (alist-get colors 'base) :background (alist-get colors 'bg1)))))
     `(helm-buffer-not-saved (((alist-get colors 'class) (:foreground (alist-get colors 'comp) :background (alist-get colors 'bg1)))))
     `(helm-buffer-process (((alist-get colors 'class) (:foreground (alist-get colors 'keyword) :background (alist-get colors 'bg1)))))
     `(helm-buffer-saved-out (((alist-get colors 'class) (:foreground (alist-get colors 'base) :background (alist-get colors 'bg1)))))
     `(helm-buffer-size (((alist-get colors 'class) (:foreground (alist-get colors 'base) :background (alist-get colors 'bg1)))))
     `(helm-candidate-number (((alist-get colors 'class) (:background (alist-get colors 'bg1) :foreground (alist-get colors 'keyword) :inherit bold))))
     `(helm-ff-directory (((alist-get colors 'class) (:foreground (alist-get colors 'keyword) :background (alist-get colors 'bg1) :inherit bold))))
     `(helm-ff-dotted-directory (((alist-get colors 'class) (:foreground (alist-get colors 'keyword) :background (alist-get colors 'bg1) :inherit bold))))
     `(helm-ff-dotted-symlink-directory (((alist-get colors 'class) (:foreground (alist-get colors 'cyan) :background (alist-get colors 'bg1) :inherit bold))))
     `(helm-ff-executable (((alist-get colors 'class) (:foreground (alist-get colors 'suc) :background (alist-get colors 'bg1) :weight normal))))
     `(helm-ff-file (((alist-get colors 'class) (:foreground (alist-get colors 'base) :background (alist-get colors 'bg1) :weight normal))))
     `(helm-ff-invalid-symlink (((alist-get colors 'class) (:foreground (alist-get colors 'red) :background (alist-get colors 'bg1) :inherit bold))))
     `(helm-ff-prefix (((alist-get colors 'class) (:foreground (alist-get colors 'bg1) :background (alist-get colors 'keyword) :weight normal))))
     `(helm-ff-symlink (((alist-get colors 'class) (:foreground (alist-get colors 'cyan) :background (alist-get colors 'bg1) :inherit bold))))
     `(helm-grep-cmd-line (((alist-get colors 'class) (:foreground (alist-get colors 'base) :background (alist-get colors 'bg1)))))
     `(helm-grep-file (((alist-get colors 'class) (:foreground (alist-get colors 'base) :background (alist-get colors 'bg1)))))
     `(helm-grep-finish (((alist-get colors 'class) (:foreground (alist-get colors 'base) :background (alist-get colors 'bg1)))))
     `(helm-grep-lineno (((alist-get colors 'class) (:foreground (alist-get colors 'type) :background (alist-get colors 'bg1) :inherit bold))))
     `(helm-grep-match (((alist-get colors 'class) (:foreground nil :background nil :inherit helm-match))))
     `(helm-header (((alist-get colors 'class) (:foreground (alist-get colors 'base) :background (alist-get colors 'bg1) :underline nil :box nil))))
     `(helm-header-line-left-margin (((alist-get colors 'class) (:foreground (alist-get colors 'keyword) :background (alist-get colors 'nil)))))
     `(helm-match (((alist-get colors 'class) (:background (alist-get colors 'head1)-bg :foreground (alist-get colors 'head1)))))
     `(helm-match-item (((alist-get colors 'class) (:background (alist-get colors 'head1)-bg :foreground (alist-get colors 'head1)))))
     `(helm-moccur-buffer (((alist-get colors 'class) (:foreground (alist-get colors 'var) :background (alist-get colors 'bg1)))))
     `(helm-selection (((alist-get colors 'class) (:background (alist-get colors 'highlight)))))
     `(helm-selection-line (((alist-get colors 'class) (:background (alist-get colors 'bg2)))))
     `(helm-separator (((alist-get colors 'class) (:foreground (alist-get colors 'comp) :background (alist-get colors 'bg1)))))
     `(helm-source-header (((alist-get colors 'class) (:background (alist-get colors 'comp) :foreground (alist-get colors 'bg1) :inherit bold))))
     `(helm-time-zone-current (((alist-get colors 'class) (:foreground (alist-get colors 'keyword) :background (alist-get colors 'bg1)))))
     `(helm-time-zone-home (((alist-get colors 'class) (:foreground (alist-get colors 'comp) :background (alist-get colors 'bg1)))))
     `(helm-visible-mark (((alist-get colors 'class) (:foreground (alist-get colors 'keyword) :background (alist-get colors 'bg3)))))

;;;;; helm-swoop
     `(helm-swoop-target-line-block-face (((alist-get colors 'class) (:foreground (alist-get colors 'base) :background (alist-get colors 'highlight)))))
     `(helm-swoop-target-line-face (((alist-get colors 'class) (:background (alist-get colors 'highlight)))))
     `(helm-swoop-target-word-face (((alist-get colors 'class) (:background (alist-get colors 'highlight) :foreground (alist-get colors 'mat)))))

;;;;; highlights
     `(hi-green  (((alist-get colors 'class) (:foreground (alist-get colors 'green) :background (alist-get colors 'green)-bg))))
     `(hi-yellow (((alist-get colors 'class) (:foreground (alist-get colors 'yellow) :background (alist-get colors 'yellow)-bg))))

;;;;; highlight-indentation
     `(highlight-indentation-face (((alist-get colors 'class) (:background (alist-get colors 'comment)-bg))))

;;;;; highlight-symbol
     `(highlight-symbol-face (((alist-get colors 'class) (:background (alist-get colors 'bg2)))))

;;;;; hydra
     `(hydra-face-blue (((alist-get colors 'class) (:foreground (alist-get colors 'blue)))))
     `(hydra-face-red (((alist-get colors 'class) (:foreground (alist-get colors 'red)))))

;;;;; ido
     `(ido-first-match (((alist-get colors 'class) (:foreground (alist-get colors 'comp) :inherit bold))))
     `(ido-only-match (((alist-get colors 'class) (:foreground (alist-get colors 'mat) :inherit bold))))
     `(ido-subdir (((alist-get colors 'class) (:foreground (alist-get colors 'keyword)))))
     `(ido-vertical-match-face (((alist-get colors 'class) (:foreground (alist-get colors 'comp) :underline nil))))

;;;;; info
     `(info-header-xref (((alist-get colors 'class) (:foreground (alist-get colors 'func) :underline t))))
     `(info-menu (((alist-get colors 'class) (:foreground (alist-get colors 'suc)))))
     `(info-node (((alist-get colors 'class) (:foreground (alist-get colors 'func) :inherit bold))))
     `(info-quoted-name (((alist-get colors 'class) (:foreground (alist-get colors 'keyword)))))
     `(info-reference-item (((alist-get colors 'class) (:background nil :underline t :inherit bold))))
     `(info-string (((alist-get colors 'class) (:foreground (alist-get colors 'str)))))
     `(info-title-1 (((alist-get colors 'class) (:height 1.4 :inherit bold))))
     `(info-title-2 (((alist-get colors 'class) (:height 1.3 :inherit bold))))
     `(info-title-3 (((alist-get colors 'class) (:height 1.3))))
     `(info-title-4 (((alist-get colors 'class) (:height 1.2))))

;;;;; ivy
     `(ivy-current-match (((alist-get colors 'class) (:background (alist-get colors 'highlight) :inherit bold))))
     `(ivy-minibuffer-match-face-1 (((alist-get colors 'class) (:inherit bold))))
     `(ivy-minibuffer-match-face-2 (((alist-get colors 'class) (:foreground (alist-get colors 'head1) :underline t))))
     `(ivy-minibuffer-match-face-3 (((alist-get colors 'class) (:foreground (alist-get colors 'head4) :underline t))))
     `(ivy-minibuffer-match-face-4 (((alist-get colors 'class) (:foreground (alist-get colors 'head3) :underline t))))
     `(ivy-remote (((alist-get colors 'class) (:foreground (alist-get colors 'cyan)))))

;;;;; latex
     `(font-latex-bold-face (((alist-get colors 'class) (:foreground (alist-get colors 'comp)))))
     `(font-latex-italic-face (((alist-get colors 'class) (:foreground (alist-get colors 'keyword) :italic t))))
     `(font-latex-match-reference-keywords (((alist-get colors 'class) (:foreground (alist-get colors 'const)))))
     `(font-latex-match-variable-keywords (((alist-get colors 'class) (:foreground (alist-get colors 'var)))))
     `(font-latex-sectioning-0-face (((alist-get colors 'class) (:inherit bold :foreground (alist-get colors 'head3) :height (alist-get colors ')(if spacemacs-theme-org-height 1.3 1.0) :background (alist-get colors ')(when spacemacs-theme-org-highlight head3-bg)))))
     `(font-latex-sectioning-1-face (((alist-get colors 'class) (:inherit bold :foreground (alist-get colors 'head4) :height (alist-get colors ')(if spacemacs-theme-org-height 1.3 1.0) :background (alist-get colors ')(when spacemacs-theme-org-highlight head4-bg)))))
     `(font-latex-sectioning-2-face (((alist-get colors 'class) (:inherit bold :foreground (alist-get colors 'head1) :height (alist-get colors ')(if spacemacs-theme-org-height 1.3 1.0) :background (alist-get colors ')(when spacemacs-theme-org-highlight head1-bg)))))
     `(font-latex-sectioning-3-face (((alist-get colors 'class) (:inherit bold :foreground (alist-get colors 'head2) :height (alist-get colors ')(if spacemacs-theme-org-height 1.2 1.0) :background (alist-get colors ')(when spacemacs-theme-org-highlight head2-bg)))))
     `(font-latex-sectioning-4-face (((alist-get colors 'class) (:bold nil :foreground (alist-get colors 'head3) :height (alist-get colors ')(if spacemacs-theme-org-height 1.1 1.0) :background (alist-get colors ')(when spacemacs-theme-org-highlight head3-bg)))))
     `(font-latex-sectioning-5-face (((alist-get colors 'class) (:bold nil :foreground (alist-get colors 'head4) :background (alist-get colors ')(when spacemacs-theme-org-highlight head4-bg)))))
     `(font-latex-string-face (((alist-get colors 'class) (:foreground (alist-get colors 'str)))))
     `(font-latex-warning-face (((alist-get colors 'class) (:foreground (alist-get colors 'war)))))

;;;;; ledger-mode
     `(ledger-font-directive-face (((alist-get colors 'class) (:foreground (alist-get colors 'meta)))))
     `(ledger-font-posting-amount-face (((alist-get colors 'class) (:foreground (alist-get colors 'yellow)))))
     `(ledger-font-posting-date-face (((alist-get colors 'class) (:foreground (alist-get colors 'head1)))))
     `(ledger-occur-xact-face (((alist-get colors 'class) (:background (alist-get colors 'bg2)))))

;;;;; linum-mode
     `(linum (((alist-get colors 'class) (:foreground (alist-get colors 'lnum) :background (alist-get colors 'bg2) :inherit default))))

;;;;; line-numbers
     `(line-number (((alist-get colors 'class) (:foreground (alist-get colors 'lnum) :background (alist-get colors 'bg2) :inherit default))))
     `(line-number-current-line (((alist-get colors 'class) (:foreground (alist-get colors 'base) :background (alist-get colors 'bg2) :inherit line-number))))

;;;;; linum-relative
     `(linum-relative-current-face (((alist-get colors 'class) (:foreground (alist-get colors 'comp)))))

;;;;; magit
     `(magit-blame-culprit (((alist-get colors 'class) :background (alist-get colors 'yellow)-bg :foreground (alist-get colors 'yellow))))
     `(magit-blame-date    (((alist-get colors 'class) :background (alist-get colors 'yellow)-bg :foreground (alist-get colors 'green))))
     `(magit-blame-hash    (((alist-get colors 'class) :background (alist-get colors 'yellow)-bg :foreground (alist-get colors 'func))))
     `(magit-blame-header  (((alist-get colors 'class) :background (alist-get colors 'yellow)-bg :foreground (alist-get colors 'green))))
     `(magit-blame-heading (((alist-get colors 'class) :background (alist-get colors 'yellow)-bg :foreground (alist-get colors 'green))))
     `(magit-blame-name    (((alist-get colors 'class) :background (alist-get colors 'yellow)-bg :foreground (alist-get colors 'yellow))))
     `(magit-blame-sha1    (((alist-get colors 'class) :background (alist-get colors 'yellow)-bg :foreground (alist-get colors 'func))))
     `(magit-blame-subject (((alist-get colors 'class) :background (alist-get colors 'yellow)-bg :foreground (alist-get colors 'yellow))))
     `(magit-blame-summary (((alist-get colors 'class) :background (alist-get colors 'yellow)-bg :foreground (alist-get colors 'yellow))))
     `(magit-blame-time    (((alist-get colors 'class) :background (alist-get colors 'yellow)-bg :foreground (alist-get colors 'green))))
     `(magit-branch (((alist-get colors 'class) (:foreground (alist-get colors 'const) :inherit bold))))
     `(magit-branch-current (((alist-get colors 'class) (:background (alist-get colors 'blue)-bg :foreground (alist-get colors 'blue) :inherit bold :box t))))
     `(magit-branch-local (((alist-get colors 'class) (:background (alist-get colors 'blue)-bg :foreground (alist-get colors 'blue) :inherit bold))))
     `(magit-branch-remote (((alist-get colors 'class) (:background (alist-get colors 'aqua)-bg :foreground (alist-get colors 'aqua) :inherit bold))))
     `(magit-diff-context-highlight (((alist-get colors 'class) (:background (alist-get colors 'bg2) :foreground (alist-get colors 'base)))))
     `(magit-diff-hunk-heading (((alist-get colors 'class) (:background (alist-get colors 'ttip)-bg :foreground (alist-get colors 'ttip)))))
     `(magit-diff-hunk-heading-highlight (((alist-get colors 'class) (:background (alist-get colors 'ttip)-sl :foreground (alist-get colors 'base)))))
     `(magit-hash (((alist-get colors 'class) (:foreground (alist-get colors 'var)))))
     `(magit-hunk-heading           (((alist-get colors 'class) (:background (alist-get colors 'bg3)))))
     `(magit-hunk-heading-highlight (((alist-get colors 'class) (:background (alist-get colors 'bg3)))))
     `(magit-item-highlight (((alist-get colors 'class) :background (alist-get colors 'bg2))))
     `(magit-log-author (((alist-get colors 'class) (:foreground (alist-get colors 'func)))))
     `(magit-log-head-label-head (((alist-get colors 'class) (:background (alist-get colors 'yellow) :foreground (alist-get colors 'bg1) :inherit bold))))
     `(magit-log-head-label-local (((alist-get colors 'class) (:background (alist-get colors 'keyword) :foreground (alist-get colors 'bg1) :inherit bold))))
     `(magit-log-head-label-remote (((alist-get colors 'class) (:background (alist-get colors 'suc) :foreground (alist-get colors 'bg1) :inherit bold))))
     `(magit-log-head-label-tags (((alist-get colors 'class) (:background (alist-get colors 'magenta) :foreground (alist-get colors 'bg1) :inherit bold))))
     `(magit-log-head-label-wip (((alist-get colors 'class) (:background (alist-get colors 'cyan) :foreground (alist-get colors 'bg1) :inherit bold))))
     `(magit-log-sha1 (((alist-get colors 'class) (:foreground (alist-get colors 'str)))))
     `(magit-process-ng (((alist-get colors 'class) (:foreground (alist-get colors 'war) :inherit bold))))
     `(magit-process-ok (((alist-get colors 'class) (:foreground (alist-get colors 'func) :inherit bold))))
     `(magit-reflog-amend (((alist-get colors 'class) (:foreground (alist-get colors 'magenta)))))
     `(magit-reflog-checkout (((alist-get colors 'class) (:foreground (alist-get colors 'blue)))))
     `(magit-reflog-cherry-pick (((alist-get colors 'class) (:foreground (alist-get colors 'green)))))
     `(magit-reflog-commit (((alist-get colors 'class) (:foreground (alist-get colors 'green)))))
     `(magit-reflog-merge (((alist-get colors 'class) (:foreground (alist-get colors 'green)))))
     `(magit-reflog-other (((alist-get colors 'class) (:foreground (alist-get colors 'cyan)))))
     `(magit-reflog-rebase (((alist-get colors 'class) (:foreground (alist-get colors 'magenta)))))
     `(magit-reflog-remote (((alist-get colors 'class) (:foreground (alist-get colors 'cyan)))))
     `(magit-reflog-reset (((alist-get colors 'class) (:foreground (alist-get colors 'red)))))
     `(magit-section-heading        (((alist-get colors 'class) (:foreground (alist-get colors 'keyword) :inherit bold))))
     `(magit-section-highlight      (((alist-get colors 'class) (:background (alist-get colors 'bg2)))))
     `(magit-section-title (((alist-get colors 'class) (:background (alist-get colors 'bg1) :foreground (alist-get colors 'keyword) :inherit bold))))

;;;;; man
     `(Man-overstrike (((alist-get colors 'class) (:foreground (alist-get colors 'head1) :inherit bold))))
     `(Man-reverse (((alist-get colors 'class) (:foreground (alist-get colors 'highlight)))))
     `(Man-underline (((alist-get colors 'class) (:foreground (alist-get colors 'comp) :underline t))))

;;;;; markdown
     `(markdown-header-face-1 (((alist-get colors 'class) (:inherit bold :foreground (alist-get colors 'head1) :height (alist-get colors ')(if spacemacs-theme-org-height 1.3 1.0) :background (alist-get colors ')(when spacemacs-theme-org-highlight head1-bg)))))
     `(markdown-header-face-2 (((alist-get colors 'class) (:inherit bold :foreground (alist-get colors 'head2) :height (alist-get colors ')(if spacemacs-theme-org-height 1.2 1.0) :background (alist-get colors ')(when spacemacs-theme-org-highlight head2-bg)))))
     `(markdown-header-face-3 (((alist-get colors 'class) (:bold nil :foreground (alist-get colors 'head3) :height (alist-get colors ')(if spacemacs-theme-org-height 1.1 1.0) :background (alist-get colors ')(when spacemacs-theme-org-highlight head3-bg)))))
     `(markdown-header-face-4 (((alist-get colors 'class) (:bold nil :foreground (alist-get colors 'head4) :background (alist-get colors ')(when spacemacs-theme-org-highlight head4-bg)))))
     `(markdown-header-face-5 (((alist-get colors 'class) (:bold nil :foreground (alist-get colors 'head1)))))
     `(markdown-header-face-6 (((alist-get colors 'class) (:bold nil :foreground (alist-get colors 'head2)))))
     `(markdown-table-face (((alist-get colors 'class) (:foreground (alist-get colors 'base) :background (alist-get colors 'head1)-bg))))

;;;;; mode-line
     `(mode-line           (((alist-get colors 'class) (:foreground (alist-get colors 'base) :background (alist-get colors 'act1) :box (:color (alist-get colors 'border) :line-width 1)))))
     `(mode-line-buffer-id (((alist-get colors 'class) (:inherit bold :foreground (alist-get colors 'func)))))
     `(mode-line-inactive  (((alist-get colors 'class) (:foreground (alist-get colors 'base) :background (alist-get colors 'bg1)  :box (:color (alist-get colors 'border) :line-width 1)))))

;;;;; mu4e
     `(mu4e-attach-number-face (((alist-get colors 'class) (:foreground (alist-get colors 'var)))))
     `(mu4e-cited-1-face (((alist-get colors 'class) (:foreground (alist-get colors 'head1)))))
     `(mu4e-cited-2-face (((alist-get colors 'class) (:foreground (alist-get colors 'head2)))))
     `(mu4e-cited-3-face (((alist-get colors 'class) (:foreground (alist-get colors 'head3)))))
     `(mu4e-cited-4-face (((alist-get colors 'class) (:foreground (alist-get colors 'head4)))))
     `(mu4e-cited-5-face (((alist-get colors 'class) (:foreground (alist-get colors 'head1)))))
     `(mu4e-cited-6-face (((alist-get colors 'class) (:foreground (alist-get colors 'head2)))))
     `(mu4e-cited-7-face (((alist-get colors 'class) (:foreground (alist-get colors 'head3)))))
     `(mu4e-contact-face (((alist-get colors 'class) (:foreground (alist-get colors 'func)))))
     `(mu4e-draft-face (((alist-get colors 'class) (:foreground (alist-get colors 'var)))))
     `(mu4e-flagged-face (((alist-get colors 'class) (:foreground (alist-get colors 'yellow) :inherit bold))))
     `(mu4e-header-key-face (((alist-get colors 'class) (:foreground (alist-get colors 'meta) :inherit bold))))
     `(mu4e-header-title-face (((alist-get colors 'class) (:foreground (alist-get colors 'keyword) :inherit bold))))
     `(mu4e-header-marks-face (((alist-get colors 'class) (:foreground (alist-get colors 'comp)))))
     `(mu4e-header-value-face (((alist-get colors 'class) (:foreground (alist-get colors 'keyword) :inherit bold))))
     `(mu4e-header-highlight-face (((alist-get colors 'class) (:background (alist-get colors 'highlight)))))
     `(mu4e-highlight-face (((alist-get colors 'class) (:foreground (alist-get colors 'comp)))))
     `(mu4e-title-face (((alist-get colors 'class) (:foreground (alist-get colors 'head2) :inherit bold))))
     `(mu4e-replied-face (((alist-get colors 'class) (:foreground (alist-get colors 'green)))))
     `(mu4e-modeline-face (((alist-get colors 'class) (:foreground (alist-get colors 'yellow)))))
     `(mu4e-special-header-value-face (((alist-get colors 'class) (:foreground (alist-get colors 'green)))))
     `(mu4e-unread-face (((alist-get colors 'class) (:foreground (alist-get colors 'head1) :inherit bold))))
     `(mu4e-view-url-number-face (((alist-get colors 'class) (:foreground (alist-get colors 'comp)))))

;;;;; mu4e-maildirs
     `(mu4e-maildirs-extension-maildir-hl-face (((alist-get colors 'class) (:foreground (alist-get colors 'head1) :inherit bold))))

;;;;; notmuch
     `(notmuch-search-date (((alist-get colors 'class) (:foreground (alist-get colors 'func)))))
     `(notmuch-search-flagged-face (((alist-get colors 'class) (:weight extra-bold))))
     `(notmuch-search-non-matching-authors (((alist-get colors 'class) (:foreground (alist-get colors 'base)-dim))))
     `(notmuch-search-unread-face (((alist-get colors 'class) (:background (alist-get colors 'highlight)-dim :box (alist-get colors 'border)))))
     `(notmuch-tag-face (((alist-get colors 'class) (:foreground (alist-get colors 'keyword)))))
     `(notmuch-tag-flagged (((alist-get colors 'class) (:foreground (alist-get colors 'war)))))

;;;;; neotree
     `(neo-dir-link-face (((alist-get colors 'class) (:foreground (alist-get colors 'keyword) :inherit bold))))
     `(neo-expand-btn-face (((alist-get colors 'class) (:foreground (alist-get colors 'base)))))
     `(neo-file-link-face (((alist-get colors 'class) (:foreground (alist-get colors 'base)))))
     `(neo-root-dir-face (((alist-get colors 'class) (:foreground (alist-get colors 'func) :inherit bold))))

;;;;; org
     `(org-agenda-clocking (((alist-get colors 'class) (:background (alist-get colors 'highlight) :foreground (alist-get colors 'comp)))))
     `(org-agenda-date (((alist-get colors 'class) (:foreground (alist-get colors 'var) :height (alist-get colors ')(if spacemacs-theme-org-agenda-height 1.1 1.0)))))
     `(org-agenda-date-today (((alist-get colors 'class) (:foreground (alist-get colors 'keyword) :inherit bold :height (alist-get colors ')(if spacemacs-theme-org-agenda-height 1.3 1.0)))))
     `(org-agenda-date-weekend (((alist-get colors 'class) (:inherit bold :foreground (alist-get colors 'var)))))
     `(org-agenda-done (((alist-get colors 'class) (:foreground (alist-get colors 'suc) :height (alist-get colors ')(if spacemacs-theme-org-agenda-height 1.2 1.0)))))
     `(org-agenda-structure (((alist-get colors 'class) (:inherit bold :foreground (alist-get colors 'comp)))))
     `(org-block (((alist-get colors 'class) (:background (alist-get colors 'cblk)-bg :foreground (alist-get colors 'cblk)))))
     `(org-block-begin-line (((alist-get colors 'class) (:background (alist-get colors 'cblk)-ln-bg :foreground (alist-get colors 'cblk)-ln))))
     `(org-block-end-line (((alist-get colors 'class) (:background (alist-get colors 'cblk)-ln-bg :foreground (alist-get colors 'cblk)-ln))))
     `(org-clock-overlay (((alist-get colors 'class) (:foreground (alist-get colors 'comp)))))
     `(org-code (((alist-get colors 'class) (:foreground (alist-get colors 'cyan)))))
     `(org-column (((alist-get colors 'class) (:background (alist-get colors 'highlight)))))
     `(org-column-title (((alist-get colors 'class) (:background (alist-get colors 'highlight)))))
     `(org-date (((alist-get colors 'class) (:underline t :foreground (alist-get colors 'var)))))
     `(org-date-selected (((alist-get colors 'class) (:background (alist-get colors 'func) :foreground (alist-get colors 'bg1)))))
     `(org-document-info-keyword (((alist-get colors 'class) (:foreground (alist-get colors 'meta)))))
     `(org-document-title (((alist-get colors 'class) (:foreground (alist-get colors 'func) :inherit bold :height (alist-get colors ')(if spacemacs-theme-org-height 1.4 1.0) :underline t))))
     `(org-done (((alist-get colors 'class) (:foreground (alist-get colors 'suc) :inherit bold :background (alist-get colors 'green)-bg))))
     `(org-ellipsis (((alist-get colors 'class) (:foreground (alist-get colors 'keyword)))))
     `(org-footnote  (((alist-get colors 'class) (:underline t :foreground (alist-get colors 'base)))))
     `(org-hide (((alist-get colors 'class) (:foreground (alist-get colors 'base)))))
     `(org-kbd (((alist-get colors 'class) (:inherit region :foreground (alist-get colors 'base) :box (:line-width 1 :style released-button)))))
     `(org-level-1 (((alist-get colors 'class) (:inherit bold :bold (alist-get colors ')(if spacemacs-theme-org-bold 'unspecified nil) :foreground (alist-get colors 'head1) :height (alist-get colors ')(if spacemacs-theme-org-height 1.3 1.0) :background (alist-get colors ')(when spacemacs-theme-org-highlight head1-bg)))))
     `(org-level-2 (((alist-get colors 'class) (:inherit bold :bold (alist-get colors ')(if spacemacs-theme-org-bold 'unspecified nil) :foreground (alist-get colors 'head2) :height (alist-get colors ')(if spacemacs-theme-org-height 1.2 1.0) :background (alist-get colors ')(when spacemacs-theme-org-highlight head2-bg)))))
     `(org-level-3 (((alist-get colors 'class) (:bold nil :foreground (alist-get colors 'head3) :height (alist-get colors ')(if spacemacs-theme-org-height 1.1 1.0) :background (alist-get colors ')(when spacemacs-theme-org-highlight head3-bg)))))
     `(org-level-4 (((alist-get colors 'class) (:bold nil :foreground (alist-get colors 'head4) :background (alist-get colors ')(when spacemacs-theme-org-highlight head4-bg)))))
     `(org-level-5 (((alist-get colors 'class) (:bold nil :foreground (alist-get colors 'head1)))))
     `(org-level-6 (((alist-get colors 'class) (:bold nil :foreground (alist-get colors 'head2)))))
     `(org-level-7 (((alist-get colors 'class) (:bold nil :foreground (alist-get colors 'head3)))))
     `(org-level-8 (((alist-get colors 'class) (:bold nil :foreground (alist-get colors 'head4)))))
     `(org-link (((alist-get colors 'class) (:underline t :foreground (alist-get colors 'comment)))))
     `(org-meta-line (((alist-get colors 'class) (:foreground (alist-get colors 'meta)))))
     `(org-mode-line-clock-overrun (((alist-get colors 'class) (:foreground (alist-get colors 'err)))))
     `(org-priority (((alist-get colors 'class) (:foreground (alist-get colors 'war) :inherit bold :bold (alist-get colors ')(if spacemacs-theme-org-priority-bold 'unspecified nil)))))
     `(org-quote (((alist-get colors 'class) (:inherit org-block :slant italic))))
     `(org-scheduled (((alist-get colors 'class) (:foreground (alist-get colors 'comp)))))
     `(org-scheduled-today (((alist-get colors 'class) (:foreground (alist-get colors 'func) :height (alist-get colors ')(if spacemacs-theme-org-agenda-height 1.2 1.0)))))
     `(org-scheduled-previously (((alist-get colors 'class) (:foreground (alist-get colors 'base) :slant italic))))
     `(org-sexp-date (((alist-get colors 'class) (:foreground (alist-get colors 'base)))))
     `(org-special-keyword (((alist-get colors 'class) (:foreground (alist-get colors 'func)))))
     `(org-table (((alist-get colors 'class) (:foreground (alist-get colors 'base) :background (alist-get colors 'head1)-bg))))
     `(org-tag (((alist-get colors 'class) (:foreground (alist-get colors 'meta)))))
     `(org-time-grid (((alist-get colors 'class) (:foreground (alist-get colors 'str)))))
     `(org-todo (((alist-get colors 'class) (:foreground (alist-get colors 'war) :inherit bold :background (alist-get colors 'yellow)-bg))))
     `(org-upcoming-deadline (((alist-get colors 'class) (:foreground (alist-get colors 'war) :inherit org-priority))))
     `(org-upcoming-distant-deadline (((alist-get colors 'class) (:foreground (alist-get colors 'suc) :inherit org-priority))))
     `(org-verbatim (((alist-get colors 'class) (:foreground (alist-get colors 'keyword)))))
     `(org-verse (((alist-get colors 'class) (:inherit org-block :slant italic))))
     `(org-warning (((alist-get colors 'class) (:foreground (alist-get colors 'err) :inherit org-priority))))

;;;;; outline
     `(outline-1 (((alist-get colors 'class) (:inherit org-level-1))))
     `(outline-2 (((alist-get colors 'class) (:inherit org-level-2))))
     `(outline-3 (((alist-get colors 'class) (:inherit org-level-3))))
     `(outline-4 (((alist-get colors 'class) (:inherit org-level-4))))
     `(outline-5 (((alist-get colors 'class) (:inherit org-level-5))))
     `(outline-6 (((alist-get colors 'class) (:inherit org-level-6))))
     `(outline-7 (((alist-get colors 'class) (:inherit org-level-7))))
     `(outline-8 (((alist-get colors 'class) (:inherit org-level-8))))

;;;;; perspective
     `(persp-selected-face (((alist-get colors 'class) (:inherit bold :foreground (alist-get colors 'func)))))

;;;;; popup
     `(popup-enu-selection-face (((alist-get colors 'class) (:background (alist-get colors 'ttip)-sl :foreground (alist-get colors 'base)))))
     `(popup-face (((alist-get colors 'class) (:background (alist-get colors 'ttip)-bg :foreground (alist-get colors 'ttip)))))
     `(popup-isearch-match (((alist-get colors 'class) (:inherit match))))
     `(popup-menu-face (((alist-get colors 'class) (:background (alist-get colors 'ttip)-bg :foreground (alist-get colors 'base)))))
     `(popup-menu-mouse-face (((alist-get colors 'class) (:inherit highlight))))
     `(popup-scroll-bar-background-face (((alist-get colors 'class) (:background (alist-get colors 'bg2)))))
     `(popup-scroll-bar-foreground-face (((alist-get colors 'class) (:background (alist-get colors 'act2)))))
     `(popup-tip-face (((alist-get colors 'class) (:background (alist-get colors 'ttip)-sl :foreground (alist-get colors 'base) :bold nil :italic nil :underline nil))))

;;;;; powerline
     `(powerline-active1 (((alist-get colors 'class) (:background (alist-get colors 'act2) :foreground (alist-get colors 'base)))))
     `(powerline-active2 (((alist-get colors 'class) (:background (alist-get colors 'act2) :foreground (alist-get colors 'base)))))
     `(powerline-inactive1 (((alist-get colors 'class) (:background (alist-get colors 'bg2) :foreground (alist-get colors 'base)))))
     `(powerline-inactive2 (((alist-get colors 'class) (:background (alist-get colors 'bg2) :foreground (alist-get colors 'base)))))

;;;;; rainbow-delimiters
     `(rainbow-delimiters-depth-1-face (((alist-get colors 'class) :foreground (alist-get colors 'keyword))))
     `(rainbow-delimiters-depth-2-face (((alist-get colors 'class) :foreground (alist-get colors 'func))))
     `(rainbow-delimiters-depth-3-face (((alist-get colors 'class) :foreground (alist-get colors 'str))))
     `(rainbow-delimiters-depth-4-face (((alist-get colors 'class) :foreground (alist-get colors 'green))))
     `(rainbow-delimiters-depth-5-face (((alist-get colors 'class) :foreground (alist-get colors 'yellow))))
     `(rainbow-delimiters-depth-6-face (((alist-get colors 'class) :foreground (alist-get colors 'keyword))))
     `(rainbow-delimiters-depth-7-face (((alist-get colors 'class) :foreground (alist-get colors 'func))))
     `(rainbow-delimiters-depth-8-face (((alist-get colors 'class) :foreground (alist-get colors 'str))))
     `(rainbow-delimiters-mismatched-face (((alist-get colors 'class) :foreground (alist-get colors 'err) :overline t)))
     `(rainbow-delimiters-unmatched-face (((alist-get colors 'class) :foreground (alist-get colors 'err) :overline t)))

;;;;; rcirc
     `(rcirc-bright-nick (((alist-get colors 'class) (:background (alist-get colors 'aqua)-bg :foreground (alist-get colors 'cyan)))))
     `(rcirc-dim-nick (((alist-get colors 'class) (:foreground (alist-get colors 'base)-dim))))
     `(rcirc-keyword (((alist-get colors 'class) (:background (alist-get colors 'green)-bg-s :foreground (alist-get colors 'green)))))
     `(rcirc-timestamp (((alist-get colors 'class) (:foreground (alist-get colors 'keyword)))))
     `(rcirc-track-keyword (((alist-get colors 'class) (:background (alist-get colors 'green) :foreground (alist-get colors 'bg1)))))
     `(rcirc-url (((alist-get colors 'class) (:inherit link))))

;;;;; shm
     `(shm-current-face (((alist-get colors 'class) (:background (alist-get colors 'green)-bg-s))))
     `(shm-quarantine-face (((alist-get colors 'class) (:background (alist-get colors 'red)-bg-s))))

;;;;; show-paren
     `(show-paren-match (((alist-get colors 'class) (:foreground (alist-get colors 'mat) :inherit bold  :underline (alist-get colors ')(when spacemacs-theme-underline-parens t)))))
     `(show-paren-match-expression (((alist-get colors 'class) (:background (alist-get colors 'green)-bg-s))))
     `(show-paren-mismatch (((alist-get colors 'class) (:foreground (alist-get colors 'err) :inherit bold :underline (alist-get colors ')(when spacemacs-theme-underline-parens t)))))

;;;;; smartparens
     `(sp-pair-overlay-face (((alist-get colors 'class) (:background (alist-get colors 'highlight) :foreground nil))))
     `(sp-show-pair-match-face (((alist-get colors 'class) (:foreground (alist-get colors 'mat) :inherit bold  :underline (alist-get colors ')(when spacemacs-theme-underline-parens t)))))

;;;;; smerge
     `(smerge-base (((alist-get colors 'class) (:background (alist-get colors 'yellow)-bg))))
     `(smerge-markers (((alist-get colors 'class) (:background (alist-get colors 'ttip)-bg :foreground (alist-get colors 'ttip)))))
     `(smerge-mine (((alist-get colors 'class) (:background (alist-get colors 'red)-bg))))
     `(smerge-other (((alist-get colors 'class) (:background (alist-get colors 'green)-bg))))
     `(smerge-refined-added (((alist-get colors 'class) (:background (alist-get colors 'green)-bg-s :foreground (alist-get colors 'green)))))
     `(smerge-refined-changed (((alist-get colors 'class) (:background (alist-get colors 'blue)-bg-s :foreground (alist-get colors 'blue)))))
     `(smerge-refined-removed (((alist-get colors 'class) (:background (alist-get colors 'red)-bg-s :foreground (alist-get colors 'red)))))

;;;;; spaceline
     `(spaceline-flycheck-error  (((alist-get colors 'class) (:foreground (alist-get colors 'err)))))
     `(spaceline-flycheck-info   (((alist-get colors 'class) (:foreground (alist-get colors 'keyword)))))
     `(spaceline-flycheck-warning(((alist-get colors 'class) (:foreground (alist-get colors 'war)))))
     `(spaceline-python-venv (((alist-get colors 'class) (:foreground (alist-get colors 'comp)))))

;;;;; spacemacs-specific
     `(spacemacs-transient-state-title-face (((alist-get colors 'class) (:background nil :foreground (alist-get colors 'comp) :box nil :inherit bold))))

;;;;; swiper
     `(swiper-line-face (((alist-get colors 'class) (:background (alist-get colors 'highlight) :inherit bold))))
     `(swiper-match-face-1 (((alist-get colors 'class) (:inherit bold))))
     `(swiper-match-face-2 (((alist-get colors 'class) (:foreground (alist-get colors 'head1) :underline t))))
     `(swiper-match-face-3 (((alist-get colors 'class) (:foreground (alist-get colors 'head4) :underline t))))
     `(swiper-match-face-4 (((alist-get colors 'class) (:foreground (alist-get colors 'head3) :underline t))))

;;;;; tabbar
     `(tabbar-button (((alist-get colors 'class) (:inherit tabbar-default ))))
     `(tabbar-button-highlight (((alist-get colors 'class) (:inherit tabbar-default))))
     `(tabbar-default (((alist-get colors 'class) (:background (alist-get colors 'bg1) :foreground (alist-get colors 'head1) :height 0.9))))
     `(tabbar-highlight (((alist-get colors 'class) (:underline t))))
     `(tabbar-selected (((alist-get colors 'class) (:inherit tabbar-default :foreground (alist-get colors 'func) :weight bold))))
     `(tabbar-separator (((alist-get colors 'class) (:inherit tabbar-default))))
     `(tabbar-unselected (((alist-get colors 'class) (:inherit tabbar-default :background (alist-get colors 'bg1) :slant italic :weight light))))

;;;;; term
     `(term (((alist-get colors 'class) (:foreground (alist-get colors 'base) :background (alist-get colors 'bg1)))))
     `(term-color-black (((alist-get colors 'class) (:foreground (alist-get colors 'bg4)))))
     `(term-color-blue (((alist-get colors 'class) (:foreground (alist-get colors 'keyword)))))
     `(term-color-cyan (((alist-get colors 'class) (:foreground (alist-get colors 'cyan)))))
     `(term-color-green (((alist-get colors 'class) (:foreground (alist-get colors 'green)))))
     `(term-color-magenta (((alist-get colors 'class) (:foreground (alist-get colors 'magenta)))))
     `(term-color-red (((alist-get colors 'class) (:foreground (alist-get colors 'red)))))
     `(term-color-white (((alist-get colors 'class) (:foreground (alist-get colors 'base)))))
     `(term-color-yellow (((alist-get colors 'class) (:foreground (alist-get colors 'yellow)))))

;;;;; tide
     `(tide-hl-identifier-face (((alist-get colors 'class) (:foreground (alist-get colors 'yellow) :background (alist-get colors 'yellow)-bg))))

;;;;; treemacs
     `(treemacs-git-added-face (((alist-get colors 'class) (:foreground (alist-get colors 'green) :background (alist-get colors 'green)-bg))))
     `(treemacs-git-conflict-face (((alist-get colors 'class) (:foreground (alist-get colors 'red) :background (alist-get colors 'red)-bg))))
     `(treemacs-git-ignored-face (((alist-get colors 'class) (:foreground (alist-get colors 'yellow)))))
     `(treemacs-git-modified-face (((alist-get colors 'class) (:foreground (alist-get colors 'blue) :background (alist-get colors 'blue)-bg))))
     `(treemacs-git-untracked-face (((alist-get colors 'class) (:foreground (alist-get colors 'aqua) :background (alist-get colors 'aqua)-bg))))

;;;;; web-mode
     `(web-mode-builtin-face (((alist-get colors 'class) (:inherit (alist-get colors 'font)-lock-builtin-face))))
     `(web-mode-comment-face (((alist-get colors 'class) (:inherit (alist-get colors 'font)-lock-comment-face))))
     `(web-mode-constant-face (((alist-get colors 'class) (:inherit (alist-get colors 'font)-lock-constant-face))))
     `(web-mode-current-element-highlight-face (((alist-get colors 'class) (:background (alist-get colors 'bg3)))))
     `(web-mode-doctype-face (((alist-get colors 'class) (:inherit (alist-get colors 'font)-lock-comment-face))))
     `(web-mode-function-name-face (((alist-get colors 'class) (:inherit (alist-get colors 'font)-lock-function-name-face))))
     `(web-mode-html-attr-name-face (((alist-get colors 'class) (:foreground (alist-get colors 'func)))))
     `(web-mode-html-attr-value-face (((alist-get colors 'class) (:foreground (alist-get colors 'keyword)))))
     `(web-mode-html-tag-face (((alist-get colors 'class) (:foreground (alist-get colors 'keyword)))))
     `(web-mode-keyword-face (((alist-get colors 'class) (:foreground (alist-get colors 'keyword)))))
     `(web-mode-string-face (((alist-get colors 'class) (:foreground (alist-get colors 'str)))))
     `(web-mode-symbol-face (((alist-get colors 'class) (:foreground (alist-get colors 'type)))))
     `(web-mode-type-face (((alist-get colors 'class) (:inherit (alist-get colors 'font)-lock-type-face))))
     `(web-mode-warning-face (((alist-get colors 'class) (:inherit (alist-get colors 'font)-lock-warning-face))))

;;;;; which-key
     `(which-key-command-description-face (((alist-get colors 'class) (:foreground (alist-get colors 'base)))))
     `(which-key-group-description-face (((alist-get colors 'class) (:foreground (alist-get colors 'keyword)))))
     `(which-key-key-face (((alist-get colors 'class) (:foreground (alist-get colors 'func) :inherit bold))))
     `(which-key-separator-face (((alist-get colors 'class) (:background nil :foreground (alist-get colors 'str)))))
     `(which-key-special-key-face (((alist-get colors 'class) (:background (alist-get colors 'func) :foreground (alist-get colors 'bg1)))))

;;;;; which-function-mode
     `(which-func (((alist-get colors 'class) (:foreground (alist-get colors 'func)))))

;;;;; whitespace-mode
     `(whitespace-empty (((alist-get colors 'class) (:background nil :foreground (alist-get colors 'yellow)))))
     `(whitespace-indentation (((alist-get colors 'class) (:background nil :foreground (alist-get colors 'war)))))
     `(whitespace-line (((alist-get colors 'class) (:background nil :foreground (alist-get colors 'comp)))))
     `(whitespace-newline (((alist-get colors 'class) (:background nil :foreground (alist-get colors 'comp)))))
     `(whitespace-space (((alist-get colors 'class) (:background nil :foreground (alist-get colors 'act2)))))
     `(whitespace-space-after-tab (((alist-get colors 'class) (:background nil :foreground (alist-get colors 'yellow)))))
     `(whitespace-space-before-tab (((alist-get colors 'class) (:background nil :foreground (alist-get colors 'yellow)))))
     `(whitespace-tab (((alist-get colors 'class) (:background nil :foreground (alist-get colors 'act2)))))
     `(whitespace-trailing (((alist-get colors 'class) (:background (alist-get colors 'err) :foreground (alist-get colors 'war)))))

;;;;; other(alist-get colors ') need more work
     `(ac-completion-face (((alist-get colors 'class) (:underline t :foreground (alist-get colors 'keyword)))))
     `(ffap (((alist-get colors 'class) (:foreground (alist-get colors 'base)))))
     `(flx-highlight-face (((alist-get colors 'class) (:foreground (alist-get colors 'comp) :underline nil))))
     `(icompletep-determined (((alist-get colors 'class) :foreground (alist-get colors 'keyword))))
     `(js2-external-variable (((alist-get colors 'class) (:foreground (alist-get colors 'comp)))))
     `(js2-function-param (((alist-get colors 'class) (:foreground (alist-get colors 'const)))))
     `(js2-jsdoc-html-tag-delimiter (((alist-get colors 'class) (:foreground (alist-get colors 'str)))))
     `(js2-jsdoc-html-tag-name (((alist-get colors 'class) (:foreground (alist-get colors 'keyword)))))
     `(js2-jsdoc-value (((alist-get colors 'class) (:foreground (alist-get colors 'str)))))
     `(js2-private-function-call (((alist-get colors 'class) (:foreground (alist-get colors 'const)))))
     `(js2-private-member (((alist-get colors 'class) (:foreground (alist-get colors 'base)))))
     `(js3-error-face (((alist-get colors 'class) (:underline (alist-get colors 'war)))))
     `(js3-external-variable-face (((alist-get colors 'class) (:foreground (alist-get colors 'var)))))
     `(js3-function-param-face (((alist-get colors 'class) (:foreground (alist-get colors 'keyword)))))
     `(js3-instance-member-face (((alist-get colors 'class) (:foreground (alist-get colors 'const)))))
     `(js3-jsdoc-tag-face (((alist-get colors 'class) (:foreground (alist-get colors 'keyword)))))
     `(js3-warning-face (((alist-get colors 'class) (:underline (alist-get colors 'keyword)))))
     `(slime-repl-inputed-output-face (((alist-get colors 'class) (:foreground (alist-get colors 'comp)))))
     `(trailing-whitespace (((alist-get colors 'class) :foreground nil :background (alist-get colors 'err))))
     `(undo-tree-visualizer-current-face (((alist-get colors 'class) :foreground (alist-get colors 'keyword))))
     `(undo-tree-visualizer-default-face (((alist-get colors 'class) :foreground (alist-get colors 'base))))
     `(undo-tree-visualizer-register-face (((alist-get colors 'class) :foreground (alist-get colors 'comp))))
     `(undo-tree-visualizer-unmodified-face (((alist-get colors 'class) :foreground (alist-get colors 'var)))))

    (custom-theme-set-variables
     wal-theme

;;;;; ansi-color-names
     `(ansi-color-names-vector [(alist-get colors 'bg4) (alist-get colors 'red) (alist-get colors 'green) (alist-get colors 'yellow) (alist-get colors 'blue) (alist-get colors 'magenta) (alist-get colors 'cyan) (alist-get colors 'base)])

;;;;; hl-todo
     `(hl-todo-keyword-faces '(("TODO"   . (alist-get colors 'war))
                               ("NEXT"   . (alist-get colors 'war))
                               ("THEM"   . (alist-get colors 'aqua))
                               ("PROG"   . (alist-get colors 'blue))
                               ("OKAY"   . (alist-get colors 'blue))
                               ("DONT"   . (alist-get colors 'red))
                               ("FAIL"   . (alist-get colors 'red))
                               ("DONE"   . (alist-get colors 'suc))
                               ("NOTE"   . (alist-get colors 'yellow))
                               ("KLUDGE" . (alist-get colors 'yellow))
                               ("HACK"   . (alist-get colors 'yellow))
                               ("TEMP"   . (alist-get colors 'yellow))
                               ("FIXME"  . (alist-get colors 'war))
                               ("XXX"    . (alist-get colors 'war))
                               ("XXXX"   . (alist-get colors 'war))
                               ("???"    . (alist-get colors 'war))))

;;;;; pdf-tools
    `(pdf-view-midnight-colors '((alist-get colors 'base) . (alist-get colors 'bg1))))
      )))

(provide 'wal-theme-common)
;;; wal-theme-common ends here
