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


(defgroup wal-theme nil
  "Wal-theme options."
  :group 'faces)

(defcustom wal-theme-wal-cache-json
  (substitute-in-file-name "$HOME/.cache/wal/colors.json")
  "Location of cached `wal' theme in json format."
  :type 'string
  :group 'wal-theme)

(defcustom wal-theme-own-cache-dir "cache"
  "Location of cached `wal' theme in json format."
  :type 'string
  :group 'wal-theme)

(defcustom wal-theme-canonical-tty-color-names
  '(black red green yellow blue magenta cyan white)
  "The 8 most universaly supported tty color names.
Their look-alikes will be extracted from the `wal' cache, and
with the right escape sequences---i.e. (cat
~/.cache/wal/sequences &)---should be viewable even in the Linux
console. NOTE: Order matters."
  :type 'list
  :group 'wal-theme)

(defcustom wal-theme-force-tty-colors nil
  "Whether to use a non-extended version of wall theme.
Meant for setting tty theme regardless of gui support."
  :type 'boolean
  :group 'wal-theme)

(defcustom wal-theme-accent-color nil
  "Whether to use a non-extended version of wall theme.
Meant for setting tty theme regardless of gui support."
  :type 'symbol
  :group 'wal-theme)

(defcustom wal-theme-base-palette nil
  "Unmodified colors extracted directly from `wal'.
Stored in a flat alist."
  :type 'alist
  :group 'wal-theme)

(defcustom wal-theme-extended-palette nil
  "Extended color palette stored as flat alist."
  :type 'alist
  :group 'wal-theme)

(defcustom wal-theme-full-theme-colors nil
  "Colors in use in current wal-theme.
Generated from `wal-theme-base-palette'"
  :type 'alist
  :group 'wal-theme)

(defcustom wal-theme-tty-theme-colors nil
  "Colors in use in current wal-theme.
Extracted from `wal-theme-base-palette'"
  :type 'alist
  :group 'wal-theme)

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
to 'auto, tags may not be properly aligned."
  :type 'boolean
  :group 'wal-theme)

(defcustom wal-theme-org-height t
  "Use varying text heights for org headings."
  :type 'boolean
  :group 'wal-theme)

(defcustom wal-theme-org-bold t
  "Inherit text bold for org headings."
  :type 'boolean
  :group 'wal-theme)

(defcustom wal-theme-org-priority-bold t
  "Inherit text bold for priority items in agenda view."
  :type 'boolean
  :group 'wal-theme)

(defcustom wal-theme-org-highlight nil
  "Highlight org headings."
  :type 'boolean
  :group 'wal-theme)

(defcustom wal-theme-underline-parens t
  "If non-nil, underline matching parens.
when using command `show-paren-mode' or similar."
  :type 'boolean
  :group 'wal-theme)

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

(defun wal-theme-create-theme (&optional theme-name tty accent-color)
  "Create new wal-theme.
Do so by either from loading from wal-theme cache or generating
from wal cache. TTY deafults to \(display-graphic-p\) unless
overridden by `wal-theme-force-tty-colors', while ACCENT-COLOR
defaults to `wal-theme-accent-color' if set, 'magenta otherwise.
THEME-NAME gives a title to the generated theme."
  (if (file-newer-than-file-p
       wal-theme-wal-cache-json
       (concat (file-name-as-directory wal-theme-own-cache-dir) "full-theme-colors.json"))
      (wal-theme--load-current-theme-vars)
    (wal-theme--load-cached-theme-vars))
  (let ((tty (or tty wal-theme-force-tty-colors (display-graphic-p)))
        (accent-color (or accent-color wal-theme-accent-color 'magenta)))
    (let ((colors (if tty wal-theme-tty-theme-colors wal-theme-full-theme-colors)))
    (custom-theme-set-faces
     theme-name

;;;;; basics
     `(cursor (((alist-get 'class colors) (:background (alist-get 'cursor colors)))))
     `(custom-button (((alist-get 'class colors) :background (alist-get 'bg2 colors) :foreground (alist-get 'base colors) :box (:line-width 2 :style released-button))))
     `(default (((alist-get 'class colors) (:background (alist-get 'bg1 colors) :foreground (alist-get 'base colors)))))
     `(default-italic (((alist-get 'class colors) (:italic t))))
     `(error (((alist-get 'class colors) (:foreground (alist-get 'err colors)))))
     `(eval-sexp-fu-flash (((alist-get 'class colors) (:background (alist-get 'suc colors) :foreground (alist-get 'bg1 colors)))))
     `(eval-sexp-fu-flash-error (((alist-get 'class colors) (:background (alist-get 'err colors) :foreground (alist-get 'bg1 colors)))))
     `(font-lock-builtin-face (((alist-get 'class colors) (:foreground (alist-get 'keyword colors)))))
     `(font-lock-comment-face (((alist-get 'class colors) (:foreground ,(if wal-theme-comment-italic (alist-get 'comment-light colors) (alist-get 'comment colors)) :background ,(when wal-theme-comment-bg (alist-get 'comment-bg colors)) :slant ,(if wal-theme-comment-italic 'italic 'normal)))))
     `(font-lock-constant-face (((alist-get 'class colors) (:foreground (alist-get 'const colors)))))
     `(font-lock-doc-face (((alist-get 'class colors) (:foreground (alist-get 'meta colors)))))
     `(font-lock-function-name-face (((alist-get 'class colors) (:foreground (alist-get 'func colors) :inherit bold))))
     `(font-lock-keyword-face (((alist-get 'class colors) (:inherit bold :foreground (alist-get 'keyword colors) :slant ,(if wal-theme-keyword-italic 'italic 'normal)))))
     `(font-lock-negation-char-face (((alist-get 'class colors) (:foreground (alist-get 'const colors)))))
     `(font-lock-preprocessor-face (((alist-get 'class colors) (:foreground (alist-get 'func colors)))))
     `(font-lock-reference-face (((alist-get 'class colors) (:foreground (alist-get 'const colors)))))
     `(font-lock-string-face (((alist-get 'class colors) (:foreground (alist-get 'str colors)))))
     `(font-lock-type-face (((alist-get 'class colors) (:foreground (alist-get 'type colors) :inherit bold))))
     `(font-lock-variable-name-face (((alist-get 'class colors) (:foreground (alist-get 'var colors)))))
     `(font-lock-warning-face (((alist-get 'class colors) (:foreground (alist-get 'war colors) :background (alist-get 'bg1 colors)))))
     `(fringe (((alist-get 'class colors) (:background (alist-get 'bg1 colors) :foreground (alist-get 'base colors)))))
     `(header-line (((alist-get 'class colors) :background (alist-get 'bg4 colors))))
     `(highlight (((alist-get 'class colors) (:foreground (alist-get 'base colors) :background (alist-get 'highlight colors)))))
     `(hl-line (((alist-get 'class colors) (:background (alist-get 'bg2 colors)))))
     `(isearch (((alist-get 'class colors) (:foreground (alist-get 'bg1 colors) :background (alist-get 'mat colors)))))
     `(lazy-highlight (((alist-get 'class colors) (:background (alist-get 'green colors)-bg-s :weight normal))))
     `(link (((alist-get 'class colors) (:foreground (alist-get 'comment colors) :underline t))))
     `(link-visited (((alist-get 'class colors) (:foreground (alist-get 'comp colors) :underline t))))
     `(match (((alist-get 'class colors) (:background (alist-get 'highlight colors) :foreground (alist-get 'mat colors)))))
     `(minibuffer-prompt (((alist-get 'class colors) (:inherit bold :foreground (alist-get 'keyword colors)))))
     `(page-break-lines (((alist-get 'class colors) (:foreground (alist-get 'act2 colors)))))
     `(region (((alist-get 'class colors) (:background (alist-get 'highlight colors)))))
     `(secondary-selection (((alist-get 'class colors) (:background (alist-get 'bg3 colors)))))
     `(shadow (((alist-get 'class colors) (:foreground (alist-get 'base colors)-dim))))
     `(success (((alist-get 'class colors) (:foreground (alist-get 'suc colors)))))
     `(tooltip (((alist-get 'class colors) (:background (alist-get 'ttip colors)-sl :foreground (alist-get 'base colors) :bold nil :italic nil :underline nil))))
     `(vertical-border (((alist-get 'class colors) (:foreground (alist-get 'border colors)))))
     `(warning (((alist-get 'class colors) (:foreground (alist-get 'war colors)))))

;;;;; ace-window
     `(aw-leading-char-face (((alist-get 'class colors) (:foreground (alist-get 'func colors) :weight bold :height 2.0 :box (:line-width 1 :color (alist-get 'keyword colors) :style released-button)))))

;;;;; ahs
     `(ahs-face (((alist-get 'class colors) (:background (alist-get 'highlight colors)))))
     `(ahs-plugin-whole-buffer-face (((alist-get 'class colors) (:background (alist-get 'mat colors) :foreground (alist-get 'bg1 colors)))))

;;;;; anzu-mode
     `(anzu-mode-line (((alist-get 'class colors) (:foreground (alist-get 'yellow colors) :inherit bold))))

;;;;; auto-complete
     `(ac-completion-face (((alist-get 'class colors) (:background (alist-get 'ttip colors)-bg :foreground (alist-get 'ttip colors)))))

;;;;; avy
     `(avy-lead-face   (((alist-get 'class colors) (:background (alist-get 'green colors)-bg :foreground (alist-get 'green colors)))))
     `(avy-lead-face-0 (((alist-get 'class colors) (:background (alist-get 'green colors)-bg :foreground (alist-get 'yellow colors)))))
     `(avy-lead-face-1 (((alist-get 'class colors) (:background (alist-get 'green colors)-bg :foreground (alist-get 'magenta colors)))))
     `(avy-lead-face-2 (((alist-get 'class colors) (:background (alist-get 'green colors)-bg :foreground (alist-get 'blue colors)))))

;;;;; calfw
     `(cfw:face-title               (((alist-get 'class colors) (:foreground (alist-get 'head1 colors) :height 2.0 :weight bold :inherit variable-pitch))))
     `(cfw:face-header              (((alist-get 'class colors) (:foreground (alist-get 'base colors) :weight bold))))
     `(cfw:face-saturday            (((alist-get 'class colors) (:foreground (alist-get 'base colors) :weight bold))))
     `(cfw:face-sunday              (((alist-get 'class colors) (:foreground (alist-get 'base colors) :weight bold))))
     `(cfw:face-holiday             (((alist-get 'class colors) (:foreground (alist-get 'head1 colors) :weight bold))))
     `(cfw:face-grid                (((alist-get 'class colors) (:foreground (alist-get 'border colors)))))
     `(cfw:face-default-content     (((alist-get 'class colors) (:foreground (alist-get 'green colors)))))
     `(cfw:face-periods             (((alist-get 'class colors) (:foreground (alist-get 'cyan colors)))))
     `(cfw:face-day-title           (((alist-get 'class colors) (:background (alist-get 'head1 colors)-bg))))
     `(cfw:face-default-day         (((alist-get 'class colors) (:foreground (alist-get 'base colors) :weight bold))))
     `(cfw:face-annotation          (((alist-get 'class colors) (:foreground (alist-get 'aqua colors)))))
     `(cfw:face-disable             (((alist-get 'class colors) (:foreground (alist-get 'base colors)-dim))))
     `(cfw:face-today-title         (((alist-get 'class colors) (:background (alist-get 'blue colors) :weight bold))))
     `(cfw:face-today               (((alist-get 'class colors) (:background (alist-get 'head1 colors)-bg :weight bold))))
     `(cfw:face-select              (((alist-get 'class colors) (:background (alist-get 'magenta colors) :weight bold))))
     `(cfw:face-toolbar             (((alist-get 'class colors) (:foreground (alist-get 'base colors) :background (alist-get 'bg1 colors)))))
     `(cfw:face-toolbar-button-off  (((alist-get 'class colors) (:foreground (alist-get 'base colors) :weight bold))))
     `(cfw:face-toolbar-button-on   (((alist-get 'class colors) (:foreground (alist-get 'base colors) :weight bold))))

;;;;; cider
     `(cider-enlightened (((alist-get 'class colors) (:background nil :box (:color (alist-get 'yellow colors) :line-width -1 :style nil) :foreground (alist-get 'yellow colors)))))
     `(cider-enlightened-local (((alist-get 'class colors) (:foreground (alist-get 'yellow colors)))))
     `(cider-instrumented-face (((alist-get 'class colors) (:background nil :box (:color (alist-get 'red colors) :line-width -1 :style nil) :foreground (alist-get 'red colors)))))
     `(cider-result-overlay-face (((alist-get 'class colors) (:background nil :box (:color (alist-get 'blue colors) :line-width -1 :style nil) :foreground (alist-get 'blue colors)))))
     `(cider-test-error-face (((alist-get 'class colors) (:background (alist-get 'war colors) :foreground (alist-get 'bg1 colors)))))
     `(cider-test-failure-face (((alist-get 'class colors) (:background (alist-get 'err colors) :foreground (alist-get 'bg1 colors)))))
     `(cider-test-success-face (((alist-get 'class colors) (:background (alist-get 'suc colors) :foreground (alist-get 'bg1 colors)))))
     `(cider-traced-face (((alist-get 'class colors) :box (:color (alist-get 'cyan colors) :line-width -1 :style nil))))

;;;;; company
     `(company-echo-common (((alist-get 'class colors) (:background (alist-get 'base colors) :foreground (alist-get 'bg1 colors)))))
     `(company-preview (((alist-get 'class colors) (:background (alist-get 'ttip colors)-bg :foreground (alist-get 'ttip colors)))))
     `(company-preview-common (((alist-get 'class colors) (:background (alist-get 'ttip colors)-bg :foreground (alist-get 'base colors)))))
     `(company-preview-search (((alist-get 'class colors) (:inherit match))))
     `(company-scrollbar-bg (((alist-get 'class colors) (:background (alist-get 'bg2 colors)))))
     `(company-scrollbar-fg (((alist-get 'class colors) (:background (alist-get 'act2 colors)))))
     `(company-template-field (((alist-get 'class colors) (:inherit region))))
     `(company-tooltip (((alist-get 'class colors) (:background (alist-get 'ttip colors)-bg :foreground (alist-get 'ttip colors)))))
     `(company-tooltip-annotation (((alist-get 'class colors) (:foreground (alist-get 'type colors)))))
     `(company-tooltip-common (((alist-get 'class colors) (:background (alist-get 'ttip colors)-bg :foreground (alist-get 'keyword colors)))))
     `(company-tooltip-common-selection (((alist-get 'class colors) (:foreground (alist-get 'base colors)))))
     `(company-tooltip-mouse (((alist-get 'class colors) (:inherit highlight))))
     `(company-tooltip-search (((alist-get 'class colors) (:inherit match))))
     `(company-tooltip-selection (((alist-get 'class colors) (:background (alist-get 'ttip colors)-sl :foreground (alist-get 'base colors)))))

;;;;; diff
     `(diff-added             (((alist-get 'class colors) :background nil :foreground (alist-get 'green colors))))
     `(diff-changed           (((alist-get 'class colors) :background nil :foreground (alist-get 'blue colors))))
     `(diff-header            (((alist-get 'class colors) :background (alist-get 'cblk colors)-ln-bg :foreground (alist-get 'func colors))))
     `(diff-file-header       (((alist-get 'class colors) :background (alist-get 'cblk colors)-ln-bg :foreground (alist-get 'cblk colors))))
     `(diff-indicator-added   (((alist-get 'class colors) :background nil :foreground (alist-get 'green colors))))
     `(diff-indicator-changed (((alist-get 'class colors) :background nil :foreground (alist-get 'blue colors))))
     `(diff-indicator-removed (((alist-get 'class colors) :background nil :foreground (alist-get 'red colors))))
     `(diff-refine-added      (((alist-get 'class colors) :background (alist-get 'green colors) :foreground (alist-get 'bg1 colors))))
     `(diff-refine-changed    (((alist-get 'class colors) :background (alist-get 'blue colors) :foreground (alist-get 'bg1 colors))))
     `(diff-refine-removed    (((alist-get 'class colors) :background (alist-get 'red colors) :foreground (alist-get 'bg1 colors))))
     `(diff-removed           (((alist-get 'class colors) :background nil :foreground (alist-get 'red colors))))

;;;;; diff-hl
     `(diff-hl-change (((alist-get 'class colors) :background (alist-get 'blue colors)-bg-s :foreground (alist-get 'blue colors))))
     `(diff-hl-delete (((alist-get 'class colors) :background (alist-get 'red colors)-bg-s :foreground (alist-get 'red colors))))
     `(diff-hl-insert (((alist-get 'class colors) :background (alist-get 'green colors)-bg-s :foreground (alist-get 'green colors))))

;;;;; dired
     `(dired-directory (((alist-get 'class colors) (:foreground (alist-get 'keyword colors) :background (alist-get 'bg1 colors) :inherit bold))))
     `(dired-flagged (((alist-get 'class colors) (:foreground (alist-get 'red colors)))))
     `(dired-header (((alist-get 'class colors) (:foreground (alist-get 'comp colors) :inherit bold))))
     `(dired-ignored (((alist-get 'class colors) (:inherit shadow))))
     `(dired-mark (((alist-get 'class colors) (:foreground (alist-get 'comp colors) :inherit bold))))
     `(dired-marked (((alist-get 'class colors) (:foreground (alist-get 'magenta colors) :inherit bold))))
     `(dired-perm-write (((alist-get 'class colors) (:foreground (alist-get 'base colors) :underline t))))
     `(dired-symlink (((alist-get 'class colors) (:foreground (alist-get 'cyan colors) :background (alist-get 'bg1 colors) :inherit bold))))
     `(dired-warning (((alist-get 'class colors) (:foreground (alist-get 'war colors)))))

;;;;; ediff
     `(ediff-current-diff-A (((alist-get 'class colors)(:background (alist-get 'red colors)-bg :foreground (alist-get 'red colors)))))
     `(ediff-current-diff-Ancestor (((alist-get 'class colors)(:background (alist-get 'aqua colors)-bg :foreground (alist-get 'aqua colors)))))
     `(ediff-current-diff-B (((alist-get 'class colors)(:background (alist-get 'green colors)-bg :foreground (alist-get 'green colors)))))
     `(ediff-current-diff-C (((alist-get 'class colors)(:background (alist-get 'blue colors)-bg :foreground (alist-get 'blue colors)))))
     `(ediff-even-diff-A (((alist-get 'class colors)(:background (alist-get 'bg3 colors)))))
     `(ediff-even-diff-Ancestor (((alist-get 'class colors)(:background (alist-get 'bg3 colors)))))
     `(ediff-even-diff-B (((alist-get 'class colors)(:background (alist-get 'bg3 colors)))))
     `(ediff-even-diff-C (((alist-get 'class colors)(:background (alist-get 'bg3 colors)))))
     `(ediff-fine-diff-A (((alist-get 'class colors)(:background (alist-get 'red colors) :foreground (alist-get 'bg1 colors)))))
     `(ediff-fine-diff-Ancestor (((alist-get 'class colors)(:background nil :inherit bold))))
     `(ediff-fine-diff-B (((alist-get 'class colors)(:background (alist-get 'green colors) :foreground (alist-get 'bg1 colors)))))
     `(ediff-fine-diff-C (((alist-get 'class colors)(:background (alist-get 'blue colors) :foreground (alist-get 'bg1 colors)))))
     `(ediff-odd-diff-A (((alist-get 'class colors)(:background (alist-get 'bg4 colors)))))
     `(ediff-odd-diff-Ancestor (((alist-get 'class colors)(:background (alist-get 'bg4 colors)))))
     `(ediff-odd-diff-B (((alist-get 'class colors)(:background (alist-get 'bg4 colors)))))
     `(ediff-odd-diff-C (((alist-get 'class colors)(:background (alist-get 'bg4 colors)))))

;;;;; ein
     `(ein:cell-input-area(((alist-get 'class colors) (:background (alist-get 'bg2 colors)))))
     `(ein:cell-input-prompt (((alist-get 'class colors) (:foreground (alist-get 'suc colors)))))
     `(ein:cell-output-prompt (((alist-get 'class colors) (:foreground (alist-get 'err colors)))))
     `(ein:notification-tab-normal (((alist-get 'class colors) (:foreground (alist-get 'keyword colors)))))
     `(ein:notification-tab-selected (((alist-get 'class colors) (:foreground (alist-get 'suc colors) :inherit bold))))

;;;;; eldoc
     `(eldoc-highlight-function-argument (((alist-get 'class colors) (:foreground (alist-get 'mat colors) :inherit bold))))

;;;;; elfeed
     `(elfeed-search-date-face (((alist-get 'class colors) (:foreground (alist-get 'head2 colors)))))
     `(elfeed-search-feed-face (((alist-get 'class colors) (:foreground (alist-get 'blue colors)))))
     `(elfeed-search-tag-face (((alist-get 'class colors) (:foreground (alist-get 'func colors)))))
     `(elfeed-search-title-face (((alist-get 'class colors) (:foreground (alist-get 'var colors)))))
     `(elfeed-search-unread-title-face (((alist-get 'class colors) (:foreground (alist-get 'base colors)))))

;;;;; enh-ruby
     `(enh-ruby-op-face (((alist-get 'class colors) (:background (alist-get 'bg1 colors) :foreground (alist-get 'base colors)))))
     `(enh-ruby-string-delimiter-face (((alist-get 'class colors) (:foreground (alist-get 'str colors)))))

;;;;; erc
     `(erc-input-face (((alist-get 'class colors) (:foreground (alist-get 'func colors)))))
     `(erc-my-nick-face (((alist-get 'class colors) (:foreground (alist-get 'keyword colors)))))
     `(erc-nick-default-face (((alist-get 'class colors) (:foreground (alist-get 'keyword colors)))))
     `(erc-nick-prefix-face (((alist-get 'class colors) (:foreground (alist-get 'yellow colors)))))
     `(erc-notice-face (((alist-get 'class colors) (:foreground (alist-get 'str colors)))))
     `(erc-prompt-face (((alist-get 'class colors) (:foreground (alist-get 'mat colors) :inherit bold))))
     `(erc-timestamp-face (((alist-get 'class colors) (:foreground (alist-get 'keyword colors)))))

;;;;; eshell
     `(eshell-ls-archive (((alist-get 'class colors) (:foreground (alist-get 'red colors) :inherit bold))))
     `(eshell-ls-backup (((alist-get 'class colors) (:inherit font-lock-comment-face))))
     `(eshell-ls-clutter (((alist-get 'class colors) (:inherit font-lock-comment-face))))
     `(eshell-ls-directory (((alist-get 'class colors) (:foreground (alist-get 'keyword colors) :inherit bold))))
     `(eshell-ls-executable (((alist-get 'class colors) (:foreground (alist-get 'suc colors) :inherit bold))))
     `(eshell-ls-missing (((alist-get 'class colors) (:inherit font-lock-warning-face))))
     `(eshell-ls-product (((alist-get 'class colors) (:inherit font-lock-doc-face))))
     `(eshell-ls-special (((alist-get 'class colors) (:foreground (alist-get 'yellow colors) :inherit bold))))
     `(eshell-ls-symlink (((alist-get 'class colors) (:foreground (alist-get 'cyan colors) :inherit bold))))
     `(eshell-ls-unreadable (((alist-get 'class colors) (:foreground (alist-get 'base colors)))))
     `(eshell-prompt (((alist-get 'class colors) (:foreground (alist-get 'keyword colors) :inherit bold))))

;;;;; ESS
     `(ess-assignment-face (((alist-get 'class colors) (:foreground (alist-get 'type colors) :inherit bold))))
     `(ess-backquoted-face (((alist-get 'class colors) (:foreground (alist-get 'var colors)))))
     `(ess-constant-face (((alist-get 'class colors) (:inherit font-lock-constant-face))))
     `(ess-f-t-face (((alist-get 'class colors) (:inherit font-lock-constant-face))))
     `(ess-function-call-face (((alist-get 'class colors) (:foreground (alist-get 'func colors)))))
     `(ess-keyword-face (((alist-get 'class colors) (:inherit font-lock-keyword-face))))
     `(ess-matrix-face (((alist-get 'class colors) (:foreground (alist-get 'base colors)-dim))))
     `(ess-modifiers-face (((alist-get 'class colors) (:foreground (alist-get 'keyword colors)))))
     `(ess-numbers-face (((alist-get 'class colors) (:inherit font-lock-constant-face))))
     `(ess-operator-face (((alist-get 'class colors) (:foreground (alist-get 'var colors)))))
     `(ess-paren-face (((alist-get 'class colors) (:foreground (alist-get 'blue colors)))))
     `(ess-r-control-flow-keyword-face (((alist-get 'class colors) (:foreground (alist-get 'keyword colors)))))
     `(ess-r-signal-keyword-face (((alist-get 'class colors) (:foreground (alist-get 'war colors)))))

;;;;; evil
     `(evil-ex-substitute-matches (((alist-get 'class colors) (:background (alist-get 'red colors)-bg :foreground (alist-get 'red colors)))))
     `(evil-ex-substitute-replacement (((alist-get 'class colors) (:background (alist-get 'green colors)-bg :foreground (alist-get 'green colors)))))

;;;;; evil-goggles
      `(evil-goggles--pulse-face (((alist-get 'class colors) (:background (alist-get 'yellow colors)-bg :foreground (alist-get 'yellow colors)))))
      `(evil-goggles-change-face (((alist-get 'class colors) (:background (alist-get 'blue colors)-bg-s :foreground (alist-get 'blue colors)))))
      `(evil-goggles-commentary-face (((alist-get 'class colors) (:background (alist-get 'aqua colors)-bg :foreground (alist-get 'aqua colors)))))
      `(evil-goggles-delete-face (((alist-get 'class colors) (:background (alist-get 'red colors)-bg-s :foreground (alist-get 'red colors)))))
      `(evil-goggles-fill-and-move-face (((alist-get 'class colors) (:background (alist-get 'green colors)-bg-s :foreground (alist-get 'green colors)))))
      `(evil-goggles-indent-face (((alist-get 'class colors) (:background (alist-get 'green colors)-bg-s :foreground (alist-get 'green colors)))))
      `(evil-goggles-join-face (((alist-get 'class colors) (:background (alist-get 'green colors)-bg-s :foreground (alist-get 'green colors)))))
      `(evil-goggles-nerd-commenter-face (((alist-get 'class colors) (:background (alist-get 'aqua colors)-bg :foreground (alist-get 'aqua colors)))))
      `(evil-goggles-paste-face (((alist-get 'class colors) (:background (alist-get 'green colors)-bg-s :foreground (alist-get 'green colors)))))
      `(evil-goggles-record-macro-face (((alist-get 'class colors) (:background (alist-get 'blue colors)-bg-s :foreground (alist-get 'blue colors)))))
      `(evil-goggles-replace-with-register-face (((alist-get 'class colors) (:background (alist-get 'yellow colors)-bg :foreground (alist-get 'yellow colors)))))
      `(evil-goggles-set-marker-face (((alist-get 'class colors) (:background (alist-get 'blue colors)-bg-s :foreground (alist-get 'blue colors)))))
      `(evil-goggles-shift-face (((alist-get 'class colors) (:background (alist-get 'blue colors)-bg-s :foreground (alist-get 'blue colors)))))
      `(evil-goggles-surround-face (((alist-get 'class colors) (:background (alist-get 'blue colors)-bg-s :foreground (alist-get 'blue colors)))))
      `(evil-goggles-yank-face (((alist-get 'class colors) (:background (alist-get 'blue colors)-bg-s :foreground (alist-get 'blue colors)))))
      `(evil-goggles-undo-redo-add-face (((alist-get 'class colors) (:background (alist-get 'green colors)-bg-s :foreground (alist-get 'green colors)))))
      `(evil-goggles-undo-redo-change-face (((alist-get 'class colors) (:background (alist-get 'blue colors)-bg-s :foreground (alist-get 'blue colors)))))
      `(evil-goggles-undo-redo-remove-face (((alist-get 'class colors) (:background (alist-get 'red colors)-bg-s :foreground (alist-get 'red colors)))))

;;;;; flycheck
     `(flycheck-error
       ((,(append '((supports :underline (:style line))) class)
         (:underline (:style line :color (alist-get 'err colors))))
        ((alist-get 'class colors) (:foreground (alist-get 'base colors) :background (alist-get 'err colors) :inherit bold :underline t))))
     `(flycheck-error-list-checker-name (((alist-get 'class colors) (:foreground (alist-get 'keyword colors)))))
     `(flycheck-fringe-error (((alist-get 'class colors) (:foreground (alist-get 'err colors) :inherit bold))))
     `(flycheck-fringe-info (((alist-get 'class colors) (:foreground (alist-get 'keyword colors) :inherit bold))))
     `(flycheck-fringe-warning (((alist-get 'class colors) (:foreground (alist-get 'war colors) :inherit bold))))
     `(flycheck-info
       ((,(append '((supports :underline (:style line))) class)
         (:underline (:style line :color (alist-get 'keyword colors))))
        ((alist-get 'class colors) (:foreground (alist-get 'base colors) :background (alist-get 'keyword colors) :inherit bold :underline t))))
     `(flycheck-warning
       ((,(append '((supports :underline (:style line))) class)
         (:underline (:style line :color (alist-get 'war colors))))
        ((alist-get 'class colors) (:foreground (alist-get 'base colors) :background (alist-get 'war colors) :inherit bold :underline t))))

;;;;; flymake
     `(flymake-error ((,(append '((supports :underline (:style line))) class)
                       (:underline (:style line :color (alist-get 'err colors))))
                      ((alist-get 'class colors) (:foreground (alist-get 'base colors) :background (alist-get 'err colors) :inherit bold :underline t))))
     `(flymake-note ((,(append '((supports :underline (:style line))) class)
                      (:underline (:style wave :color (alist-get 'keyword colors))))
                     ((alist-get 'class colors) (:foreground (alist-get 'base colors) :background (alist-get 'keyword colors) :inherit bold :underline t))))
     `(flymake-warning ((,(append '((supports :underline (:style line))) class)
                         (:underline (:style line :color (alist-get 'war colors))))
                        ((alist-get 'class colors) (:foreground (alist-get 'base colors) :background (alist-get 'war colors) :inherit bold :underline t))))

;;;;; flyspell
     `(flyspell-incorrect ((,(append '((supports :underline (:style line))) class)
                            (:underline (:style wave :color (alist-get 'war colors))))
                           ((alist-get 'class colors) (:foreground (alist-get 'base colors) :background (alist-get 'war colors) :inherit bold :underline t))))
     `(flyspell-duplicate ((,(append '((supports :underline (:style line))) class)
                            (:underline (:style wave :color (alist-get 'keyword colors))))
                           ((alist-get 'class colors) (:foreground (alist-get 'base colors) :background (alist-get 'keyword colors) :inherit bold :underline t))))

;;;;; jabber
     `(jabber-activity-face (((alist-get 'class colors) (:inherit bold :foreground (alist-get 'red colors)))))
     `(jabber-activity-personal-face (((alist-get 'class colors) (:inherit bold :foreground (alist-get 'blue colors)))))
     `(jabber-chat-error (((alist-get 'class colors) (:inherit bold :foreground (alist-get 'red colors)))))
     `(jabber-chat-prompt-foreign (((alist-get 'class colors) (:inherit bold :foreground (alist-get 'red colors)))))
     `(jabber-chat-prompt-local (((alist-get 'class colors) (:inherit bold :foreground (alist-get 'blue colors)))))
     `(jabber-chat-prompt-system (((alist-get 'class colors) (:inherit bold :foreground (alist-get 'green colors)))))
     `(jabber-chat-text-foreign (((alist-get 'class colors) (:foreground (alist-get 'base colors)))))
     `(jabber-chat-text-local (((alist-get 'class colors) (:foreground (alist-get 'base colors)))))
     `(jabber-rare-time-face (((alist-get 'class colors) (:foreground (alist-get 'green colors)))))
     `(jabber-roster-user-away (((alist-get 'class colors) (:foreground (alist-get 'yellow colors)))))
     `(jabber-roster-user-chatty (((alist-get 'class colors) (:inherit bold :foreground (alist-get 'green colors)))))
     `(jabber-roster-user-dnd (((alist-get 'class colors) (:foreground (alist-get 'red colors)))))
     `(jabber-roster-user-error (((alist-get 'class colors) (:foreground (alist-get 'err colors)))))
     `(jabber-roster-user-offline (((alist-get 'class colors) (:foreground (alist-get 'base colors)))))
     `(jabber-roster-user-online (((alist-get 'class colors) (:inherit bold :foreground (alist-get 'green colors)))))
     `(jabber-roster-user-xa (((alist-get 'class colors) (:foreground (alist-get 'aqua colors)))))

;;;;; git-gutter-fr
     `(git-gutter-fr:added (((alist-get 'class colors) (:foreground (alist-get 'green colors) :inherit bold))))
     `(git-gutter-fr:deleted (((alist-get 'class colors) (:foreground (alist-get 'war colors) :inherit bold))))
     `(git-gutter-fr:modified (((alist-get 'class colors) (:foreground (alist-get 'keyword colors) :inherit bold))))

;;;;; git-timemachine
     `(git-timemachine-minibuffer-detail-face (((alist-get 'class colors) (:foreground (alist-get 'blue colors) :inherit bold :background (alist-get 'blue colors)-bg))))

;;;;; gnus
     `(gnus-emphasis-highlight-words (((alist-get 'class colors) (:background (alist-get 'suc colors) :foreground (alist-get 'bg1 colors)))))
     `(gnus-header-content (((alist-get 'class colors) (:foreground (alist-get 'keyword colors)))))
     `(gnus-header-from (((alist-get 'class colors) (:foreground (alist-get 'var colors)))))
     `(gnus-header-name (((alist-get 'class colors) (:foreground (alist-get 'comp colors)))))
     `(gnus-header-subject (((alist-get 'class colors) (:foreground (alist-get 'func colors) :inherit bold))))
     `(gnus-summary-cancelled (((alist-get 'class colors) (:background (alist-get 'war colors) :foreground (alist-get 'bg1 colors)))))

;;;;; guide-key
     `(guide-key/highlight-command-face (((alist-get 'class colors) (:foreground (alist-get 'base colors)))))
     `(guide-key/key-face (((alist-get 'class colors) (:foreground (alist-get 'keyword colors)))))
     `(guide-key/prefix-command-face (((alist-get 'class colors) (:foreground (alist-get 'keyword colors) :inherit bold))))

;;;;; helm
     `(helm-bookmark-directory (((alist-get 'class colors) (:inherit helm-ff-directory))))
     `(helm-bookmark-file (((alist-get 'class colors) (:foreground (alist-get 'base colors)))))
     `(helm-bookmark-gnus (((alist-get 'class colors) (:foreground (alist-get 'comp colors)))))
     `(helm-bookmark-info (((alist-get 'class colors) (:foreground (alist-get 'comp colors)))))
     `(helm-bookmark-man (((alist-get 'class colors) (:foreground (alist-get 'comp colors)))))
     `(helm-bookmark-w3m (((alist-get 'class colors) (:foreground (alist-get 'comp colors)))))
     `(helm-buffer-directory (((alist-get 'class colors) (:foreground (alist-get 'base colors) :background (alist-get 'bg1 colors)))))
     `(helm-buffer-file (((alist-get 'class colors) (:foreground (alist-get 'base colors) :background (alist-get 'bg1 colors)))))
     `(helm-buffer-not-saved (((alist-get 'class colors) (:foreground (alist-get 'comp colors) :background (alist-get 'bg1 colors)))))
     `(helm-buffer-process (((alist-get 'class colors) (:foreground (alist-get 'keyword colors) :background (alist-get 'bg1 colors)))))
     `(helm-buffer-saved-out (((alist-get 'class colors) (:foreground (alist-get 'base colors) :background (alist-get 'bg1 colors)))))
     `(helm-buffer-size (((alist-get 'class colors) (:foreground (alist-get 'base colors) :background (alist-get 'bg1 colors)))))
     `(helm-candidate-number (((alist-get 'class colors) (:background (alist-get 'bg1 colors) :foreground (alist-get 'keyword colors) :inherit bold))))
     `(helm-ff-directory (((alist-get 'class colors) (:foreground (alist-get 'keyword colors) :background (alist-get 'bg1 colors) :inherit bold))))
     `(helm-ff-dotted-directory (((alist-get 'class colors) (:foreground (alist-get 'keyword colors) :background (alist-get 'bg1 colors) :inherit bold))))
     `(helm-ff-dotted-symlink-directory (((alist-get 'class colors) (:foreground (alist-get 'cyan colors) :background (alist-get 'bg1 colors) :inherit bold))))
     `(helm-ff-executable (((alist-get 'class colors) (:foreground (alist-get 'suc colors) :background (alist-get 'bg1 colors) :weight normal))))
     `(helm-ff-file (((alist-get 'class colors) (:foreground (alist-get 'base colors) :background (alist-get 'bg1 colors) :weight normal))))
     `(helm-ff-invalid-symlink (((alist-get 'class colors) (:foreground (alist-get 'red colors) :background (alist-get 'bg1 colors) :inherit bold))))
     `(helm-ff-prefix (((alist-get 'class colors) (:foreground (alist-get 'bg1 colors) :background (alist-get 'keyword colors) :weight normal))))
     `(helm-ff-symlink (((alist-get 'class colors) (:foreground (alist-get 'cyan colors) :background (alist-get 'bg1 colors) :inherit bold))))
     `(helm-grep-cmd-line (((alist-get 'class colors) (:foreground (alist-get 'base colors) :background (alist-get 'bg1 colors)))))
     `(helm-grep-file (((alist-get 'class colors) (:foreground (alist-get 'base colors) :background (alist-get 'bg1 colors)))))
     `(helm-grep-finish (((alist-get 'class colors) (:foreground (alist-get 'base colors) :background (alist-get 'bg1 colors)))))
     `(helm-grep-lineno (((alist-get 'class colors) (:foreground (alist-get 'type colors) :background (alist-get 'bg1 colors) :inherit bold))))
     `(helm-grep-match (((alist-get 'class colors) (:foreground nil :background nil :inherit helm-match))))
     `(helm-header (((alist-get 'class colors) (:foreground (alist-get 'base colors) :background (alist-get 'bg1 colors) :underline nil :box nil))))
     `(helm-header-line-left-margin (((alist-get 'class colors) (:foreground (alist-get 'keyword colors) :background (alist-get 'nil colors)))))
     `(helm-match (((alist-get 'class colors) (:background (alist-get 'head1 colors)-bg :foreground (alist-get 'head1 colors)))))
     `(helm-match-item (((alist-get 'class colors) (:background (alist-get 'head1 colors)-bg :foreground (alist-get 'head1 colors)))))
     `(helm-moccur-buffer (((alist-get 'class colors) (:foreground (alist-get 'var colors) :background (alist-get 'bg1 colors)))))
     `(helm-selection (((alist-get 'class colors) (:background (alist-get 'highlight colors)))))
     `(helm-selection-line (((alist-get 'class colors) (:background (alist-get 'bg2 colors)))))
     `(helm-separator (((alist-get 'class colors) (:foreground (alist-get 'comp colors) :background (alist-get 'bg1 colors)))))
     `(helm-source-header (((alist-get 'class colors) (:background (alist-get 'comp colors) :foreground (alist-get 'bg1 colors) :inherit bold))))
     `(helm-time-zone-current (((alist-get 'class colors) (:foreground (alist-get 'keyword colors) :background (alist-get 'bg1 colors)))))
     `(helm-time-zone-home (((alist-get 'class colors) (:foreground (alist-get 'comp colors) :background (alist-get 'bg1 colors)))))
     `(helm-visible-mark (((alist-get 'class colors) (:foreground (alist-get 'keyword colors) :background (alist-get 'bg3 colors)))))

;;;;; helm-swoop
     `(helm-swoop-target-line-block-face (((alist-get 'class colors) (:foreground (alist-get 'base colors) :background (alist-get 'highlight colors)))))
     `(helm-swoop-target-line-face (((alist-get 'class colors) (:background (alist-get 'highlight colors)))))
     `(helm-swoop-target-word-face (((alist-get 'class colors) (:background (alist-get 'highlight colors) :foreground (alist-get 'mat colors)))))

;;;;; highlights
     `(hi-green  (((alist-get 'class colors) (:foreground (alist-get 'green colors) :background (alist-get 'green colors)-bg))))
     `(hi-yellow (((alist-get 'class colors) (:foreground (alist-get 'yellow colors) :background (alist-get 'yellow colors)-bg))))

;;;;; highlight-indentation
     `(highlight-indentation-face (((alist-get 'class colors) (:background (alist-get 'comment colors)-bg))))

;;;;; highlight-symbol
     `(highlight-symbol-face (((alist-get 'class colors) (:background (alist-get 'bg2 colors)))))

;;;;; hydra
     `(hydra-face-blue (((alist-get 'class colors) (:foreground (alist-get 'blue colors)))))
     `(hydra-face-red (((alist-get 'class colors) (:foreground (alist-get 'red colors)))))

;;;;; ido
     `(ido-first-match (((alist-get 'class colors) (:foreground (alist-get 'comp colors) :inherit bold))))
     `(ido-only-match (((alist-get 'class colors) (:foreground (alist-get 'mat colors) :inherit bold))))
     `(ido-subdir (((alist-get 'class colors) (:foreground (alist-get 'keyword colors)))))
     `(ido-vertical-match-face (((alist-get 'class colors) (:foreground (alist-get 'comp colors) :underline nil))))

;;;;; info
     `(info-header-xref (((alist-get 'class colors) (:foreground (alist-get 'func colors) :underline t))))
     `(info-menu (((alist-get 'class colors) (:foreground (alist-get 'suc colors)))))
     `(info-node (((alist-get 'class colors) (:foreground (alist-get 'func colors) :inherit bold))))
     `(info-quoted-name (((alist-get 'class colors) (:foreground (alist-get 'keyword colors)))))
     `(info-reference-item (((alist-get 'class colors) (:background nil :underline t :inherit bold))))
     `(info-string (((alist-get 'class colors) (:foreground (alist-get 'str colors)))))
     `(info-title-1 (((alist-get 'class colors) (:height 1.4 :inherit bold))))
     `(info-title-2 (((alist-get 'class colors) (:height 1.3 :inherit bold))))
     `(info-title-3 (((alist-get 'class colors) (:height 1.3))))
     `(info-title-4 (((alist-get 'class colors) (:height 1.2))))

;;;;; ivy
     `(ivy-current-match (((alist-get 'class colors) (:background (alist-get 'highlight colors) :inherit bold))))
     `(ivy-minibuffer-match-face-1 (((alist-get 'class colors) (:inherit bold))))
     `(ivy-minibuffer-match-face-2 (((alist-get 'class colors) (:foreground (alist-get 'head1 colors) :underline t))))
     `(ivy-minibuffer-match-face-3 (((alist-get 'class colors) (:foreground (alist-get 'head4 colors) :underline t))))
     `(ivy-minibuffer-match-face-4 (((alist-get 'class colors) (:foreground (alist-get 'head3 colors) :underline t))))
     `(ivy-remote (((alist-get 'class colors) (:foreground (alist-get 'cyan colors)))))

;;;;; latex
     `(font-latex-bold-face (((alist-get 'class colors) (:foreground (alist-get 'comp colors)))))
     `(font-latex-italic-face (((alist-get 'class colors) (:foreground (alist-get 'keyword colors) :italic t))))
     `(font-latex-match-reference-keywords (((alist-get 'class colors) (:foreground (alist-get 'const colors)))))
     `(font-latex-match-variable-keywords (((alist-get 'class colors) (:foreground (alist-get 'var colors)))))
     `(font-latex-sectioning-0-face (((alist-get 'class colors) (:inherit bold :foreground (alist-get 'head3 colors) :height ,(if wal-theme-org-height 1.3 1.0) :background ,(when wal-theme-org-highlight head3-bg)))))
     `(font-latex-sectioning-1-face (((alist-get 'class colors) (:inherit bold :foreground (alist-get 'head4 colors) :height ,(if wal-theme-org-height 1.3 1.0) :background ,(when wal-theme-org-highlight head4-bg)))))
     `(font-latex-sectioning-2-face (((alist-get 'class colors) (:inherit bold :foreground (alist-get 'head1 colors) :height ,(if wal-theme-org-height 1.3 1.0) :background ,(when wal-theme-org-highlight head1-bg)))))
     `(font-latex-sectioning-3-face (((alist-get 'class colors) (:inherit bold :foreground (alist-get 'head2 colors) :height ,(if wal-theme-org-height 1.2 1.0) :background ,(when wal-theme-org-highlight head2-bg)))))
     `(font-latex-sectioning-4-face (((alist-get 'class colors) (:bold nil :foreground (alist-get 'head3 colors) :height ,(if wal-theme-org-height 1.1 1.0) :background ,(when wal-theme-org-highlight head3-bg)))))
     `(font-latex-sectioning-5-face (((alist-get 'class colors) (:bold nil :foreground (alist-get 'head4 colors) :background ,(when wal-theme-org-highlight head4-bg)))))
     `(font-latex-string-face (((alist-get 'class colors) (:foreground (alist-get 'str colors)))))
     `(font-latex-warning-face (((alist-get 'class colors) (:foreground (alist-get 'war colors)))))

;;;;; ledger-mode
     `(ledger-font-directive-face (((alist-get 'class colors) (:foreground (alist-get 'meta colors)))))
     `(ledger-font-posting-amount-face (((alist-get 'class colors) (:foreground (alist-get 'yellow colors)))))
     `(ledger-font-posting-date-face (((alist-get 'class colors) (:foreground (alist-get 'head1 colors)))))
     `(ledger-occur-xact-face (((alist-get 'class colors) (:background (alist-get 'bg2 colors)))))

;;;;; linum-mode
     `(linum (((alist-get 'class colors) (:foreground (alist-get 'lnum colors) :background (alist-get 'bg2 colors) :inherit default))))

;;;;; line-numbers
     `(line-number (((alist-get 'class colors) (:foreground (alist-get 'lnum colors) :background (alist-get 'bg2 colors) :inherit default))))
     `(line-number-current-line (((alist-get 'class colors) (:foreground (alist-get 'base colors) :background (alist-get 'bg2 colors) :inherit line-number))))

;;;;; linum-relative
     `(linum-relative-current-face (((alist-get 'class colors) (:foreground (alist-get 'comp colors)))))

;;;;; magit
     `(magit-blame-culprit (((alist-get 'class colors) :background (alist-get 'yellow colors)-bg :foreground (alist-get 'yellow colors))))
     `(magit-blame-date    (((alist-get 'class colors) :background (alist-get 'yellow colors)-bg :foreground (alist-get 'green colors))))
     `(magit-blame-hash    (((alist-get 'class colors) :background (alist-get 'yellow colors)-bg :foreground (alist-get 'func colors))))
     `(magit-blame-header  (((alist-get 'class colors) :background (alist-get 'yellow colors)-bg :foreground (alist-get 'green colors))))
     `(magit-blame-heading (((alist-get 'class colors) :background (alist-get 'yellow colors)-bg :foreground (alist-get 'green colors))))
     `(magit-blame-name    (((alist-get 'class colors) :background (alist-get 'yellow colors)-bg :foreground (alist-get 'yellow colors))))
     `(magit-blame-sha1    (((alist-get 'class colors) :background (alist-get 'yellow colors)-bg :foreground (alist-get 'func colors))))
     `(magit-blame-subject (((alist-get 'class colors) :background (alist-get 'yellow colors)-bg :foreground (alist-get 'yellow colors))))
     `(magit-blame-summary (((alist-get 'class colors) :background (alist-get 'yellow colors)-bg :foreground (alist-get 'yellow colors))))
     `(magit-blame-time    (((alist-get 'class colors) :background (alist-get 'yellow colors)-bg :foreground (alist-get 'green colors))))
     `(magit-branch (((alist-get 'class colors) (:foreground (alist-get 'const colors) :inherit bold))))
     `(magit-branch-current (((alist-get 'class colors) (:background (alist-get 'blue colors)-bg :foreground (alist-get 'blue colors) :inherit bold :box t))))
     `(magit-branch-local (((alist-get 'class colors) (:background (alist-get 'blue colors)-bg :foreground (alist-get 'blue colors) :inherit bold))))
     `(magit-branch-remote (((alist-get 'class colors) (:background (alist-get 'aqua colors)-bg :foreground (alist-get 'aqua colors) :inherit bold))))
     `(magit-diff-context-highlight (((alist-get 'class colors) (:background (alist-get 'bg2 colors) :foreground (alist-get 'base colors)))))
     `(magit-diff-hunk-heading (((alist-get 'class colors) (:background (alist-get 'ttip colors)-bg :foreground (alist-get 'ttip colors)))))
     `(magit-diff-hunk-heading-highlight (((alist-get 'class colors) (:background (alist-get 'ttip colors)-sl :foreground (alist-get 'base colors)))))
     `(magit-hash (((alist-get 'class colors) (:foreground (alist-get 'var colors)))))
     `(magit-hunk-heading           (((alist-get 'class colors) (:background (alist-get 'bg3 colors)))))
     `(magit-hunk-heading-highlight (((alist-get 'class colors) (:background (alist-get 'bg3 colors)))))
     `(magit-item-highlight (((alist-get 'class colors) :background (alist-get 'bg2 colors))))
     `(magit-log-author (((alist-get 'class colors) (:foreground (alist-get 'func colors)))))
     `(magit-log-head-label-head (((alist-get 'class colors) (:background (alist-get 'yellow colors) :foreground (alist-get 'bg1 colors) :inherit bold))))
     `(magit-log-head-label-local (((alist-get 'class colors) (:background (alist-get 'keyword colors) :foreground (alist-get 'bg1 colors) :inherit bold))))
     `(magit-log-head-label-remote (((alist-get 'class colors) (:background (alist-get 'suc colors) :foreground (alist-get 'bg1 colors) :inherit bold))))
     `(magit-log-head-label-tags (((alist-get 'class colors) (:background (alist-get 'magenta colors) :foreground (alist-get 'bg1 colors) :inherit bold))))
     `(magit-log-head-label-wip (((alist-get 'class colors) (:background (alist-get 'cyan colors) :foreground (alist-get 'bg1 colors) :inherit bold))))
     `(magit-log-sha1 (((alist-get 'class colors) (:foreground (alist-get 'str colors)))))
     `(magit-process-ng (((alist-get 'class colors) (:foreground (alist-get 'war colors) :inherit bold))))
     `(magit-process-ok (((alist-get 'class colors) (:foreground (alist-get 'func colors) :inherit bold))))
     `(magit-reflog-amend (((alist-get 'class colors) (:foreground (alist-get 'magenta colors)))))
     `(magit-reflog-checkout (((alist-get 'class colors) (:foreground (alist-get 'blue colors)))))
     `(magit-reflog-cherry-pick (((alist-get 'class colors) (:foreground (alist-get 'green colors)))))
     `(magit-reflog-commit (((alist-get 'class colors) (:foreground (alist-get 'green colors)))))
     `(magit-reflog-merge (((alist-get 'class colors) (:foreground (alist-get 'green colors)))))
     `(magit-reflog-other (((alist-get 'class colors) (:foreground (alist-get 'cyan colors)))))
     `(magit-reflog-rebase (((alist-get 'class colors) (:foreground (alist-get 'magenta colors)))))
     `(magit-reflog-remote (((alist-get 'class colors) (:foreground (alist-get 'cyan colors)))))
     `(magit-reflog-reset (((alist-get 'class colors) (:foreground (alist-get 'red colors)))))
     `(magit-section-heading        (((alist-get 'class colors) (:foreground (alist-get 'keyword colors) :inherit bold))))
     `(magit-section-highlight      (((alist-get 'class colors) (:background (alist-get 'bg2 colors)))))
     `(magit-section-title (((alist-get 'class colors) (:background (alist-get 'bg1 colors) :foreground (alist-get 'keyword colors) :inherit bold))))

;;;;; man
     `(Man-overstrike (((alist-get 'class colors) (:foreground (alist-get 'head1 colors) :inherit bold))))
     `(Man-reverse (((alist-get 'class colors) (:foreground (alist-get 'highlight colors)))))
     `(Man-underline (((alist-get 'class colors) (:foreground (alist-get 'comp colors) :underline t))))

;;;;; markdown
     `(markdown-header-face-1 (((alist-get 'class colors) (:inherit bold :foreground (alist-get 'head1 colors) :height ,(if wal-theme-org-height 1.3 1.0) :background ,(when wal-theme-org-highlight head1-bg)))))
     `(markdown-header-face-2 (((alist-get 'class colors) (:inherit bold :foreground (alist-get 'head2 colors) :height ,(if wal-theme-org-height 1.2 1.0) :background ,(when wal-theme-org-highlight head2-bg)))))
     `(markdown-header-face-3 (((alist-get 'class colors) (:bold nil :foreground (alist-get 'head3 colors) :height ,(if wal-theme-org-height 1.1 1.0) :background ,(when wal-theme-org-highlight head3-bg)))))
     `(markdown-header-face-4 (((alist-get 'class colors) (:bold nil :foreground (alist-get 'head4 colors) :background ,(when wal-theme-org-highlight head4-bg)))))
     `(markdown-header-face-5 (((alist-get 'class colors) (:bold nil :foreground (alist-get 'head1 colors)))))
     `(markdown-header-face-6 (((alist-get 'class colors) (:bold nil :foreground (alist-get 'head2 colors)))))
     `(markdown-table-face (((alist-get 'class colors) (:foreground (alist-get 'base colors) :background (alist-get 'head1 colors)-bg))))

;;;;; mode-line
     `(mode-line           (((alist-get 'class colors) (:foreground (alist-get 'base colors) :background (alist-get 'act1 colors) :box (:color (alist-get 'border colors) :line-width 1)))))
     `(mode-line-buffer-id (((alist-get 'class colors) (:inherit bold :foreground (alist-get 'func colors)))))
     `(mode-line-inactive  (((alist-get 'class colors) (:foreground (alist-get 'base colors) :background (alist-get 'bg1 colors)  :box (:color (alist-get 'border colors) :line-width 1)))))

;;;;; mu4e
     `(mu4e-attach-number-face (((alist-get 'class colors) (:foreground (alist-get 'var colors)))))
     `(mu4e-cited-1-face (((alist-get 'class colors) (:foreground (alist-get 'head1 colors)))))
     `(mu4e-cited-2-face (((alist-get 'class colors) (:foreground (alist-get 'head2 colors)))))
     `(mu4e-cited-3-face (((alist-get 'class colors) (:foreground (alist-get 'head3 colors)))))
     `(mu4e-cited-4-face (((alist-get 'class colors) (:foreground (alist-get 'head4 colors)))))
     `(mu4e-cited-5-face (((alist-get 'class colors) (:foreground (alist-get 'head1 colors)))))
     `(mu4e-cited-6-face (((alist-get 'class colors) (:foreground (alist-get 'head2 colors)))))
     `(mu4e-cited-7-face (((alist-get 'class colors) (:foreground (alist-get 'head3 colors)))))
     `(mu4e-contact-face (((alist-get 'class colors) (:foreground (alist-get 'func colors)))))
     `(mu4e-draft-face (((alist-get 'class colors) (:foreground (alist-get 'var colors)))))
     `(mu4e-flagged-face (((alist-get 'class colors) (:foreground (alist-get 'yellow colors) :inherit bold))))
     `(mu4e-header-key-face (((alist-get 'class colors) (:foreground (alist-get 'meta colors) :inherit bold))))
     `(mu4e-header-title-face (((alist-get 'class colors) (:foreground (alist-get 'keyword colors) :inherit bold))))
     `(mu4e-header-marks-face (((alist-get 'class colors) (:foreground (alist-get 'comp colors)))))
     `(mu4e-header-value-face (((alist-get 'class colors) (:foreground (alist-get 'keyword colors) :inherit bold))))
     `(mu4e-header-highlight-face (((alist-get 'class colors) (:background (alist-get 'highlight colors)))))
     `(mu4e-highlight-face (((alist-get 'class colors) (:foreground (alist-get 'comp colors)))))
     `(mu4e-title-face (((alist-get 'class colors) (:foreground (alist-get 'head2 colors) :inherit bold))))
     `(mu4e-replied-face (((alist-get 'class colors) (:foreground (alist-get 'green colors)))))
     `(mu4e-modeline-face (((alist-get 'class colors) (:foreground (alist-get 'yellow colors)))))
     `(mu4e-special-header-value-face (((alist-get 'class colors) (:foreground (alist-get 'green colors)))))
     `(mu4e-unread-face (((alist-get 'class colors) (:foreground (alist-get 'head1 colors) :inherit bold))))
     `(mu4e-view-url-number-face (((alist-get 'class colors) (:foreground (alist-get 'comp colors)))))

;;;;; mu4e-maildirs
     `(mu4e-maildirs-extension-maildir-hl-face (((alist-get 'class colors) (:foreground (alist-get 'head1 colors) :inherit bold))))

;;;;; notmuch
     `(notmuch-search-date (((alist-get 'class colors) (:foreground (alist-get 'func colors)))))
     `(notmuch-search-flagged-face (((alist-get 'class colors) (:weight extra-bold))))
     `(notmuch-search-non-matching-authors (((alist-get 'class colors) (:foreground (alist-get 'base colors)-dim))))
     `(notmuch-search-unread-face (((alist-get 'class colors) (:background (alist-get 'highlight colors)-dim :box (alist-get 'border colors)))))
     `(notmuch-tag-face (((alist-get 'class colors) (:foreground (alist-get 'keyword colors)))))
     `(notmuch-tag-flagged (((alist-get 'class colors) (:foreground (alist-get 'war colors)))))

;;;;; neotree
     `(neo-dir-link-face (((alist-get 'class colors) (:foreground (alist-get 'keyword colors) :inherit bold))))
     `(neo-expand-btn-face (((alist-get 'class colors) (:foreground (alist-get 'base colors)))))
     `(neo-file-link-face (((alist-get 'class colors) (:foreground (alist-get 'base colors)))))
     `(neo-root-dir-face (((alist-get 'class colors) (:foreground (alist-get 'func colors) :inherit bold))))

;;;;; org
     `(org-agenda-clocking (((alist-get 'class colors) (:background (alist-get 'highlight colors) :foreground (alist-get 'comp colors)))))
     `(org-agenda-date (((alist-get 'class colors) (:foreground (alist-get 'var colors) :height ,(if wal-theme-org-agenda-height 1.1 1.0)))))
     `(org-agenda-date-today (((alist-get 'class colors) (:foreground (alist-get 'keyword colors) :inherit bold :height ,(if wal-theme-org-agenda-height 1.3 1.0)))))
     `(org-agenda-date-weekend (((alist-get 'class colors) (:inherit bold :foreground (alist-get 'var colors)))))
     `(org-agenda-done (((alist-get 'class colors) (:foreground (alist-get 'suc colors) :height ,(if wal-theme-org-agenda-height 1.2 1.0)))))
     `(org-agenda-structure (((alist-get 'class colors) (:inherit bold :foreground (alist-get 'comp colors)))))
     `(org-block (((alist-get 'class colors) (:background (alist-get 'cblk colors)-bg :foreground (alist-get 'cblk colors)))))
     `(org-block-begin-line (((alist-get 'class colors) (:background (alist-get 'cblk colors)-ln-bg :foreground (alist-get 'cblk colors)-ln))))
     `(org-block-end-line (((alist-get 'class colors) (:background (alist-get 'cblk colors)-ln-bg :foreground (alist-get 'cblk colors)-ln))))
     `(org-clock-overlay (((alist-get 'class colors) (:foreground (alist-get 'comp colors)))))
     `(org-code (((alist-get 'class colors) (:foreground (alist-get 'cyan colors)))))
     `(org-column (((alist-get 'class colors) (:background (alist-get 'highlight colors)))))
     `(org-column-title (((alist-get 'class colors) (:background (alist-get 'highlight colors)))))
     `(org-date (((alist-get 'class colors) (:underline t :foreground (alist-get 'var colors)))))
     `(org-date-selected (((alist-get 'class colors) (:background (alist-get 'func colors) :foreground (alist-get 'bg1 colors)))))
     `(org-document-info-keyword (((alist-get 'class colors) (:foreground (alist-get 'meta colors)))))
     `(org-document-title (((alist-get 'class colors) (:foreground (alist-get 'func colors) :inherit bold :height ,(if wal-theme-org-height 1.4 1.0) :underline t))))
     `(org-done (((alist-get 'class colors) (:foreground (alist-get 'suc colors) :inherit bold :background (alist-get 'green colors)-bg))))
     `(org-ellipsis (((alist-get 'class colors) (:foreground (alist-get 'keyword colors)))))
     `(org-footnote  (((alist-get 'class colors) (:underline t :foreground (alist-get 'base colors)))))
     `(org-hide (((alist-get 'class colors) (:foreground (alist-get 'base colors)))))
     `(org-kbd (((alist-get 'class colors) (:inherit region :foreground (alist-get 'base colors) :box (:line-width 1 :style released-button)))))
     `(org-level-1 (((alist-get 'class colors) (:inherit bold :bold ,(if wal-theme-org-bold 'unspecified nil) :foreground (alist-get 'head1 colors) :height ,(if wal-theme-org-height 1.3 1.0) :background ,(when wal-theme-org-highlight head1-bg)))))
     `(org-level-2 (((alist-get 'class colors) (:inherit bold :bold ,(if wal-theme-org-bold 'unspecified nil) :foreground (alist-get 'head2 colors) :height ,(if wal-theme-org-height 1.2 1.0) :background ,(when wal-theme-org-highlight head2-bg)))))
     `(org-level-3 (((alist-get 'class colors) (:bold nil :foreground (alist-get 'head3 colors) :height ,(if wal-theme-org-height 1.1 1.0) :background ,(when wal-theme-org-highlight head3-bg)))))
     `(org-level-4 (((alist-get 'class colors) (:bold nil :foreground (alist-get 'head4 colors) :background ,(when wal-theme-org-highlight head4-bg)))))
     `(org-level-5 (((alist-get 'class colors) (:bold nil :foreground (alist-get 'head1 colors)))))
     `(org-level-6 (((alist-get 'class colors) (:bold nil :foreground (alist-get 'head2 colors)))))
     `(org-level-7 (((alist-get 'class colors) (:bold nil :foreground (alist-get 'head3 colors)))))
     `(org-level-8 (((alist-get 'class colors) (:bold nil :foreground (alist-get 'head4 colors)))))
     `(org-link (((alist-get 'class colors) (:underline t :foreground (alist-get 'comment colors)))))
     `(org-meta-line (((alist-get 'class colors) (:foreground (alist-get 'meta colors)))))
     `(org-mode-line-clock-overrun (((alist-get 'class colors) (:foreground (alist-get 'err colors)))))
     `(org-priority (((alist-get 'class colors) (:foreground (alist-get 'war colors) :inherit bold :bold ,(if wal-theme-org-priority-bold 'unspecified nil)))))
     `(org-quote (((alist-get 'class colors) (:inherit org-block :slant italic))))
     `(org-scheduled (((alist-get 'class colors) (:foreground (alist-get 'comp colors)))))
     `(org-scheduled-today (((alist-get 'class colors) (:foreground (alist-get 'func colors) :height ,(if wal-theme-org-agenda-height 1.2 1.0)))))
     `(org-scheduled-previously (((alist-get 'class colors) (:foreground (alist-get 'base colors) :slant italic))))
     `(org-sexp-date (((alist-get 'class colors) (:foreground (alist-get 'base colors)))))
     `(org-special-keyword (((alist-get 'class colors) (:foreground (alist-get 'func colors)))))
     `(org-table (((alist-get 'class colors) (:foreground (alist-get 'base colors) :background (alist-get 'head1 colors)-bg))))
     `(org-tag (((alist-get 'class colors) (:foreground (alist-get 'meta colors)))))
     `(org-time-grid (((alist-get 'class colors) (:foreground (alist-get 'str colors)))))
     `(org-todo (((alist-get 'class colors) (:foreground (alist-get 'war colors) :inherit bold :background (alist-get 'yellow colors)-bg))))
     `(org-upcoming-deadline (((alist-get 'class colors) (:foreground (alist-get 'war colors) :inherit org-priority))))
     `(org-upcoming-distant-deadline (((alist-get 'class colors) (:foreground (alist-get 'suc colors) :inherit org-priority))))
     `(org-verbatim (((alist-get 'class colors) (:foreground (alist-get 'keyword colors)))))
     `(org-verse (((alist-get 'class colors) (:inherit org-block :slant italic))))
     `(org-warning (((alist-get 'class colors) (:foreground (alist-get 'err colors) :inherit org-priority))))

;;;;; outline
     `(outline-1 (((alist-get 'class colors) (:inherit org-level-1))))
     `(outline-2 (((alist-get 'class colors) (:inherit org-level-2))))
     `(outline-3 (((alist-get 'class colors) (:inherit org-level-3))))
     `(outline-4 (((alist-get 'class colors) (:inherit org-level-4))))
     `(outline-5 (((alist-get 'class colors) (:inherit org-level-5))))
     `(outline-6 (((alist-get 'class colors) (:inherit org-level-6))))
     `(outline-7 (((alist-get 'class colors) (:inherit org-level-7))))
     `(outline-8 (((alist-get 'class colors) (:inherit org-level-8))))

;;;;; perspective
     `(persp-selected-face (((alist-get 'class colors) (:inherit bold :foreground (alist-get 'func colors)))))

;;;;; popup
     `(popup-enu-selection-face (((alist-get 'class colors) (:background (alist-get 'ttip colors)-sl :foreground (alist-get 'base colors)))))
     `(popup-face (((alist-get 'class colors) (:background (alist-get 'ttip colors)-bg :foreground (alist-get 'ttip colors)))))
     `(popup-isearch-match (((alist-get 'class colors) (:inherit match))))
     `(popup-menu-face (((alist-get 'class colors) (:background (alist-get 'ttip colors)-bg :foreground (alist-get 'base colors)))))
     `(popup-menu-mouse-face (((alist-get 'class colors) (:inherit highlight))))
     `(popup-scroll-bar-background-face (((alist-get 'class colors) (:background (alist-get 'bg2 colors)))))
     `(popup-scroll-bar-foreground-face (((alist-get 'class colors) (:background (alist-get 'act2 colors)))))
     `(popup-tip-face (((alist-get 'class colors) (:background (alist-get 'ttip colors)-sl :foreground (alist-get 'base colors) :bold nil :italic nil :underline nil))))

;;;;; powerline
     `(powerline-active1 (((alist-get 'class colors) (:background (alist-get 'act2 colors) :foreground (alist-get 'base colors)))))
     `(powerline-active2 (((alist-get 'class colors) (:background (alist-get 'act2 colors) :foreground (alist-get 'base colors)))))
     `(powerline-inactive1 (((alist-get 'class colors) (:background (alist-get 'bg2 colors) :foreground (alist-get 'base colors)))))
     `(powerline-inactive2 (((alist-get 'class colors) (:background (alist-get 'bg2 colors) :foreground (alist-get 'base colors)))))

;;;;; rainbow-delimiters
     `(rainbow-delimiters-depth-1-face (((alist-get 'class colors) :foreground (alist-get 'keyword colors))))
     `(rainbow-delimiters-depth-2-face (((alist-get 'class colors) :foreground (alist-get 'func colors))))
     `(rainbow-delimiters-depth-3-face (((alist-get 'class colors) :foreground (alist-get 'str colors))))
     `(rainbow-delimiters-depth-4-face (((alist-get 'class colors) :foreground (alist-get 'green colors))))
     `(rainbow-delimiters-depth-5-face (((alist-get 'class colors) :foreground (alist-get 'yellow colors))))
     `(rainbow-delimiters-depth-6-face (((alist-get 'class colors) :foreground (alist-get 'keyword colors))))
     `(rainbow-delimiters-depth-7-face (((alist-get 'class colors) :foreground (alist-get 'func colors))))
     `(rainbow-delimiters-depth-8-face (((alist-get 'class colors) :foreground (alist-get 'str colors))))
     `(rainbow-delimiters-mismatched-face (((alist-get 'class colors) :foreground (alist-get 'err colors) :overline t)))
     `(rainbow-delimiters-unmatched-face (((alist-get 'class colors) :foreground (alist-get 'err colors) :overline t)))

;;;;; rcirc
     `(rcirc-bright-nick (((alist-get 'class colors) (:background (alist-get 'aqua colors)-bg :foreground (alist-get 'cyan colors)))))
     `(rcirc-dim-nick (((alist-get 'class colors) (:foreground (alist-get 'base colors)-dim))))
     `(rcirc-keyword (((alist-get 'class colors) (:background (alist-get 'green colors)-bg-s :foreground (alist-get 'green colors)))))
     `(rcirc-timestamp (((alist-get 'class colors) (:foreground (alist-get 'keyword colors)))))
     `(rcirc-track-keyword (((alist-get 'class colors) (:background (alist-get 'green colors) :foreground (alist-get 'bg1 colors)))))
     `(rcirc-url (((alist-get 'class colors) (:inherit link))))

;;;;; shm
     `(shm-current-face (((alist-get 'class colors) (:background (alist-get 'green colors)-bg-s))))
     `(shm-quarantine-face (((alist-get 'class colors) (:background (alist-get 'red colors)-bg-s))))

;;;;; show-paren
     `(show-paren-match (((alist-get 'class colors) (:foreground (alist-get 'mat colors) :inherit bold  :underline ,(when wal-theme-underline-parens t)))))
     `(show-paren-match-expression (((alist-get 'class colors) (:background (alist-get 'green colors)-bg-s))))
     `(show-paren-mismatch (((alist-get 'class colors) (:foreground (alist-get 'err colors) :inherit bold :underline ,(when wal-theme-underline-parens t)))))

;;;;; smartparens
     `(sp-pair-overlay-face (((alist-get 'class colors) (:background (alist-get 'highlight colors) :foreground nil))))
     `(sp-show-pair-match-face (((alist-get 'class colors) (:foreground (alist-get 'mat colors) :inherit bold  :underline ,(when wal-theme-underline-parens t)))))

;;;;; smerge
     `(smerge-base (((alist-get 'class colors) (:background (alist-get 'yellow colors)-bg))))
     `(smerge-markers (((alist-get 'class colors) (:background (alist-get 'ttip colors)-bg :foreground (alist-get 'ttip colors)))))
     `(smerge-mine (((alist-get 'class colors) (:background (alist-get 'red colors)-bg))))
     `(smerge-other (((alist-get 'class colors) (:background (alist-get 'green colors)-bg))))
     `(smerge-refined-added (((alist-get 'class colors) (:background (alist-get 'green colors)-bg-s :foreground (alist-get 'green colors)))))
     `(smerge-refined-changed (((alist-get 'class colors) (:background (alist-get 'blue colors)-bg-s :foreground (alist-get 'blue colors)))))
     `(smerge-refined-removed (((alist-get 'class colors) (:background (alist-get 'red colors)-bg-s :foreground (alist-get 'red colors)))))

;;;;; spaceline
     `(spaceline-flycheck-error  (((alist-get 'class colors) (:foreground (alist-get 'err colors)))))
     `(spaceline-flycheck-info   (((alist-get 'class colors) (:foreground (alist-get 'keyword colors)))))
     `(spaceline-flycheck-warning(((alist-get 'class colors) (:foreground (alist-get 'war colors)))))
     `(spaceline-python-venv (((alist-get 'class colors) (:foreground (alist-get 'comp colors)))))

;;;;; wal-specific
     `(wal-transient-state-title-face (((alist-get 'class colors) (:background nil :foreground (alist-get 'comp colors) :box nil :inherit bold))))

;;;;; swiper
     `(swiper-line-face (((alist-get 'class colors) (:background (alist-get 'highlight colors) :inherit bold))))
     `(swiper-match-face-1 (((alist-get 'class colors) (:inherit bold))))
     `(swiper-match-face-2 (((alist-get 'class colors) (:foreground (alist-get 'head1 colors) :underline t))))
     `(swiper-match-face-3 (((alist-get 'class colors) (:foreground (alist-get 'head4 colors) :underline t))))
     `(swiper-match-face-4 (((alist-get 'class colors) (:foreground (alist-get 'head3 colors) :underline t))))

;;;;; tabbar
     `(tabbar-button (((alist-get 'class colors) (:inherit tabbar-default ))))
     `(tabbar-button-highlight (((alist-get 'class colors) (:inherit tabbar-default))))
     `(tabbar-default (((alist-get 'class colors) (:background (alist-get 'bg1 colors) :foreground (alist-get 'head1 colors) :height 0.9))))
     `(tabbar-highlight (((alist-get 'class colors) (:underline t))))
     `(tabbar-selected (((alist-get 'class colors) (:inherit tabbar-default :foreground (alist-get 'func colors) :weight bold))))
     `(tabbar-separator (((alist-get 'class colors) (:inherit tabbar-default))))
     `(tabbar-unselected (((alist-get 'class colors) (:inherit tabbar-default :background (alist-get 'bg1 colors) :slant italic :weight light))))

;;;;; term
     `(term (((alist-get 'class colors) (:foreground (alist-get 'base colors) :background (alist-get 'bg1 colors)))))
     `(term-color-black (((alist-get 'class colors) (:foreground (alist-get 'bg4 colors)))))
     `(term-color-blue (((alist-get 'class colors) (:foreground (alist-get 'keyword colors)))))
     `(term-color-cyan (((alist-get 'class colors) (:foreground (alist-get 'cyan colors)))))
     `(term-color-green (((alist-get 'class colors) (:foreground (alist-get 'green colors)))))
     `(term-color-magenta (((alist-get 'class colors) (:foreground (alist-get 'magenta colors)))))
     `(term-color-red (((alist-get 'class colors) (:foreground (alist-get 'red colors)))))
     `(term-color-white (((alist-get 'class colors) (:foreground (alist-get 'base colors)))))
     `(term-color-yellow (((alist-get 'class colors) (:foreground (alist-get 'yellow colors)))))

;;;;; tide
     `(tide-hl-identifier-face (((alist-get 'class colors) (:foreground (alist-get 'yellow colors) :background (alist-get 'yellow colors)-bg))))

;;;;; treemacs
     `(treemacs-git-added-face (((alist-get 'class colors) (:foreground (alist-get 'green colors) :background (alist-get 'green colors)-bg))))
     `(treemacs-git-conflict-face (((alist-get 'class colors) (:foreground (alist-get 'red colors) :background (alist-get 'red colors)-bg))))
     `(treemacs-git-ignored-face (((alist-get 'class colors) (:foreground (alist-get 'yellow colors)))))
     `(treemacs-git-modified-face (((alist-get 'class colors) (:foreground (alist-get 'blue colors) :background (alist-get 'blue colors)-bg))))
     `(treemacs-git-untracked-face (((alist-get 'class colors) (:foreground (alist-get 'aqua colors) :background (alist-get 'aqua colors)-bg))))

;;;;; web-mode
     `(web-mode-builtin-face (((alist-get 'class colors) (:inherit (alist-get 'font colors)-lock-builtin-face))))
     `(web-mode-comment-face (((alist-get 'class colors) (:inherit (alist-get 'font colors)-lock-comment-face))))
     `(web-mode-constant-face (((alist-get 'class colors) (:inherit (alist-get 'font colors)-lock-constant-face))))
     `(web-mode-current-element-highlight-face (((alist-get 'class colors) (:background (alist-get 'bg3 colors)))))
     `(web-mode-doctype-face (((alist-get 'class colors) (:inherit (alist-get 'font colors)-lock-comment-face))))
     `(web-mode-function-name-face (((alist-get 'class colors) (:inherit (alist-get 'font colors)-lock-function-name-face))))
     `(web-mode-html-attr-name-face (((alist-get 'class colors) (:foreground (alist-get 'func colors)))))
     `(web-mode-html-attr-value-face (((alist-get 'class colors) (:foreground (alist-get 'keyword colors)))))
     `(web-mode-html-tag-face (((alist-get 'class colors) (:foreground (alist-get 'keyword colors)))))
     `(web-mode-keyword-face (((alist-get 'class colors) (:foreground (alist-get 'keyword colors)))))
     `(web-mode-string-face (((alist-get 'class colors) (:foreground (alist-get 'str colors)))))
     `(web-mode-symbol-face (((alist-get 'class colors) (:foreground (alist-get 'type colors)))))
     `(web-mode-type-face (((alist-get 'class colors) (:inherit (alist-get 'font colors)-lock-type-face))))
     `(web-mode-warning-face (((alist-get 'class colors) (:inherit (alist-get 'font colors)-lock-warning-face))))

;;;;; which-key
     `(which-key-command-description-face (((alist-get 'class colors) (:foreground (alist-get 'base colors)))))
     `(which-key-group-description-face (((alist-get 'class colors) (:foreground (alist-get 'keyword colors)))))
     `(which-key-key-face (((alist-get 'class colors) (:foreground (alist-get 'func colors) :inherit bold))))
     `(which-key-separator-face (((alist-get 'class colors) (:background nil :foreground (alist-get 'str colors)))))
     `(which-key-special-key-face (((alist-get 'class colors) (:background (alist-get 'func colors) :foreground (alist-get 'bg1 colors)))))

;;;;; which-function-mode
     `(which-func (((alist-get 'class colors) (:foreground (alist-get 'func colors)))))

;;;;; whitespace-mode
     `(whitespace-empty (((alist-get 'class colors) (:background nil :foreground (alist-get 'yellow colors)))))
     `(whitespace-indentation (((alist-get 'class colors) (:background nil :foreground (alist-get 'war colors)))))
     `(whitespace-line (((alist-get 'class colors) (:background nil :foreground (alist-get 'comp colors)))))
     `(whitespace-newline (((alist-get 'class colors) (:background nil :foreground (alist-get 'comp colors)))))
     `(whitespace-space (((alist-get 'class colors) (:background nil :foreground (alist-get 'act2 colors)))))
     `(whitespace-space-after-tab (((alist-get 'class colors) (:background nil :foreground (alist-get 'yellow colors)))))
     `(whitespace-space-before-tab (((alist-get 'class colors) (:background nil :foreground (alist-get 'yellow colors)))))
     `(whitespace-tab (((alist-get 'class colors) (:background nil :foreground (alist-get 'act2 colors)))))
     `(whitespace-trailing (((alist-get 'class colors) (:background (alist-get 'err colors) :foreground (alist-get 'war colors)))))

;;;;; other, need more work
     `(ac-completion-face (((alist-get 'class colors) (:underline t :foreground (alist-get 'keyword colors)))))
     `(ffap (((alist-get 'class colors) (:foreground (alist-get 'base colors)))))
     `(flx-highlight-face (((alist-get 'class colors) (:foreground (alist-get 'comp colors) :underline nil))))
     `(icompletep-determined (((alist-get 'class colors) :foreground (alist-get 'keyword colors))))
     `(js2-external-variable (((alist-get 'class colors) (:foreground (alist-get 'comp colors)))))
     `(js2-function-param (((alist-get 'class colors) (:foreground (alist-get 'const colors)))))
     `(js2-jsdoc-html-tag-delimiter (((alist-get 'class colors) (:foreground (alist-get 'str colors)))))
     `(js2-jsdoc-html-tag-name (((alist-get 'class colors) (:foreground (alist-get 'keyword colors)))))
     `(js2-jsdoc-value (((alist-get 'class colors) (:foreground (alist-get 'str colors)))))
     `(js2-private-function-call (((alist-get 'class colors) (:foreground (alist-get 'const colors)))))
     `(js2-private-member (((alist-get 'class colors) (:foreground (alist-get 'base colors)))))
     `(js3-error-face (((alist-get 'class colors) (:underline (alist-get 'war colors)))))
     `(js3-external-variable-face (((alist-get 'class colors) (:foreground (alist-get 'var colors)))))
     `(js3-function-param-face (((alist-get 'class colors) (:foreground (alist-get 'keyword colors)))))
     `(js3-instance-member-face (((alist-get 'class colors) (:foreground (alist-get 'const colors)))))
     `(js3-jsdoc-tag-face (((alist-get 'class colors) (:foreground (alist-get 'keyword colors)))))
     `(js3-warning-face (((alist-get 'class colors) (:underline (alist-get 'keyword colors)))))
     `(slime-repl-inputed-output-face (((alist-get 'class colors) (:foreground (alist-get 'comp colors)))))
     `(trailing-whitespace (((alist-get 'class colors) :foreground nil :background (alist-get 'err colors))))
     `(undo-tree-visualizer-current-face (((alist-get 'class colors) :foreground (alist-get 'keyword colors))))
     `(undo-tree-visualizer-default-face (((alist-get 'class colors) :foreground (alist-get 'base colors))))
     `(undo-tree-visualizer-register-face (((alist-get 'class colors) :foreground (alist-get 'comp colors))))
     `(undo-tree-visualizer-unmodified-face (((alist-get 'class colors) :foreground (alist-get 'var colors)))))

    (custom-theme-set-variables
     theme-name

;;;;; ansi-color-names
     `(ansi-color-names-vector [(alist-get 'bg4 colors) (alist-get 'red colors) (alist-get 'green colors) (alist-get 'yellow colors) (alist-get 'blue colors) (alist-get 'magenta colors) (alist-get 'cyan colors) (alist-get 'base colors)])

;;;;; hl-todo
     `(hl-todo-keyword-faces '(("TODO"   . (alist-get 'war colors))
                               ("NEXT"   . (alist-get 'war colors))
                               ("THEM"   . (alist-get 'aqua colors))
                               ("PROG"   . (alist-get 'blue colors))
                               ("OKAY"   . (alist-get 'blue colors))
                               ("DONT"   . (alist-get 'red colors))
                               ("FAIL"   . (alist-get 'red colors))
                               ("DONE"   . (alist-get 'suc colors))
                               ("NOTE"   . (alist-get 'yellow colors))
                               ("KLUDGE" . (alist-get 'yellow colors))
                               ("HACK"   . (alist-get 'yellow colors))
                               ("TEMP"   . (alist-get 'yellow colors))
                               ("FIXME"  . (alist-get 'war colors))
                               ("XXX"    . (alist-get 'war colors))
                               ("XXXX"   . (alist-get 'war colors))
                               ("???"    . (alist-get 'war colors))))

;;;;; pdf-tools
    `(pdf-view-midnight-colors '((alist-get 'base colors) . (alist-get 'bg1 colors)))))))

(provide 'wal-theme-common)
;;; wal-theme-common ends here
