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
(require 'color)

(defgroup wal-theme nil
  "Wal-theme options."
  :group 'faces)

(defcustom wal-theme-wal-cache-dir
  (file-name-as-directory (expand-file-name "~/.cache/wal"))
  "Location of wal cache directory."
  :type 'string
  :group 'wal-theme)

(defvar wal-theme--wal-cache-json-file
  (concat wal-theme-wal-cache-dir "colors.json")
  "Location of cached wal theme in json format.")

(defcustom wal-theme-own-cache-dir
  (file-name-as-directory (expand-file-name "./cache"))
  "Location of wal-theme cache directory."
  :type 'string
  :group 'wal-theme)

(defcustom wal-theme-ansi-color-names
  '(black red green yellow blue magenta cyan white)
  "The 8 most universaly supported TTY color names.
They will be extracted from `wal-theme--cache-json-file', and
with the right escape sequences applied using

source ${HOME}/.cache/wal/colors-tty.sh

should be viewable even in the Linux console (See
https://github.com/dylanaraps/pywal/wiki/Getting-Started#applying-the-theme-to-new-terminals
for more details). NOTE: Order matters."
  :type 'list
  :group 'wal-theme)

(defvar wal-theme--own-cache-base-palette-json-file
  (concat wal-theme-own-cache-dir "base-palette.json")
  "Location of cached base wal theme palette with no additional colors.
The colors are named using `wal-theme-ansi-color-names'")

(defvar wal-theme--own-cache-extended-palette-json-file
  (concat wal-theme-own-cache-dir "extended-palette.json")
  "Location of cached extended wal theme palette with additional colors.
The colors are named using `wal-theme-ansi-color-names'")

(defvar wal-theme--own-cache-semantic-tty-colors-json-file
  (concat wal-theme-own-cache-dir "tty-theme-semantic-colors.json")
  "Location of cached TTY wal theme colors with semantic color names.")

(defvar wal-theme--own-cache-semantic-gui-colors-json-file
  (concat wal-theme-own-cache-dir "gui-theme-semantic-colors.json")
  "Location of cached GUI wal theme colors with semantic color names.")

(defcustom wal-theme-force-tty-colors nil
  "Whether to use TTY version of wall theme.
Meant for setting TTY theme regardless of GUI support."
  :type 'boolean
  :group 'wal-theme)

(defcustom wal-theme-accent-color 'magenta
  "Predominant color in generated wal-theme."
  :type 'symbol
  :group 'wal-theme)

(defcustom wal-theme-base-palette nil
  "Current base palette extracted from `wal-theme--wal-cache-json-file'.
Stored as a flat alist, and cached in `wal-theme--own-cache-base-palette-json-file'."
  :type 'alist
  :group 'wal-theme)

(defcustom wal-theme-extended-palette nil
  "Extended palette computed from `wal-theme--wal-cache-json-file'.
Stored as a flat alist, and cached in `wal-theme--own-cache-extended-palette-json-file'."
  :type 'alist
  :group 'wal-theme)

(defcustom wal-theme-semantic-gui-colors nil
  "Currently applied wal-theme's GUI colors.
Stored as a flat alist, and cached in `wal-theme--own-cache-semantic-gui-colors-json-file'."
  :type 'alist
  :group 'wal-theme)

(defcustom wal-theme-semantic-tty-colors nil
  "Currently applied wal-theme's TTY colors.
Stored as a flat alist, and cached in `wal-theme--own-cache-semantic-tty-colors-json-file'."
  :type 'alist
  :group 'wal-theme)

(defcustom wal-theme-comment-bg nil
  "Use a background for comment lines."
  :type 'boolean
  :group 'wal-theme)

(defcustom wal-theme-comment-italic nil
  "Enable italics for comments and also disable background."
  :type 'boolean
  :group 'wal-theme)

(defcustom wal-theme-keyword-italic t
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

(defcustom wal-theme-org-highlight t
  "Highlight org headings."
  :type 'boolean
  :group 'wal-theme)

(defcustom wal-theme-underline-parens t
  "If non-nil, underline matching parens.
when using command `show-paren-mode' or similar."
  :type 'boolean
  :group 'wal-theme)

(defun wal-theme--load-wal-theme (&optional json color-names)
  "Read JSON as the most complete of the cached wal files.
COLOR-NAMES will be associated with the first 8 colors of the
cached wal colors. COLOR-NAMES are meant to be used in
conjunction with `wal-theme-ansi-color-names'.
\"Special\" wal colors such as \"background\", \"foreground\",
and \"cursor\", tend to \(but do not always\) correspond to the
remaining colors generated by wal. Add those special colors to
the returned alist."
  (let ((json (or json wal-theme--wal-cache-json-file))
        (json-object-type 'alist)
        (color-names (or color-names wal-theme-ansi-color-names)))
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
  "Extend \(darken \(-\) or lighten \(+\)\) COLOR.
Do so by 2 * NUM-DEGREES \(NUM-DEGREES lighter, and NUM-DEGREES
darker\), in increments of DEGREE-SIZE percentage points. Return
list of extended colors"
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
  "Use `wal-theme--extend-base-color' to extend entire base PALETTE.
which defaults to `wal-theme-base-palette' and returns an
extended palette alist intended to be stored in
`wal-theme-extended-palette'. Like
`wal-theme--extend-base-color', extend \(darken \(-\) or
lighten \(+\)\) COLOR. Do so by 2 * NUM-DEGREES \(NUM-DEGREES
lighter, and NUM-DEGREES darker\), in increments of DEGREE-SIZE
percentage points."
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
  "Return SHADE of COLOR from current `wal-theme' PALETTE.
Choose color that is darker (-) or lightener (+) than COLOR by
SHADE. SHADE defaults to 0, returning original wal COLOR. If
SHADE exceeds number of available shades, the darkest/lightest
shade is returned. If TTY is t, return original, TTY compatible
`wal' color regardless od SHADE."
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
ACCENT-COLOR sets the main theme color---defaults to `wal-theme-accent-color'"
  (let ((accent-color (or accent-color 'wal-theme-accent-color)))
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


(defun wal-theme--cache-own-theme (&optional base-palette extended-palette tty-theme-colors
                                                 gui-theme-colors)
  "Serializes all palettes and colors in json format.
BASE-PALETTE, EXTENDED-PALETTE, TTY-THEME-COLORS, and
GUI-THEME-COLORS all refer to the variables provided by
wal-theme by default, prefixed with the package name."
  (let ((json-encoding-pretty-print t)
        (base-palette (or base-palette wal-theme-base-palette))
        (extended-palette (or extended-palette wal-theme-extended-palette))
        (tty-theme-colors (or base-palette wal-theme-semantic-tty-colors))
        (gui-theme-colors (or base-palette wal-theme-semantic-gui-colors)))
      (if (null (and
                 (file-exists-p wal-theme--own-cache-base-palette-json-file)
                 (file-exists-p wal-theme--own-cache-extended-palette-json-file)
                 (file-exists-p wal-theme--own-cache-semantic-tty-colors-json-file)
                 (file-exists-p wal-theme--own-cache-semantic-gui-colors-json-file)))
          (progn
            (if (null (file-directory-p wal-theme-own-cache-dir))
                (make-directory wal-theme-own-cache-dir))
            (with-temp-file wal-theme--own-cache-base-palette-json-file
              (insert (json-encode-list wal-theme-base-palette)))
            (with-temp-file wal-theme--own-cache-extended-palette-json-file
              (insert (json-encode-list wal-theme-extended-palette)))
            (with-temp-file wal-theme--own-cache-semantic-tty-colors-json-file
              (insert (json-encode-list wal-theme-semantic-tty-colors)))
            (with-temp-file wal-theme--own-cache-semantic-gui-colors-json-file
              (insert (json-encode-list wal-theme-semantic-gui-colors)))))))

(defun wal-theme--load-own-theme (&optional load-from-cache)
  "Load current wal-theme variables for use in `wal-theme-create-theme'.
If LOAD-FROM-CACHE is t, then load from cache. Otherwise compute
palettes and colors afresh and cache them."
  (let ((load-from-cache (or load-from-cache
                             (and (file-exists-p wal-theme--own-cache-base-palette-json-file))
                             (file-newer-than-file-p wal-theme--own-cache-base-palette-json-file
                                                     wal-theme--wal-cache-json-file))))
    (if load-from-cache
        (progn
          (setq wal-theme-base-palette (wal-theme--load-wal-theme))
          (setq wal-theme-extended-palette (wal-theme--extend-base-palette 4 5))
          (setq wal-theme-semantic-gui-colors (wal-theme--generate-theme-colors nil))
          (setq wal-theme-semantic-tty-colors (wal-theme--generate-theme-colors t))
          (wal-theme--cache-own-theme))
      (progn
        (setq wal-theme-base-palette (json-read-file wal-theme--own-cache-base-palette-json-file))
        (setq wal-theme-extended-palette (json-read-file wal-theme--own-cache-extended-palette-json-file))
        (setq wal-theme-semantic-tty-colors (json-read-file wal-theme--own-cache-semantic-gui-colors-json-file))
        (setq wal-theme-semantic-gui-colors (json-read-file wal-theme--own-cache-semantic-tty-colors-json-file))))))

(defun wal-theme-create-theme (&optional theme-name tty accent-color)
  "Create new wal-theme.
Do so by either from loading from wal-theme cache or generating
from wal cache. TTY deafults to \(display-graphic-p\) unless
overridden by `wal-theme-force-tty-colors', while ACCENT-COLOR
defaults to `wal-theme-accent-color' if set, 'magenta otherwise.
THEME-NAME gives a title to the generated theme."
  (wal-theme--load-own-theme)
  (let ((tty (or tty wal-theme-force-tty-colors (display-graphic-p))))
    (let ((colors (if tty wal-theme-semantic-tty-colors wal-theme-semantic-gui-colors))
          (accent-color (or accent-color wal-theme-accent-color 'magenta))
          (class '((class color) (min-colors 89))))

      (progn
        (custom-theme-set-faces
        theme-name

         ;; basics
        `(cursor ((,class (:background ,(alist-get 'cursor colors)))))
        `(custom-button ((,class :background ,(alist-get 'bg2 colors) :foreground ,(alist-get 'base colors) :box (:line-width 2 :style released-button))))
        `(default ((,class (:background ,(alist-get 'bg1 colors) :foreground ,(alist-get 'base colors)))))
        `(default-italic ((,class (:italic t))))
        `(error ((,class (:foreground ,(alist-get 'err colors)))))
        `(eval-sexp-fu-flash ((,class (:background ,(alist-get 'suc colors) :foreground ,(alist-get 'bg1 colors)))))
        `(eval-sexp-fu-flash-error ((,class (:background ,(alist-get 'err colors) :foreground ,(alist-get 'bg1 colors)))))
        `(font-lock-builtin-face ((,class (:foreground ,(alist-get 'keyword colors)))))
        `(font-lock-comment-face ((,class (:foreground ,(if wal-theme-comment-italic (alist-get 'comment-light colors) (alist-get 'comment colors)) :background ,(when wal-theme-comment-bg (alist-get 'comment-bg colors)) :slant ,(if wal-theme-comment-italic 'italic 'normal)))))
        `(font-lock-constant-face ((,class (:foreground ,(alist-get 'const colors)))))
        `(font-lock-doc-face ((,class (:foreground ,(alist-get 'meta colors)))))
        `(font-lock-function-name-face ((,class (:foreground ,(alist-get 'func colors) :inherit bold))))
        `(font-lock-keyword-face ((,class (:inherit bold :foreground ,(alist-get 'keyword colors) :slant ,(if wal-theme-keyword-italic 'italic 'normal)))))
        `(font-lock-negation-char-face ((,class (:foreground ,(alist-get 'const colors)))))
        `(font-lock-preprocessor-face ((,class (:foreground ,(alist-get 'func colors)))))
        `(font-lock-reference-face ((,class (:foreground ,(alist-get 'const colors)))))
        `(font-lock-string-face ((,class (:foreground ,(alist-get 'str colors)))))
        `(font-lock-type-face ((,class (:foreground ,(alist-get 'type colors) :inherit bold))))
        `(font-lock-variable-name-face ((,class (:foreground ,(alist-get 'var colors)))))
        `(font-lock-warning-face ((,class (:foreground ,(alist-get 'war colors) :background ,(alist-get 'bg1 colors)))))
        `(fringe ((,class (:background ,(alist-get 'bg1 colors) :foreground ,(alist-get 'base colors)))))
        `(header-line ((,class :background ,(alist-get 'bg4 colors))))
        `(highlight ((,class (:foreground ,(alist-get 'base colors) :background ,(alist-get 'highlight colors)))))
        `(hl-line ((,class (:background ,(alist-get 'bg2 colors)))))
        `(isearch ((,class (:foreground ,(alist-get 'bg1 colors) :background ,(alist-get 'mat colors)))))
        `(lazy-highlight ((,class (:background ,(alist-get 'green colors) :weight normal))))
        `(link ((,class (:foreground ,(alist-get 'comment colors) :underline t))))
        `(link-visited ((,class (:foreground ,(alist-get 'comp colors) :underline t))))
        `(match ((,class (:background ,(alist-get 'highlight colors) :foreground ,(alist-get 'mat colors)))))
        `(minibuffer-prompt ((,class (:inherit bold :foreground ,(alist-get 'keyword colors)))))
        `(page-break-lines ((,class (:foreground ,(alist-get 'act2 colors)))))
        `(region ((,class (:background ,(alist-get 'highlight colors)))))
        `(secondary-selection ((,class (:background ,(alist-get 'bg3 colors)))))
        `(shadow ((,class (:foreground ,(alist-get 'base colors)))))
        `(success ((,class (:foreground ,(alist-get 'suc colors)))))
        `(tooltip ((,class (:background ,(alist-get 'ttip colors)l :foreground ,(alist-get 'base colors) :bold nil :italic nil :underline nil))))
        `(vertical-border ((,class (:foreground ,(alist-get 'border colors)))))
        `(warning ((,class (:foreground ,(alist-get 'war colors)))))

         ;; ace-window
        `(aw-leading-char-face ((,class (:foreground ,(alist-get 'func colors) :weight bold :height 2.0 :box (:line-width 1 :color ,(alist-get 'keyword colors) :style released-button)))))

         ;; ahs
        `(ahs-face ((,class (:background ,(alist-get 'highlight colors)))))
        `(ahs-plugin-whole-buffer-face ((,class (:background ,(alist-get 'mat colors) :foreground ,(alist-get 'bg1 colors)))))

         ;; anzu-mode
        `(anzu-mode-line ((,class (:foreground ,(alist-get 'yellow colors) :inherit bold))))

         ;; auto-complete
        `(ac-completion-face ((,class (:background ,(alist-get 'ttip colors) :foreground ,(alist-get 'ttip colors)))))

         ;; avy
        `(avy-lead-face   ((,class (:background ,(alist-get 'green colors) :foreground ,(alist-get 'green colors)))))
        `(avy-lead-face-0 ((,class (:background ,(alist-get 'green colors) :foreground ,(alist-get 'yellow colors)))))
        `(avy-lead-face-1 ((,class (:background ,(alist-get 'green colors) :foreground ,(alist-get 'magenta colors)))))
        `(avy-lead-face-2 ((,class (:background ,(alist-get 'green colors) :foreground ,(alist-get 'blue colors)))))

        ;; calfw
        `(cfw:face-title               ((,class (:foreground ,(alist-get 'head1 colors) :height 2.0 :weight bold :inherit variable-pitch))))
        `(cfw:face-header              ((,class (:foreground ,(alist-get 'base colors) :weight bold))))
        `(cfw:face-saturday            ((,class (:foreground ,(alist-get 'base colors) :weight bold))))
        `(cfw:face-sunday              ((,class (:foreground ,(alist-get 'base colors) :weight bold))))
        `(cfw:face-holiday             ((,class (:foreground ,(alist-get 'head1 colors) :weight bold))))
        `(cfw:face-grid                ((,class (:foreground ,(alist-get 'border colors)))))
        `(cfw:face-default-content     ((,class (:foreground ,(alist-get 'green colors)))))
        `(cfw:face-periods             ((,class (:foreground ,(alist-get 'cyan colors)))))
        `(cfw:face-day-title           ((,class (:background ,(alist-get 'head1 colors)))))
        `(cfw:face-default-day         ((,class (:foreground ,(alist-get 'base colors) :weight bold))))
        `(cfw:face-annotation          ((,class (:foreground ,(alist-get 'aqua colors)))))
        `(cfw:face-disable             ((,class (:foreground ,(alist-get 'base colors)))))
        `(cfw:face-today-title         ((,class (:background ,(alist-get 'blue colors) :weight bold))))
        `(cfw:face-today               ((,class (:background ,(alist-get 'head1 colors) :weight bold))))
        `(cfw:face-select              ((,class (:background ,(alist-get 'magenta colors) :weight bold))))
        `(cfw:face-toolbar             ((,class (:foreground ,(alist-get 'base colors) :background ,(alist-get 'bg1 colors)))))
        `(cfw:face-toolbar-button-off  ((,class (:foreground ,(alist-get 'base colors) :weight bold))))
        `(cfw:face-toolbar-button-on   ((,class (:foreground ,(alist-get 'base colors) :weight bold))))

        ;; cider
        `(cider-enlightened ((,class (:background nil :box (:color ,(alist-get 'yellow colors) :line-width -1 :style nil) :foreground ,(alist-get 'yellow colors)))))
        `(cider-enlightened-local ((,class (:foreground ,(alist-get 'yellow colors)))))
        `(cider-instrumented-face ((,class (:background nil :box (:color ,(alist-get 'red colors) :line-width -1 :style nil) :foreground ,(alist-get 'red colors)))))
        `(cider-result-overlay-face ((,class (:background nil :box (:color ,(alist-get 'blue colors) :line-width -1 :style nil) :foreground ,(alist-get 'blue colors)))))
        `(cider-test-error-face ((,class (:background ,(alist-get 'war colors) :foreground ,(alist-get 'bg1 colors)))))
        `(cider-test-failure-face ((,class (:background ,(alist-get 'err colors) :foreground ,(alist-get 'bg1 colors)))))
        `(cider-test-success-face ((,class (:background ,(alist-get 'suc colors) :foreground ,(alist-get 'bg1 colors)))))
        `(cider-traced-face ((,class :box (:color ,(alist-get 'cyan colors) :line-width -1 :style nil))))

        ;; company
        `(company-echo-common ((,class (:background ,(alist-get 'base colors) :foreground ,(alist-get 'bg1 colors)))))
        `(company-preview ((,class (:background ,(alist-get 'ttip colors) :foreground ,(alist-get 'ttip colors)))))
        `(company-preview-common ((,class (:background ,(alist-get 'ttip colors) :foreground ,(alist-get 'base colors)))))
        `(company-preview-search ((,class (:inherit match))))
        `(company-scrollbar-bg ((,class (:background ,(alist-get 'bg2 colors)))))
        `(company-scrollbar-fg ((,class (:background ,(alist-get 'act2 colors)))))
        `(company-template-field ((,class (:inherit region))))
        `(company-tooltip ((,class (:background ,(alist-get 'ttip colors) :foreground ,(alist-get 'ttip colors)))))
        `(company-tooltip-annotation ((,class (:foreground ,(alist-get 'type colors)))))
        `(company-tooltip-common ((,class (:background ,(alist-get 'ttip colors) :foreground ,(alist-get 'keyword colors)))))
        `(company-tooltip-common-selection ((,class (:foreground ,(alist-get 'base colors)))))
        `(company-tooltip-mouse ((,class (:inherit highlight))))
        `(company-tooltip-search ((,class (:inherit match))))
        `(company-tooltip-selection ((,class (:background ,(alist-get 'ttip colors)l :foreground ,(alist-get 'base colors)))))

        ;; diff
        `(diff-added             ((,class :background nil :foreground ,(alist-get 'green colors))))
        `(diff-changed           ((,class :background nil :foreground ,(alist-get 'blue colors))))
        `(diff-header            ((,class :background ,(alist-get 'cblk colors) :foreground ,(alist-get 'func colors))))
        `(diff-file-header       ((,class :background ,(alist-get 'cblk colors) :foreground ,(alist-get 'cblk colors))))
        `(diff-indicator-added   ((,class :background nil :foreground ,(alist-get 'green colors))))
        `(diff-indicator-changed ((,class :background nil :foreground ,(alist-get 'blue colors))))
        `(diff-indicator-removed ((,class :background nil :foreground ,(alist-get 'red colors))))
        `(diff-refine-added      ((,class :background ,(alist-get 'green colors) :foreground ,(alist-get 'bg1 colors))))
        `(diff-refine-changed    ((,class :background ,(alist-get 'blue colors) :foreground ,(alist-get 'bg1 colors))))
        `(diff-refine-removed    ((,class :background ,(alist-get 'red colors) :foreground ,(alist-get 'bg1 colors))))
        `(diff-removed           ((,class :background nil :foreground ,(alist-get 'red colors))))

        ;; diff-hl
        `(diff-hl-change ((,class :background ,(alist-get 'blue colors) :foreground ,(alist-get 'blue colors))))
        `(diff-hl-delete ((,class :background ,(alist-get 'red colors) :foreground ,(alist-get 'red colors))))
        `(diff-hl-insert ((,class :background ,(alist-get 'green colors) :foreground ,(alist-get 'green colors))))

        ;; dired
        `(dired-directory ((,class (:foreground ,(alist-get 'keyword colors) :background ,(alist-get 'bg1 colors) :inherit bold))))
        `(dired-flagged ((,class (:foreground ,(alist-get 'red colors)))))
        `(dired-header ((,class (:foreground ,(alist-get 'comp colors) :inherit bold))))
        `(dired-ignored ((,class (:inherit shadow))))
        `(dired-mark ((,class (:foreground ,(alist-get 'comp colors) :inherit bold))))
        `(dired-marked ((,class (:foreground ,(alist-get 'magenta colors) :inherit bold))))
        `(dired-perm-write ((,class (:foreground ,(alist-get 'base colors) :underline t))))
        `(dired-symlink ((,class (:foreground ,(alist-get 'cyan colors) :background ,(alist-get 'bg1 colors) :inherit bold))))
        `(dired-warning ((,class (:foreground ,(alist-get 'war colors)))))

        ;; ediff
        `(ediff-current-diff-A ((,class(:background ,(alist-get 'red colors) :foreground ,(alist-get 'red colors)))))
        `(ediff-current-diff-Ancestor ((,class(:background ,(alist-get 'aqua colors) :foreground ,(alist-get 'aqua colors)))))
        `(ediff-current-diff-B ((,class(:background ,(alist-get 'green colors) :foreground ,(alist-get 'green colors)))))
        `(ediff-current-diff-C ((,class(:background ,(alist-get 'blue colors) :foreground ,(alist-get 'blue colors)))))
        `(ediff-even-diff-A ((,class(:background ,(alist-get 'bg3 colors)))))
        `(ediff-even-diff-Ancestor ((,class(:background ,(alist-get 'bg3 colors)))))
        `(ediff-even-diff-B ((,class(:background ,(alist-get 'bg3 colors)))))
        `(ediff-even-diff-C ((,class(:background ,(alist-get 'bg3 colors)))))
        `(ediff-fine-diff-A ((,class(:background ,(alist-get 'red colors) :foreground ,(alist-get 'bg1 colors)))))
        `(ediff-fine-diff-Ancestor ((,class(:background nil :inherit bold))))
        `(ediff-fine-diff-B ((,class(:background ,(alist-get 'green colors) :foreground ,(alist-get 'bg1 colors)))))
        `(ediff-fine-diff-C ((,class(:background ,(alist-get 'blue colors) :foreground ,(alist-get 'bg1 colors)))))
        `(ediff-odd-diff-A ((,class(:background ,(alist-get 'bg4 colors)))))
        `(ediff-odd-diff-Ancestor ((,class(:background ,(alist-get 'bg4 colors)))))
        `(ediff-odd-diff-B ((,class(:background ,(alist-get 'bg4 colors)))))
        `(ediff-odd-diff-C ((,class(:background ,(alist-get 'bg4 colors)))))

        ;; ein
        `(ein:cell-input-area((,class (:background ,(alist-get 'bg2 colors)))))
        `(ein:cell-input-prompt ((,class (:foreground ,(alist-get 'suc colors)))))
        `(ein:cell-output-prompt ((,class (:foreground ,(alist-get 'err colors)))))
        `(ein:notification-tab-normal ((,class (:foreground ,(alist-get 'keyword colors)))))
        `(ein:notification-tab-selected ((,class (:foreground ,(alist-get 'suc colors) :inherit bold))))

        ;; eldoc
        `(eldoc-highlight-function-argument ((,class (:foreground ,(alist-get 'mat colors) :inherit bold))))

        ;; elfeed
        `(elfeed-search-date-face ((,class (:foreground ,(alist-get 'head2 colors)))))
        `(elfeed-search-feed-face ((,class (:foreground ,(alist-get 'blue colors)))))
        `(elfeed-search-tag-face ((,class (:foreground ,(alist-get 'func colors)))))
        `(elfeed-search-title-face ((,class (:foreground ,(alist-get 'var colors)))))
        `(elfeed-search-unread-title-face ((,class (:foreground ,(alist-get 'base colors)))))

        ;; enh-ruby
        `(enh-ruby-op-face ((,class (:background ,(alist-get 'bg1 colors) :foreground ,(alist-get 'base colors)))))
        `(enh-ruby-string-delimiter-face ((,class (:foreground ,(alist-get 'str colors)))))

        ;; erc
        `(erc-input-face ((,class (:foreground ,(alist-get 'func colors)))))
        `(erc-my-nick-face ((,class (:foreground ,(alist-get 'keyword colors)))))
        `(erc-nick-default-face ((,class (:foreground ,(alist-get 'keyword colors)))))
        `(erc-nick-prefix-face ((,class (:foreground ,(alist-get 'yellow colors)))))
        `(erc-notice-face ((,class (:foreground ,(alist-get 'str colors)))))
        `(erc-prompt-face ((,class (:foreground ,(alist-get 'mat colors) :inherit bold))))
        `(erc-timestamp-face ((,class (:foreground ,(alist-get 'keyword colors)))))

        ;; eshell
        `(eshell-ls-archive ((,class (:foreground ,(alist-get 'red colors) :inherit bold))))
        `(eshell-ls-backup ((,class (:inherit font-lock-comment-face))))
        `(eshell-ls-clutter ((,class (:inherit font-lock-comment-face))))
        `(eshell-ls-directory ((,class (:foreground ,(alist-get 'keyword colors) :inherit bold))))
        `(eshell-ls-executable ((,class (:foreground ,(alist-get 'suc colors) :inherit bold))))
        `(eshell-ls-missing ((,class (:inherit font-lock-warning-face))))
        `(eshell-ls-product ((,class (:inherit font-lock-doc-face))))
        `(eshell-ls-special ((,class (:foreground ,(alist-get 'yellow colors) :inherit bold))))
        `(eshell-ls-symlink ((,class (:foreground ,(alist-get 'cyan colors) :inherit bold))))
        `(eshell-ls-unreadable ((,class (:foreground ,(alist-get 'base colors)))))
        `(eshell-prompt ((,class (:foreground ,(alist-get 'keyword colors) :inherit bold))))

        ;; ESS
        `(ess-assignment-face ((,class (:foreground ,(alist-get 'type colors) :inherit bold))))
        `(ess-backquoted-face ((,class (:foreground ,(alist-get 'var colors)))))
        `(ess-constant-face ((,class (:inherit font-lock-constant-face))))
        `(ess-f-t-face ((,class (:inherit font-lock-constant-face))))
        `(ess-function-call-face ((,class (:foreground ,(alist-get 'func colors)))))
        `(ess-keyword-face ((,class (:inherit font-lock-keyword-face))))
        `(ess-matrix-face ((,class (:foreground ,(alist-get 'base colors)))))
        `(ess-modifiers-face ((,class (:foreground ,(alist-get 'keyword colors)))))
        `(ess-numbers-face ((,class (:inherit font-lock-constant-face))))
        `(ess-operator-face ((,class (:foreground ,(alist-get 'var colors)))))
        `(ess-paren-face ((,class (:foreground ,(alist-get 'blue colors)))))
        `(ess-r-control-flow-keyword-face ((,class (:foreground ,(alist-get 'keyword colors)))))
        `(ess-r-signal-keyword-face ((,class (:foreground ,(alist-get 'war colors)))))

        ;; evil
        `(evil-ex-substitute-matches ((,class (:background ,(alist-get 'red colors) :foreground ,(alist-get 'red colors)))))
        `(evil-ex-substitute-replacement ((,class (:background ,(alist-get 'green colors) :foreground ,(alist-get 'green colors)))))

        ;; evil-goggles
        `(evil-goggles--pulse-face ((,class (:background ,(alist-get 'yellow colors) :foreground ,(alist-get 'yellow colors)))))
        `(evil-goggles-change-face ((,class (:background ,(alist-get 'blue colors) :foreground ,(alist-get 'blue colors)))))
        `(evil-goggles-commentary-face ((,class (:background ,(alist-get 'aqua colors) :foreground ,(alist-get 'aqua colors)))))
        `(evil-goggles-delete-face ((,class (:background ,(alist-get 'red colors) :foreground ,(alist-get 'red colors)))))
        `(evil-goggles-fill-and-move-face ((,class (:background ,(alist-get 'green colors) :foreground ,(alist-get 'green colors)))))
        `(evil-goggles-indent-face ((,class (:background ,(alist-get 'green colors) :foreground ,(alist-get 'green colors)))))
        `(evil-goggles-join-face ((,class (:background ,(alist-get 'green colors) :foreground ,(alist-get 'green colors)))))
        `(evil-goggles-nerd-commenter-face ((,class (:background ,(alist-get 'aqua colors) :foreground ,(alist-get 'aqua colors)))))
        `(evil-goggles-paste-face ((,class (:background ,(alist-get 'green colors) :foreground ,(alist-get 'green colors)))))
        `(evil-goggles-record-macro-face ((,class (:background ,(alist-get 'blue colors) :foreground ,(alist-get 'blue colors)))))
        `(evil-goggles-replace-with-register-face ((,class (:background ,(alist-get 'yellow colors) :foreground ,(alist-get 'yellow colors)))))
        `(evil-goggles-set-marker-face ((,class (:background ,(alist-get 'blue colors) :foreground ,(alist-get 'blue colors)))))
        `(evil-goggles-shift-face ((,class (:background ,(alist-get 'blue colors) :foreground ,(alist-get 'blue colors)))))
        `(evil-goggles-surround-face ((,class (:background ,(alist-get 'blue colors) :foreground ,(alist-get 'blue colors)))))
        `(evil-goggles-yank-face ((,class (:background ,(alist-get 'blue colors) :foreground ,(alist-get 'blue colors)))))
        `(evil-goggles-undo-redo-add-face ((,class (:background ,(alist-get 'green colors) :foreground ,(alist-get 'green colors)))))
        `(evil-goggles-undo-redo-change-face ((,class (:background ,(alist-get 'blue colors) :foreground ,(alist-get 'blue colors)))))
        `(evil-goggles-undo-redo-remove-face ((,class (:background ,(alist-get 'red colors) :foreground ,(alist-get 'red colors)))))

        ;; flycheck
        `(flycheck-error
          ((,(append '((supports :underline (:style line))) class)
            (:underline (:style line :color ,(alist-get 'err colors))))
            (,class (:foreground ,(alist-get 'base colors) :background ,(alist-get 'err colors) :inherit bold :underline t))))
        `(flycheck-error-list-checker-name ((,class (:foreground ,(alist-get 'keyword colors)))))
        `(flycheck-fringe-error ((,class (:foreground ,(alist-get 'err colors) :inherit bold))))
        `(flycheck-fringe-info ((,class (:foreground ,(alist-get 'keyword colors) :inherit bold))))
        `(flycheck-fringe-warning ((,class (:foreground ,(alist-get 'war colors) :inherit bold))))
        `(flycheck-info
          ((,(append '((supports :underline (:style line))) class)
            (:underline (:style line :color ,(alist-get 'keyword colors))))
            (,class (:foreground ,(alist-get 'base colors) :background ,(alist-get 'keyword colors) :inherit bold :underline t))))
        `(flycheck-warning
          ((,(append '((supports :underline (:style line))) class)
            (:underline (:style line :color ,(alist-get 'war colors))))
            (,class (:foreground ,(alist-get 'base colors) :background ,(alist-get 'war colors) :inherit bold :underline t))))

        ;; flymake
        `(flymake-error ((,(append '((supports :underline (:style line))) class)
                          (:underline (:style line :color ,(alist-get 'err colors))))
                          (,class (:foreground ,(alist-get 'base colors) :background ,(alist-get 'err colors) :inherit bold :underline t))))
        `(flymake-note ((,(append '((supports :underline (:style line))) class)
                          (:underline (:style wave :color ,(alist-get 'keyword colors))))
                        (,class (:foreground ,(alist-get 'base colors) :background ,(alist-get 'keyword colors) :inherit bold :underline t))))
        `(flymake-warning ((,(append '((supports :underline (:style line))) class)
                            (:underline (:style line :color ,(alist-get 'war colors))))
                            (,class (:foreground ,(alist-get 'base colors) :background ,(alist-get 'war colors) :inherit bold :underline t))))

        ;; flyspell
        `(flyspell-incorrect ((,(append '((supports :underline (:style line))) class)
                                (:underline (:style wave :color ,(alist-get 'war colors))))
                              (,class (:foreground ,(alist-get 'base colors) :background ,(alist-get 'war colors) :inherit bold :underline t))))
        `(flyspell-duplicate ((,(append '((supports :underline (:style line))) class)
                                (:underline (:style wave :color ,(alist-get 'keyword colors))))
                              (,class (:foreground ,(alist-get 'base colors) :background ,(alist-get 'keyword colors) :inherit bold :underline t))))

        ;; jabber
        `(jabber-activity-face ((,class (:inherit bold :foreground ,(alist-get 'red colors)))))
        `(jabber-activity-personal-face ((,class (:inherit bold :foreground ,(alist-get 'blue colors)))))
        `(jabber-chat-error ((,class (:inherit bold :foreground ,(alist-get 'red colors)))))
        `(jabber-chat-prompt-foreign ((,class (:inherit bold :foreground ,(alist-get 'red colors)))))
        `(jabber-chat-prompt-local ((,class (:inherit bold :foreground ,(alist-get 'blue colors)))))
        `(jabber-chat-prompt-system ((,class (:inherit bold :foreground ,(alist-get 'green colors)))))
        `(jabber-chat-text-foreign ((,class (:foreground ,(alist-get 'base colors)))))
        `(jabber-chat-text-local ((,class (:foreground ,(alist-get 'base colors)))))
        `(jabber-rare-time-face ((,class (:foreground ,(alist-get 'green colors)))))
        `(jabber-roster-user-away ((,class (:foreground ,(alist-get 'yellow colors)))))
        `(jabber-roster-user-chatty ((,class (:inherit bold :foreground ,(alist-get 'green colors)))))
        `(jabber-roster-user-dnd ((,class (:foreground ,(alist-get 'red colors)))))
        `(jabber-roster-user-error ((,class (:foreground ,(alist-get 'err colors)))))
        `(jabber-roster-user-offline ((,class (:foreground ,(alist-get 'base colors)))))
        `(jabber-roster-user-online ((,class (:inherit bold :foreground ,(alist-get 'green colors)))))
        `(jabber-roster-user-xa ((,class (:foreground ,(alist-get 'aqua colors)))))

        ;; git-gutter-fr
        `(git-gutter-fr:added ((,class (:foreground ,(alist-get 'green colors) :inherit bold))))
        `(git-gutter-fr:deleted ((,class (:foreground ,(alist-get 'war colors) :inherit bold))))
        `(git-gutter-fr:modified ((,class (:foreground ,(alist-get 'keyword colors) :inherit bold))))

        ;; git-timemachine
        `(git-timemachine-minibuffer-detail-face ((,class (:foreground ,(alist-get 'blue colors) :inherit bold :background ,(alist-get 'blue colors)))))

        ;; gnus
        `(gnus-emphasis-highlight-words ((,class (:background ,(alist-get 'suc colors) :foreground ,(alist-get 'bg1 colors)))))
        `(gnus-header-content ((,class (:foreground ,(alist-get 'keyword colors)))))
        `(gnus-header-from ((,class (:foreground ,(alist-get 'var colors)))))
        `(gnus-header-name ((,class (:foreground ,(alist-get 'comp colors)))))
        `(gnus-header-subject ((,class (:foreground ,(alist-get 'func colors) :inherit bold))))
        `(gnus-summary-cancelled ((,class (:background ,(alist-get 'war colors) :foreground ,(alist-get 'bg1 colors)))))

        ;; guide-key
        `(guide-key/highlight-command-face ((,class (:foreground ,(alist-get 'base colors)))))
        `(guide-key/key-face ((,class (:foreground ,(alist-get 'keyword colors)))))
        `(guide-key/prefix-command-face ((,class (:foreground ,(alist-get 'keyword colors) :inherit bold))))

        ;; helm
        `(helm-bookmark-directory ((,class (:inherit helm-ff-directory))))
        `(helm-bookmark-file ((,class (:foreground ,(alist-get 'base colors)))))
        `(helm-bookmark-gnus ((,class (:foreground ,(alist-get 'comp colors)))))
        `(helm-bookmark-info ((,class (:foreground ,(alist-get 'comp colors)))))
        `(helm-bookmark-man ((,class (:foreground ,(alist-get 'comp colors)))))
        `(helm-bookmark-w3m ((,class (:foreground ,(alist-get 'comp colors)))))
        `(helm-buffer-directory ((,class (:foreground ,(alist-get 'base colors) :background ,(alist-get 'bg1 colors)))))
        `(helm-buffer-file ((,class (:foreground ,(alist-get 'base colors) :background ,(alist-get 'bg1 colors)))))
        `(helm-buffer-not-saved ((,class (:foreground ,(alist-get 'comp colors) :background ,(alist-get 'bg1 colors)))))
        `(helm-buffer-process ((,class (:foreground ,(alist-get 'keyword colors) :background ,(alist-get 'bg1 colors)))))
        `(helm-buffer-saved-out ((,class (:foreground ,(alist-get 'base colors) :background ,(alist-get 'bg1 colors)))))
        `(helm-buffer-size ((,class (:foreground ,(alist-get 'base colors) :background ,(alist-get 'bg1 colors)))))
        `(helm-candidate-number ((,class (:background ,(alist-get 'bg1 colors) :foreground ,(alist-get 'keyword colors) :inherit bold))))
        `(helm-ff-directory ((,class (:foreground ,(alist-get 'keyword colors) :background ,(alist-get 'bg1 colors) :inherit bold))))
        `(helm-ff-dotted-directory ((,class (:foreground ,(alist-get 'keyword colors) :background ,(alist-get 'bg1 colors) :inherit bold))))
        `(helm-ff-dotted-symlink-directory ((,class (:foreground ,(alist-get 'cyan colors) :background ,(alist-get 'bg1 colors) :inherit bold))))
        `(helm-ff-executable ((,class (:foreground ,(alist-get 'suc colors) :background ,(alist-get 'bg1 colors) :weight normal))))
        `(helm-ff-file ((,class (:foreground ,(alist-get 'base colors) :background ,(alist-get 'bg1 colors) :weight normal))))
        `(helm-ff-invalid-symlink ((,class (:foreground ,(alist-get 'red colors) :background ,(alist-get 'bg1 colors) :inherit bold))))
        `(helm-ff-prefix ((,class (:foreground ,(alist-get 'bg1 colors) :background ,(alist-get 'keyword colors) :weight normal))))
        `(helm-ff-symlink ((,class (:foreground ,(alist-get 'cyan colors) :background ,(alist-get 'bg1 colors) :inherit bold))))
        `(helm-grep-cmd-line ((,class (:foreground ,(alist-get 'base colors) :background ,(alist-get 'bg1 colors)))))
        `(helm-grep-file ((,class (:foreground ,(alist-get 'base colors) :background ,(alist-get 'bg1 colors)))))
        `(helm-grep-finish ((,class (:foreground ,(alist-get 'base colors) :background ,(alist-get 'bg1 colors)))))
        `(helm-grep-lineno ((,class (:foreground ,(alist-get 'type colors) :background ,(alist-get 'bg1 colors) :inherit bold))))
        `(helm-grep-match ((,class (:foreground nil :background nil :inherit helm-match))))
        `(helm-header ((,class (:foreground ,(alist-get 'base colors) :background ,(alist-get 'bg1 colors) :underline nil :box nil))))
        `(helm-header-line-left-margin ((,class (:foreground ,(alist-get 'keyword colors) :background ,(alist-get 'nil colors)))))
        `(helm-match ((,class (:background ,(alist-get 'head1 colors) :foreground ,(alist-get 'head1 colors)))))
        `(helm-match-item ((,class (:background ,(alist-get 'head1 colors) :foreground ,(alist-get 'head1 colors)))))
        `(helm-moccur-buffer ((,class (:foreground ,(alist-get 'var colors) :background ,(alist-get 'bg1 colors)))))
        `(helm-selection ((,class (:background ,(alist-get 'highlight colors)))))
        `(helm-selection-line ((,class (:background ,(alist-get 'bg2 colors)))))
        `(helm-separator ((,class (:foreground ,(alist-get 'comp colors) :background ,(alist-get 'bg1 colors)))))
        `(helm-source-header ((,class (:background ,(alist-get 'comp colors) :foreground ,(alist-get 'bg1 colors) :inherit bold))))
        `(helm-time-zone-current ((,class (:foreground ,(alist-get 'keyword colors) :background ,(alist-get 'bg1 colors)))))
        `(helm-time-zone-home ((,class (:foreground ,(alist-get 'comp colors) :background ,(alist-get 'bg1 colors)))))
        `(helm-visible-mark ((,class (:foreground ,(alist-get 'keyword colors) :background ,(alist-get 'bg3 colors)))))

        ;; helm-swoop
        `(helm-swoop-target-line-block-face ((,class (:foreground ,(alist-get 'base colors) :background ,(alist-get 'highlight colors)))))
        `(helm-swoop-target-line-face ((,class (:background ,(alist-get 'highlight colors)))))
        `(helm-swoop-target-word-face ((,class (:background ,(alist-get 'highlight colors) :foreground ,(alist-get 'mat colors)))))

        ;; highlights
        `(hi-green  ((,class (:foreground ,(alist-get 'green colors) :background ,(alist-get 'green colors)))))
        `(hi-yellow ((,class (:foreground ,(alist-get 'yellow colors) :background ,(alist-get 'yellow colors)))))

        ;; highlight-indentation
        `(highlight-indentation-face ((,class (:background ,(alist-get 'comment colors)))))

        ;; highlight-symbol
        `(highlight-symbol-face ((,class (:background ,(alist-get 'bg2 colors)))))

        ;; hydra
        `(hydra-face-blue ((,class (:foreground ,(alist-get 'blue colors)))))
        `(hydra-face-red ((,class (:foreground ,(alist-get 'red colors)))))

        ;; ido
        `(ido-first-match ((,class (:foreground ,(alist-get 'comp colors) :inherit bold))))
        `(ido-only-match ((,class (:foreground ,(alist-get 'mat colors) :inherit bold))))
        `(ido-subdir ((,class (:foreground ,(alist-get 'keyword colors)))))
        `(ido-vertical-match-face ((,class (:foreground ,(alist-get 'comp colors) :underline nil))))

        ;; info
        `(info-header-xref ((,class (:foreground ,(alist-get 'func colors) :underline t))))
        `(info-menu ((,class (:foreground ,(alist-get 'suc colors)))))
        `(info-node ((,class (:foreground ,(alist-get 'func colors) :inherit bold))))
        `(info-quoted-name ((,class (:foreground ,(alist-get 'keyword colors)))))
        `(info-reference-item ((,class (:background nil :underline t :inherit bold))))
        `(info-string ((,class (:foreground ,(alist-get 'str colors)))))
        `(info-title-1 ((,class (:height 1.4 :inherit bold))))
        `(info-title-2 ((,class (:height 1.3 :inherit bold))))
        `(info-title-3 ((,class (:height 1.3))))
        `(info-title-4 ((,class (:height 1.2))))

        ;; ivy
        `(ivy-current-match ((,class (:background ,(alist-get 'highlight colors) :inherit bold))))
        `(ivy-minibuffer-match-face-1 ((,class (:inherit bold))))
        `(ivy-minibuffer-match-face-2 ((,class (:foreground ,(alist-get 'head1 colors) :underline t))))
        `(ivy-minibuffer-match-face-3 ((,class (:foreground ,(alist-get 'head4 colors) :underline t))))
        `(ivy-minibuffer-match-face-4 ((,class (:foreground ,(alist-get 'head3 colors) :underline t))))
        `(ivy-remote ((,class (:foreground ,(alist-get 'cyan colors)))))

        ;; latex
        `(font-latex-bold-face ((,class (:foreground ,(alist-get 'comp colors)))))
        `(font-latex-italic-face ((,class (:foreground ,(alist-get 'keyword colors) :italic t))))
        `(font-latex-match-reference-keywords ((,class (:foreground ,(alist-get 'const colors)))))
        `(font-latex-match-variable-keywords ((,class (:foreground ,(alist-get 'var colors)))))
        `(font-latex-sectioning-0-face ((,class (:inherit bold :foreground ,(alist-get 'head3 colors) :height ,(if wal-theme-org-height 1.3 1.0) :background ,(when wal-theme-org-highlight ,(alist-get 'head3-bg colors))))))
        `(font-latex-sectioning-1-face ((,class (:inherit bold :foreground ,(alist-get 'head4 colors) :height ,(if wal-theme-org-height 1.3 1.0) :background ,(when wal-theme-org-highlight ,(alist-get 'head4-bg colors))))))
        `(font-latex-sectioning-2-face ((,class (:inherit bold :foreground ,(alist-get 'head1 colors) :height ,(if wal-theme-org-height 1.3 1.0) :background ,(when wal-theme-org-highlight ,(alist-get 'head1-bg colors))))))
        `(font-latex-sectioning-3-face ((,class (:inherit bold :foreground ,(alist-get 'head2 colors) :height ,(if wal-theme-org-height 1.2 1.0) :background ,(when wal-theme-org-highlight ,(alist-get 'head2-bg colors))))))
        `(font-latex-sectioning-4-face ((,class (:bold nil :foreground ,(alist-get 'head3 colors) :height ,(if wal-theme-org-height 1.1 1.0) :background ,(when wal-theme-org-highlight ,(alist-get 'head3-bg colors))))))
        `(font-latex-sectioning-5-face ((,class (:bold nil :foreground ,(alist-get 'head4 colors) :background ,(when wal-theme-org-highlight ,(alist-get 'head4-bg colors))))))
        `(font-latex-string-face ((,class (:foreground ,(alist-get 'str colors)))))
        `(font-latex-warning-face ((,class (:foreground ,(alist-get 'war colors)))))

        ;; ledger-mode
        `(ledger-font-directive-face ((,class (:foreground ,(alist-get 'meta colors)))))
        `(ledger-font-posting-amount-face ((,class (:foreground ,(alist-get 'yellow colors)))))
        `(ledger-font-posting-date-face ((,class (:foreground ,(alist-get 'head1 colors)))))
        `(ledger-occur-xact-face ((,class (:background ,(alist-get 'bg2 colors)))))

        ;; linum-mode
        `(linum ((,class (:foreground ,(alist-get 'lnum colors) :background ,(alist-get 'bg2 colors) :inherit default))))

        ;; line-numbers
        `(line-number ((,class (:foreground ,(alist-get 'lnum colors) :background ,(alist-get 'bg2 colors) :inherit default))))
        `(line-number-current-line ((,class (:foreground ,(alist-get 'base colors) :background ,(alist-get 'bg2 colors) :inherit line-number))))

        ;; linum-relative
        `(linum-relative-current-face ((,class (:foreground ,(alist-get 'comp colors)))))

        ;; magit
        `(magit-blame-culprit ((,class :background ,(alist-get 'yellow colors) :foreground ,(alist-get 'yellow colors))))
        `(magit-blame-date    ((,class :background ,(alist-get 'yellow colors) :foreground ,(alist-get 'green colors))))
        `(magit-blame-hash    ((,class :background ,(alist-get 'yellow colors) :foreground ,(alist-get 'func colors))))
        `(magit-blame-header  ((,class :background ,(alist-get 'yellow colors) :foreground ,(alist-get 'green colors))))
        `(magit-blame-heading ((,class :background ,(alist-get 'yellow colors) :foreground ,(alist-get 'green colors))))
        `(magit-blame-name    ((,class :background ,(alist-get 'yellow colors) :foreground ,(alist-get 'yellow colors))))
        `(magit-blame-sha1    ((,class :background ,(alist-get 'yellow colors) :foreground ,(alist-get 'func colors))))
        `(magit-blame-subject ((,class :background ,(alist-get 'yellow colors) :foreground ,(alist-get 'yellow colors))))
        `(magit-blame-summary ((,class :background ,(alist-get 'yellow colors) :foreground ,(alist-get 'yellow colors))))
        `(magit-blame-time    ((,class :background ,(alist-get 'yellow colors) :foreground ,(alist-get 'green colors))))
        `(magit-branch ((,class (:foreground ,(alist-get 'const colors) :inherit bold))))
        `(magit-branch-current ((,class (:background ,(alist-get 'blue colors) :foreground ,(alist-get 'blue colors) :inherit bold :box t))))
        `(magit-branch-local ((,class (:background ,(alist-get 'blue colors) :foreground ,(alist-get 'blue colors) :inherit bold))))
        `(magit-branch-remote ((,class (:background ,(alist-get 'aqua colors) :foreground ,(alist-get 'aqua colors) :inherit bold))))
        `(magit-diff-context-highlight ((,class (:background ,(alist-get 'bg2 colors) :foreground ,(alist-get 'base colors)))))
        `(magit-diff-hunk-heading ((,class (:background ,(alist-get 'ttip colors) :foreground ,(alist-get 'ttip colors)))))
        `(magit-diff-hunk-heading-highlight ((,class (:background ,(alist-get 'ttip colors)l :foreground ,(alist-get 'base colors)))))
        `(magit-hash ((,class (:foreground ,(alist-get 'var colors)))))
        `(magit-hunk-heading           ((,class (:background ,(alist-get 'bg3 colors)))))
        `(magit-hunk-heading-highlight ((,class (:background ,(alist-get 'bg3 colors)))))
        `(magit-item-highlight ((,class :background ,(alist-get 'bg2 colors))))
        `(magit-log-author ((,class (:foreground ,(alist-get 'func colors)))))
        `(magit-log-head-label-head ((,class (:background ,(alist-get 'yellow colors) :foreground ,(alist-get 'bg1 colors) :inherit bold))))
        `(magit-log-head-label-local ((,class (:background ,(alist-get 'keyword colors) :foreground ,(alist-get 'bg1 colors) :inherit bold))))
        `(magit-log-head-label-remote ((,class (:background ,(alist-get 'suc colors) :foreground ,(alist-get 'bg1 colors) :inherit bold))))
        `(magit-log-head-label-tags ((,class (:background ,(alist-get 'magenta colors) :foreground ,(alist-get 'bg1 colors) :inherit bold))))
        `(magit-log-head-label-wip ((,class (:background ,(alist-get 'cyan colors) :foreground ,(alist-get 'bg1 colors) :inherit bold))))
        `(magit-log-sha1 ((,class (:foreground ,(alist-get 'str colors)))))
        `(magit-process-ng ((,class (:foreground ,(alist-get 'war colors) :inherit bold))))
        `(magit-process-ok ((,class (:foreground ,(alist-get 'func colors) :inherit bold))))
        `(magit-reflog-amend ((,class (:foreground ,(alist-get 'magenta colors)))))
        `(magit-reflog-checkout ((,class (:foreground ,(alist-get 'blue colors)))))
        `(magit-reflog-cherry-pick ((,class (:foreground ,(alist-get 'green colors)))))
        `(magit-reflog-commit ((,class (:foreground ,(alist-get 'green colors)))))
        `(magit-reflog-merge ((,class (:foreground ,(alist-get 'green colors)))))
        `(magit-reflog-other ((,class (:foreground ,(alist-get 'cyan colors)))))
        `(magit-reflog-rebase ((,class (:foreground ,(alist-get 'magenta colors)))))
        `(magit-reflog-remote ((,class (:foreground ,(alist-get 'cyan colors)))))
        `(magit-reflog-reset ((,class (:foreground ,(alist-get 'red colors)))))
        `(magit-section-heading        ((,class (:foreground ,(alist-get 'keyword colors) :inherit bold))))
        `(magit-section-highlight      ((,class (:background ,(alist-get 'bg2 colors)))))
        `(magit-section-title ((,class (:background ,(alist-get 'bg1 colors) :foreground ,(alist-get 'keyword colors) :inherit bold))))

        ;; man
        `(Man-overstrike ((,class (:foreground ,(alist-get 'head1 colors) :inherit bold))))
        `(Man-reverse ((,class (:foreground ,(alist-get 'highlight colors)))))
        `(Man-underline ((,class (:foreground ,(alist-get 'comp colors) :underline t))))

        ;; markdown
        `(markdown-header-face-1 ((,class (:inherit bold :foreground ,(alist-get 'head1 colors) :height ,(if wal-theme-org-height 1.3 1.0) :background ,(when wal-theme-org-highlight ,(alist-get 'head1-bg colors))))))
        `(markdown-header-face-2 ((,class (:inherit bold :foreground ,(alist-get 'head2 colors) :height ,(if wal-theme-org-height 1.2 1.0) :background ,(when wal-theme-org-highlight ,(alist-get 'head2-bg colors))))))
        `(markdown-header-face-3 ((,class (:bold nil :foreground ,(alist-get 'head3 colors) :height ,(if wal-theme-org-height 1.1 1.0) :background ,(when wal-theme-org-highlight ,(alist-get 'head3-bg colors))))))
        `(markdown-header-face-4 ((,class (:bold nil :foreground ,(alist-get 'head4 colors) :background ,(when wal-theme-org-highlight ,(alist-get 'head4-bg colors))))))
        `(markdown-header-face-5 ((,class (:bold nil :foreground ,(alist-get 'head1 colors)))))
        `(markdown-header-face-6 ((,class (:bold nil :foreground ,(alist-get 'head2 colors)))))
        `(markdown-table-face ((,class (:foreground ,(alist-get 'base colors) :background ,(alist-get 'head1 colors)))))

        ;; mode-line
        `(mode-line           ((,class (:foreground ,(alist-get 'base colors) :background ,(alist-get 'act1 colors) :box (:color ,(alist-get 'border colors) :line-width 1)))))
        `(mode-line-buffer-id ((,class (:inherit bold :foreground ,(alist-get 'func colors)))))
        `(mode-line-inactive  ((,class (:foreground ,(alist-get 'base colors) :background ,(alist-get 'bg1 colors)  :box (:color ,(alist-get 'border colors) :line-width 1)))))

        ;; mu4e
        `(mu4e-attach-number-face ((,class (:foreground ,(alist-get 'var colors)))))
        `(mu4e-cited-1-face ((,class (:foreground ,(alist-get 'head1 colors)))))
        `(mu4e-cited-2-face ((,class (:foreground ,(alist-get 'head2 colors)))))
        `(mu4e-cited-3-face ((,class (:foreground ,(alist-get 'head3 colors)))))
        `(mu4e-cited-4-face ((,class (:foreground ,(alist-get 'head4 colors)))))
        `(mu4e-cited-5-face ((,class (:foreground ,(alist-get 'head1 colors)))))
        `(mu4e-cited-6-face ((,class (:foreground ,(alist-get 'head2 colors)))))
        `(mu4e-cited-7-face ((,class (:foreground ,(alist-get 'head3 colors)))))
        `(mu4e-contact-face ((,class (:foreground ,(alist-get 'func colors)))))
        `(mu4e-draft-face ((,class (:foreground ,(alist-get 'var colors)))))
        `(mu4e-flagged-face ((,class (:foreground ,(alist-get 'yellow colors) :inherit bold))))
        `(mu4e-header-key-face ((,class (:foreground ,(alist-get 'meta colors) :inherit bold))))
        `(mu4e-header-title-face ((,class (:foreground ,(alist-get 'keyword colors) :inherit bold))))
        `(mu4e-header-marks-face ((,class (:foreground ,(alist-get 'comp colors)))))
        `(mu4e-header-value-face ((,class (:foreground ,(alist-get 'keyword colors) :inherit bold))))
        `(mu4e-header-highlight-face ((,class (:background ,(alist-get 'highlight colors)))))
        `(mu4e-highlight-face ((,class (:foreground ,(alist-get 'comp colors)))))
        `(mu4e-title-face ((,class (:foreground ,(alist-get 'head2 colors) :inherit bold))))
        `(mu4e-replied-face ((,class (:foreground ,(alist-get 'green colors)))))
        `(mu4e-modeline-face ((,class (:foreground ,(alist-get 'yellow colors)))))
        `(mu4e-special-header-value-face ((,class (:foreground ,(alist-get 'green colors)))))
        `(mu4e-unread-face ((,class (:foreground ,(alist-get 'head1 colors) :inherit bold))))
        `(mu4e-view-url-number-face ((,class (:foreground ,(alist-get 'comp colors)))))

        ;; mu4e-maildirs
        `(mu4e-maildirs-extension-maildir-hl-face ((,class (:foreground ,(alist-get 'head1 colors) :inherit bold))))

        ;; notmuch
        `(notmuch-search-date ((,class (:foreground ,(alist-get 'func colors)))))
        `(notmuch-search-flagged-face ((,class (:weight extra-bold))))
        `(notmuch-search-non-matching-authors ((,class (:foreground ,(alist-get 'base colors)))))
        `(notmuch-search-unread-face ((,class (:background ,(alist-get 'highlight colors) :box ,(alist-get 'border colors)))))
        `(notmuch-tag-face ((,class (:foreground ,(alist-get 'keyword colors)))))
        `(notmuch-tag-flagged ((,class (:foreground ,(alist-get 'war colors)))))

        ;; neotree
        `(neo-dir-link-face ((,class (:foreground ,(alist-get 'keyword colors) :inherit bold))))
        `(neo-expand-btn-face ((,class (:foreground ,(alist-get 'base colors)))))
        `(neo-file-link-face ((,class (:foreground ,(alist-get 'base colors)))))
        `(neo-root-dir-face ((,class (:foreground ,(alist-get 'func colors) :inherit bold))))

        ;; org
        `(org-agenda-clocking ((,class (:background ,(alist-get 'highlight colors) :foreground ,(alist-get 'comp colors)))))
        `(org-agenda-date ((,class (:foreground ,(alist-get 'var colors) :height ,(if wal-theme-org-agenda-height 1.1 1.0)))))
        `(org-agenda-date-today ((,class (:foreground ,(alist-get 'keyword colors) :inherit bold :height ,(if wal-theme-org-agenda-height 1.3 1.0)))))
        `(org-agenda-date-weekend ((,class (:inherit bold :foreground ,(alist-get 'var colors)))))
        `(org-agenda-done ((,class (:foreground ,(alist-get 'suc colors) :height ,(if wal-theme-org-agenda-height 1.2 1.0)))))
        `(org-agenda-structure ((,class (:inherit bold :foreground ,(alist-get 'comp colors)))))
        `(org-block ((,class (:background ,(alist-get 'cblk colors) :foreground ,(alist-get 'cblk colors)))))
        `(org-block-begin-line ((,class (:background ,(alist-get 'cblk colors) :foreground ,(alist-get 'cblk colors)-ln))))
        `(org-block-end-line ((,class (:background ,(alist-get 'cblk colors) :foreground ,(alist-get 'cblk colors)-ln))))
        `(org-clock-overlay ((,class (:foreground ,(alist-get 'comp colors)))))
        `(org-code ((,class (:foreground ,(alist-get 'cyan colors)))))
        `(org-column ((,class (:background ,(alist-get 'highlight colors)))))
        `(org-column-title ((,class (:background ,(alist-get 'highlight colors)))))
        `(org-date ((,class (:underline t :foreground ,(alist-get 'var colors)))))
        `(org-date-selected ((,class (:background ,(alist-get 'func colors) :foreground ,(alist-get 'bg1 colors)))))
        `(org-document-info-keyword ((,class (:foreground ,(alist-get 'meta colors)))))
        `(org-document-title ((,class (:foreground ,(alist-get 'func colors) :inherit bold :height ,(if wal-theme-org-height 1.4 1.0) :underline t))))
        `(org-done ((,class (:foreground ,(alist-get 'suc colors) :inherit bold :background ,(alist-get 'green colors)))))
        `(org-ellipsis ((,class (:foreground ,(alist-get 'keyword colors)))))
        `(org-footnote  ((,class (:underline t :foreground ,(alist-get 'base colors)))))
        `(org-hide ((,class (:foreground ,(alist-get 'base colors)))))
        `(org-kbd ((,class (:inherit region :foreground ,(alist-get 'base colors) :box (:line-width 1 :style released-button)))))
        `(org-level-1 ((,class (:inherit bold :bold ,(if wal-theme-org-bold 'unspecified nil) :foreground ,(alist-get 'head1 colors) :height ,(if wal-theme-org-height 1.3 1.0) :background ,(when wal-theme-org-highlight ,(alist-get 'head1-bg colors))))))
        `(org-level-2 ((,class (:inherit bold :bold ,(if wal-theme-org-bold 'unspecified nil) :foreground ,(alist-get 'head2 colors) :height ,(if wal-theme-org-height 1.2 1.0) :background ,(when wal-theme-org-highlight ,(alist-get 'head2-bg colors))))))
        `(org-level-3 ((,class (:bold nil :foreground ,(alist-get 'head3 colors) :height ,(if wal-theme-org-height 1.1 1.0) :background ,(when wal-theme-org-highlight ,(alist-get 'head3-bg colors))))))
        `(org-level-4 ((,class (:bold nil :foreground ,(alist-get 'head4 colors) :background ,(when wal-theme-org-highlight ,(alist-get 'head4-bg colors))))))
        `(org-level-5 ((,class (:bold nil :foreground ,(alist-get 'head1 colors)))))
        `(org-level-6 ((,class (:bold nil :foreground ,(alist-get 'head2 colors)))))
        `(org-level-7 ((,class (:bold nil :foreground ,(alist-get 'head3 colors)))))
        `(org-level-8 ((,class (:bold nil :foreground ,(alist-get 'head4 colors)))))
        `(org-link ((,class (:underline t :foreground ,(alist-get 'comment colors)))))
        `(org-meta-line ((,class (:foreground ,(alist-get 'meta colors)))))
        `(org-mode-line-clock-overrun ((,class (:foreground ,(alist-get 'err colors)))))
        `(org-priority ((,class (:foreground ,(alist-get 'war colors) :inherit bold :bold ,(if wal-theme-org-priority-bold 'unspecified nil)))))
        `(org-quote ((,class (:inherit org-block :slant italic))))
        `(org-scheduled ((,class (:foreground ,(alist-get 'comp colors)))))
        `(org-scheduled-today ((,class (:foreground ,(alist-get 'func colors) :height ,(if wal-theme-org-agenda-height 1.2 1.0)))))
        `(org-scheduled-previously ((,class (:foreground ,(alist-get 'base colors) :slant italic))))
        `(org-sexp-date ((,class (:foreground ,(alist-get 'base colors)))))
        `(org-special-keyword ((,class (:foreground ,(alist-get 'func colors)))))
        `(org-table ((,class (:foreground ,(alist-get 'base colors) :background ,(alist-get 'head1 colors)))))
        `(org-tag ((,class (:foreground ,(alist-get 'meta colors)))))
        `(org-time-grid ((,class (:foreground ,(alist-get 'str colors)))))
        `(org-todo ((,class (:foreground ,(alist-get 'war colors) :inherit bold :background ,(alist-get 'yellow colors)))))
        `(org-upcoming-deadline ((,class (:foreground ,(alist-get 'war colors) :inherit org-priority))))
        `(org-upcoming-distant-deadline ((,class (:foreground ,(alist-get 'suc colors) :inherit org-priority))))
        `(org-verbatim ((,class (:foreground ,(alist-get 'keyword colors)))))
        `(org-verse ((,class (:inherit org-block :slant italic))))
        `(org-warning ((,class (:foreground ,(alist-get 'err colors) :inherit org-priority))))

        ;; outline
        `(outline-1 ((,class (:inherit org-level-1))))
        `(outline-2 ((,class (:inherit org-level-2))))
        `(outline-3 ((,class (:inherit org-level-3))))
        `(outline-4 ((,class (:inherit org-level-4))))
        `(outline-5 ((,class (:inherit org-level-5))))
        `(outline-6 ((,class (:inherit org-level-6))))
        `(outline-7 ((,class (:inherit org-level-7))))
        `(outline-8 ((,class (:inherit org-level-8))))

        ;; perspective
        `(persp-selected-face ((,class (:inherit bold :foreground ,(alist-get 'func colors)))))

        ;; popup
        `(popup-enu-selection-face ((,class (:background ,(alist-get 'ttip colors)l :foreground ,(alist-get 'base colors)))))
        `(popup-face ((,class (:background ,(alist-get 'ttip colors) :foreground ,(alist-get 'ttip colors)))))
        `(popup-isearch-match ((,class (:inherit match))))
        `(popup-menu-face ((,class (:background ,(alist-get 'ttip colors) :foreground ,(alist-get 'base colors)))))
        `(popup-menu-mouse-face ((,class (:inherit highlight))))
        `(popup-scroll-bar-background-face ((,class (:background ,(alist-get 'bg2 colors)))))
        `(popup-scroll-bar-foreground-face ((,class (:background ,(alist-get 'act2 colors)))))
        `(popup-tip-face ((,class (:background ,(alist-get 'ttip colors)l :foreground ,(alist-get 'base colors) :bold nil :italic nil :underline nil))))

        ;; powerline
        `(powerline-active1 ((,class (:background ,(alist-get 'act2 colors) :foreground ,(alist-get 'base colors)))))
        `(powerline-active2 ((,class (:background ,(alist-get 'act2 colors) :foreground ,(alist-get 'base colors)))))
        `(powerline-inactive1 ((,class (:background ,(alist-get 'bg2 colors) :foreground ,(alist-get 'base colors)))))
        `(powerline-inactive2 ((,class (:background ,(alist-get 'bg2 colors) :foreground ,(alist-get 'base colors)))))

        ;; rainbow-delimiters
        `(rainbow-delimiters-depth-1-face ((,class :foreground ,(alist-get 'keyword colors))))
        `(rainbow-delimiters-depth-2-face ((,class :foreground ,(alist-get 'func colors))))
        `(rainbow-delimiters-depth-3-face ((,class :foreground ,(alist-get 'str colors))))
        `(rainbow-delimiters-depth-4-face ((,class :foreground ,(alist-get 'green colors))))
        `(rainbow-delimiters-depth-5-face ((,class :foreground ,(alist-get 'yellow colors))))
        `(rainbow-delimiters-depth-6-face ((,class :foreground ,(alist-get 'keyword colors))))
        `(rainbow-delimiters-depth-7-face ((,class :foreground ,(alist-get 'func colors))))
        `(rainbow-delimiters-depth-8-face ((,class :foreground ,(alist-get 'str colors))))
        `(rainbow-delimiters-mismatched-face ((,class :foreground ,(alist-get 'err colors) :overline t)))
        `(rainbow-delimiters-unmatched-face ((,class :foreground ,(alist-get 'err colors) :overline t)))

        ;; rcirc
        `(rcirc-bright-nick ((,class (:background ,(alist-get 'aqua colors) :foreground ,(alist-get 'cyan colors)))))
        `(rcirc-dim-nick ((,class (:foreground ,(alist-get 'base colors)))))
        `(rcirc-keyword ((,class (:background ,(alist-get 'green colors) :foreground ,(alist-get 'green colors)))))
        `(rcirc-timestamp ((,class (:foreground ,(alist-get 'keyword colors)))))
        `(rcirc-track-keyword ((,class (:background ,(alist-get 'green colors) :foreground ,(alist-get 'bg1 colors)))))
        `(rcirc-url ((,class (:inherit link))))

        ;; shm
        `(shm-current-face ((,class (:background ,(alist-get 'green colors)))))
        `(shm-quarantine-face ((,class (:background ,(alist-get 'red colors)))))

        ;; show-paren
        `(show-paren-match ((,class (:foreground ,(alist-get 'mat colors) :inherit bold  :underline ,(when wal-theme-underline-parens t)))))
        `(show-paren-match-expression ((,class (:background ,(alist-get 'green colors)))))
        `(show-paren-mismatch ((,class (:foreground ,(alist-get 'err colors) :inherit bold :underline ,(when wal-theme-underline-parens t)))))

        ;; smartparens
        `(sp-pair-overlay-face ((,class (:background ,(alist-get 'highlight colors) :foreground nil))))
        `(sp-show-pair-match-face ((,class (:foreground ,(alist-get 'mat colors) :inherit bold  :underline ,(when wal-theme-underline-parens t)))))

        ;; smerge
        `(smerge-base ((,class (:background ,(alist-get 'yellow colors)))))
        `(smerge-markers ((,class (:background ,(alist-get 'ttip colors) :foreground ,(alist-get 'ttip colors)))))
        `(smerge-mine ((,class (:background ,(alist-get 'red colors)))))
        `(smerge-other ((,class (:background ,(alist-get 'green colors)))))
        `(smerge-refined-added ((,class (:background ,(alist-get 'green colors) :foreground ,(alist-get 'green colors)))))
        `(smerge-refined-changed ((,class (:background ,(alist-get 'blue colors) :foreground ,(alist-get 'blue colors)))))
        `(smerge-refined-removed ((,class (:background ,(alist-get 'red colors) :foreground ,(alist-get 'red colors)))))

        ;; spaceline
        `(spaceline-flycheck-error  ((,class (:foreground ,(alist-get 'err colors)))))
        `(spaceline-flycheck-info   ((,class (:foreground ,(alist-get 'keyword colors)))))
        `(spaceline-flycheck-warning((,class (:foreground ,(alist-get 'war colors)))))
        `(spaceline-python-venv ((,class (:foreground ,(alist-get 'comp colors)))))

        ;; wal-specific
        `(wal-transient-state-title-face ((,class (:background nil :foreground ,(alist-get 'comp colors) :box nil :inherit bold))))

        ;; swiper
        `(swiper-line-face ((,class (:background ,(alist-get 'highlight colors) :inherit bold))))
        `(swiper-match-face-1 ((,class (:inherit bold))))
        `(swiper-match-face-2 ((,class (:foreground ,(alist-get 'head1 colors) :underline t))))
        `(swiper-match-face-3 ((,class (:foreground ,(alist-get 'head4 colors) :underline t))))
        `(swiper-match-face-4 ((,class (:foreground ,(alist-get 'head3 colors) :underline t))))

        ;; tabbar
        `(tabbar-button ((,class (:inherit tabbar-default ))))
        `(tabbar-button-highlight ((,class (:inherit tabbar-default))))
        `(tabbar-default ((,class (:background ,(alist-get 'bg1 colors) :foreground ,(alist-get 'head1 colors) :height 0.9))))
        `(tabbar-highlight ((,class (:underline t))))
        `(tabbar-selected ((,class (:inherit tabbar-default :foreground ,(alist-get 'func colors) :weight bold))))
        `(tabbar-separator ((,class (:inherit tabbar-default))))
        `(tabbar-unselected ((,class (:inherit tabbar-default :background ,(alist-get 'bg1 colors) :slant italic :weight light))))

        ;; term
        `(term ((,class (:foreground ,(alist-get 'base colors) :background ,(alist-get 'bg1 colors)))))
        `(term-color-black ((,class (:foreground ,(alist-get 'bg4 colors)))))
        `(term-color-blue ((,class (:foreground ,(alist-get 'keyword colors)))))
        `(term-color-cyan ((,class (:foreground ,(alist-get 'cyan colors)))))
        `(term-color-green ((,class (:foreground ,(alist-get 'green colors)))))
        `(term-color-magenta ((,class (:foreground ,(alist-get 'magenta colors)))))
        `(term-color-red ((,class (:foreground ,(alist-get 'red colors)))))
        `(term-color-white ((,class (:foreground ,(alist-get 'base colors)))))
        `(term-color-yellow ((,class (:foreground ,(alist-get 'yellow colors)))))

        ;; tide
        `(tide-hl-identifier-face ((,class (:foreground ,(alist-get 'yellow colors) :background ,(alist-get 'yellow colors)))))

        ;; treemacs
        `(treemacs-git-added-face ((,class (:foreground ,(alist-get 'green colors) :background ,(alist-get 'green colors)))))
        `(treemacs-git-conflict-face ((,class (:foreground ,(alist-get 'red colors) :background ,(alist-get 'red colors)))))
        `(treemacs-git-ignored-face ((,class (:foreground ,(alist-get 'yellow colors)))))
        `(treemacs-git-modified-face ((,class (:foreground ,(alist-get 'blue colors) :background ,(alist-get 'blue colors)))))
        `(treemacs-git-untracked-face ((,class (:foreground ,(alist-get 'aqua colors) :background ,(alist-get 'aqua colors)))))

        ;; web-mode
        `(web-mode-builtin-face ((,class (:inherit ,(alist-get 'font colors)))))
        `(web-mode-comment-face ((,class (:inherit ,(alist-get 'font colors)))))
        `(web-mode-constant-face ((,class (:inherit ,(alist-get 'font colors)))))
        `(web-mode-current-element-highlight-face ((,class (:background ,(alist-get 'bg3 colors)))))
        `(web-mode-doctype-face ((,class (:inherit ,(alist-get 'font colors)))))
        `(web-mode-function-name-face ((,class (:inherit ,(alist-get 'font colors)))))
        `(web-mode-html-attr-name-face ((,class (:foreground ,(alist-get 'func colors)))))
        `(web-mode-html-attr-value-face ((,class (:foreground ,(alist-get 'keyword colors)))))
        `(web-mode-html-tag-face ((,class (:foreground ,(alist-get 'keyword colors)))))
        `(web-mode-keyword-face ((,class (:foreground ,(alist-get 'keyword colors)))))
        `(web-mode-string-face ((,class (:foreground ,(alist-get 'str colors)))))
        `(web-mode-symbol-face ((,class (:foreground ,(alist-get 'type colors)))))
        `(web-mode-type-face ((,class (:inherit ,(alist-get 'font colors)))))
        `(web-mode-warning-face ((,class (:inherit ,(alist-get 'font colors)))))

        ;; which-key
        `(which-key-command-description-face ((,class (:foreground ,(alist-get 'base colors)))))
        `(which-key-group-description-face ((,class (:foreground ,(alist-get 'keyword colors)))))
        `(which-key-key-face ((,class (:foreground ,(alist-get 'func colors) :inherit bold))))
        `(which-key-separator-face ((,class (:background nil :foreground ,(alist-get 'str colors)))))
        `(which-key-special-key-face ((,class (:background ,(alist-get 'func colors) :foreground ,(alist-get 'bg1 colors)))))

        ;; which-function-mode
        `(which-func ((,class (:foreground ,(alist-get 'func colors)))))

        ;; whitespace-mode
        `(whitespace-empty ((,class (:background nil :foreground ,(alist-get 'yellow colors)))))
        `(whitespace-indentation ((,class (:background nil :foreground ,(alist-get 'war colors)))))
        `(whitespace-line ((,class (:background nil :foreground ,(alist-get 'comp colors)))))
        `(whitespace-newline ((,class (:background nil :foreground ,(alist-get 'comp colors)))))
        `(whitespace-space ((,class (:background nil :foreground ,(alist-get 'act2 colors)))))
        `(whitespace-space-after-tab ((,class (:background nil :foreground ,(alist-get 'yellow colors)))))
        `(whitespace-space-before-tab ((,class (:background nil :foreground ,(alist-get 'yellow colors)))))
        `(whitespace-tab ((,class (:background nil :foreground ,(alist-get 'act2 colors)))))
        `(whitespace-trailing ((,class (:background ,(alist-get 'err colors) :foreground ,(alist-get 'war colors)))))

        ;; other, need more work
        `(ac-completion-face ((,class (:underline t :foreground ,(alist-get 'keyword colors)))))
        `(ffap ((,class (:foreground ,(alist-get 'base colors)))))
        `(flx-highlight-face ((,class (:foreground ,(alist-get 'comp colors) :underline nil))))
        `(icompletep-determined ((,class :foreground ,(alist-get 'keyword colors))))
        `(js2-external-variable ((,class (:foreground ,(alist-get 'comp colors)))))
        `(js2-function-param ((,class (:foreground ,(alist-get 'const colors)))))
        `(js2-jsdoc-html-tag-delimiter ((,class (:foreground ,(alist-get 'str colors)))))
        `(js2-jsdoc-html-tag-name ((,class (:foreground ,(alist-get 'keyword colors)))))
        `(js2-jsdoc-value ((,class (:foreground ,(alist-get 'str colors)))))
        `(js2-private-function-call ((,class (:foreground ,(alist-get 'const colors)))))
        `(js2-private-member ((,class (:foreground ,(alist-get 'base colors)))))
        `(js3-error-face ((,class (:underline ,(alist-get 'war colors)))))
        `(js3-external-variable-face ((,class (:foreground ,(alist-get 'var colors)))))
        `(js3-function-param-face ((,class (:foreground ,(alist-get 'keyword colors)))))
        `(js3-instance-member-face ((,class (:foreground ,(alist-get 'const colors)))))
        `(js3-jsdoc-tag-face ((,class (:foreground ,(alist-get 'keyword colors)))))
        `(js3-warning-face ((,class (:underline ,(alist-get 'keyword colors)))))
        `(slime-repl-inputed-output-face ((,class (:foreground ,(alist-get 'comp colors)))))
        `(trailing-whitespace ((,class :foreground nil :background ,(alist-get 'err colors))))
        `(undo-tree-visualizer-current-face ((,class :foreground ,(alist-get 'keyword colors))))
        `(undo-tree-visualizer-default-face ((,class :foreground ,(alist-get 'base colors))))
        `(undo-tree-visualizer-register-face ((,class :foreground ,(alist-get 'comp colors))))
        `(undo-tree-visualizer-unmodified-face ((,class :foreground ,(alist-get 'var colors)))))

        (custom-theme-set-variables
        theme-name

        ;; ansi-color-names
        `(ansi-color-names-vector [(alist-get 'bg4 colors) ,(alist-get 'red colors) ,(alist-get 'green colors) ,(alist-get 'yellow colors) ,(alist-get 'blue colors) ,(alist-get 'magenta colors) ,(alist-get 'cyan colors) ,(alist-get 'base colors)])

        ;; hl-todo
        `(hl-todo-keyword-faces '(("TODO"   . ,(alist-get 'war colors))
                                  ("NEXT"   . ,(alist-get 'war colors))
                                  ("THEM"   . ,(alist-get 'aqua colors))
                                  ("PROG"   . ,(alist-get 'blue colors))
                                  ("OKAY"   . ,(alist-get 'blue colors))
                                  ("DONT"   . ,(alist-get 'red colors))
                                  ("FAIL"   . ,(alist-get 'red colors))
                                  ("DONE"   . ,(alist-get 'suc colors))
                                  ("NOTE"   . ,(alist-get 'yellow colors))
                                  ("KLUDGE" . ,(alist-get 'yellow colors))
                                  ("HACK"   . ,(alist-get 'yellow colors))
                                  ("TEMP"   . ,(alist-get 'yellow colors))
                                  ("FIXME"  . ,(alist-get 'war colors))
                                  ("XXX"    . ,(alist-get 'war colors))
                                  ("XXXX"   . ,(alist-get 'war colors))
                                  ("???"    . ,(alist-get 'war colors))))

        ;; pdf-tools
        `(pdf-view-midnight-colors '((alist-get 'base colors) . ,(alist-get 'bg1 colors))))))))

(provide 'wal-theme-common)
;;; wal-theme-common ends here
