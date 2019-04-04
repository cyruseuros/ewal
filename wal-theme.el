;;; wal-theme.el --- A wal-based automatic, terminal aware theme generator.

;; Copyright (C) 2015-2018 Nasser Alshammari
;; Copyright (C) 2019 Uros Perisic

;; Author: Nasser Alshammari
;; URL: <https://github.com/nashamri/spacemacs-theme>
;;
;; Version: 0.1
;; Keywords: color, theme, generator, wal, pywal
;; Package-Requires: ((emacs "24") (cl-lib) (json) (color))

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

;; This is a color theme generator for Emacs with an eye towards Spacemacs
;; <https://github.com/syl20bnr/spacemacs>.

;;; Code:
(require 'json)
(require 'cl-lib)
(require 'color)
(require 'term/tty-colors)
(require 'config nil t)
(require 'spacemacs-common nil t)
;; declare undetected functions
(declare-function pairlis 'cl-lib)


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
  (file-name-as-directory (expand-file-name "~/.cache/wal-theme"))
  "Location of wal-theme cache directory."
  :type 'string
  :group 'wal-theme)

(defcustom wal-theme-ansi-color-names
  (mapcar 'intern
          (cl-loop for (key . value)
                   in tty-defined-color-alist
                   collect key))
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

(defcustom wal-theme-primary-accent-color 'magenta
  "Predominant color in generated wal-theme.
Must be one of `wal-theme-ansi-color-names'"
  :type 'symbol
  :group 'wal-theme)

(defcustom wal-theme-secondary-accent-color 'blue
  "Second most predominant color in generated wal-theme.
Must be one of `wal-theme-ansi-color-names'"
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

(defun wal-theme-clear-cache ()
  "Clear wal-theme cache."
  (interactive)
  (delete-directory wal-theme-own-cache-dir t))


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
        (json-array-type 'list)
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

(defun wal-theme-get-color (color &optional shade tty approximate palette)
  "Return SHADE of COLOR from current `wal-theme' PALETTE.
Choose color that is darker (-) or lightener (+) than COLOR by
SHADE. SHADE defaults to 0, returning original wal COLOR. If
SHADE exceeds number of available shades, the darkest/lightest
shade is returned. If TTY is t, return original, TTY compatible
`wal' color regardless od SHADE. If APPROXIMATE is set,
approximate color using `tty-color-approximate', otherwise return
default (non-extended) wal color."
  (let ((palette (or palette wal-theme-extended-palette))
        (tty (or tty nil))
        (middle (/ (- (length (car wal-theme-extended-palette)) 1) 2))
        (shade (or shade 0)))
    (let ((return-color (nth (+ middle shade) (alist-get color palette))))
      (let ((bound-color (if return-color
                             return-color
                           (car (last (alist-get color palette))))))
        (if tty
            (if approximate
                (apply 'color-rgb-to-hex
                       (cddr (tty-color-approximate
                              (tty-color-standard-values bound-color))))
              (nth middle (alist-get color palette)))
          bound-color)))))

(defun wal-theme--generate-theme-colors (&optional tty primary-accent-color secondary-accent-color)
  "Make theme colorscheme from theme palettes.
If TTY is t colorscheme is reduced to basic tty supported colors.
PRIMARY-ACCENT-COLOR sets the main theme color---defaults to
`wal-theme-primary-accent-color'. Ditto for
SECONDARY-ACCENT-COLOR"
  (let ((primary-accent-color (or primary-accent-color wal-theme-primary-accent-color))
        (secondary-accent-color (or secondary-accent-color wal-theme-secondary-accent-color)))
    (let ((theme-colors
          `((act1          . ,(wal-theme-get-color 'background -2 tty))
            (act2          . ,(wal-theme-get-color primary-accent-color 0 tty))
            (base          . ,(wal-theme-get-color 'foreground 0 tty))
            (base-dim      . ,(wal-theme-get-color 'foreground -4 tty))
            (bg1           . ,(wal-theme-get-color 'background 0 tty))
            (bg2           . ,(wal-theme-get-color 'background -2 tty))
            (bg3           . ,(wal-theme-get-color 'background -3 tty))
            (bg4           . ,(wal-theme-get-color 'background -4 tty))
            (border        . ,(wal-theme-get-color 'background 0 tty))
            (cblk          . ,(wal-theme-get-color 'background 2 tty))
            (cblk-bg       . ,(wal-theme-get-color 'background -2 tty))
            (cblk-ln       . ,(wal-theme-get-color primary-accent-color 2 tty))
            (cblk-ln-bg    . ,(wal-theme-get-color primary-accent-color -2 tty))
            (cursor        . ,(wal-theme-get-color 'foreground -2 tty))
            (const         . ,(wal-theme-get-color primary-accent-color 3 tty))
            (comment       . ,(wal-theme-get-color 'background 3 tty))
            (comment-bg    . ,(wal-theme-get-color 'background 0 tty))
            (comp          . ,(wal-theme-get-color secondary-accent-color 0 tty))
            (err           . ,(wal-theme-get-color 'red 4 tty))
            (func          . ,(wal-theme-get-color primary-accent-color 0 tty))
            (head1         . ,(wal-theme-get-color primary-accent-color 0 tty))
            (head1-bg      . ,(wal-theme-get-color 'background -2 tty))
            (head2         . ,(wal-theme-get-color secondary-accent-color 0 tty))
            (head2-bg      . ,(wal-theme-get-color 'background -2 tty))
            (head3         . ,(wal-theme-get-color 'cyan 0 tty))
            (head3-bg      . ,(wal-theme-get-color 'background -2 tty))
            (head4         . ,(wal-theme-get-color 'yellow 0 tty))
            (head4-bg      . ,(wal-theme-get-color 'background -2 tty))
            (highlight     . ,(wal-theme-get-color 'background 4 tty))
            (highlight-dim . ,(wal-theme-get-color 'background 3 tty))
            (keyword       . ,(wal-theme-get-color secondary-accent-color 2 tty))
            (lnum          . ,(wal-theme-get-color 'background 2 tty))
            (mat           . ,(wal-theme-get-color 'green 1 tty))
            (meta          . ,(wal-theme-get-color 'yellow 4 tty))
            (str           . ,(wal-theme-get-color 'cyan -1 tty))
            (suc           . ,(wal-theme-get-color 'green 4 tty))
            (ttip          . ,(wal-theme-get-color 'background 2 tty))
            (ttip-sl       . ,(wal-theme-get-color 'background 4 tty))
            (ttip-bg       . ,(wal-theme-get-color 'background 0 tty))
            (type          . ,(wal-theme-get-color 'red 2 tty))
            (var           . ,(wal-theme-get-color secondary-accent-color 4 tty))
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


(defun wal-theme--cache-own-theme (&optional base-palette extended-palette
                                             tty-colors gui-colors)
  "Serializes all palettes and colors in json format.
BASE-PALETTE, EXTENDED-PALETTE, TTY-COLORS, and GUI-COLORS all
refer to the variables provided by wal-theme by default, prefixed
with the package name."
  (let ((json-encoding-pretty-print t)
        (base-palette (or base-palette wal-theme-base-palette))
        (extended-palette (or extended-palette wal-theme-extended-palette))
        (tty-colors (or tty-colors wal-theme-semantic-tty-colors))
        (gui-colors (or gui-colors wal-theme-semantic-gui-colors)))
          (progn
            (if (null (file-directory-p wal-theme-own-cache-dir))
                (make-directory wal-theme-own-cache-dir t))
            (with-temp-file wal-theme--own-cache-base-palette-json-file
              (insert (json-encode-list base-palette)))
            (with-temp-file wal-theme--own-cache-extended-palette-json-file
              (insert (json-encode-list extended-palette)))
            (with-temp-file wal-theme--own-cache-semantic-tty-colors-json-file
              (insert (json-encode-list tty-colors)))
            (with-temp-file wal-theme--own-cache-semantic-gui-colors-json-file
              (insert (json-encode-list gui-colors))))))

(defun wal-theme--load-own-theme ()
  "Load current wal-theme variables for use in `wal-theme-create-theme'."
  (let ((load-from-cache (and (file-exists-p wal-theme--own-cache-base-palette-json-file)
                              (file-newer-than-file-p wal-theme--own-cache-base-palette-json-file
                                                      wal-theme--wal-cache-json-file)
                              (equal wal-theme-primary-accent-color
                                     (cadar (get 'wal-theme-primary-accent-color 'standard-value)))
                              (equal wal-theme-secondary-accent-color
                                     (cadar (get 'wal-theme-secondary-accent-color 'standard-value)))))
        (json-object-type 'alist)
        (json-array-type 'list))
    (if load-from-cache
        (progn
          (setq wal-theme-base-palette (json-read-file wal-theme--own-cache-base-palette-json-file))
          (setq wal-theme-extended-palette (json-read-file wal-theme--own-cache-extended-palette-json-file))
          (setq wal-theme-semantic-tty-colors (json-read-file wal-theme--own-cache-semantic-tty-colors-json-file))
          (setq wal-theme-semantic-gui-colors (json-read-file wal-theme--own-cache-semantic-gui-colors-json-file)))
      (progn
        (wal-theme-clear-cache)
        (setq wal-theme-base-palette (wal-theme--load-wal-theme))
        (setq wal-theme-extended-palette (wal-theme--extend-base-palette 4 5))
        (setq wal-theme-semantic-gui-colors (wal-theme--generate-theme-colors nil))
        (setq wal-theme-semantic-tty-colors (wal-theme--generate-theme-colors t))
        (wal-theme--cache-own-theme)))))

(defun wal-theme-get-spacemacs-cursors-colors (&optional tty)
  "Use wal colors to customize `spacemacs-evil-cursors'.
TTY specifies whether to use TTY or GUI colors."
  (wal-theme--load-own-theme)
  (let ((tty (if (boundp tty) tty
               (or wal-theme-force-tty-colors
                   (display-graphic-p)))))
           `(("normal" ,(wal-theme-get-color 'cursor 0 tty) box)
             ("insert" ,(wal-theme-get-color 'green 0 tty) (bar . 2))
             ("emacs" ,(wal-theme-get-color 'blue 0 tty) box)
             ("hybrid" ,(wal-theme-get-color 'blue 0 tty) (bar . 2))
             ("evilified" ,(wal-theme-get-color 'red 0 tty) box)
             ("visual" ,(wal-theme-get-color 'white -4 tty) (hbar . 2))
             ("motion" ,(wal-theme-get-color 'magenta 0) box)
             ("replace" ,(wal-theme-get-color 'red -4 tty) (hbar . 2))
             ("lisp" ,(wal-theme-get-color 'magenta 4 tty) box)
             ("iedit" ,(wal-theme-get-color 'red 0 tty) box)
             ("iedit-insert" ,(wal-theme-get-color 'red 0 tty) (bar . 2)))))


(defun wal-theme-get-spacemacs-theme-colors (&optional tty)
  "Use wal colors to customize spacemacs-theme.
To be found at: <https://github.com/nashamri/spacemacs-theme>.
TTY defualts to `wal-theme-force-tty-colors' or
`display-graphic-p'."
  (wal-theme--load-own-theme)
  (let ((tty (if (boundp tty) tty
               (or wal-theme-force-tty-colors
                   (display-graphic-p)))))
    (if tty
        wal-theme-semantic-tty-colors
      wal-theme-semantic-gui-colors
      )))
(provide 'wal-theme)
;;; wal-theme ends here
