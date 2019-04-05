;;; ewal.el --- A wal-based <https://github.com/dylanaraps/pywal>,
;;; automatic, terminal aware theme generator.

;; Copyright (C) 2019 Uros Perisic
;; Copyright (C) 2019 Grant Shangreaux

;; Author: Uros Perisic
;; URL: <https://gitlab.com/jjzmajic/ewal.el>
;;
;; Version: 0.1
;; Keywords: color, theme, generator, wal, pywal
;; Package-Requires: ((emacs "24") (cl-lib) (json) (color) (term/tty-colors))

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
;; <https://github.com/syl20bnr/spacemacs>, and `spacemacs-theme'
;; <https://github.com/nashamri/spacemacs-theme>, but no dependencies on either,
;; so you can use it to colorize your vanilla Emacs as well.

;; My hope is that `ewal' will remain theme agnostic, with people
;; contributing functions like `ewal-get-spacemacs-theme-colors' for other
;; popular themes such as `solarized-emacs'
;; <https://github.com/bbatsov/solarized-emacs>, making it easy to keep the
;; style of different themes, while adapting them to the rest of your theming
;; setup. No problem should ever have to be solved twice!

;;; Code:

;; deps
(require 'json)
(require 'cl-lib)
(require 'color)
(require 'term/tty-colors)
;; optional deps
(require 'pcache nil t)
;; declare undetected functions

(defgroup ewal nil
  "ewal options."
  :group 'faces)

(defcustom ewal-wal-cache-dir
  (file-name-as-directory (expand-file-name "~/.cache/wal"))
  "Location of wal cache directory."
  :type 'string
  :group 'ewal)

(defvar ewal--wal-cache-json-file
  (concat ewal-wal-cache-dir "colors.json")
  "Location of cached wal theme in json format.")

(defcustom ewal-ansi-color-name-symbols
  (mapcar 'intern
          (cl-loop for (key . value)
                   in tty-defined-color-alist
                   collect key))
  "The 8 most universaly supported TTY color names.
They will be extracted from `ewal--cache-json-file', and
with the right escape sequences applied using

source ${HOME}/.cache/wal/colors-tty.sh

should be viewable even in the Linux console (See
https://github.com/dylanaraps/pywal/wiki/Getting-Started#applying-the-theme-to-new-terminals
for more details). NOTE: Order matters."
  :type 'list
  :group 'ewal)


(defcustom ewal-force-tty-colors nil
  "Whether to use TTY version of wall theme.
Meant for setting TTY theme regardless of GUI support."
  :type 'boolean
  :group 'ewal)

(defcustom ewal-primary-accent-color 'magenta
  "Predominant color in generated ewal.
Must be one of `ewal-ansi-color-name-symbols'"
  :type 'symbol
  :group 'ewal)

(defvar ewal-secondary-accent-color 'blue
  "Second most predominant color in generated ewal.
Must be one of `ewal-ansi-color-name-symbols'")

(defvar ewal-base-palette nil
  "Current base palette extracted from `ewal--wal-cache-json-file'.
Stored as a flat alist, and cached in `ewal--own-cache-base-palette-json-file'.")

(defvar ewal-extended-palette nil
  "Extended palette computed from `ewal--wal-cache-json-file'.
Stored as a flat alist, and cached in `ewal--own-cache-extended-palette-json-file'.")

(defvar ewal-spacemacs-theme-gui-colors nil
  "`spacemacs-theme' compatible GUI colors.
Extracted from current `ewal' theme.")

(defvar ewal-spacemacs-theme-tty-colors nil
  "`spacemacs-theme' compatible TTY colors.
Extracted from current `ewal' theme.")

(defvar ewal-spacemacs-evil-cursors-gui-colors nil
  "`spacemacs-evil-cursors' compatible GUI colors.
Extracted from current `ewal' theme.")

(defvar ewal-spacemacs-evil-cursors-tty-colors nil
  "`spacemacs-evil-cursors' compatible TTY colors.
Extracted from current `ewal' theme.")

(defcustom ewal-use-pcache-p (require 'pcache nil t)
  "Whether to use pcache to store `ewal' variables.
Since this fetaure depends on `pcache', and computing `ewal'
variables is not all that costly, `ewal-use-pcache-p' defaults to
the return value of \(require 'pcache nil t\)."
  :type 'boolean
  :group 'ewal)

(defvar ewal--pcache-repo-name
  (when ewal-use-pcache-p "ewal")
  "`pcache' repository name used by `ewal'.
Only set when `ewal-use-pcache-p' is t.")

(defvar ewal--pcache-repo
  (when ewal-use-pcache-p (pcache-repository ewal--pcache-repo-name))
  "`pcache' repository used by `ewal'.
Only set when `ewal-use-pcache-p' is t.")

(defvar ewal--pcache-repo-file
  (when ewal-use-pcache-p
    (concat (file-name-as-directory pcache-directory)
            ewal--pcache-repo-name))
  "`pcache' repository file used by `ewal'.
Only set when `ewal-use-pcache-p' is t.")

(defun ewal--cache-expired-p ()
  "Check whether `ewal' pcache has expired."
  (file-newer-than-file-p ewal--wal-cache-json-file ewal--pcache-repo-file))

(defun ewal--load-from-cache-p ()
  "Check whether `ewal-use-pcache-p' is t.
Also check whether `ewal--cache-expired-p'."
  (and ewal-use-pcache-p (ewal--cache-expired-p)))

(defun ewal--use-tty-colors-p (tty)
  "Utility function to check if TTY colors should be used."
  (if (boundp tty) tty
    (or ewal-force-tty-colors
        (display-graphic-p))))

(defun ewal-clear-cache ()
  "Clear ewal cache."
  (interactive)
  (pcache-destroy-repository ewal--pcache-repo-name))


(defun ewal--load-wal-theme (&optional json color-names)
  "Read JSON as the most complete of the cached wal files.
COLOR-NAMES will be associated with the first 8 colors of the
cached wal colors. COLOR-NAMES are meant to be used in
conjunction with `ewal-ansi-color-name-symbols'.
\"Special\" wal colors such as \"background\", \"foreground\",
and \"cursor\", tend to \(but do not always\) correspond to the
remaining colors generated by wal. Add those special colors to
the returned alist."
  (let ((json (or json ewal--wal-cache-json-file))
        (json-object-type 'alist)
        (json-array-type 'list)
        (color-names (or color-names ewal-ansi-color-name-symbols)))
    (let ((colors (json-read-file json)))
      (let ((special-colors (alist-get 'special colors))
            (regular-colors (alist-get 'colors colors)))
        (let ((regular-color-values (cl-loop for
                                             (key . value)
                                             in
                                             regular-colors
                                             collect
                                             value)))
          (let ((cannonical-colors (cl-pairlis color-names regular-color-values)))
            (append special-colors cannonical-colors)))))))

(defun ewal--extend-base-color (color num-degrees degree-size)
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

(defun ewal--extend-base-palette (num-degrees degree-size &optional palette)
  "Use `ewal--extend-base-color' to extend entire base PALETTE.
which defaults to `ewal-base-palette' and returns an
extended palette alist intended to be stored in
`ewal-extended-palette'. Like
`ewal--extend-base-color', extend \(darken \(-\) or
lighten \(+\)\) COLOR. Do so by 2 * NUM-DEGREES \(NUM-DEGREES
lighter, and NUM-DEGREES darker\), in increments of DEGREE-SIZE
percentage points."
  (let ((palette (or palette ewal-base-palette)))
    (cl-loop for
             (key . value)
             in
             palette
             collect
             `(,key
               . ,(ewal--extend-base-color value num-degrees
                                                degree-size)))))

(defun ewal-get-color (color &optional shade tty approximate palette)
  "Return SHADE of COLOR from current `ewal' PALETTE.
Choose color that is darker (-) or lightener (+) than COLOR
\(must be one of `ewal-ansi-color-name-symbols'\) by SHADE. SHADE
defaults to 0, returning original wal COLOR. If SHADE exceeds
number of available shades, the darkest/lightest shade is
returned. If TTY is t, return original, TTY compatible `wal'
color regardless od SHADE. If APPROXIMATE is set, approximate
color using `tty-color-approximate', otherwise return
default (non-extended) wal color."
  (let ((palette (or palette ewal-extended-palette))
        (tty (or tty nil))
        (middle (/ (- (length (car ewal-extended-palette)) 1) 2))
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

(defun ewal--generate-spacemacs-theme-colors (&optional tty primary-accent-color secondary-accent-color)
  "Make theme colorscheme from theme palettes.
If TTY is t, colorscheme is reduced to basic tty supported colors.
PRIMARY-ACCENT-COLOR sets the main theme color---defaults to
`ewal-primary-accent-color'. Ditto for
SECONDARY-ACCENT-COLOR"
  (let ((primary-accent-color (or primary-accent-color ewal-primary-accent-color))
        (secondary-accent-color (or secondary-accent-color ewal-secondary-accent-color)))
    (let ((theme-colors
          `((act1          . ,(ewal-get-color 'background -2 tty))
            (act2          . ,(ewal-get-color primary-accent-color 0 tty))
            (base          . ,(ewal-get-color 'foreground 0 tty))
            (base-dim      . ,(ewal-get-color 'foreground -4 tty))
            (bg1           . ,(ewal-get-color 'background 0 tty))
            (bg2           . ,(ewal-get-color 'background -2 tty))
            (bg3           . ,(ewal-get-color 'background -3 tty))
            (bg4           . ,(ewal-get-color 'background -4 tty))
            (border        . ,(ewal-get-color 'background 0 tty))
            (cblk          . ,(ewal-get-color 'background 2 tty))
            (cblk-bg       . ,(ewal-get-color 'background -2 tty))
            (cblk-ln       . ,(ewal-get-color primary-accent-color 2 tty))
            (cblk-ln-bg    . ,(ewal-get-color primary-accent-color -2 tty))
            (cursor        . ,(ewal-get-color 'foreground -2 tty))
            (const         . ,(ewal-get-color primary-accent-color 3 tty))
            (comment       . ,(ewal-get-color 'background 3 tty))
            (comment-bg    . ,(ewal-get-color 'background 0 tty))
            (comp          . ,(ewal-get-color secondary-accent-color 0 tty))
            (err           . ,(ewal-get-color 'red 4 tty))
            (func          . ,(ewal-get-color primary-accent-color 0 tty))
            (head1         . ,(ewal-get-color primary-accent-color 0 tty))
            (head1-bg      . ,(ewal-get-color 'background -2 tty))
            (head2         . ,(ewal-get-color secondary-accent-color 0 tty))
            (head2-bg      . ,(ewal-get-color 'background -2 tty))
            (head3         . ,(ewal-get-color 'cyan 0 tty))
            (head3-bg      . ,(ewal-get-color 'background -2 tty))
            (head4         . ,(ewal-get-color 'yellow 0 tty))
            (head4-bg      . ,(ewal-get-color 'background -2 tty))
            (highlight     . ,(ewal-get-color 'background 4 tty))
            (highlight-dim . ,(ewal-get-color 'background 3 tty))
            (keyword       . ,(ewal-get-color secondary-accent-color 2 tty))
            (lnum          . ,(ewal-get-color 'background 2 tty))
            (mat           . ,(ewal-get-color 'green 1 tty))
            (meta          . ,(ewal-get-color 'yellow 4 tty))
            (str           . ,(ewal-get-color 'cyan -1 tty))
            (suc           . ,(ewal-get-color 'green 4 tty))
            (ttip          . ,(ewal-get-color 'background 2 tty))
            (ttip-sl       . ,(ewal-get-color 'background 4 tty))
            (ttip-bg       . ,(ewal-get-color 'background 0 tty))
            (type          . ,(ewal-get-color 'red 2 tty))
            (var           . ,(ewal-get-color secondary-accent-color 4 tty))
            (war           . ,(ewal-get-color 'red 1 tty))

            ;; colors
            (aqua          . ,(ewal-get-color 'cyan 0 tty))
            (aqua-bg       . ,(ewal-get-color 'cyan -4 tty))
            (green         . ,(ewal-get-color 'green 0 tty))
            (green-bg      . ,(ewal-get-color 'green -4 tty))
            (green-bg-s    . ,(ewal-get-color 'green -3 tty))
            (cyan          . ,(ewal-get-color 'cyan 4 tty))
            (red           . ,(ewal-get-color 'red 0 tty))
            (red-bg        . ,(ewal-get-color 'red -4 tty))
            (red-bg-s      . ,(ewal-get-color 'red -3 tty))
            (blue          . ,(ewal-get-color 'blue 0 tty))
            (blue-bg       . ,(ewal-get-color 'blue -4 tty))
            (blue-bg-s     . ,(ewal-get-color 'blue -3 tty))
            (magenta       . ,(ewal-get-color 'magenta 0 tty))
            (yellow        . ,(ewal-get-color 'yellow 0 tty))
            (yellow-bg     . ,(ewal-get-color 'yellow -4 tty)))))
          theme-colors)))

(defun ewal--generate-spacemacs-evil-cursors-colors (&optional tty)
  "Use wal colors to customize `spacemacs-evil-cursors'.
TTY specifies whether to use TTY or GUI colors."
  (let ((tty (if (boundp tty) tty
               (or ewal-force-tty-colors
                   (display-graphic-p)))))
    `(("normal" ,(ewal-get-color 'cursor 0 tty) box)
      ("insert" ,(ewal-get-color 'green 0 tty) (bar . 2))
      ("emacs" ,(ewal-get-color 'blue 0 tty) box)
      ("hybrid" ,(ewal-get-color 'blue 0 tty) (bar . 2))
      ("evilified" ,(ewal-get-color 'red 0 tty) box)
      ("visual" ,(ewal-get-color 'white -4 tty) (hbar . 2))
      ("motion" ,(ewal-get-color 'magenta 0) box)
      ("replace" ,(ewal-get-color 'red -4 tty) (hbar . 2))
      ("lisp" ,(ewal-get-color 'magenta 4 tty) box)
      ("iedit" ,(ewal-get-color 'red 0 tty) box)
      ("iedit-insert" ,(ewal-get-color 'red 0 tty) (bar . 2)))))

(defun ewal--cache-ewal-theme ()
  "Cache all `ewal' palettes and colors."
  (let ((json-encoding-pretty-print t))
          (progn
            (pcache-put ewal--pcache-repo 'ewal-base-palette ewal-base-palette)
            (pcache-put ewal--pcache-repo 'ewal-extended-palette ewal-extended-palette)
            (pcache-put ewal--pcache-repo 'ewal-spacemacs-theme-gui-colors
                        ewal-spacemacs-theme-gui-colors)
            (pcache-put ewal--pcache-repo 'ewal-spacemacs-theme-tty-colors
                        ewal-spacemacs-theme-gui-colors)
            (pcache-put ewal--pcache-repo 'ewal-spacemacs-evil-cursors-gui-colors
                        ewal-spacemacs-evil-cursors-gui-colors)
            (pcache-put ewal--pcache-repo 'ewal-spacemacs-evil-cursors-tty-colors
                        ewal-spacemacs-evil-cursors-tty-colors))))

(defun ewal-load-ewal-theme ()
  "Load all `ewal' palettes and colors.
If `ewal--load-from-cache-p' returns t, load from cache.
Otherwise regenerate palettes and colors."
  (if (ewal--load-from-cache-p)
      (progn
        (setq ewal-base-palette (pcache-get ewal--pcache-repo 'ewal-base-palette))
        (setq ewal-extended-palette (pcache-get ewal--pcache-repo 'ewal-extended-palette))
        (setq ewal-spacemacs-theme-gui-colors
              (pcache-get ewal--pcache-repo 'ewal-spacemacs-evil-cursors-gui-colors))
        (setq ewal-spacemacs-theme-tty-colors
              (pcache-get ewal--pcache-repo 'ewal-spacemacs-theme-tty-colors))
        (setq ewal-spacemacs-evil-cursors-gui-colors
              (pcache-get ewal--pcache-repo 'ewal-spacemacs-evil-cursors-gui-colors))
        (setq ewal-spacemacs-evil-cursors-tty-colors
              (pcache-get ewal--pcache-repo 'ewal-spacemacs-evil-cursors-tty-colors)))
    (progn
      (ewal-clear-cache)
      (setq ewal-base-palette (ewal--load-wal-theme))
      (setq ewal-extended-palette (ewal--extend-base-palette 4 5))
      (setq ewal-spacemacs-theme-gui-colors (ewal--generate-spacemacs-theme-colors nil))
      (setq ewal-spacemacs-theme-tty-colors (ewal--generate-spacemacs-theme-colors t))
      (setq ewal-spacemacs-evil-cursors-gui-colors (ewal--generate-spacemacs-evil-cursors-colors nil))
      (setq ewal-spacemacs-evil-cursors-tty-colors (ewal--generate-spacemacs-evil-cursors-colors t))
      (ewal--cache-ewal-theme))))

(defun ewal-get-spacemacs-theme-colors (&optional tty)
  "Get `spacemacs-theme' colors.
For usage see: <https://github.com/nashamri/spacemacs-theme>.
TTY defaults to return value of `ewal--use-tty-colors-p'."
  (ewal-load-ewal-theme)
  (let ((tty (ewal--use-tty-colors-p tty)))
    (if tty
        ewal-spacemacs-theme-tty-colors
      ewal-spacemacs-theme-gui-colors)))

(defun ewal-get-spacemacs-evil-cursors-colors (&optional tty)
  "Get `spacemacs-evil-cursors' colors.
TTY defaults to return value of `ewal--use-tty-colors-p'."
  (ewal-load-ewal-theme)
  (let ((tty (ewal--use-tty-colors-p tty)))
    (if tty
        ewal-spacemacs-evil-cursors-tty-colors
      ewal-spacemacs-evil-cursors-gui-colors)))

(provide 'ewal)
;;; ewal ends here
