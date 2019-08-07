;;; ewal.el --- A pywal-based theme generator -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Uros Perisic
;; Copyright (C) 2019 Grant Shangreaux
;; Copyright (C) 2016-2018 Henrik Lissner

;; Author: Uros Perisic
;; URL: https://gitlab.com/jjzmajic/ewal
;;
;; Version: 0.1
;; Keywords: faces
;; Package-Requires: ((emacs "25"))

;; This program is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; this program. If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of Emacs.

;;; Commentary:

;; A dependency-free, pywal-based, automatic, terminal-aware Emacs
;; color-picker and theme generator.

;; My hope is that `ewal' will remain theme agnostic, with people
;; contributing functions like `ewal-get-spacemacs-theme-colors' from
;; `ewal-spacemacs-themes' for other popular themes such as
;; `solarized-emacs' <https://github.com/bbatsov/solarized-emacs>,
;; making it easy to keep the style of different themes, while
;; adapting them to the rest of your theming setup.  No problem should
;; ever have to be solved twice!

;;; Code:

;; deps
(require 'cl-lib)
(require 'color)
(require 'json)
;; (require 'term/tty-colors)

(defgroup ewal nil
  "ewal options."
  :group 'faces)

(defcustom ewal-wal-cache-dir
  (file-name-as-directory (expand-file-name "~/.cache/wal"))
  "Location of wal cache directory."
  :type 'string
  :group 'ewal)

(defcustom ewal-wal-cache-json-file
  (concat ewal-wal-cache-dir "colors.json")
  "Location of cached wal theme in json format."
  :type 'string
  :group 'ewal)

(defcustom ewal-ansi-color-name-symbols
  (mapcar #'intern
          (cl-loop for (key . _value)
                   in tty-defined-color-alist
                   collect key))
  "The 8 most universaly supported TTY color names.
They will be extracted from `ewal--cache-json-file', and with the
right escape sequences applied using:

#+BEGIN_SRC shell
${HOME}/.cache/wal/colors-tty.sh
#+END_SRC

The colors should be viewable even in the Linux console (See
https://github.com/dylanaraps/pywal/wiki/Getting-Started#applying-the-theme-to-new-terminals
for more details).  NOTE: Order matters."
  :type 'list
  :group 'ewal)

(defcustom ewal-force-tty-colors-in-daemon-p nil
  "Whether to use TTY version of `ewal' colors in Emacs daemon.
It's a numbers game.  Set to t if you connect to your Emacs
server from a TTY most of the time, unless you want to run `ewal'
every time you connect with `emacsclient'."
  :type 'boolean
  :group 'ewal)

(defcustom ewal-force-tty-colors-p nil
  "Whether to use TTY version of `ewal' colors.
Meant for setting TTY theme regardless of GUI support."
  :type 'boolean
  :group 'ewal)

(defcustom ewal-primary-accent-color 'magenta
  "Predominant `ewal' color.
Must be one of `ewal-ansi-color-name-symbols'"
  :type 'symbol
  :group 'ewal)

(defcustom ewal-secondary-accent-color 'blue
  "Second most predominant `ewal' color.
Must be one of `ewal-ansi-color-name-symbols'"
  :type 'symbol
  :group 'ewal)

(defcustom ewal-dark-palette-p t
  "Assume `ewal' theme is a dark theme.
Relevant either when using `ewal's built-in palettes, or when
guessing which colors to use as the special \"background\" and
\"foreground\" `wal' colors."
  :type 'boolean
  :group 'ewal)

(defcustom ewal-built-in-palette "sexy-material"
  "Whether to skip reading the `wal' cache and use built-in palettes.
Only applies when `wal' cache is unreadable for some reason."
  :type 'string
  :group 'ewal)

(defcustom ewal-use-built-in-always-p nil
  "Whether to skip reading the `wal' cache and use built-in palettes."
  :type 'boolean
  :group 'ewal)

(defcustom ewal-use-built-in-on-failure-p t
  "Whether to skip reading the `wal' cache and use built-in palettes.
Only applies when `wal' cache is unreadable for some reason."
  :type 'boolean
  :group 'ewal)

(defcustom ewal-cursor-color (symbol-name ewal-primary-accent-color)
  "Assumed color of special \"cursor\" color in `wal' themes.
Only relevant in TTY/terminal."
  :type 'string
  :group 'ewal)

(defvar ewal-built-in-json-file
  (concat (file-name-directory load-file-name)
          "palettes/"
          (if ewal-dark-palette-p "dark/" "light/")
          ewal-built-in-palette
          ".json")
  "Json file to be used in case `ewal-use-built-in-always-p' is t.
Also if `ewal-use-built-in-on-failure-p' is t and something goes wrong.")

(defvar ewal-ansi-background-name (if ewal-dark-palette-p "black" "white")
  "Ansi color to use for background in tty.")

(defvar ewal-ansi-foreground-name (if ewal-dark-palette-p "white" "black")
  "Ansi color to use for background in tty.")

(defvar ewal-base-palette nil
  "Current base palette extracted from `ewal-wal-cache-json-file'.")

(defvar ewal-extended-palette nil
  "Extended palette based on `ewal-base-palette'.")

(defvar ewal-high-contrast-p nil
  "Whether to increase the contrast of colors.
Essentially just double the argument passed to
`ewal-get-color'.")

;;;###autoload
(defun ewal-load-wal-colors (&optional json color-names)
  "Read JSON as the most complete of the cached wal files.
COLOR-NAMES will be associated with the first 8 colors of the
cached wal colors.  COLOR-NAMES are meant to be used in
conjunction with `ewal-ansi-color-name-symbols'.  \"Special\" wal
colors such as \"background\", \"foreground\", and \"cursor\",
tend to \(but do not always\) correspond to the remaining colors
generated by wal. Add those special colors to the returned
alist. Return nil on failure."
  (condition-case nil
      (let* ((json (or json ewal-wal-cache-json-file))
             (json-object-type 'alist)
             (json-array-type 'list)
             (color-names (or color-names ewal-ansi-color-name-symbols))
             (colors (json-read-file json))
             (special-colors (alist-get 'special colors))
             (regular-colors (alist-get 'colors colors))
             (regular-color-values (cl-loop for (_key . value)
                                            in regular-colors
                                            collect value))
             (cannonical-colors (cl-pairlis color-names regular-color-values)))
        ;; unofficial comment color (always used as such)
        (cl-pushnew (cons 'comment (nth 8 regular-color-values)) special-colors)
        (append special-colors cannonical-colors))
    (error nil)))

;; Color helper functions, shamelessly *borrowed* from solarized
(defun ewal--color-name-to-rgb (color)
  "Retrieves the hex string represented the named COLOR (e.g. \"red\")."
  (cl-loop with div = (float (car (tty-color-standard-values "#ffffff")))
           for x in (tty-color-standard-values (downcase color))
           collect (/ x div)))

(defun ewal--color-blend (color1 color2 alpha)
  "Blend COLOR1 and COLOR2 (hex strings) together by a coefficient ALPHA.
\(a float between 0 and 1\)"
  (when (and color1 color2)
    (cond ((and color1 color2 (symbolp color1) (symbolp color2))
           (ewal--color-blend (ewal--get-color color1 0)
                              (ewal--get-color color2 0) alpha))

          ((or (listp color1) (listp color2))
           (cl-loop for x in color1
                    when (if (listp color2) (pop color2) color2)
                    collect (ewal--color-blend x it alpha)))

          ((and (string-prefix-p "#" color1) (string-prefix-p "#" color2))
           (apply (lambda (r g b) (format "#%02x%02x%02x"
                                          (* r 255) (* g 255) (* b 255)))
                  (cl-loop for it    in (ewal--color-name-to-rgb color1)
                           for other in (ewal--color-name-to-rgb color2)
                           collect (+ (* alpha it) (* other (- 1 alpha))))))

          (t color1))))

(defun ewal--color-darken (color alpha)
  "Darken a COLOR \(a hexidecimal string\) by a coefficient ALPHA.
\(a float between 0 and 1\)."
  (cond ((and color (symbolp color))
         (ewal--color-darken (ewal--get-color color 0) alpha))
        ((listp color)
         (cl-loop for c in color collect (ewal--color-darken c alpha)))
        (t
         (ewal--color-blend color "#000000" (- 1 alpha)))))

(defun ewal--color-lighten (color alpha)
  "Brighten a COLOR (a hexidecimal string) by a coefficient ALPHA.
\(a float between 0 and 1\)."
  (cond ((and color (symbolp color))
         (ewal--color-lighten (ewal--get-color color 0) alpha))
        ((listp color)
         (cl-loop for c in color collect (ewal--color-lighten c alpha)))
        (t
         (ewal--color-blend color "#FFFFFF" (- 1 alpha)))))

(defun ewal--extend-base-color (color num-shades shade-percent-difference)
  "Extend \(darken \(-\) or lighten \(+\)\) COLOR.
Do so by 2 * NUM-SHADES \(NUM-SHADES lighter, and NUM-SHADES
darker\), in increments of SHADE-PERCENT-DIFFERENCE percentage
points.  Return list of extended colors."
  (let ((darker-colors
         (cl-loop for i from num-shades downto 1 by 1
                  collect (ewal--color-darken
                          color (/ (* i shade-percent-difference)
                                    (float 100)))))
        (lighter-colors
         (cl-loop for i from 1 upto num-shades by 1
                  collect (ewal--color-lighten
                            color (/ (* i shade-percent-difference)
                                    (float 100))))))
    (append darker-colors (list color) lighter-colors)))

(defun ewal--extend-base-palette (num-shades shade-percent-difference
                                             &optional palette)
  "Use `ewal--extend-base-color' to extend entire base PALETTE.
which defaults to `ewal-base-palette' and returns an extended
palette alist intended to be stored in `ewal-extended-palette'.
Like `ewal--extend-base-color', extend \(darken \(-\) or lighten
\(+\)\) COLOR.  Do so by 2 * NUM-SHADES \(NUM-SHADES lighter, and
NUM-SHADES darker\), in increments of SHADE-PERCENT-DIFFERENCE
percentage points.  Return list of extended colors"
  (let ((palette (or palette ewal-base-palette)))
    (cl-loop for (key . value)
             in palette
             collect `(,key . ,(ewal--extend-base-color
                                value num-shades shade-percent-difference)))))

(defun ewal--get-color (color &optional shade tty palette)
  "Return SHADE of COLOR from current `ewal' PALETTE.
Choose color that is darker (-) or lightener (+) than COLOR
\(must be one of `ewal-ansi-color-name-symbols'\) by SHADE.
SHADE defaults to 0, returning original wal COLOR.  If SHADE
exceeds number of available shades, the darkest/lightest shade is
returned.  If TTY is t, return original, TTY compatible `wal'
color regardless od SHADE.  If `ewal-high-contrast-p' is t,
double SHADE."
  (let* ((palette (or palette ewal-extended-palette))
         ;; override, broad, narrow, fallback
         (tty (or tty
                  ewal-force-tty-colors-p
                  (and (daemonp) ewal-force-tty-colors-in-daemon-p)
                  (and (not (daemonp)) (not (display-graphic-p)))))
         (middle (/ (- (length (car ewal-extended-palette)) 1) 2))
         (shade (or (* (if ewal-high-contrast-p 2 1) shade) 0))
         (requested-color (nth (+ middle shade) (alist-get color palette)))
         (defined-requested-color (if requested-color
                                 requested-color
                               (car (last (alist-get color palette))))))
    (if tty
        (let ((color-name (symbol-name color)))
          (cond ((string= color-name "background") ewal-ansi-background-name)
                ((string= color-name "foreground") ewal-ansi-foreground-name)
                ((string= color-name "comment") "brightblack")
                ((string= color-name "cursor") ewal-cursor-color)
                (t color-name)))
      defined-requested-color)))

;;;###autoload
(defun ewal-load-ewal-colors (&optional force-reload vars funcs args)
  "Load all relevant `ewal' palettes and colors as environment variables.
Use TTY to determine whether to use TTY colors.  Reload
environment variables even if they have already been set if
FORCE-RELOAD is t. Always set `ewal-base-palette' and
`ewal-extended-palette'.  Set all extra variables specified in
ordered list VARS, using ordered list FUNCS, applying extra
arguments from nested, ordered list ARGS.  VARS, FUNCS, and ARGS
must be of the same length if passed at all."
    (when (or (null ewal-base-palette)
              (null ewal-extended-palette)
              force-reload)
      (setq ewal-base-palette (if ewal-use-built-in-always-p
                                  (ewal-load-wal-colors ewal-built-in-json-file)
                                (ewal-load-wal-colors))))
    ;; reload if failed and user wants you to
    (when (and (null ewal-base-palette) ewal-use-built-in-on-failure-p)
      (setq ewal-base-palette (ewal-load-wal-colors ewal-built-in-json-file)))
    (setq ewal-extended-palette (ewal--extend-base-palette 8 5))
    ;; let errors propagate if only some args are set
    (when (or vars funcs args)
      ;; accept atoms as well as lists
      (let ((vars (if (atom vars) (list vars) vars))
            (funcs (if (atom funcs) (list funcs) funcs))
            (args (if (atom args) (list args) args)))
        (cl-loop for var in vars
                 for func in funcs
                 for arglist in args
                 do (set var (if (atom arglist)
                                 (if (null arglist)
                                     (funcall func)
                                   (funcall func arglist))
                               (apply func arglist))))))
  ewal-extended-palette)


;;;###autoload
(defun ewal-get-color (color &optional shade)
  "Same as `ewal--get-color' but call `ewal-load-ewal-colors' first.
Pass COLOR and SHADE to `ewal--get-color'.  Meant to be called
from user config."
  (ewal-load-ewal-colors)
  (ewal--get-color color shade))

(provide 'ewal)

;;; ewal.el ends here
