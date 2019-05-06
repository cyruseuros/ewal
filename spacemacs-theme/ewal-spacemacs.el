;;; ewal-spacemacs.el --- An `ewal'-based theme -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Uros Perisic

;; Author: Uros Perisic
;; URL: https://gitlab.com/jjzmajic/ewal.el
;;
;; Version: 0.1
;; Keywords: faces
;; Package-Requires: ((emacs "25") (ewal "0.1") (spacemacs-theme 0.1))

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

;; This is an `ewal'-based theme, using `spacemacs-theme'
;; <https://github.com/nashamri/spacemacs-theme> as its base. Emulate this file
;; if you want to contribute other `ewal' customized themes.

;;; Code:
(require 'ewal "./ewal.el")

(defvar ewal-spacemacs-colors nil
  "`spacemacs-theme' compatible colors.
Extracted from current `ewal' theme.")

(defun ewal-spacemacs--double (condition num)
  "Double NUM if CONDITION is t.
Meant for use in `ewal-spacemacs--generate-colors' when a high
contrast theme is desired."
  (if condition (* 2 num) num))

(defun ewal-spacemacs--generate-colors (&optional borders high-contrast)
  "Make theme colorscheme from theme palettes.
If TTY is t, colorscheme is reduced to basic  supported
colors. If BORDERS is t use `ewal-primary-accent-color' for
borders. I prefer to remove them."
  (let* ((primary-accent-color ewal-primary-accent-color)
         (secondary-accent-color ewal-secondary-accent-color)
         (border-color (if borders primary-accent-color 'background))
         (theme-colors
          `((act1          . ,(ewal--get-color 'background (ewal-spacemacs--double high-contrast -3)))
            (act2          . ,(ewal--get-color primary-accent-color (ewal-spacemacs--double high-contrast 0)))
            (base          . ,(ewal--get-color 'foreground (ewal-spacemacs--double high-contrast 0)))
            (base-dim      . ,(ewal--get-color 'foreground (ewal-spacemacs--double high-contrast -4)))
            (bg1           . ,(ewal--get-color 'background (ewal-spacemacs--double high-contrast 0)))
            ;; used to highlight current line
            (bg2           . ,(ewal--get-color 'background (ewal-spacemacs--double high-contrast -2)))
            (bg3           . ,(ewal--get-color 'background (ewal-spacemacs--double high-contrast -3)))
            (bg4           . ,(ewal--get-color 'background (ewal-spacemacs--double high-contrast -4)))
            (border        . ,(ewal--get-color border-color (ewal-spacemacs--double high-contrast 0)))
            (cblk          . ,(ewal--get-color 'foreground (ewal-spacemacs--double high-contrast -3)))
            (cblk-bg       . ,(ewal--get-color 'background (ewal-spacemacs--double high-contrast -3)))
            (cblk-ln       . ,(ewal--get-color primary-accent-color (ewal-spacemacs--double high-contrast 4)))
            (cblk-ln-bg    . ,(ewal--get-color primary-accent-color (ewal-spacemacs--double high-contrast -4)))
            (cursor        . ,(ewal--get-color 'cursor (ewal-spacemacs--double high-contrast 0)))
            (const         . ,(ewal--get-color primary-accent-color (ewal-spacemacs--double high-contrast 4)))
            (comment       . ,(ewal--get-color 'comment (ewal-spacemacs--double high-contrast 0)))
            (comment-bg    . ,(ewal--get-color 'background (ewal-spacemacs--double high-contrast 0)))
            (comp          . ,(ewal--get-color secondary-accent-color (ewal-spacemacs--double high-contrast 0)))
            (err           . ,(ewal--get-color 'red (ewal-spacemacs--double high-contrast 0)))
            (func          . ,(ewal--get-color primary-accent-color (ewal-spacemacs--double high-contrast 0)))
            (head1         . ,(ewal--get-color primary-accent-color (ewal-spacemacs--double high-contrast 0)))
            (head1-bg      . ,(ewal--get-color 'background (ewal-spacemacs--double high-contrast -3)))
            (head2         . ,(ewal--get-color secondary-accent-color (ewal-spacemacs--double high-contrast 0)))
            (head2-bg      . ,(ewal--get-color 'background (ewal-spacemacs--double high-contrast -3)))
            (head3         . ,(ewal--get-color 'cyan (ewal-spacemacs--double high-contrast 0)))
            (head3-bg      . ,(ewal--get-color 'background (ewal-spacemacs--double high-contrast -3)))
            (head4         . ,(ewal--get-color 'yellow (ewal-spacemacs--double high-contrast 0)))
            (head4-bg      . ,(ewal--get-color 'background (ewal-spacemacs--double high-contrast -3)))
            (highlight     . ,(ewal--get-color 'background (ewal-spacemacs--double high-contrast 4)))
            (highlight-dim . ,(ewal--get-color 'background (ewal-spacemacs--double high-contrast 2)))
            (keyword       . ,(ewal--get-color secondary-accent-color (ewal-spacemacs--double high-contrast 0)))
            (lnum          . ,(ewal--get-color 'comment (ewal-spacemacs--double high-contrast 0)))
            (mat           . ,(ewal--get-color 'green (ewal-spacemacs--double high-contrast 0)))
            (meta          . ,(ewal--get-color 'yellow (ewal-spacemacs--double high-contrast 4)))
            (str           . ,(ewal--get-color 'cyan (ewal-spacemacs--double high-contrast 0)))
            (suc           . ,(ewal--get-color 'green (ewal-spacemacs--double high-contrast 4)))
            (ttip          . ,(ewal--get-color 'comment (ewal-spacemacs--double high-contrast 0)))
            ;; same as `bg2'
            (ttip-sl       . ,(ewal--get-color 'background (ewal-spacemacs--double high-contrast -2)))
            (ttip-bg       . ,(ewal--get-color 'background (ewal-spacemacs--double high-contrast 0)))
            (type          . ,(ewal--get-color 'red (ewal-spacemacs--double high-contrast 2)))
            (var           . ,(ewal--get-color secondary-accent-color (ewal-spacemacs--double high-contrast 4)))
            (war           . ,(ewal--get-color 'red (ewal-spacemacs--double high-contrast 4)))
            ;; colors
            (aqua          . ,(ewal--get-color 'cyan (ewal-spacemacs--double high-contrast 0)))
            (aqua-bg       . ,(ewal--get-color 'cyan (ewal-spacemacs--double high-contrast -3)))
            (green         . ,(ewal--get-color 'green (ewal-spacemacs--double high-contrast 0)))
            (green-bg      . ,(ewal--get-color 'green (ewal-spacemacs--double high-contrast -3)))
            (green-bg-s    . ,(ewal--get-color 'green (ewal-spacemacs--double high-contrast -4)))
            ;; literally the same as `aqua' in web development
            (cyan          . ,(ewal--get-color 'cyan (ewal-spacemacs--double high-contrast 0)))
            (red           . ,(ewal--get-color 'red (ewal-spacemacs--double high-contrast 0)))
            (red-bg        . ,(ewal--get-color 'red (ewal-spacemacs--double high-contrast -3)))
            (red-bg-s      . ,(ewal--get-color 'red (ewal-spacemacs--double high-contrast -4)))
            (blue          . ,(ewal--get-color 'blue (ewal-spacemacs--double high-contrast 0)))
            (blue-bg       . ,(ewal--get-color 'blue (ewal-spacemacs--double high-contrast -3)))
            (blue-bg-s     . ,(ewal--get-color 'blue (ewal-spacemacs--double high-contrast -4)))
            (magenta       . ,(ewal--get-color 'magenta (ewal-spacemacs--double high-contrast 0)))
            (yellow        . ,(ewal--get-color 'yellow (ewal-spacemacs--double high-contrast 0)))
            (yellow-bg     . ,(ewal--get-color 'yellow (ewal-spacemacs--double high-contrast -3))))))
         theme-colors))

;;;###autoload
(cl-defun ewal-spacemacs-get-colors
    (&key apply force-reload borders high-contrast)
  "Get `spacemacs-theme' colors.
For usage see: <https://github.com/nashamri/spacemacs-theme>. If
APPLY is t, set relevant environment variable for the user.
Reload `ewal' environment variables before returning colors even
if they have already been computed if FORCE-RELOAD is t. TTY
defaults to return value of `ewal--use-tty-colors-p'. if TTY is
t, use TTY colors."
  (ewal-load-ewal-colors force-reload
                         'ewal-spacemacs-colors
                         #'ewal-spacemacs--generate-colors
                         `((,borders ,high-contrast)))
  (if apply
      (setq spacemacs-theme-custom-colors ewal-spacemacs-colors)
    ewal-spacemacs-colors))

(provide 'ewal-spacemacs)

;;; ewal-spacemacs.el ends here
