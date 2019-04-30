;;; ewal-spacemacs-theme.el --- An `ewal'-based theme -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Uros Perisic

;; Author: Uros Perisic
;; URL: https://gitlab.com/jjzmajic/ewal.el
;;
;; Version: 0.1
;; Keywords: faces
;; Package-Requires: ((emacs "25") (ewal "0.1") (spacemacs-theme "0.1"))

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
(require 'spacemacs-common)

(defvar ewal-spacemacs-theme-colors nil
  "`spacemacs-theme' compatible colors.
Extracted from current `ewal' theme.")

(defun ewal-spacemacs-theme--generate-colors (&optional borders)
  "Make theme colorscheme from theme palettes.
If TTY is t, colorscheme is reduced to basic  supported
colors. If BORDERS is t use `ewal-primary-accent-color' for
borders. I prefer to remove them."
  (let* ((primary-accent-color ewal-primary-accent-color)
         (secondary-accent-color ewal-secondary-accent-color)
         (border-color (if borders primary-accent-color 'background))
         (theme-colors
          `((act1          . ,(ewal--get-color 'background -3))
            (act2          . ,(ewal--get-color primary-accent-color 0))
            (base          . ,(ewal--get-color 'foreground 0))
            (base-dim      . ,(ewal--get-color 'foreground -4))
            (bg1           . ,(ewal--get-color 'background 0))
            ;; used to highlight current line
            (bg2           . ,(ewal--get-color 'background -2))
            (bg3           . ,(ewal--get-color 'background -3))
            (bg4           . ,(ewal--get-color 'background -4))
            (border        . ,(ewal--get-color border-color 0))
            (cblk          . ,(ewal--get-color 'foreground -3))
            (cblk-bg       . ,(ewal--get-color 'background -3))
            (cblk-ln       . ,(ewal--get-color primary-accent-color 4))
            (cblk-ln-bg    . ,(ewal--get-color primary-accent-color -4))
            (cursor        . ,(ewal--get-color 'cursor 0))
            (const         . ,(ewal--get-color primary-accent-color 4))
            (comment       . ,(ewal--get-color 'comment 0))
            (comment-bg    . ,(ewal--get-color 'background 0))
            (comp          . ,(ewal--get-color secondary-accent-color 0))
            (err           . ,(ewal--get-color 'red 0))
            (func          . ,(ewal--get-color primary-accent-color 0))
            (head1         . ,(ewal--get-color primary-accent-color 0))
            (head1-bg      . ,(ewal--get-color 'background -3))
            (head2         . ,(ewal--get-color secondary-accent-color 0))
            (head2-bg      . ,(ewal--get-color 'background -3))
            (head3         . ,(ewal--get-color 'cyan 0))
            (head3-bg      . ,(ewal--get-color 'background -3))
            (head4         . ,(ewal--get-color 'yellow 0))
            (head4-bg      . ,(ewal--get-color 'background -3))
            (highlight     . ,(ewal--get-color 'background 4))
            (highlight-dim . ,(ewal--get-color 'background 2))
            (keyword       . ,(ewal--get-color secondary-accent-color 0))
            (lnum          . ,(ewal--get-color 'comment 0))
            (mat           . ,(ewal--get-color 'green 0))
            (meta          . ,(ewal--get-color 'yellow 4))
            (str           . ,(ewal--get-color 'cyan 0))
            (suc           . ,(ewal--get-color 'green 4))
            (ttip          . ,(ewal--get-color 'comment 0))
            ;; same as `bg2'
            (ttip-sl       . ,(ewal--get-color 'background -2))
            (ttip-bg       . ,(ewal--get-color 'background 0))
            (type          . ,(ewal--get-color 'red 2))
            (var           . ,(ewal--get-color secondary-accent-color 4))
            (war           . ,(ewal--get-color 'red 4))
            ;; colors
            (aqua          . ,(ewal--get-color 'cyan 0))
            (aqua-bg       . ,(ewal--get-color 'cyan -3))
            (green         . ,(ewal--get-color 'green 0))
            (green-bg      . ,(ewal--get-color 'green -3))
            (green-bg-s    . ,(ewal--get-color 'green -4))
            ;; literally the same as `aqua' in web development
            (cyan          . ,(ewal--get-color 'cyan 0))
            (red           . ,(ewal--get-color 'red 0))
            (red-bg        . ,(ewal--get-color 'red -3))
            (red-bg-s      . ,(ewal--get-color 'red -4))
            (blue          . ,(ewal--get-color 'blue 0))
            (blue-bg       . ,(ewal--get-color 'blue -3))
            (blue-bg-s     . ,(ewal--get-color 'blue -4))
            (magenta       . ,(ewal--get-color 'magenta 0))
            (yellow        . ,(ewal--get-color 'yellow 0))
            (yellow-bg     . ,(ewal--get-color 'yellow -3)))))
         theme-colors))

;;;###autoload
(cl-defun ewal-spacemacs-theme-get-colors
    (&key apply force-reload borders)
  "Get `spacemacs-theme' colors.
For usage see: <https://github.com/nashamri/spacemacs-theme>. If
APPLY is t, set relevant environment variable for the user.
Reload `ewal' environment variables before returning colors even
if they have already been computed if FORCE-RELOAD is t. TTY
defaults to return value of `ewal--use-tty-colors-p'. if TTY is
t, use TTY colors."
  (ewal-load-ewal-colors force-reload
                         'ewal-spacemacs-theme-colors
                         #'ewal-spacemacs-theme--generate-colors
                         borders)
  (if apply
      (setq spacemacs-theme-custom-colors ewal-spacemacs-theme-colors)
    ewal-spacemacs-theme-colors))

(provide-theme 'ewal-spacemacs-theme)
;;; ewal-spacemacs-theme.el ends here
