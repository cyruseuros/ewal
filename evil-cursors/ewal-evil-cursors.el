;;; ewal-evil-cursors.el --- `ewal'-colored evil cursor for Emacs and Spacemacs -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Uros Perisic

;; Author: Uros Perisic
;; URL: https://gitlab.com/jjzmajic/ewal.el
;;
;; Version: 0.1
;; Keywords: faces
;; Package-Requires: ((emacs "25") (ewal "0.1"))

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

;; This is an `ewal'-based `evil' cursor colorscheme in both Spacemacs and
;; vanilla Emacs format.

;;; Code:
(require 'ewal "./ewal.el")

(defvar ewal-spacemacs-evil-cursors-colors nil
  "`spacemacs-evil-cursors' compatible colors.
Extracted from current `ewal' palette.")

(defvar ewal-emacs-evil-cursors-colors nil
  "Vanilla Emacs Evil compatible colors.
Extracted from current `ewal' palette, and stored in a plist for
easy application.")

(defun ewal-evil-cursors--generate-spacemacs-colors ()
  "Use wal colors to customize `spacemacs-evil-cursors'.
TTY specifies whether to use TTY or GUI colors."
  `(("normal" ,(ewal--get-color 'cursor 0) box)
    ("insert" ,(ewal--get-color 'green 0) (bar . 2))
    ("emacs" ,(ewal--get-color 'blue 0) box)
    ("hybrid" ,(ewal--get-color 'blue 0) (bar . 2))
    ("evilified" ,(ewal--get-color 'red 0) box)
    ("visual" ,(ewal--get-color 'white -4) (hbar . 2))
    ("motion" ,(ewal--get-color ewal-primary-accent-color 0) box)
    ("replace" ,(ewal--get-color 'red -4) (hbar . 2))
    ("lisp" ,(ewal--get-color 'magenta 4) box)
    ("iedit" ,(ewal--get-color 'magenta -4) box)
    ("iedit-insert" ,(ewal--get-color 'magenta -4) (bar . 2))))

(defun ewal-evil-cursors--generate-emacs-colors ()
  "Use wal colors to customize vanilla Emacs Evil cursor colors.
TTY specifies whether to use or GUI colors."
  `((evil-normal-state-cursor (,(ewal--get-color 'cursor 0) box))
    (evil-insert-state-cursor (,(ewal--get-color 'green 0) (bar . 2)))
    (evil-emacs-state-cursor (,(ewal--get-color 'blue 0) box))
    (evil-hybrid-state-cursor (,(ewal--get-color 'blue 0) (bar . 2)))
    (evil-evilified-state-cursor (,(ewal--get-color 'red 0) box))
    (evil-visual-state-cursor (,(ewal--get-color 'white -4) (hbar . 2)))
    (evil-motion-state-cursor (,(ewal--get-color ewal-primary-accent-color 0) box))
    (evil-replace-state-cursor (,(ewal--get-color 'red -4) (hbar . 2)))
    (evil-lisp-state-cursor (,(ewal--get-color 'magenta 4) box))
    (evil-iedit-state-cursor (,(ewal--get-color 'magenta -4) box))
    (evil-iedit-insert-state-cursor (,(ewal--get-color 'magenta -4) (bar . 2)))))

;;;###autoload
(cl-defun ewal-evil-cursors-get-spacemacs-colors
    (&key apply force-reload)
  "Get `spacemacs-evil-cursors' colors.
If APPLY is t, set relevant environment variable for the user.
Reload `ewal' environment variables before returning colors even
if they have already been computed if FORCE-RELOAD is t. TTY
defaults to return value of `ewal--use-tty-colors-p'. If TTY is
t, use TTY colors."
  (ewal-load-ewal-colors force-reload 'ewal-spacemacs-evil-cursors-colors
                         #'ewal-evil-cursors--generate-spacemacs-colors
                         nil)
  (if apply
      (setq spacemacs-evil-cursors ewal-spacemacs-evil-cursors-colors)
    ewal-spacemacs-evil-cursors-colors))

;;;###autoload
(cl-defun ewal-evil-cursors-get-emacs-colors
    (&key apply force-reload)
  "Get vanilla Emacs Evil cursor colors.
If APPLY is t, set relevant environment variables for the user.
Reload `ewal' environment variables before returning colors even
if they have already been computed if FORCE-RELOAD is t. TTY
defaults to return value of `ewal--use-tty-colors-p'. If TTY is
t, use TTY colors."
  (ewal-load-ewal-colors force-reload 'ewal-spacemacs-evil-cursors-colors
                         #'ewal-evil-cursors--generate-spacemacs-colors
                         nil)
  (if apply
      (cl-loop for (key . value)
               in ewal-emacs-evil-cursors-colors
               do (set key value))
    ewal-emacs-evil-cursors-colors))

(provide 'ewal-evil-cursors)
;;; ewal-evil-cursors.el ends here
