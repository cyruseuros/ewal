;;; ewal-evil-cursors.el --- `ewal'-colored evil cursor for Emacs and Spacemacs -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Uros Perisic

;; Author: Uros Perisic
;; URL: https://gitlab.com/jjzmajic/ewal
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
(declare-function #'spaceline-highlight-face-default "ext:noop")

(defvar ewal-evil-cursors-spacemacs-colors nil
  "`spacemacs-evil-cursors' compatible colors.
Extracted from current `ewal' palette.")

(defvar ewal-evil-cursors-emacs-colors nil
  "Vanilla Emacs Evil compatible colors.
Extracted from current `ewal' palette, and stored in a plist for
easy application.")

(defvar ewal-evil-cursors-obey-evil-p t
  "Whether to respect evil settings.
I.e. call insert state hybrid state if insert bindings are
disabled.")

(defvar ewal-evil-cursors-evil-state-faces
  '((normal . ewal-evil-cursors-normal-state)
    (insert . ewal-evil-cursors-insert-state)
    (emacs . ewal-evil-cursors-emacs-state)
    (hybrid . ewal-evil-cursors-hybrid-state)
    (replace . ewal-evil-cursors-replace-state)
    (visual . ewal-evil-cursors-visual-state)
    (motion . ewal-evil-cursors-motion-state)
    (iedit . ewal-evil-cursors-iedit-state)
    (iedit-insert . ewal-evil-cursors-iedit-state))
  "Association list mapping evil states to their corresponding highlight faces.
Is used by ‘ewal-evil-cursors-highlight-face-evil-state’.")

(defgroup ewal-evil-cursors nil
  "Ewal evil faces.
Originally indented to be used in spaceline for state indication,
but might be useful otherwise"
  :group 'faces)

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
    (evil-insert-state-cursor (,(ewal--get-color
                                 (if (and ewal-evil-cursors-obey-evil-p
                                          (bound-and-true-p evil-disable-insert-state-bindings))
                                     'blue 'green) 0) (bar . 2)))
    (evil-emacs-state-cursor (,(ewal--get-color 'blue 0) box))
    (evil-hybrid-state-cursor (,(ewal--get-color 'blue 0) (bar . 2)))
    (evil-evilified-state-cursor (,(ewal--get-color 'red 0) box))
    (evil-visual-state-cursor (,(ewal--get-color 'white -4) (hbar . 2)))
    (evil-motion-state-cursor (,(ewal--get-color ewal-primary-accent-color 0) box))
    (evil-replace-state-cursor (,(ewal--get-color 'red -4) (hbar . 2)))
    (evil-lisp-state-cursor (,(ewal--get-color 'magenta 4) box))
    (evil-iedit-state-cursor (,(ewal--get-color 'magenta -4) box))
    (evil-iedit-insert-state-cursor (,(ewal--get-color 'magenta -4) (bar . 2)))))


(defun ewal-evil-cursors--generate-evil-faces ()
  "Define evil faces.
Later to be used in `ewal-evil-cursors-highlight-face-evil-state'."
  (defvar dyn-color)
  (defvar dyn-state)
  (let ((face-string "ewal-evil-cursors-%s-state")
        (doc-string "Ewal evil %s state face."))
    (cl-loop for (key . value) in (or ewal-evil-cursors-emacs-colors (ewal-evil-cursors--generate-emacs-colors))
             ;; only check single string
             ;; iedit-insert is the same color anyway
             as dyn-state = (cadr (split-string (symbol-name key) "-"))
             as dyn-color = (caar value) do
             (eval `(defface ,(intern (format face-string dyn-state))
                      `((t (:background ,dyn-color
                            :foreground ,(ewal--get-color 'background -3)
                            :inherit 'mode-line)))
                      ,(format doc-string dyn-state)
                      :group 'spaceline)))))

(ewal-evil-cursors--generate-evil-faces)

;;;###autoload
(defun ewal-evil-cursors-highlight-face-evil-state ()
  "Set the highlight face depending on the evil state.
Set `spaceline-highlight-face-func' to
`ewal-evil-cursors-highlight-face-evil-state' to use this. Could
be useful elsewhere too."
  (if (bound-and-true-p evil-local-mode)
      (let* ((state (if (eq 'operator evil-state) evil-previous-state evil-state))
             (face (assq state ewal-evil-cursors-evil-state-faces)))
        (if face (cdr face) (spaceline-highlight-face-default)))
    (spaceline-highlight-face-default)))

;;;###autoload
(cl-defun ewal-evil-cursors-get-spacemacs-colors
    (&key apply force-reload)
  "Get `spacemacs-evil-cursors' colors.
If APPLY is t, set relevant environment variable for the user.
Reload `ewal' environment variables before returning colors even
if they have already been computed if FORCE-RELOAD is t. TTY
defaults to return value of `ewal--use-tty-colors-p'. If TTY is
t, use TTY colors."
  (ewal-load-ewal-colors force-reload 'ewal-evil-cursors-spacemacs-colors
                         #'ewal-evil-cursors--generate-spacemacs-colors
                         nil)
  (if apply
      (if (boundp 'spacemacs/add-evil-cursor)
          (when (functionp 'spacemacs/add-evil-cursor)
            (cl-loop for (state color shape) in ewal-evil-cursors-spacemacs-colors
                     do (spacemacs/add-evil-cursor state color shape)))
        (if (boundp 'spacemacs-evil-cursors)
            (cl-loop for cursor in ewal-evil-cursors-spacemacs-colors
                     do (add-to-list spacemacs-evil-cursors cursor))
          (setq spacemacs-evil-cursors ewal-evil-cursors-spacemacs-colors)))
    ewal-evil-cursors-spacemacs-colors))

;;;###autoload
(cl-defun ewal-evil-cursors-get-emacs-colors
    (&key apply force-reload)
  "Get vanilla Emacs Evil cursor colors.
If APPLY is t, set relevant environment variables for the user.
Reload `ewal' environment variables before returning colors even
if they have already been computed if FORCE-RELOAD is t. TTY
defaults to return value of `ewal--use-tty-colors-p'. If TTY is
t, use TTY colors."
  (ewal-load-ewal-colors force-reload 'ewal-evil-cursors-emacs-colors
                         #'ewal-evil-cursors--generate-emacs-colors
                         nil)
  (if apply
      (cl-loop for (key . value)
               in ewal-evil-cursors-emacs-colors
               do (set key (car value)))
    ewal-evil-cursors-emacs-colors))

(provide 'ewal-evil-cursors)
;;; ewal-evil-cursors.el ends here
