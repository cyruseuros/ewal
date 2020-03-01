;;; ewal-doom-vibrant-theme.el --- Dread the vibrancy of darkness -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Uros Perisic

;; Author: Uros Perisic
;; URL: https://gitlab.com/jjzmajic/ewal
;;
;; Version: 0.1
;; Keywords: faces
;; Package-Requires: ((emacs "25") (ewal "0.1") (doom-themes "0.1"))

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

;; An `ewal'-based theme, created using `doom-vibrant' as its base.

;;; Code:
(require 'ewal-doom-themes)

(defgroup ewal-doom-vibrant-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom ewal-doom-vibrant-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'ewal-doom-vibrant-theme
  :type 'boolean)

(defcustom ewal-doom-vibrant-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'ewal-doom-vibrant-theme
  :type 'boolean)

(defcustom ewal-doom-vibrant-comment-bg ewal-doom-vibrant-brighter-comments
  "If non-nil, comments will have a subtle, darker background."
  :group 'ewal-doom-vibrant-theme
  :type 'boolean)

(defcustom ewal-doom-vibrant-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'ewal-doom-vibrant-theme
  :type '(choice integer boolean))

(ewal-load-colors)

;; HACK: fixes bytecode overflow
(defvar ewal-doom-one-hack
  (ewal-doom-themes-get-color 'background 0))

(def-doom-theme ewal-doom-vibrant
  "A dark theme based off of doom-one with more vibrant `ewal' colors."

  ((bg         (ewal-doom-themes-get-color 'background  0))
   (bg-alt     (ewal-doom-themes-get-color 'background -3))
   (base0      (ewal-doom-themes-get-color 'background -5))
   (base1      (ewal-doom-themes-get-color 'background -4))
   (base2      (ewal-doom-themes-get-color 'background -2))
   (base3      (ewal-doom-themes-get-color 'background -1))
   (base4      (ewal-doom-themes-get-color 'background +1))
   (base5      (ewal-doom-themes-get-color 'comment     0))
   (base6      (ewal-doom-themes-get-color 'background +4))
   (base7      (ewal-doom-themes-get-color 'background +5))
   (base8      (ewal-doom-themes-get-color 'foreground +1))
   (fg         (ewal-doom-themes-get-color 'foreground  0))
   (fg-alt     (ewal-doom-themes-get-color 'foreground -1))

   (grey       base4)
   (red        (ewal-doom-themes-get-color 'red      -2))
   (orange     (ewal-doom-themes-get-color 'red       0))
   (green      (ewal-doom-themes-get-color 'green    -2))
   (teal       (ewal-doom-themes-get-color 'green     0))
   (yellow     (ewal-doom-themes-get-color 'yellow   -2))
   (blue       (ewal-doom-themes-get-color 'blue      0))
   (dark-blue  (ewal-doom-themes-get-color 'blue     -2))
   (magenta    (ewal-doom-themes-get-color 'magenta   0))
   (violet     (ewal-doom-themes-get-color 'magenta  -2))
   (cyan       (ewal-doom-themes-get-color 'cyan      0))
   (dark-cyan  (ewal-doom-themes-get-color 'cyan     -2))

   ;; face categories
   (highlight      blue)
   (vertical-bar   base0)
   (selection      dark-blue)
   (builtin        magenta)
   (comments       (if ewal-doom-vibrant-brighter-comments dark-cyan base5))
   (doc-comments   (if ewal-doom-vibrant-brighter-comments (doom-lighten dark-cyan 0.15) (doom-lighten base4 0.3)))
   (constants      violet)
   (functions      cyan)
   (keywords       blue)
   (methods        violet)
   (operators      magenta)
   (type           yellow)
   (strings        green)
   (variables      (doom-lighten magenta 0.3))
   (numbers        orange)
   (region         "#3d4451")
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    yellow)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (hidden-alt `(,(car bg-alt) "black" "black"))
   (-modeline-pad
    (when ewal-doom-vibrant-padded-modeline
      (if (integerp ewal-doom-vibrant-padded-modeline) ewal-doom-vibrant-padded-modeline 4)))

   (modeline-fg     (ewal-get-color 'foreground 0))
   (modeline-fg-alt (doom-blend blue grey (if ewal-doom-vibrant-brighter-modeline 0.4 0.08)))

   (modeline-bg
    (if ewal-doom-vibrant-brighter-modeline
        `(,(car bg-alt) ,@(cdr base1))
      `(,(car bg-alt) ,@(cdr base0))))
   (modeline-bg-l
    (if ewal-doom-vibrant-brighter-modeline
        modeline-bg
      `(,(doom-darken (car bg) 0.15) ,@(cdr base1))))
   (modeline-bg-inactive   (doom-darken bg 0.25))
   (modeline-bg-inactive-l `(,(doom-darken (car bg-alt) 0.2) ,@(cdr base0))))


  ;; --- extra faces ------------------------
  ((elscreen-tab-other-screen-face :background base8 :foreground base0)

   (font-lock-comment-face
    :foreground comments
    :background (if ewal-doom-vibrant-comment-bg (doom-darken bg-alt 0.095)))
   (font-lock-doc-face
    :inherit 'font-lock-comment-face
    :foreground doc-comments)

   ((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground blue :bold bold)

   (doom-modeline-bar :background (if ewal-doom-vibrant-brighter-modeline modeline-bg highlight))
   (doom-modeline-buffer-path :foreground (if ewal-doom-vibrant-brighter-modeline base8 blue) :bold bold)

   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis
    :foreground (if ewal-doom-vibrant-brighter-modeline base8 highlight))

   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))

   (whitespace-empty :background base2)

   ;; --- major-mode faces -------------------
   ;; css-mode / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)

   ;; markdown-mode
   (markdown-header-face :inherit 'bold :foreground red)

   ;; org-mode
   (org-hide :foreground hidden)
   (solaire-org-hide-face :foreground hidden-alt))


  ;; --- extra variables --------------------
  ;; ()
  )

(provide-theme 'ewal-doom-vibrant)

;;; ewal-doom-vibrant-theme.el ends here
