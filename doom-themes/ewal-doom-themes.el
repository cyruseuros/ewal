;;; ewal-doom-themes.el --- Dread the colors of darkness -*- lexical-binding: t; -*-

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

;; An `ewal'-based theme library, to be used when working with
;; `doom-themes' as a base.

;;; Code:
(require 'ewal)
(require 'doom-themes)

(defun ewal-doom-themes-get-color (color &optional shade shade-percent-difference)
  "Return COLOR of SHADE with SHADE-PERCENT-DIFFERENCE.
Return one accurate hex color, and 2 tty approximation colors to
fit `def-doom-theme'."
  (let ((color (ewal-get-color color shade shade-percent-difference))
        (tty-color (let ((ewal-force-tty-colors-p t))
                     (ewal-get-color color shade shade-percent-difference))))
    `(,color ,tty-color ,tty-color)))

;;;###autoload
(when (and (boundp 'custom-theme-load-path)
           load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory
                (file-name-directory load-file-name))))


(provide 'ewal-doom-themes)
;;; ewal-doom-themes.el ends here
