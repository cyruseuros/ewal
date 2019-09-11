;;; ewal-doom-themes.el --- Dread the colors of darkness -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Uros Perisic

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

;; An `ewal'-based theme, created using `doom-themes' as its base. Emulate this
;; file if you want to contribute other `ewal' customized themes.

;;; Code:
(require 'ewal)
(require 'doom-themes)

(defun ewal-doom-themes-get-color (color &optional shade shade-percent-difference)
  "Return COLOR of SHADE with SHADE-PERCENT-DIFFERENCE repeated 3 times.
This fits `def-doom-theme' and works because `ewal' automatically
deals with tty contexts."
  (let ((color (ewal-get-color color shade shade-percent-difference)))
    `(,color ,color ,color)))

;;;###autoload
(when (and (boundp 'custom-theme-load-path)
           load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory
                (file-name-directory load-file-name))))

(provide 'ewal-doom-themes)

;;; ewal-doom-themes.el ends here
