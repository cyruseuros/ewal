(defun wal-theme--colors-approximate (colors)
  "Find which COLORS most resemble canonical `tty' colors.
These refer to, `(black red green yellow blue magenta cyan
white)'. Return them as an alist of color name symbols as keys
and hex color code strings as values. Not usable in production
given inaccuracy, but may be usable later."
  (let ((approximate-colors ()))
    (dolist (color colors approximate-colors)
      (let ((approximate-color-name (car (tty-color-approximate (tty-color-standard-values color)))))
        (if (null (assoc (make-symbol approximate-color-name) approximate-colors))
            (add-to-list 'approximate-colors
                         `(,(make-symbol approximate-color-name)
                           . ,color)))))))

(defun wal-theme--wal-cache-json-load (&optional json)
  "Read JSON as the most complete of the stored files."
  (let ((json (or json wal-theme-wal-cache-json))
        (colors (json-read-file wal-theme-wal-cache-json)))
    (let ((special-colors (alist-get 'special colors))
          (regular-colors (wal-theme--colors-approximate (cl-loop for
                                                                  (key . value)
                                                                  in
                                                                  (alist-get 'colors colors)
                                                                  collect
                                                                  value))))
      (append special-colors regular-colors))))
