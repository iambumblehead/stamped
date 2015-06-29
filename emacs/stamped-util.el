(defun stamped-util-filter (condp lst)
  (delq nil (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

(defun stamped-util-is-js-file? (filename) (interactive)
       (and filename (equal (file-name-extension filename) "js")))

(defun stamped-util-is-css-file? (filename) (interactive)
       (and filename (equal (file-name-extension filename) "css")))

(defun stamped-util-is-less-file? (filename) (interactive)
       (and filename (equal (file-name-extension filename) "less")))

(defun stamped-util-is-src-ext-file? (filename) (interactive)
       (or (stamped-util-is-js-file? filename)
           (stamped-util-is-css-file? filename)
           (stamped-util-is-less-file? filename)))
