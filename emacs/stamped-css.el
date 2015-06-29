;; stamp .css files
;;
;; ex:
;; ```
;; /* Filename: chat_app.css
;;  * Timestamp: 2015.01.13-12:10:19 (last modified)
;;  * Copyright: mycopy  
;;  * Author(s): me <me@domain.com>
;;  */
;; ```
;;
;; stamp is **only** added when,
;;  - buffer file is an erl file
;;  - buffer file is in from src/ of the active project
;;  - st-author or st-copy are defined for the active project
;;
;; when the file is saved,
;;  - if no stamp exists in the buffer file, 
;;    - one is added to the top of that file when it is saved
;;  - if stamp exists in the buffer file
;;    - the the timstamp is updated to the current time when it is saved
;;  - if either st-author or st-copy is undefined
;;    - the undefined is not included in the created stamp 

(defvar *STAMPED-css-timestamp-spec* 
  "%Y.%m.%d-%H:%M:%S")
(defvar *STAMPED-css-timestamp-re* 
  "^ *.*Timestamp:\[a-Z ]*\\(\[0-9\]+.\[0-9\]+.\[0-9\]+\\)\.*")

(defvar *STAMPED-css-timestamp-str* 
  " * Timestamp: :timestamp (last modified)")
(defvar *STAMPED-css-filename-str* 
  "/* Filename: :filename  ")
(defvar *STAMPED-css-copyright-str* 
  " * Copyright: :copyright  ")
(defvar *STAMPED-css-author-str* 
  " * Author(s): :author  ")
(defvar *STAMPED-css-end-str* 
  " */")

(defun stamped-css-get-time ()
  "get the formatted time usable for a timestamp"
  (format-time-string *STAMPED-css-timestamp-spec*))

(defun stamped-css-get-filename (&optional file-name) (interactive)
  "get full formatted filename line for the stamp"
  (let ((name (if file-name file-name buffer-file-name)))
    (if name
        (let ((filename (file-name-nondirectory name)))
          (replace-regexp-in-string 
           ":filename" filename *STAMPED-css-filename-str*))
      (warn "[!!!] stamped: invalid filename"))))

(defun stamped-css-get-timestamp () (interactive)
  "get full formatted timestamp line for the stamp"
  (replace-regexp-in-string 
   ":timestamp" (stamped-css-get-time) *STAMPED-css-timestamp-str*))

(defun stamped-css-get-author (&optional author) (interactive)
  "get full formatted author line for the stamp"
  (replace-regexp-in-string 
   ":author" (or author (stamped-proj-get-st-author)) *STAMPED-css-author-str*))

(defun stamped-css-get-copyright (&optional copy) (interactive)
  "get full formatted copyright line for the stamp"
  (replace-regexp-in-string 
   ":copyright" (or copy (stamped-proj-get-st-copy)) *STAMPED-css-copyright-str*))

(defun stamped-css-full () (interactive)
  "get full stamp at top of buffer file"
  (let ((copy   (stamped-proj-get-st-copy))
        (author (stamped-proj-get-st-author)))
    (concat 
     (stamped-css-get-filename) "\n"
     (stamped-css-get-timestamp) "\n"
     (if copy   (concat (stamped-css-get-copyright) "\n") "") 
     (if author (concat (stamped-css-get-author)    "\n") "")
     *STAMPED-css-end-str*  "\n")))

(defun stamped-css-action () (interactive)
  "if the stamp timestamp exists in the buffer file update it, else add a new
stamp to the top of the file."
  (let ((regexp *STAMPED-css-timestamp-re*)
        (timestamp (stamped-css-get-timestamp))
        (time (stamped-css-get-time))
        (old-point (point)))
    (beginning-of-buffer) 
    (if (re-search-forward regexp nil t)
        (if (equal (match-string 1) time)
            (goto-char old-point)
          (print timestamp)
          (replace-match timestamp)
          (goto-char old-point))
      (insert (stamped-css-full)))))

(defun stamped-css-is-css-file? (filename)
  (interactive)
  (and filename
       (or (equal (file-name-extension filename) "less")
           (equal (file-name-extension filename) "css"))))

(defun stamped-css-action-try ()
  (interactive)
  "if the buffer file is stampable then add stamp"
  (when (and (stamped-css-is-css-file? buffer-file-name)
             (stamped-proj-is-stampable-file? buffer-file-name))
    (stamped-css-action)))

(add-hook 'write-file-hooks '(lambda () (stamped-css-action-try) nil))
