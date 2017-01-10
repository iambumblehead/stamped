;; stamp .js and .css files
;;
;; ex:
;; ```
;; // Filename: chat_app.js
;; // Timestamp: 2015.01.13-12:10:19 (last modified)
;; // Copyright: mycopy  
;; // Author(s): me <me@domain.com>  
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

(defvar *STAMPED-js-timestamp-spec* 
  "%Y.%m.%d-%H:%M:%S")
(defvar *STAMPED-js-timestamp-re* 
  "^/.*Timestamp:\[a-Z ]*\\(\[0-9\]+.\[0-9\]+.\[0-9\]+\\)\.*")

(defvar *STAMPED-js-timestamp-str* 
  "// Timestamp: :timestamp (last modified)")
(defvar *STAMPED-js-filename-str* 
  "// Filename: :filename  ")
(defvar *STAMPED-js-copyright-str* 
  "// Copyright: :copyright  ")
(defvar *STAMPED-js-author-str* 
  "// Author(s): :author  ")

(defun stamped-js-get-time ()
  "get the formatted time usable for a timestamp"
  (format-time-string *STAMPED-js-timestamp-spec*))

(defun stamped-js-get-filename (&optional file-name) (interactive)
  "get full formatted filename line for the stamp"
  (let ((name (if file-name file-name buffer-file-name)))
    (if name
        (let ((filename (file-name-nondirectory name)))
          (replace-regexp-in-string 
           ":filename" filename *STAMPED-js-filename-str*))
      (warn "[!!!] stamped: invalid filename"))))

(defun stamped-js-get-timestamp () (interactive)
  "get full formatted timestamp line for the stamp"
  (replace-regexp-in-string 
   ":timestamp" (stamped-js-get-time) *STAMPED-js-timestamp-str*))

(defun stamped-js-get-author (&optional author) (interactive)
  "get full formatted author line for the stamp"
  (replace-regexp-in-string 
   ":author" (or author (stamped-proj-get-st-author)) *STAMPED-js-author-str*))

(defun stamped-js-get-copyright (&optional copy) (interactive)
  "get full formatted copyright line for the stamp"
  (replace-regexp-in-string 
   ":copyright" (or copy (stamped-proj-get-st-copy)) *STAMPED-js-copyright-str*))

(defun stamped-js-full () (interactive)
  "get full stamp at top of buffer file"
  (let ((copy   (stamped-proj-get-st-copy))
        (author (stamped-proj-get-st-author)))
    (concat 
     (stamped-js-get-filename) "\n"
     (stamped-js-get-timestamp) "\n"
     (if copy   (concat (stamped-js-get-copyright) "\n") "") 
     (if author (concat (stamped-js-get-author)    "\n") ""))))

(defun stamped-js-action () (interactive)
  "if the stamp timestamp exists in the buffer file update it, else add a new
stamp to the top of the file."
  (let ((regexp *STAMPED-js-timestamp-re*)
        (timestamp (stamped-js-get-timestamp))
        (time (stamped-js-get-time))
        (old-point (point)))
    (beginning-of-buffer) 
    (if (re-search-forward regexp nil t)
        (if (equal (match-string 1) time)
            (goto-char old-point)
          (print timestamp)
          (replace-match timestamp)
          (goto-char old-point))
      (insert (stamped-js-full)))))

(defun stamped-js-is-js-file? (filename) (interactive)
       (and filename (equal (file-name-extension filename) "js")))

(defun stamped-js-action-try ()
  (interactive)
  "if the buffer file is stampable then add stamp"
  (when (and (stamped-js-is-js-file? buffer-file-name)
             (stamped-proj-is-stampable-file? buffer-file-name))
    (stamped-js-action)
    (stamped-proj-node-updatefile buffer-file-name)))

(add-hook 'write-file-hooks '(lambda () (stamped-js-action-try) nil))
