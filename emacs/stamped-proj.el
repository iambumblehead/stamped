(defvar *STAMPED-PROJ* "")

(defvar *STAMPED-PROJ-config* (make-hash-table :test 'equal))
(defvar *STAMPED-PROJ-root-dir* (make-hash-table :test 'equal))
(defvar *STAMPED-PROJ-log-dir* (make-hash-table :test 'equal))
(defvar *STAMPED-PROJ-st-author* (make-hash-table :test 'equal))
(defvar *STAMPED-PROJ-st-copy* (make-hash-table :test 'equal))

;; abstracts some boilerplace used for the get and set values to hash tables.
;; values for a project are defined to a key named to that projects name.
;;
;; ex.,
;;   (gethash *STAMPED-PROJ-log-dir* "sampleproject")
;;   (gethash *STAMPED-PROJ-log-dir* "otherproject")
(defun stamped-proj-hget (htable &optional key)
  (gethash (stamped-proj-get-name key) htable))
(defun stamped-proj-hset (htable val &optional key)
  (puthash (stamped-proj-get-name key) val htable))

(defun stamped-proj-get-root-dir (&optional n) (interactive)
  (expand-file-name (stamped-proj-hget *STAMPED-PROJ-root-dir* n)))
(defun stamped-proj-get-root-dir-join (childpath &optional name)
  (f-join (stamped-proj-get-root-dir name) childpath))
(defun stamped-proj-get-root-dirpath (&optional n) (interactive)
  (let ((dir (stamped-proj-hget *STAMPED-PROJ-root-dir* n)))
    (when dir (stamped-proj-get-root-dir-join dir n))))
(defun stamped-proj-set-root-dir (val &optional n) (interactive)
  (stamped-proj-hset *STAMPED-PROJ-root-dir* val n))

(defun stamped-proj-get-log-dir (&optional n) (interactive)
  (stamped-proj-hget *STAMPED-PROJ-log-dir* n))
(defun stamped-proj-set-log-dir (val &optional n) (interactive)
  (stamped-proj-hset *STAMPED-PROJ-log-dir* val n))

(defun stamped-proj-get-config (&optional n) (interactive)
  (stamped-proj-hget *STAMPED-PROJ-config* n))
(defun stamped-proj-get-configpath (&optional n) (interactive)
  (let ((config (stamped-proj-hget *STAMPED-PROJ-config* n)))
    (when config (stamped-proj-get-root-dir-join config n))))
(defun stamped-proj-set-config (val &optional n) (interactive)
  (stamped-proj-hset *STAMPED-PROJ-config* val n))

(defun stamped-proj-get-start-cmd (&optional n) (interactive)
  (stamped-proj-hget *STAMPED-PROJ-start-cmd* n))
(defun stamped-proj-set-start-cmd (val &optional n) (interactive)
  (stamped-proj-hset *STAMPED-PROJ-start-cmd* val n))

(defun stamped-proj-get-st-author (&optional n) (interactive)
  (stamped-proj-hget *STAMPED-PROJ-st-author* n))
(defun stamped-proj-set-st-author (val &optional n) (interactive)
  (stamped-proj-hset *STAMPED-PROJ-st-author* val n))

(defun stamped-proj-get-st-copy (&optional n) (interactive)
  (stamped-proj-hget *STAMPED-PROJ-st-copy* n))
(defun stamped-proj-set-st-copy (val &optional n) (interactive)
  (stamped-proj-hset *STAMPED-PROJ-st-copy* val n))

;;;
;;; set / get project names
;;;
(defun stamped-proj-get-names () (interactive)
  "print available project names"
  (let ((keys-list '()))
    (maphash '(lambda (key value) 
                (add-to-list 'keys-list key)) *STAMPED-PROJ-root-dir*)
    (mapconcat 'identity keys-list ", ")))

(defun stamped-proj-is-name-valid? (&optional name)
  (when name (and (equal (stringp name) t)
                  (not (equal "" name)))))

(defun stamped-proj-get-name (&optional name)
  (or (if (stamped-proj-is-name-valid? name) name *STAMPED-PROJ*) 
      (warn "[!!!] stamped: project name is invalid")))

(defun stamped-proj-set-name (name) (interactive "sstamped project name: ")
  (setq *STAMPED-PROJ* name))

(defun stamped-proj-get-nodename (&optional name) (interactive)
  "node name -name as an alphanumeric string"
  (replace-regexp-in-string "\\." "" (stamped-proj-get-name name)))

;;;
;;; set entire project 
;;;
(defmacro stamped-alist-key (alist key)
  `(cdr (assoc ,key ,alist)))

(defun stamped-proj-set-opts (name opts) (interactive)
  "set all values specific to the 'name' project"
  (stamped-proj-set-name          name)
  (stamped-proj-set-root-dir      (stamped-alist-key opts 'root-dir)  name)
  (when (assoc 'log-dir opts)  
    (stamped-proj-set-log-dir       (stamped-alist-key opts 'log-dir)   name))
  (when (assoc 'st-author opts)
    (stamped-proj-set-st-author     (stamped-alist-key opts 'st-author) name))
  (when (assoc 'st-copy opts)
    (stamped-proj-set-st-copy       (stamped-alist-key opts 'st-copy)   name)))

(defun stamped-proj-show-opts (&optional name) (interactive)
  (print 
   (concat
    " " (stamped-proj-get-name name) ","
    "\n proj-root-dir  : " (stamped-proj-get-root-dir      name)
    "\n proj-log-dir   : " (stamped-proj-get-log-dir       name)
    "\n proj-st-author : " (stamped-proj-get-st-author     name)
    "\n proj-st-copy   : " (stamped-proj-get-st-copy       name)
    "\n")))

;;;
;;; proj-specific convenience functions
;;;
(defun stamped-proj-is-src-file? (filepath &optional name)
  (interactive)
  "'true' indicates a filepath in the project src/"
  (let ((full-filepath (expand-file-name filepath))
        (full-srcpath (stamped-proj-get-root-dirpath name)))
    (equal (string-match full-srcpath full-filepath) 0)))

(defun stamped-proj-is-src-ext-file? (filepath &optional name) (interactive)
  (when (stamped-util-is-src-ext-file? filepath)
    (stamped-proj-is-src-file? filepath name)))

(defun stamped-proj-is-stampable-file? (filepath &optional name) (interactive)
       "true if filepath in proj src/ and proj includes st-author or st-copy"
       (print "is it a src-ext file goddamn??")
       (print (stamped-proj-is-src-ext-file? filepath))       
  (and (stamped-proj-is-src-ext-file? filepath)
       (or (stamped-proj-get-st-author)
           (stamped-proj-get-st-copy))))

(defun stamped-proj-node-start (&optional name)
  (interactive "snpm start for which project? (leave empty for 'active' project): ")  
  "start the active project"
  (let ((compile-dir (stamped-proj-get-root-dir (if name name *STAMPED-PROJ*)))
        (persist-dir default-directory))
    (cd compile-dir)
    (setq compilation-scroll-output t)
    (compile "npm start")
    (cd persist-dir)))

(defun stamped-proj-tail-log (&optional name)
  (interactive "stail log for which project? (leave empty for 'active' project): ")
  (let ((logfile (stamped-proj-get-log-dir))
        (buffer  (create-file-buffer (concat (stamped-proj-get-name name) "-log"))))
    (switch-to-buffer-other-window buffer)
    (find-file logfile)
    (auto-revert-tail-mode)
    (setq compilation-scroll-output t)))

(defun stamped-proj-node-test (&optional name)
  (interactive "snpm test for which project? (leave empty for 'active' project): ")  
  "unit test the focus site"
  (let ((compile-dir (stamped-proj-get-root-dir (if name name *STAMPED-PROJ*)))
        (persist-dir default-directory))
    (cd compile-dir)
    (setq compilation-scroll-output t)
    (compile "npm test")
    (cd persist-dir)))
