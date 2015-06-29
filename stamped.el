(require 'f)

(let ((default-directory (file-name-directory load-file-name)))
  (load-file (file-truename "./emacs/stamped-proj.el"))
  (load-file (file-truename "./emacs/stamped-util.el"))
  (load-file (file-truename "./emacs/stamped-js.el"))  
  (load-file (file-truename "./emacs/stamped-css.el")))

(defun stamped-set-opts (opts)
  (interactive)
  (add-to-list 'opts '(root-dir  . "/"))
  (stamped-proj-set-opts "global" opts))

(defun stamped-disable ()
  (interactive)
  (stamped-proj-set-name ""))

(defun stamped-enable ()
  (interactive)
  (stamped-proj-set-name "global"))

(defun stamped-project (name)
  (interactive "sstamped project name: ")
  (setq *STAMPED-PROJ* name))
