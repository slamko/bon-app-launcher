
(defmacro do-new-list (n new-list base-list body)
  (let (new-list)
	(dolist (n base-list new-list)
	  body)))

(defun bon-app-do-launch (command)
  (interactive (list (read-shell-command $ )))
  (start-process-shell-command command nil command))

(defun get-bin-directories ()
  (let ((path-var (getenv "PATH")))
	(when path-var
	  (split-string path-var ":"))))

(defun all-bins ()
  (interactive)
  (let (all-bin-names)
	(dolist (bin-path (get-bin-directories) all-bin-names)
	  (if (file-exists-p bin-path)
		  (setq all-bin-names (append (directory-files bin-path nil "^[a-zA-Z0-9].*$") all-bin-names))))))

(defun list-binaries ()
  (interactive)
  (let (binary-list)
	(dolist (bin-path (all-bins) binary-list)
	  (setq binary-list (append (list (file-name-nondirectory bin-path)) binary-list)))))

(defun list-binary-entries (binary-list)
  (interactive)
  (let (binary-entries)
	(dolist (bin-path binary-list binary-entries)
	  (setq binary-entries (append binary-entries (cons (file-name-nondirectory bin-path) bin-path))))))

(defun bon-app-launcher (&optional arg)
  (interactive "P")
  (ivy-read "Run application: " (list-binaries)
            :action #'bon-app-do-launch
            :caller 'bon-app-launcher))


(global-set-key (kbd "s-p") 'bon-app-launcher)
