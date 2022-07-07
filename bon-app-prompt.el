
(defmacro do-new-list (n new-list base-list body)
  (let (new-list)
	(dolist (n base-list new-list)
	  body)))

(defun bon-app-do-launch (command)
  (interactive (list (read-shell-command "$" )))
  (start-process-shell-command command nil command))

(defun get-bin-directories ()
  (let ((path-var (getenv "PATH")))
	(when path-var
	  (split-string path-var ":"))))

(defun all-bins (bin-directories)
  (interactive)
  (let (all-bin-names)
	(dolist (bin-path bin-directories all-bin-names)
	  (if (file-exists-p bin-path)
		  (setq all-bin-names (append (directory-files bin-path nil "^[a-zA-Z0-9].*$") all-bin-names))))))

(defun unique-bins (bins)
  (interactive)
  (let (unique-bin-names)
	(dolist (bin-path bins unique-bin-names)
	  (if (not (member bin-path unique-bin-names))
		  (setq unique-bin-names (append (list bin-path) unique-bin-names))))))

(defun list-binaries (bins)
  (interactive)
  (reverse (mapcar 'file-name-nondirectory bins)))
  
(defun list-binary-entries (binary-list)
  (interactive)
  (let (binary-entries)
	(dolist (bin-path binary-list binary-entries)
	  (setq binary-entries (append binary-entries (cons (file-name-nondirectory bin-path) bin-path))))))

(defun bon-app-launcher (&optional arg)
  (interactive "P")
  (let
	((applications (list-binaries (unique-bins (all-bins (get-bin-directories))))))
	(ivy-read "Run application: " applications
            :action #'bon-app-do-launch
            :caller 'bon-app-launcher)))


(global-set-key (kbd "s-p") 'bon-app-launcher)
