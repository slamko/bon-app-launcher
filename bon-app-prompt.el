
(defun bon-app-do-launch (command)
  (interactive (list (read-shell-command $ )))
  (start-process-shell-command command nil command))

(defun list-binaries ()
  (interactive)
  (let (binary-list)
	(dolist (bin-path (directory-files "/usr/bin/" nil "^[a-zA-Z0-9]*$") binary-list)
	  (setq binary-list (append binary-list (list bin-path))))))

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
