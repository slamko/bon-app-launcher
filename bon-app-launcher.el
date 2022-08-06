;; bon-app-launcher.el - Dmenu-like app launcher for EXWM.

;; Copyright (C) 2022 Viacheslav Chepelyk-Kozhin.
;;
;; This program is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later version.
;; This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
;; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;; See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.

(require 'f)

(defvar-local cache-dir "~/.cache/bon-app-launcher/")

(defvar-local last-entries-list nil)

(defun bal--append-cache-dir (file)
  (concat cache-dir file))

(setq bon-app-launcher--last-entry "")

(when (not (file-directory-p cache-dir))
      (make-directory cache-dir))

(when (not bon-app-launcher--last-entry)
  (setq bon-app-launcher--last-entry (f-read-text (bal--append-cache-dir "last"))))

(defmacro do-new-list (n new-list base-list body)
  (let (new-list)
	(dolist (n base-list new-list)
	  body)))

(defun read-file-to-list (file)
  (with-current-buffer
    (find-file-noselect file)
    (split-string
     (save-restriction
       (widen)
       (buffer-substring-no-properties
        (point-min)
        (point-max))) "\n" t)))

(defun bon-app-launcher--do-launch (command)
  (interactive (list (read-shell-command "$ " )))
  (setq bon-app-launcher--last-entry (car (split-string command " ")))
  (with-temp-file (bal--append-cache-dir "last") (insert bon-app-launcher--last-entry))
  (when (not (member command last-entries-list))
    (setq last-entries-list (push command last-entries-list))
    (f-append-text (concat command "\n") 'utf-8 (bal--append-cache-dir "all")))
  (start-process-shell-command bon-app-launcher--last-entry nil command))

(defun get-bin-directories ()
  (let ((path-var (getenv "PATH")))
	(when path-var
	  (split-string path-var ":"))))

(defun all-bins (bin-directories)
  (when (not last-entries-list)
    (setq last-entries-list (read-file-to-list (bal--append-cache-dir "all"))))
  (let (all-bin-names)
	(dolist (bin-path bin-directories all-bin-names)
	  (if (file-exists-p bin-path)
		  (setq all-bin-names (append (directory-files bin-path t "^[a-zA-Z0-9].*$") all-bin-names))))))

(defun unique-bins (bins)
  (let (unique-bin-names)
	(dolist (bin-path bins unique-bin-names)
	  (let ((file-name-bin (file-name-nondirectory bin-path)))
		(if
            (and
             (not (member file-name-bin unique-bin-names))
             (not (member file-name-bin last-entries-list))
             (file-executable-p bin-path) (not (file-directory-p bin-path)))
			(setq unique-bin-names (append (list file-name-bin) unique-bin-names)))))))

(defun list-binaries (bins)
  (append last-entries-list (reverse (mapcar 'file-name-nondirectory bins))))

(defun bon-app-launcher--list-binaries (&optional bin-path)
  (interactive)
  (list-binaries
   (unique-bins
	 (all-bins
	  (if bin-path (list bin-path) (get-bin-directories))))))

(defun list-binary-entries (binary-list)
  (let (binary-entries)
	(dolist (bin-path binary-list binary-entries)
	  (setq binary-entries (append binary-entries (cons (file-name-nondirectory bin-path) bin-path))))))

(defun bon-app-launcher (&optional bin-path)
  (interactive (list
				(read-directory-name "Binaries location path: " "/" "/usr/bin" t "usr/bin")))
  (if (fboundp 'ivy-read)
	  (let
		  ((applications (bon-app-launcher--list-binaries bin-path)))
		(ivy-read "Run application: " applications
				  :action #'bon-app-launcher--do-launch
				  :caller 'bon-app-launcher
				  :require-match nil
				  :preselect bon-app-launcher--last-entry))
	(bon-app-launcher--do-launch (read-shell-command "$ " ))))

(defun bon-app-launcher-usr-bin ()
  (interactive)
  (bon-app-launcher "/usr/bin"))

(provide 'bon-app-launcher)
