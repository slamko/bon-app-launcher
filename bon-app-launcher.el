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


(setq bon-app-launcher--last-entry "")

(defmacro do-new-list (n new-list base-list body)
  (let (new-list)
	(dolist (n base-list new-list)
	  body)))

(defun bon-app-launcher--do-launch (command)
  (interactive (list (read-shell-command "$ " )))
  (setq bon-app-launcher--last-entry (car (split-string command " ")))
  (start-process-shell-command command nil command))

(defun get-bin-directories ()
  (let ((path-var (getenv "PATH")))
	(when path-var
	  (split-string path-var ":"))))

(defun all-bins (bin-directories)
  (let (all-bin-names)
	(dolist (bin-path bin-directories all-bin-names)
	  (if (file-exists-p bin-path)
		  (setq all-bin-names (append (directory-files bin-path nil "^[a-zA-Z0-9].*$") all-bin-names))))))

(defun unique-bins (bins)
  (let (unique-bin-names)
	(dolist (bin-path bins unique-bin-names)
	  (if (not (member bin-path unique-bin-names))
		  (setq unique-bin-names (append (list bin-path) unique-bin-names))))))

(defun list-binaries (bins)
  (reverse (mapcar 'file-name-nondirectory bins)))

(defun bon-app-launcher--list-binaries ()
  (interactive)
  (list-binaries
   (unique-bins
	 (all-bins
	  (get-bin-directories)))))

(defun list-binary-entries (binary-list)
  (let (binary-entries)
	(dolist (bin-path binary-list binary-entries)
	  (setq binary-entries (append binary-entries (cons (file-name-nondirectory bin-path) bin-path))))))

(defun bon-app-launcher ()
  (interactive)
  (if (fboundp 'ivy-read)
	  (let
		  ((applications (bon-app-launcher--list-binaries)))
		(ivy-read "Run application: " applications
				  :action #'bon-app-launcher--do-launch
				  :caller 'bon-app-launcher
				  :require-match nil
				  :preselect bon-app-launcher--last-entry))
	(bon-app-launcher--do-launch (read-shell-command "$ " ))))
