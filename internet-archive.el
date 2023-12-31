;;; internet-archive.el --- Download books from the Internet Archive -*- lexical-binding: t -*-

;; Copyright (C) 2023

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/internet-archive
;; Version: 0.1

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Download books from the Internet Archive.

;;; Code:

(require 'filenotify)

;;;; Variables

;;;;; User options

;;;;; Paths

(defgroup internet-archive ()
  "Download books from the Internet Archive."
  :group 'files)

(defcustom internet-archive-cookies-file
  (expand-file-name "~/.config/cookies.txt")
  "Path to the Chrome cookies file."
  :type 'file
  :group 'internet-archive)

(defcustom internet-archive-calibre-directory
  (expand-file-name "~/Calibre Library/")
  "Path to the `Calibre Library' directory.
This is the directory where Calibre stores the book
files (https://manual.calibre-ebook.com/faq.html#id31)."
  :type 'directory
  :group 'internet-archive)

(defcustom internet-archive-ade-directory
  (expand-file-name "~/Documents/Digital Editions/")
  "Path to the Adobe Digital Editions directory."
  :type 'directory
  :group 'internet-archive)

(defcustom internet-archive-downloads-directory
  (expand-file-name "~/Downloads/")
  "Path to the directory to which the PDFs will be downloaded."
  :type 'directory
  :group 'internet-archive)

(defcustom internet-archive-wget-file
  (executable-find "wget")
  "Path to the `wget' executable."
  :type 'file
  :group 'internet-archive)

(defcustom internet-archive-calibredb-file
  (executable-find "calibredb")
  "Path to the `calibredb' executable."
  :type 'file
  :group 'internet-archive)

;;;;; Behavior

(defcustom internet-archive-ade-close-when-done nil
  "Whether to close Adobe Digital Editions immediately after the PDF downlaods.
Note that this will kill all instances of the application."
  :type 'boolean
  :group 'internet-archive)

(defcustom internet-archive-ade-open-in-background nil
  "Whether to open Adobe Digital Editions in the background.
Note that apparently the application will start downloading the file only when
it is in the foreground."
  :type 'boolean
  :group 'internet-archive)

;;;;; Internal variables

(defvar internet-archive-directory-watcher nil
  "Descriptor for the directory watch process.")

(defvar internet-archive-acsm-file
  (file-name-concat temporary-file-directory "URLLink.acsm")
  "Path to the ACSM file downloaded from Internet Archive.")

(defvar internet-archive-protocol-hook nil
  "Hook run immediately after `internet-archive-protocol' is called.")

(defconst internet-archive-prefix
  "https://archive.org/services/loans/loan/?action=media_url&identifier="
  "Prefix for Internet Archive URLs.")

(defconst internet-archive-suffix
  "&format=pdf&redirect=1"
  "Suffix for Internet Archive URLs.")

(defconst internet-archive-id-regexp
  "\\(http.*?details/\\)\\([_[:alnum:]]*\\)\\(.*\\)"
  "Regular expression for capturing the book ID in an Internet Archive URL.")

;;;; Functions

(defun internet-archive-download (&optional url)
  "Download Internet Archive PDF in URL."
  (interactive)
  (if-let ((url (or url (read-string "URL: " (current-kill 0))))
	   (id (replace-regexp-in-string internet-archive-id-regexp "\\2" url)))
      (let ((url (concat internet-archive-prefix id internet-archive-suffix)))
	(internet-archive-download-acsm url)
	(internet-archive--watch-directory))
    (user-error "No ID found in URL")))

(defun internet-archive-download-acsm (url)
  "Download ACSM file from Internet Archive URL asynchronously."
  (unless (executable-find internet-archive-wget-file)
    (user-error "Please install `wget' (https://www.gnu.org/software/wget/)"))
  (save-window-excursion
    (let ((shell-command-buffer-name-async "*internet-archive-download*"))
      (async-shell-command
       (format (concat "'%s' --load-cookies='%s' '%s' -O '%4$s'; open '%4$s'"
		       (when internet-archive-ade-open-in-background " --background"))
	       internet-archive-wget-file internet-archive-cookies-file url internet-archive-acsm-file)))))

;;;;; Directory watching

(defun internet-archive--watch-directory ()
  "Watch Adobe Digital Editions directory for new files."
  (setq internet-archive-directory-watcher
	(file-notify-add-watch internet-archive-ade-directory
			       '(change)
			       #'internet-archive--directory-watch-callback)))

(defun internet-archive--unwatch-directory ()
  "Stop watching Adobe Digital Editions directory for new files."
  (file-notify-rm-watch internet-archive-directory-watcher))

(defun internet-archive--directory-watch-callback (event)
  "Process the file-notify EVENT."
  (let ((event-type (nth 1 event))
	(file (nth 2 event)))
    (when (eq event-type 'created)
      (internet-archive--unwatch-directory)
      (internet-archive-remove-drm file)
      (internet-archive-ade-close-when-done))))

(defun internet-archive-ade-close-when-done ()
  "Close Adobe Digital Editions immediately after the PDF downlaods."
  (when internet-archive-ade-close-when-done
    (pcase system-type
      ((or 'darwin 'gnu/linux)
       (shell-command "pkill 'Adobe Digital Editions'"))
      ('windows-nt
       (shell-command "taskkill /IM \"DigitalEditions.exe\"")))))

;;;;; Calibre

(defun internet-archive-calibre-add-file (pdf)
  "Add PDF to Calibre and return its ID."
  (let ((output (shell-command-to-string (format "'%s' add '%s'"
						 internet-archive-calibredb-file pdf))))
    (string-match "Added book ids: \\([[:digit:]]+\\)" output)
    (match-string 1 output)))

(defun internet-archive-calibre-export-file (id)
  "Export Calibre file with ID to `internet-archive-downloads-directory'."
  (shell-command (format "'%s' export --dont-save-cover --dont-write-opf --single-dir --to-dir %s %s"
			 internet-archive-calibredb-file internet-archive-downloads-directory id)))

(defun internet-archive-calibre-remove-file (id)
  "Remove from Calibre file with ID."
  (shell-command (format "'%s' remove %s" internet-archive-calibredb-file id))
  (kill-buffer "*Shell Command Output*"))

(defun internet-archive-remove-drm (pdf)
  "Remove DRM from Adobe Digital Editions PDF."
  (unless (executable-find internet-archive-calibredb-file)
    (user-error "Please install `calibredb' (https://calibre-ebook.com/) and set`internet-archive-calibredb-file'"))
  (let* ((id (internet-archive-calibre-add-file pdf)))
    (internet-archive-calibre-export-file id)
    (internet-archive-calibre-remove-file id)
    (message "PDF file downloaded to %s." internet-archive-downloads-directory)))

;;;;; org-protocol

(defun internet-archive-protocol (alist)
  "Process the `org-protocol' ALIST."
  (run-hooks 'internet-archive-protocol-hook)
  (internet-archive-download (plist-get alist :url)))

(when (boundp 'org-protocol-protocol-alist)
  (push '("internet-archive"
	  :protocol "internet-archive"
	  :function internet-archive-protocol)
	org-protocol-protocol-alist))

(provide 'internet-archive)
;;; internet-archive.el ends here
