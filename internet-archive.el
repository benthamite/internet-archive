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

(defgroup internet-archive ()
  "Download books from the Internet Archive."
  :group 'emacs)

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

(defcustom internet-archive-adobe-digital-editions-directory
  (expand-file-name "~/Documents/Digital Editions/")
  "Path to the Adobe Digital Editions directory."
  :type 'directory
  :group 'internet-archive)

(defcustom internet-archive-downloads-directory
  (expand-file-name "~/Downloads/")
  "Path to the directory to which the PDFs will be downloaded."
  :type 'directory
  :group 'internet-archive)

(defcustom internet-archive-wget
  (executable-find "wget")
  "Path to the `wget' executable."
  :type 'file
  :group 'internet-archive)

(defcustom internet-archive-calibredb
  (executable-find "calibredb")
  "Path to the `calibredb' executable."
  :type 'file
  :group 'internet-archive)

(defvar internet-archive-directory-watcher nil
  "Descriptor for the directory watch process.")

(defvar internet-archive-acsm-file
  (file-name-concat temporary-file-directory "URLLink.acsm")
  "Path to the ACSM file downloaded from Internet Archive.")

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

(defun internet-archive-download (url)
  "Download Internet Archive PDF in URL."
  (interactive (list (read-string "URL: " (current-kill 0))))
  (if-let ((id (replace-regexp-in-string internet-archive-id-regexp "\\2" url)))
      (let ((url (concat internet-archive-prefix id internet-archive-suffix)))
	(internet-archive--download-async url)
	(internet-archive--watch-directory))
    (user-error "No ID found in URL")))

(defun internet-archive--download-async (url)
  "Download ACSM file from Internet Archive URL asynchronously."
  (unless (executable-find internet-archive-wget)
    (user-error "Please install `wget' (https://www.gnu.org/software/wget/)"))
  (save-window-excursion
    (let ((shell-command-buffer-name-async "*internet-archive-download*"))
      (async-shell-command
       (format
	"'%s' --load-cookies='%s' '%s' -O '%4$s'; open --background '%4$s'"
	internet-archive-wget internet-archive-cookies-file url internet-archive-acsm-file)))))

(defun internet-archive--watch-directory ()
  "Watch Adobe Digital Editions directory for new files."
  (setq internet-archive-directory-watcher
	(file-notify-add-watch internet-archive-adobe-digital-editions-directory
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
      (internet-archive-remove-drm file))))

(defun internet-archive-calibre-add-file (pdf)
  "Add PDF to Calibre and return its ID."
  (let ((output (shell-command-to-string (format "'%s' add '%s'"
						 internet-archive-calibredb pdf))))
    (string-match "Added book ids: \\([[:digit:]]+\\)" output)
    (match-string 1 output)))

(defun internet-archive-calibre-export-file (id)
  "Export Calibre file with ID to `internet-archive-downloads-directory'."
  (shell-command (format "'%s' export --dont-save-cover --dont-write-opf --single-dir --to-dir %s %s"
			 internet-archive-calibredb internet-archive-downloads-directory id)))

(defun internet-archive-calibre-remove-file (id)
  "Remove from Calibre file with ID."
  (shell-command (format "'%s' remove %s" internet-archive-calibredb id))
  (kill-buffer "*Shell Command Output*"))

(defun internet-archive-remove-drm (pdf)
  "Remove DRM from Adobe Digital Editions PDF."
  (unless (executable-find internet-archive-calibredb)
    (user-error "Please install `calibredb' (https://calibre-ebook.com/) and set`internet-archive-calibredb'"))
  (let* ((id (internet-archive-calibre-add-file pdf)))
    (internet-archive-calibre-export-file id)
    (internet-archive-calibre-remove-file id)
    (message "PDF file downloaded to %s."internet-archive-downloads-directory)))

(provide 'internet-archive)
;;; internet-archive.el ends here
