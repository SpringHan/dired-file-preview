;;; dired-file-preview.el --- File preview support for dired -*- lexical-binding: t -*-

;; Author: SpringHan
;; Maintainer: SpringHan
;; Version: 1.0
;; Package-Requires: ()
;; Homepage: https://github.com/SpringHan/dired-file-preview
;; Keywords: 


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; File preview support for dired.

;;; Code:

(defcustom dired-file-preview-preview-buffer nil
  "The buffer name of preview buffer."
  :type 'string
  :group 'dired)

(defcustom dired-file-preview-new-tab-p nil
  "If current dired window and preview window are in a new tab."
  :type 'boolean
  :group 'dired)

(defcustom dired-file-preview-window nil
  "The preview window."
  :group 'dired)

(defcustom dired-file-preview-sync-file-timer nil
  "The timer to sync file."
  :type 'timer
  :group 'dired)

(defcustom dired-file-preview-current-file nil
  "Current preview file."
  :type 'string
  :group 'dired)

(define-minor-mode dired-file-preview-mode
  "File preview mode for dired."
  :group 'dired
  (if dired-file-preview-mode
      (progn
        (unless dired-hide-details-mode
          (dired-hide-details-mode t))
        (dired-file-preview--setup))
    (cancel-timer dired-file-preview-sync-file-timer)
    (delete-window dired-file-preview-window)
    (kill-buffer dired-file-preview-preview-buffer)
    (setq-local dired-file-preview-sync-file-timer nil
                dired-file-preview-window nil
                dired-file-preview-preview-buffer nil)))

(defun dired-file-preview--setup ()
  "File preview setup."
  (unless dired-file-preview-preview-buffer
    (let ((dired-buffer (current-buffer))
          (width (number-to-string (- (* 0.7 (frame-width)))))
          (buffer-name (format "*Dired-Preview:%s*" (buffer-name))))
      (when (dired-file-preview--other-windows-p)
        (setq-local dired-file-preview-new-tab-p t)
        (tab-bar-new-tab)
        (switch-to-buffer dired-buffer))
      (setq-local dired-file-preview-window
                  (split-window nil
                                (string-to-number
                                 (progn
                                   (string-match "\\(.*\\)\\.\\(.*\\)" width)
                                   (match-string 1 width)))
                                t)
                  dired-file-preview-preview-buffer buffer-name)
      (select-window dired-file-preview-window)
      (switch-to-buffer dired-file-preview-preview-buffer)
      (text-mode)
      (other-window 1)
      (setq-local dired-file-preview-sync-file-timer
                  0 0.5 #'dired-file-preview-sync-file))))

(defun dired-file-preview-sync-file ()
  "Sync current file."
  (when (and (eq major-mode 'dired-mode)
             dired-file-preview-preview-buffer)
    (let ((current-file (ignore-errors (dired-get-filename))))
      ;; TODO: Add check for image
      (when (and current-file
                 (not (eq current-file dired-file-preview-current-file)))
        (with-current-buffer dired-file-preview-preview-buffer
          (erase-buffer)
          (insert-file-contents current-file)
          (set-syntax-table
           (derived-mode-syntax-table-name (assoc-default current-file
                                                          auto-mode-alist
                                                          'string-match)))
          (goto-char (point-min)))))))

(defun dired-file-preview--other-windows-p ()
  "To check if there're other windows in current tab."
  (let ((current-window (get-buffer-window)))
    (other-window 1)
    (prog1 (not (eq current-window (get-buffer-window)))
      (select-window current-window))))

(provide 'dired-file-preview)

;;; dired-file-preview.el ends here
