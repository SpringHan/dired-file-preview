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

(defcustom dired-file-preview-hide-details-p nil
  "If the dired defaults to hide details."
  :type 'boolean
  :group 'dired)

(defcustom dired-file-preview-literal-p t
  "If preview file without highlight."
  :type 'boolean
  :group 'dired)

(defcustom dired-file-preview-info-storage nil
  "Storage for the file preview info."
  :type 'list
  :group 'dired)

;;;###autoload
(define-minor-mode dired-file-preview-mode
  "File preview mode for dired."
  :group 'dired
  (if dired-file-preview-mode
      (progn
        (if dired-hide-details-mode
            (setq-local dired-file-preview-hide-details-p t)
          (dired-hide-details-mode t)
          (setq-local dired-file-preview-hide-details-p nil))
        (dired-file-preview--setup)
        (add-hook 'dired-before-readin-hook #'dired-file-preview--convert-infos nil t)
        (advice-add 'kill-buffer :before #'dired-file-preview-kill-buffer))
    (cancel-timer dired-file-preview-sync-file-timer)
    (delete-window dired-file-preview-window)
    (kill-buffer dired-file-preview-preview-buffer)
    (setq-local dired-file-preview-sync-file-timer nil
                dired-file-preview-window nil
                dired-file-preview-preview-buffer nil)
    (unless dired-file-preview-hide-details-p
      (dired-hide-details-mode -1))
    (remove-hook 'dired-before-readin-hook #'dired-file-preview--convert-infos t)
    (advice-remove 'kill-buffer #'dired-file-preview-kill-buffer)))

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
      (other-window 1)
      (setq-local dired-file-preview-sync-file-timer
                  (run-with-timer
                   0 0.2 #'dired-file-preview-sync-file)))))

(defun dired-file-preview-sync-file ()
  "Sync current file."
  (when (and (eq major-mode 'dired-mode)
             dired-file-preview-preview-buffer)
    (let ((current-file (ignore-errors (dired-get-filename)))
          file-mode)
      ;; TODO: Add check for image
      (when (and current-file
                 (not (file-directory-p current-file))
                 (not (eq current-file dired-file-preview-current-file)))
        (unless dired-file-preview-literal-p
          (setq file-mode (assoc-default current-file
                                         auto-mode-alist
                                         'string-match)))
        (with-current-buffer dired-file-preview-preview-buffer
          (when (eq major-mode 'image-mode)
            (text-mode))
          (setq-local buffer-read-only nil)
          (erase-buffer)
          (insert-file-contents current-file)
          (unless dired-file-preview-literal-p
            (eval
             `(progn
                (setq-local ,(intern (concat (symbol-name file-mode) "-hook"))
                            nil)
                (,file-mode))))
          (goto-char (point-min)))
        (setq-local dired-file-preview-current-file current-file)))))

(defun dired-file-preview--other-windows-p ()
  "To check if there're other windows in current tab."
  (let ((current-window (get-buffer-window)))
    (other-window 1)
    (prog1 (not (eq current-window (get-buffer-window)))
      (select-window current-window))))

(defun dired-file-preview--convert-infos ()
  "Convert current dired buffer infos into another one."
  (let ((last-buffer " **lose**")
        dired-file-name)
    (with-current-buffer last-buffer
      (setq dired-file-preview-info-storage
            `((dired-file-preview-preview-buffer . ,dired-file-preview-preview-buffer)
              (dired-file-preview-new-tab-p . ,dired-file-preview-new-tab-p)
              (dired-file-preview-window . ,dired-file-preview-window)
              (dired-file-preview-sync-file-timer . ,dired-file-preview-sync-file-timer)
              (dired-file-preview-hide-detials-p . ,dired-file-preview-hide-details-p))))
    (with-current-buffer (current-buffer)
      (setq-local dired-file-preview-mode t)
      (setq dired-file-name (buffer-name))
      (print dired-file-preview-info-storage)
      (when (and dired-file-preview-info-storage
                 (listp dired-file-preview-info-storage))
        (dolist (var dired-file-preview-info-storage)
          (eval `(setq-local ,(car var) ,(cdr var))))))
    (with-current-buffer dired-file-preview-preview-buffer
      (rename-buffer (format "*Dired-Preview:%s*" dired-file-name)))))

(defun dired-file-preview-kill-buffer (&optional buffer-or-name)
  "The function which will be called when kill dired buffer."
  (with-current-buffer buffer-or-name
    (when (eq major-mode 'dired)
      (dired-file-preview-mode -1))))

(provide 'dired-file-preview)

;;; dired-file-preview.el ends here
