;;; dired-delight.el --- Color file names in dired  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Karthik Chikmagalur

;; Author: Karthik Chikmagalur <karthikchikmagalur@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: multimedia, files
;; URL: https://github.com/karthink/dired-delight

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Color file name display in Dired persistently.
;;
;; Turn on `dired-delight-mode' in a Dired buffer, then
;;
;; - Tag a file or all marked files with a color using `dired-delight'.
;; - Mark all files of a given color with `dired-delight-mark-color'.
;;
;; Customization:
;;
;; - Choose whether to color by relative or absolute file names with
;;   `dired-delight-use-relative-names'.
;; - Choose the display style (block or background) with
;;   `dired-delight-display'.
;; - Choose where your selections are stored with `dired-delight-file'.

;;; Code:
(require 'dired)
(require 'faces)
(require 'facemenu)
(eval-when-compile
  (require 'subr-x)
  (require 'cl-lib))

(defgroup dired-delight nil
  "Colorful files in Dired."
  :group 'files)

(defcustom dired-delight-file
  (cond
   ((and-let* (((equal system-type 'gnu/linux))
              (cache (getenv "XDG_CACHE_HOME")))
             (expand-file-name "dired-delight-data.el" cache)))
   (t (expand-file-name "dired-delight-data.el" user-emacs-directory)))
  "File used to persist Dired Delight data across Emacs sessions."
  :group 'dired-delight
  :type 'file)

(defcustom dired-delight-delay 0.02
  "Idle time before updating Dired Delight colors."
  :group 'dired-delight
  :type 'number)

(defcustom dired-delight-display 'block
  "Display style for Dired Delight colors.

This is a symbol, block or background.

block: Show a colored block next to the file name.
background: Highlight the filename."
  :group 'dired-delight
  :type '(choice
          (const :tag "Block" block)
          (const :tag "Background" background)))

(defcustom dired-delight-use-relative-names nil
  "Whether Dired Delight's Coloring should be based on the file's relative name.

By default files are identified by their absolute path."
  :group 'dired-delight
  :type 'boolean)

(defvar dired-delight--color-table
  (make-hash-table :test #'equal)
  "Table mapping files to colors.")

(defvar dired-delight--color-groups
  (make-hash-table :test #'equal)
  "Table mapping colors to files.")

(defvar dired-delight--timer nil
  "Fontification timer for Dired Delight.")

(defvar-local dired-delight--propertize
  (intern (concat "dired-delight--propertize-"
                  (symbol-name dired-delight-display)))
  "Function used for applying Delight colors.")

;;;###autoload
(define-minor-mode dired-delight-mode
  "Tag files with colors in Dired."
  :lighter " :D"
  :group 'dired-delight
  (cond
   ((not (derived-mode-p 'dired-mode))
    (when dired-delight-mode
      (dired-delight-mode -1))
    (message
     "Dired Delight only works in Dired buffers."))
   (dired-delight-mode
    (setq dired-delight--propertize
          (intern (concat "dired-delight--propertize-"
                          (symbol-name dired-delight-display))))
    (or (and (not (hash-table-empty-p dired-delight--color-table))
             (not (hash-table-empty-p dired-delight--color-groups)))
        (dired-delight--read))
    (add-hook 'kill-emacs-hook #'dired-delight--save)
    ;; (add-hook 'window-state-change-hook #'dired-delight--schedule nil t)
    (add-hook 'window-scroll-functions #'dired-delight--handle-scroll nil t)
    ;; Potential fix for #4: Apply colors only after `dired-mode-hook' runs
    (run-with-timer 0 nil
                    (lambda ()
                      (dired-delight--schedule (window-start) (window-end))))
    (jit-lock-register #'dired-delight--schedule))
   (t (dired-delight--clear)
      (dired-delight--save)
      ;; (remove-hook 'window-state-change-hook #'dired-delight--schedule t)
      (remove-hook 'window-scroll-functions #'dired-delight--handle-scroll t)
      (jit-lock-unregister #'dired-delight--schedule))))

(defun dired-delight (filenames color)
  "Add Delights (colors) to files in Dired.

FILENAMES is the list of filenames to color.

COLOR is the color to use, either a name (see
`list-colors-display') or a Hex code, like #11aaef.

You can mark files in Dired and color all of them."
  (interactive (list (dired-get-marked-files
                      (and dired-delight-use-relative-names 'no-dir))
                     (completing-read
                      "Set color for files (name or hex code #aabb11):"
                      (list-colors-duplicates (defined-colors)))))
  (dolist (name filenames)
    (if-let ((old-color (gethash name dired-delight--color-table)))
      (unless (equal color old-color)
        (puthash old-color
                 (cl-remove
                  name (gethash old-color dired-delight--color-groups)
                  :test #'equal)
                 dired-delight--color-groups)
        (if (equal color "")
            (remhash name dired-delight--color-table)
          (puthash name color dired-delight--color-table)
          (cl-pushnew name (gethash color dired-delight--color-groups))))
      (puthash name color dired-delight--color-table)
      (cl-pushnew name (gethash color dired-delight--color-groups))))
  (dired-delight--apply)
  (dired-unmark-all-marks))

(defun dired-delight--apply (&optional start end)
  "Display Delights (colors) from START to END."
  (setq dired-delight--timer nil)
  (when dired-delight-mode
    (setq start (or (and start (min start (window-start)))
                    (window-start))
          end (or (and end (max end (window-end)))
                  (window-end)))
    (let ((relative-names
           (and dired-delight-use-relative-names 'no-dir)))
      (unwind-protect
          (save-excursion
            (goto-char start)
            (while (< (point) end)
              (beginning-of-line)
              (when-let* ((fname (dired-get-filename relative-names t))
                          (col (gethash fname dired-delight--color-table)))
                (goto-char (next-single-property-change
                            (point) 'dired-filename nil (line-end-position)))
                (funcall dired-delight--propertize (point) col))
              (forward-line 1)))))))

(defun dired-delight-mark-color (color)
  "Mark all files in Dired matching COLOR."
  (interactive
   (list
    (completing-read "Mark files matching color: "
                     (hash-table-keys dired-delight--color-groups)
                     nil t)))
  (let ((relative-names
         (and dired-delight-use-relative-names 'no-dir)))
    (dired-mark-if
     (and (not (looking-at-p dired-re-dot))
          (not (eolp))
          (let ((file-color
                 (gethash (dired-get-filename relative-names t)
                          dired-delight--color-table)))
            (equal file-color color)))
     "matching color")))

(defun dired-delight--propertize-block (pt color)
  "Apply COLOR at PT in Dired to a block."
  (with-silent-modifications
    (put-text-property
     (1- pt) pt 'dired-delight color)
    (put-text-property
     (1- pt) pt
     'display
     (propertize " ■" 'face `(:foreground ,color)))))

(defun dired-delight--propertize-background (pt color)
  "Apply COLOR at PT in Dired as a background."
  (let* ((lep (line-end-position))
         (end-pt (next-single-property-change
                  pt 'dired-filename nil lep)))
    (with-silent-modifications
      (put-text-property
       pt end-pt 'dired-delight color)
      (put-text-property
       pt end-pt
       'face `(:background ,color)))))

(defun dired-delight--clear (&optional start end)
  "Clear all Delights (colors) from Dired.

If START and END are specified, clear colors between them."
  (save-excursion
    (goto-char (or start (point-min)) )
    (setq end (or end (point-max)))
    (with-silent-modifications
      (while (< (point) end)
        (when-let ((next (next-single-property-change
                          (point) 'dired-delight
                          nil (point-max))))
          (goto-char next)
          (pcase dired-delight-display
            ('block (remove-text-properties
                     (1- (point)) (point)
                     '(display nil dired-delight nil)))
            ('background (remove-text-properties
                          (point) (line-end-position)
                          '(face nil dired-delight nil)))))))))

(defun dired-delight--schedule (start &optional end)
  "Schedule Delight coloring for a Dired buffer.

START and END are the bounds to color inside."
  (unless inhibit-quit
    (when (not dired-delight--timer)
      (setq dired-delight--timer
            (run-with-idle-timer
             dired-delight-delay nil
             #'dired-delight--apply
             start end)))))

(defun dired-delight--handle-scroll (_ start)
  "Schedule Delight coloring for a Dired buffer.

START is where to begin coloring.

This function is meant to run as a member of `window-scroll-functions'."
  (when dired-delight-mode
    (dired-delight--schedule start)))

(defun dired-delight--save ()
  "Write Delight data to `dired-delight-file'."
  (when (and dired-delight--color-table
             dired-delight--color-groups)
    (unless (or (hash-table-empty-p
                 dired-delight--color-table)
                (hash-table-empty-p
                 dired-delight--color-groups))
      (let ((write-region-inhibit-fsync t)
            (coding-system-for-read 'utf-8)
            (coding-system-for-write 'utf-8)
            (file-name-handler-alist nil)
            (print-level nil) (print-length nil))
        (with-temp-file dired-delight-file
          (insert ";;; -*- lisp-data -*-\n")
          (prin1 (cons dired-delight--color-table
                       dired-delight--color-groups)
                 (current-buffer)))))))

(defun dired-delight--read ()
  "Read Delight data from `dired-delight-file'."
  (and (file-exists-p dired-delight-file)
       (with-temp-buffer
         (insert-file-contents dired-delight-file)
         (condition-case nil
             (let* ((coding-system-for-read 'utf-8)
                    (data (read (current-buffer))))
               (setq dired-delight--color-table  (car data)
                     dired-delight--color-groups (cdr data)))
           (:success (message "Dired Delight data read from disk."))
           (error (message "Could not read Dired Delight data."))))))

(provide 'dired-delight)
;;; dired-delight.el ends here
