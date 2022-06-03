;;; yabridgectl.el --- A GUI for yabridgectl -*- lexical-binding: t -*-

;; Author: Kisaragi Hiu
;; Version: 0.1
;; Package-Requires: ((emacs "25.1"))
;; Homepage: https://github.com/kisaragi-hiu/yabridgectl.el
;; Keywords: convenience UI yabridgectl

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

;; A GUI for yabridgectl.

;;; Code:

(require 'cl-lib)

;;;; * Utils
(defun yabridgectl--run (insert? &rest args)
  "Run yabridgectl with ARGS.

Return the exit code from `call-process'.

When INSERT?, insert the output into the current buffer instead
of messaging it out."
  (let (code output)
    (with-temp-buffer
      (setq code
            (apply #'call-process "yabridgectl" nil
                   '(t nil)
                   nil args)
            output
            (string-trim
             (buffer-string))))
    (if insert?
        (save-excursion
          (insert output "\n"))
      (message "%s" output))
    code))

;;;; * Buttons
(defun yabridgectl--buttons/sync (&optional label)
  "Return a button labeled LABEL that runs `yabridgectl sync' when pressed."
  (make-text-button
   (or label "sync") nil
   'action (lambda (_)
             (yabridgectl--run nil "sync")
             (yabridgectl--refresh))
   'follow-link t
   'help-echo "Set up or update yabridgectl for all plugins"))

(defun yabridgectl--buttons/add-plugin-location (&optional label)
  "Return a text button labeled LABEL that adds a plugin location."
  (make-text-button
   (or label "Add a plugin install location") nil
   'action (lambda (_)
             (let* ((dir (expand-file-name
                          (read-directory-name "Directory: "))))
               (make-directory dir t)
               (yabridgectl--run nil "add" dir)
               (yabridgectl--refresh)))
   'follow-link t
   'help-echo "Add a plugin install location"))

(defun yabridgectl--buttons/link (path &optional label)
  "Return a text button labeled LABEL that visits PATH when pressed.

LABEL defaults to PATH."
  (make-text-button
   label nil
   'action (lambda (button)
             (browse-url (button-get button 'path)))
   'path path
   'face 'link
   'follow-link t
   'help-echo "Visit this link"))

(defun yabridgectl--buttons/rm (path)
  "Return a button that runs \"yabridgectl rm PATH\" when pressed."
  (make-text-button
   "[X]" nil
   'action (lambda (button)
             (yabridgectl--run nil "rm" (button-get button 'path))
             (yabridgectl--refresh))
   'path path
   'face '(error :inherit button)
   'follow-link t
   'help-echo "Remove a plugin install location"))

;;;; * Processing buffer content
(defun yabridgectl--fontify ()
  "Highlight keywords in the main view."
  (goto-char (point-min))
  (save-excursion
    (while (re-search-forward ", \\(copy\\)" nil t)
      (put-text-property
       (match-beginning 1) (match-end 1)
       'face 'match)))
  (save-excursion
    (while (re-search-forward ":: \\(VST2\\)" nil t)
      (put-text-property
       (match-beginning 1) (match-end 1)
       'face 'font-lock-constant-face)))
  (save-excursion
    (while (re-search-forward ":: \\(VST3\\)" nil t)
      (put-text-property
       (match-beginning 1) (match-end 1)
       'face 'font-lock-keyword-face)))
  (save-excursion
    (while (re-search-forward ", \\(not yet installed\\)" nil t)
      (replace-match
       (yabridgectl--buttons/sync (match-string 1))
       nil nil nil 1)
      (put-text-property
       (match-beginning 1) (match-end 1)
       'face '(warning button)))))

(defun yabridgectl--transform-dirs ()
  "Enable interactions for each install directory in the main view."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^[^ \n]" nil t)
      (let* ((start (match-beginning 0))
             (end (line-end-position))
             (path (buffer-substring-no-properties start end)))
        (delete-region start end)
        (insert
         (format "%s %s:"
                 (yabridgectl--buttons/rm path)
                 (yabridgectl--buttons/link path
                                            (abbreviate-file-name path)))))
      (forward-line)
      (unless (looking-at (rx bol "  "))
        (insert "  No plugins found at this location\n")))))

;;;; * Major mode
(define-derived-mode yabridgectl-mode special-mode "yabridge"
  "Major mode for yabridgectl."
  :interactive nil
  (setq-local revert-buffer-function #'yabridgectl--refresh))

;;;; * Main render function
(defun yabridgectl--update ()
  "Render the main view."
  (with-current-buffer (get-buffer-create "*yabridgectl.el*")
    (let ((inhibit-read-only t)
          info)
      (yabridgectl-mode)
      (erase-buffer)
      (yabridgectl--run t "status")
      ;; Extract and remove the
      ;;
      ;; > yabridgectl path: <auto>
      ;; > libyabridgectl-vst2.so: '/usr/lib/libyabridge-vst2.so'
      ;; > ...
      ;;
      ;; section.
      (forward-paragraph 2)
      (setq info (buffer-substring (point-min) (point)))
      (delete-region (point-min) (point))
      (when (equal ?\C-j (char-after))
        (delete-char 1))
      (if (= 0 (buffer-size))
          (insert
           (format "No plugin locations indexed.\n\n%s"
                   (yabridgectl--buttons/add-plugin-location)))
        (yabridgectl--transform-dirs)
        (yabridgectl--fontify)
        ;; Inserting buttons
        (goto-char (point-max))
        (insert
         (concat
          "\n"
          (format "%s: Set up or update yabridgectl for all plugins\n"
                  (yabridgectl--buttons/sync))
          (format "%s: Add a plugin install location"
                  (yabridgectl--buttons/add-plugin-location "add")))))
      ;; Reset to top the first time. Subsequent refreshes will have
      ;; point location saved by `yabridgectl--refresh'.
      (goto-char (point-min)))))

(defun yabridgectl--refresh (&rest _)
  "Refresh the main screen, keeping point in place."
  (let ((p (point))
        (inhibit-redisplay t))
    (yabridgectl--update)
    (when (<= (point-min) p (point-max))
      (goto-char p))))

;;;; * Entry point

;;;###autoload
(defun yabridgectl ()
  "Yabridgectl."
  (interactive)
  (yabridgectl--update)
  (pop-to-buffer "*yabridgectl.el*"))

(provide 'yabridgectl)

;;; yabridgectl.el ends here
