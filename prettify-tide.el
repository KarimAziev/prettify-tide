;;; prettify-tide.el --- Prettify tide documentation buffers -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/prettify-tide
;; Version: 0.1.0
;; Keywords: languages
;; Package-Requires: ((emacs "26.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Prettify tide documentation buffers

;;; Code:

(defun prettify-tide-replace-region (beg end)
  "Replace region with formatted TypeScript code.

Argument BEG is the starting position of the region to replace.

Argument END is the ending position of the region to replace."
  (when-let* ((buff (current-buffer))
              (string
               (buffer-substring-no-properties
                beg
                end))
              (content
               (when-let ((prettier-cmd
                           (let ((dir default-directory)
                                 (node-modules)
                                 (found))
                             (while
                                 (setq node-modules
                                       (unless found
                                         (setq dir
                                               (locate-dominating-file
                                                dir
                                                "node_modules"))))
                               (setq dir (let ((parent
                                                (file-name-directory
                                                 (directory-file-name
                                                  (expand-file-name
                                                   dir
                                                   default-directory)))))
                                           (when (and
                                                  (file-exists-p
                                                   dir)
                                                  (file-exists-p
                                                   parent)
                                                  (not
                                                   (equal
                                                    (file-truename
                                                     (directory-file-name
                                                      (expand-file-name
                                                       dir)))
                                                    (file-truename
                                                     (directory-file-name
                                                      (expand-file-name
                                                       parent))))))
                                             (if (file-name-absolute-p
                                                  dir)
                                                 (directory-file-name
                                                  parent)
                                               (file-relative-name
                                                parent)))))
                               (let ((file (expand-file-name
                                            "node_modules/.bin/prettier"
                                            node-modules)))
                                 (setq found
                                       (when (and (file-exists-p
                                                   file)
                                                  (file-executable-p
                                                   file))
                                         file))))
                             (or found (executable-find "prettier")))))
                 (with-temp-buffer
                   (insert
                    string)
                   (let* ((status (call-process-region
                                   (point-min)
                                   (point-max)
                                   "prettier"
                                   t
                                   t
                                   nil
                                   "--single-quote"
                                   "--parser" "typescript"))
                          (formatted (buffer-string)))
                     (if (zerop status)
                         formatted
                       string))))))
    (with-current-buffer buff
      (goto-char beg)
      (delete-region beg
                     end)
      (insert (save-excursion
                (forward-line -1)
                (if (looking-at "```")
                    "\n"
                  "\n```typescript\n"))
              (with-temp-buffer
                (delay-mode-hooks
                  (if (and
                       (fboundp 'treesit-available-p)
                       (treesit-available-p)
                       (fboundp 'typescript-ts-mode))
                      (typescript-ts-mode)
                    (when (fboundp 'typescript-mode)
                      (typescript-mode)))
                  (goto-char (point-min))
                  (insert content)
                  (indent-region (point-min)
                                 (point-max))
                  (font-lock-ensure)
                  (buffer-string))))
      (skip-chars-forward "\s\t\n")
      (unless (looking-at "```")
        (insert "```\n"))
      content)))

(defun prettify-tide-prettify (result)
  "Prettify RESULT of `tide-construct-documentation'.

Usage:

\\=(advice-add \\='tide-construct-documentation :filter-return
            #\\='prettify-tide-prettify)"
  (or
   (ignore-errors
     (with-temp-buffer
       (erase-buffer)
       (insert (replace-regexp-in-string "```ts" "```typescript" result))
       (let ((prev-beg)
             (inhibit-read-only t))
         (while
             (or
              (when-let* ((end
                           (when (re-search-backward
                                  "^```"
                                  nil
                                  t
                                  1)
                             (skip-chars-backward
                              "\n")
                             (point))))
                (save-excursion
                  (when-let ((start
                              (when (re-search-backward
                                     "^```" nil
                                     t 1)
                                (forward-line 1)
                                (point))))
                    (setq prev-beg (point))
                    (prettify-tide-replace-region
                     start end))))
              (when-let ((start (re-search-backward
                                 "^@example\n"
                                 nil t 1)))
                (save-excursion
                  (unless (re-search-forward "^```" nil t 1)
                    (forward-line 1)
                    (skip-chars-forward "\n")
                    (prettify-tide-replace-region
                     (point)
                     (or prev-beg
                         (point-max)))))))))
       (unless (= (forward-line -1) -1)
         (goto-char (point-min))
         (when (looking-at "[(]")
           (ignore-errors (forward-list 1)
                          (insert "\n")))
         (let* ((beg (point))
                (end (progn (goto-char (line-end-position))
                            (if (looking-back "{" 0)
                                (ignore-errors
                                  (forward-char -1)
                                  (forward-list 1)
                                  (point))
                              (point)))))
           (prettify-tide-replace-region
            beg
            end)))
       (goto-char (point-min))
       (while (re-search-forward "@[a-z]+" nil t 1)
         (add-face-text-property (match-beginning 0)
                                 (match-end 0)
                                 'font-lock-keyword-face))
       (string-trim-left
        (replace-regexp-in-string "^[\n][\n]+"
                                  "\n"
                                  (replace-regexp-in-string
                                   "^```\\($\\|[^\n]+\\)" ""
                                   (buffer-string))))))
   result))

(defun prettify-tide-make-help-buffer (buffer)
  "Enable `visual-line-mode' in the provided buffer.

Argument BUFFER is the buffer in which to enable `visual-line-mode' if
available."
  (with-current-buffer buffer
    (when (fboundp 'visual-line-mode)
      (visual-line-mode 1)))
  buffer)

;;;###autoload
(defun prettify-tide-add-advice ()
  "Enhance Tide's documentation display."
  (interactive)
  (advice-add 'tide-construct-documentation :filter-return
              #'prettify-tide-prettify)
  (advice-add 'tide-make-help-buffer :filter-return
              #'prettify-tide-make-help-buffer))

;;;###autoload
(defun prettify-tide-remove-advice ()
  "Remove advice to `tide-construct-documentation' and `tide-make-help-buffer'."
  (interactive)
  (advice-remove 'tide-construct-documentation
                 #'prettify-tide-prettify)
  (advice-remove 'tide-make-help-buffer
                 #'prettify-tide-make-help-buffer))

(provide 'prettify-tide)
;;; prettify-tide.el ends here