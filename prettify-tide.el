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
  "Fontify and run prettier on region between BEG and END."
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
                   (insert string)
                   (when (eq 0
                             (apply #'call-process-region
                                    (append
                                     (list (point-min)
                                           (point-max)
                                           prettier-cmd
                                           t
                                           t
                                           nil))))
                     (buffer-string))))))
    (with-current-buffer buff
      (goto-char beg)
      (delete-region beg
                     end)
      (insert (with-temp-buffer
                (delay-mode-hooks
                  (when (fboundp 'typescript-mode)
                    (typescript-mode))
                  (goto-char (point-min))
                  (insert content)
                  (font-lock-ensure)
                  (buffer-string))))
      content)))

(defun prettify-tide-prettify (result)
  "Prettify RESULT of `tide-construct-documentation'.
Usage:
\(advice-add =\\'tide-construct-documentation :filter-return
              #\\'prettify-tide-prettify)"
  (or
   (ignore-errors
     (with-temp-buffer
       (erase-buffer)
       (insert result)
       (let ((prev-beg))
         (while
             (or
              (when-let* ((end (when (re-search-backward
                                      "^```"
                                      nil
                                      t
                                      1)
                                 (skip-chars-backward
                                  "\n")
                                 (point))))
                (save-excursion
                  (when-let ((start (when (re-search-backward
                                           "^```" nil t 1)
                                      (forward-line 1)
                                      (point))))
                    (setq prev-beg (point))
                    (prettify-tide-replace-region
                     start end))))
              (when-let ((start (re-search-backward
                                 "^@example\n"
                                 nil t 1)))
                (save-excursion
                  (forward-line 1)
                  (unless (looking-at "```")
                    (prettify-tide-replace-region
                     (point)
                     (or prev-beg
                         (point-max)))
                    (setq prev-beg start)))))))
       (unless (= (forward-line -1) -1)
         (goto-char (point-min))
         (when (looking-at "[(]")
           (ignore-errors (forward-list 1)
                          (insert "\n")))
         (prettify-tide-replace-region
          (point)
          (line-end-position)))
       (buffer-string)))
   result))

(provide 'prettify-tide)
;;; prettify-tide.el ends here