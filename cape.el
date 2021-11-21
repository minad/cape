;;; cape.el --- Completion At Point Extensions -*- lexical-binding: t -*-

;; Author: Daniel Mendler
;; Created: 2021
;; License: GPL-3.0-or-later
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;; Homepage: https://github.com/minad/cape

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Make your completions fly! This package provides additional completion
;; backends in the form of capfs.

;;; Code:

(require 'dabbrev)

(defun cape--complete-in-region (thing table &rest extra)
  "Complete THING at point given completion TABLE and EXTRA properties."
  (let ((bounds (or (bounds-of-thing-at-point thing) (cons (point) (point))))
        (completion-extra-properties extra))
    (completion-in-region (car bounds) (cdr bounds) table)))

;;;###autoload
(defun cape-file-capf ()
  "File name completion-at-point-function."
  (when-let (bounds (bounds-of-thing-at-point 'filename))
    (list (car bounds) (cdr bounds)
          #'read-file-name-internal
          :exclusive 'no
          :annotation-function (lambda (_) " (File)"))))

;;;###autoload
(defun cape-file ()
  "Complete file name at point."
  (interactive)
  (cape--complete-in-region 'filename #'read-file-name-internal))

;;;###autoload
(defun cape-dabbrev-capf ()
  "Dabbrev completion-at-point-function."
  (let ((dabbrev-check-all-buffers nil)
        (dabbrev-check-other-buffers nil))
    (dabbrev--reset-global-variables))
  (let ((abbrev (ignore-errors (dabbrev--abbrev-at-point))))
    (when (and abbrev (not (string-match-p "[ \t]" abbrev)))
      (pcase ;; Interruptible scanning
          (while-no-input
            (let ((inhibit-message t)
                  (message-log-max nil))
              (or (dabbrev--find-all-expansions
                   abbrev (dabbrev--ignore-case-p abbrev))
                  t)))
        ('nil (keyboard-quit))
        ('t nil)
        (words
         ;; Ignore completions which are too short
         (let ((min-len (+ 4 (length abbrev))))
           (setq words (seq-remove (lambda (x) (< (length x) min-len)) words)))
         (when words
           (let ((beg (progn (search-backward abbrev) (point)))
                 (end (progn (search-forward abbrev) (point))))
             (unless (string-match-p "\n" (buffer-substring beg end))
               (list beg end words
                     :exclusive 'no
                     :annotation-function (lambda (_) " (Dabbrev)"))))))))))

(autoload 'ispell-lookup-words "ispell")

;;;###autoload
(defun cape-ispell-capf ()
  "Ispell completion-at-point-function."
  (when-let* ((bounds (bounds-of-thing-at-point 'word))
              (table (with-demoted-errors
                         (let ((message-log-max nil)
                               (inhibit-message t))
                           (ispell-lookup-words
                            (format "*%s*"
                                    (buffer-substring-no-properties (car bounds) (cdr bounds))))))))
    (list (car bounds) (cdr bounds) table
          :exclusive 'no
          :annotation-function (lambda (_) " (Ispell)"))))

;;;###autoload
(defun cape-ispell ()
  "Complete with Ispell at point."
  (interactive)
  (let ((completion-at-point-functions (list #'cape-ispell-capf)))
    (completion-at-point)))

(defun cape--word-capf (words)
  "Use WORDS list as completion-at-point-function."
  (when-let (bounds (bounds-of-thing-at-point 'word))
    (list (car bounds) (cdr bounds) words
          :exclusive 'no
          :annotation-function (lambda (_) " (Words)"))))

(defvar cape--dict-words nil
  "List of dictionary words.")

(defvar cape--dict-file "/etc/dictionaries-common/words"
  "Dictionary word list file.")

(defun cape--dict-words ()
  "Return list of dictionary words."
  (or cape--dict-words
      (setq cape--dict-words
            (split-string (with-temp-buffer
                            (insert-file-contents-literally cape--dict-file)
                            (buffer-string))
                          "\n"))))

;;;###autoload
(defun cape-dict-capf ()
  "Dictionary completion-at-point-function."
  (cape--word-capf (cape--dict-words)))

;;;###autoload
(defun cape-dict ()
  "Complete word at point."
  (interactive)
  (cape--complete-in-region 'word (cape--dict-words)))

(defun cape--abbrev-completions ()
  "Return all abbreviations."
  (delete "" (nconc (all-completions "" global-abbrev-table)
                    (all-completions "" local-abbrev-table))))

(defun cape--abbrev-expand (&rest _)
  "Expand abbreviation before point."
  (expand-abbrev))

;;;###autoload
(defun cape-abbrev-capf ()
  "Abbrev completion-at-point-function."
  (when-let (bounds (bounds-of-thing-at-point 'symbol))
    (list (car bounds) (cdr bounds) (cape--abbrev-completions)
          :exclusive 'no
          :exit-function #'cape--abbrev-expand
          :annotation-function (lambda (_) " (Abbrev)"))))

;;;###autoload
(defun cape-abbrev ()
  "Complete abbreviation at point."
  (interactive)
  (cape--complete-in-region 'symbol (cape--abbrev-completions)
                            :exit-function #'cape--abbrev-expand))

(provide 'cape)
;;; cape.el ends here
