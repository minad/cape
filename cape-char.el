;;; cape-char.el --- Character completion functions -*- lexical-binding: t -*-

;; Copyright (C) 2021-2023 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;; This package provides the `cape-emoji', `cape-tex', `cape-sgml' and
;; `cape-rfc1345' completion functions.

;;; Code:

(require 'cape)
(require 'quail)

(autoload 'thing-at-point-looking-at "thingatpt")

;; Declare as pure function which is evaluated at compile time. We don't use a
;; macro for this computation since packages like `helpful' will
;; `macroexpand-all' the expensive `cape-char--define' macro calls.
(eval-when-compile
  (defun cape-char--translation (method)
    "Return character translation hash for METHOD."
    (declare (pure t))
    (let* ((decode-map (list 'dm))
           (quail-current-package (assoc method quail-package-alist))
           (map-list (nth 2 quail-current-package))
           (hash (make-hash-table :test #'equal)))
      (apply #'quail-use-package method (nthcdr 5 (assoc method input-method-alist)))
      (quail-build-decode-map (list map-list) "" decode-map 0)
      (dolist (elem (cdr decode-map))
        (let ((key (car elem))
              (value (cdr elem)))
          ;; Drop all translations that map to multiple candidates, like
          ;; how quail hide them from "KEY SEQUENCES"
          (if (not (vectorp value))
              (puthash key (if (stringp value)
                               (string-to-char value)
                             value)
                       hash))))
      (quail-deactivate)
      hash)))

(defmacro cape-char--define (name method &rest prefix)
  "Define character translation Capf.
NAME is the name of the Capf.
METHOD is the input method.
PREFIX are the prefix characters."
  (let ((capf (intern (format "cape-%s" name)))
        (prefix-required (intern (format "cape-%s-prefix-required" name)))
        (hash (intern (format "cape--%s-hash" name)))
        (ann (intern (format "cape--%s-annotation" name)))
        (docsig (intern (format "cape--%s-docsig" name)))
        (exit (intern (format "cape--%s-exit" name)))
        (properties (intern (format "cape--%s-properties" name)))
        (thing-re (concat (regexp-opt (mapcar #'char-to-string prefix)) "[^ \n\t]*" )))
    `(progn
       (defvar ,hash (cape-char--translation ,method))
       (defcustom ,prefix-required t
         ,(format "Initial prefix is required for `%s' to trigger." capf)
         :type 'boolean
         :group 'cape)
       (defun ,ann (name)
         (when-let (char (gethash name ,hash))
           (format " %c" char)))
       (defun ,docsig (name)
         (when-let (char (gethash name ,hash))
           (format "%s (%s)"
                   (get-char-code-property char 'name)
                   (char-code-property-description
                    'general-category
                    (get-char-code-property char 'general-category)))))
       (defun ,exit (name status)
         (unless (eq status 'exact)
           (when-let (char (gethash name ,hash))
             (delete-region (max (point-min) (- (point) (length name))) (point))
             (insert (char-to-string char)))))
       (defvar ,properties
         (list :annotation-function #',ann
               :company-docsig #',docsig
               :exit-function #',exit
               :company-kind (lambda (_) 'text)
               :exclusive 'no)
         ,(format "Completion extra properties for `%s'." name))
       (defun ,capf (&optional interactive)
         ,(format "Complete Unicode character at point.
Uses the same input format as the %s input method,
see (describe-input-method %S). If INTERACTIVE
is nil the function acts like a Capf." method method)
         (interactive (list t))
         (if interactive
             ;; No cycling since it breaks the :exit-function.
             (let (completion-cycle-threshold ,prefix-required)
               (when (and (memq last-input-event ',prefix)
                          (not (thing-at-point-looking-at ,thing-re)))
                 (self-insert-command 1 last-input-event))
               (cape-interactive #',capf))
           (when-let (bounds
                      (cond
                       ((thing-at-point-looking-at ,thing-re)
                        (cons (match-beginning 0) (match-end 0)))
                       ((not ,prefix-required) (cons (point) (point)))))
             (append
              (list (car bounds) (cdr bounds)
                    (cape--table-with-properties ,hash :category ',capf))
              ,properties)))))))

;; TODO: use static-if as soon as compat-30 is released
(defmacro cape-char--static-if (cond then &rest else)
  "Static if COND with THEN and ELSE branch."
  (if (eval cond t) then (cons 'progn else)))

;;;###autoload (autoload 'cape-tex "cape-char" nil t)
(cape-char--define tex "TeX" ?\\ ?^ ?_)

;;;###autoload (autoload 'cape-sgml "cape-char" nil t)
(cape-char--define sgml "sgml" ?&)

;;;###autoload (autoload 'cape-rfc1345 "cape-char" nil t)
(cape-char--define rfc1345 "rfc1345" ?&)

;;;###autoload (when (> emacs-major-version 28) (autoload 'cape-emoji "cape-char" nil t))
(cape-char--static-if (> emacs-major-version 28)
  (cape-char--define emoji "emoji" ?:))

(provide 'cape-char)
;;; cape-char.el ends here
