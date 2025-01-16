;;; cape-char.el --- Character completion functions -*- lexical-binding: t -*-

;; Copyright (C) 2021-2025 Free Software Foundation, Inc.

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

(declare-function quail-deactivate "quail")
(declare-function quail-build-decode-map "quail")
(declare-function quail-map "quail")

(eval-and-compile
  (defun cape-char--translation (method prefix)
    "Return character translation hash for input method METHOD.
PREFIX are the prefix characters. Names (hash keys) that map to
multiple candidates (hash values) in the quail translation map
are not included. Hash values are either char or strings."
    (when-let ((im (assoc method input-method-alist))
               ((eq #'quail-use-package (nth 2 im))))
      (let ((hash (make-hash-table :test #'equal))
            (dm (list 'decode-map)))
        (require 'quail)
        (apply #'quail-use-package method (nthcdr 5 im))
        (quail-build-decode-map (list (quail-map)) "" dm 0)
        (pcase-dolist (`(,name . ,val) (cdr dm))
          (when (equal method "emoji")
            (setq name (replace-regexp-in-string
                        ": " "-"
                        (replace-regexp-in-string
                         "[’“”!()]" ""
                         (replace-regexp-in-string
                          "[_ &.]+" "-" name))))
            (when (string-match-p "\\`[[:alnum:]-]*\\'" name)
              (setq name (format ":%s:" name))))
          (when (memq (aref name 0) prefix)
            (puthash name (if (vectorp val) (aref val 0) val) hash)))
        (quail-deactivate)
        hash))))

(defun cape-char--annotation (hash name)
  "Lookup NAME in HASH and return annotation."
  (when-let ((char (gethash name hash)))
    (format (if (stringp char) " %s " " %c ") char)))

(defun cape-char--signature (hash name)
  "Lookup NAME in HASH and return signature."
  (when-let ((val (gethash name hash)))
    (concat
     (and (stringp val) (concat val " = "))
     (mapconcat
      (lambda (char)
        (format "%c %s (%s)"
                char
                (get-char-code-property char 'name)
                (char-code-property-description
                 'general-category
                 (get-char-code-property char 'general-category))))
      (if (stringp val) val (list val))
      " + "))))

(defun cape-char--exit (hash name status)
  "Exit function given completion STATUS, looks-up NAME in HASH."
  (when-let (((not (eq status 'exact)))
             (char (gethash name hash)))
    (delete-region (max (point-min) (- (point) (length name))) (point))
    (insert char)))

(defmacro cape-char--define (name method &rest prefix)
  "Define character translation Capf.
NAME is the name of the Capf.
METHOD is the input method.
PREFIX are the prefix characters."
  (when-let ((capf (intern (format "cape-%s" name)))
             (pre-req (intern (format "cape-%s-prefix-required" name)))
             (props (intern (format "cape--%s-properties" name)))
             (pre-rx (concat (regexp-opt (mapcar #'char-to-string prefix)) "[^ \n\t]*" ))
             (hash (intern (format "cape--%s-hash" name)))
             (hash-val (cape-char--translation method prefix)))
    `(progn
       (defvar ,hash ,hash-val)
       (defcustom ,pre-req t
         ,(format "Initial prefix is required for `%s' to trigger." capf)
         :type 'boolean
         :group 'cape)
       (defvar ,props
         (list :annotation-function (apply-partially #'cape-char--annotation ,hash)
               :company-docsig (apply-partially #'cape-char--signature ,hash)
               :exit-function (apply-partially #'cape-char--exit ,hash)
               :company-kind (lambda (_) 'text)
               :category ',capf
               :exclusive 'no)
         ,(format "Completion extra properties for `%s'." capf))
       (defun ,capf (&optional interactive)
         ,(format "Complete Unicode character at point.
Uses the input format of the %s input method,
see (describe-input-method %S). If INTERACTIVE is nil the
function acts like a Capf." method method)
         (interactive (list t))
         (if interactive
             ;; No cycling since it breaks the :exit-function.
             (let (completion-cycle-threshold ,pre-req)
               (when (and (memq last-input-event ',prefix)
                          (not (looking-back ,pre-rx (pos-bol))))
                 (self-insert-command 1 last-input-event))
               (cape-interactive #',capf))
           (when-let ((bounds
                       (cond
                        ((looking-back ,pre-rx (pos-bol))
                         (cons (match-beginning 0) (point)))
                        ((not ,pre-req) (cons (point) (point))))))
             (append (list (car bounds) (cdr bounds) ,hash) ,props)))))))

;;;###autoload (autoload 'cape-tex "cape-char" nil t)
(cape-char--define tex "TeX" ?\\ ?^ ?_)

;;;###autoload (autoload 'cape-sgml "cape-char" nil t)
(cape-char--define sgml "sgml" ?&)

;;;###autoload (autoload 'cape-rfc1345 "cape-char" nil t)
(cape-char--define rfc1345 "rfc1345" ?&)

;;;###autoload (when (> emacs-major-version 28) (autoload 'cape-emoji "cape-char" nil t))
(cape-char--define emoji "emoji" ?:)

(provide 'cape-char)
;;; cape-char.el ends here
