;;; cape.el --- Completion At Point Extensions -*- lexical-binding: t -*-

;; Copyright (C) 2021-2025 Free Software Foundation, Inc.

;; Author: Daniel Mendler <mail@daniel-mendler.de>
;; Maintainer: Daniel Mendler <mail@daniel-mendler.de>
;; Created: 2021
;; Version: 2.1
;; Package-Requires: ((emacs "28.1") (compat "30"))
;; URL: https://github.com/minad/cape
;; Keywords: abbrev, convenience, matching, completion, text

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

;; Let your completions fly! This package provides additional completion
;; backends in the form of Capfs (completion-at-point-functions).
;;
;; `cape-abbrev': Complete abbreviation (add-global-abbrev, add-mode-abbrev).
;; `cape-dabbrev': Complete word from current buffers.
;; `cape-dict': Complete word from dictionary file.
;; `cape-elisp-block': Complete Elisp in Org or Markdown code block.
;; `cape-elisp-symbol': Complete Elisp symbol.
;; `cape-emoji': Complete Emoji.
;; `cape-file': Complete file name.
;; `cape-history': Complete from Eshell, Comint or minibuffer history.
;; `cape-keyword': Complete programming language keyword.
;; `cape-line': Complete entire line from file.
;; `cape-rfc1345': Complete Unicode char using RFC 1345 mnemonics.
;; `cape-sgml': Complete Unicode char from SGML entity, e.g., &alpha.
;; `cape-tex': Complete Unicode char from TeX command, e.g. \hbar.

;;; Code:

(require 'compat)
(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

;;;; Customization

(defgroup cape nil
  "Completion At Point Extensions."
  :link '(info-link :tag "Info Manual" "(cape)")
  :link '(url-link :tag "Website" "https://github.com/minad/cape")
  :link '(emacs-library-link :tag "Library Source" "cape.el")
  :group 'convenience
  :group 'tools
  :group 'matching
  :prefix "cape-")

(defcustom cape-dict-limit 100
  "Maximal number of completion candidates returned by `cape-dict'."
  :type '(choice (const nil) natnum))

(defcustom cape-dict-file "/usr/share/dict/words"
  "Path to dictionary word list file.
This variable can also be a list of paths or
a function returning a single or more paths."
  :type '(choice string (repeat string) function))

(defcustom cape-dict-case-replace 'case-replace
  "Preserve case of input.
See `dabbrev-case-replace' for details."
  :type '(choice (const :tag "Disable" nil)
                 (const :tag "Use `case-replace'" case-replace)
                 (other :tag "Enable" t)))

(defcustom cape-dict-case-fold 'case-fold-search
  "Case fold search during search.
See `dabbrev-case-fold-search' for details."
  :type '(choice (const :tag "Disable" nil)
                 (const :tag "Use `case-fold-search'" case-fold-search)
                 (other :tag "Enable" t)))

(defcustom cape-dabbrev-buffer-function #'cape-same-mode-buffers
  "Function which returns list of buffers.
The buffers are scanned for completion candidates by `cape-dabbrev'."
  :type `(choice (const :tag "Current buffer" current-buffer)
                 (const :tag "Text buffers" ,#'cape-text-buffers)
                 (const :tag "Buffers with same mode" ,#'cape-same-mode-buffers)
                 (function :tag "Custom function")))

(defcustom cape-file-directory nil
  "Base directory used by `cape-file."
  :type '(choice (const nil) string function))

(defcustom cape-file-prefix "file:"
  "File completion trigger prefixes.
The value can be a string or a list of strings.  The default
`file:' is the prefix of Org file links which work in arbitrary
buffers via `org-open-at-point-global'."
  :type '(choice string (repeat string)))

(defcustom cape-file-directory-must-exist t
  "The parent directory must exist for file completion."
  :type 'boolean)

(defcustom cape-line-buffer-function #'cape-same-mode-buffers
  "Function which returns list of buffers.
The buffers are scanned for completion candidates by `cape-line'."
  :type `(choice (const :tag "Current buffer" current-buffer)
                 (const :tag "Text buffers" ,#'cape-text-buffers)
                 (const :tag "Buffers with same mode" ,#'cape-same-mode-buffers)
                 (function :tag "Custom function")))

(defcustom cape-elisp-symbol-wrapper
  '((org-mode ?~ ?~)
    (markdown-mode ?` ?`)
    (emacs-lisp-mode ?` ?')
    (rst-mode "``" "``")
    (log-edit-mode "`" "'")
    (change-log-mode "`" "'")
    (message-mode "`" "'")
    (rcirc-mode "`" "'"))
  "Wrapper characters for symbols."
  :type '(alist :key-type symbol :value-type (list (choice character string)
                                                   (choice character string))))

;;;; Helpers

(defun cape--buffer-list (pred)
  "Return list of buffers satisfying PRED."
  (let* ((cur (current-buffer))
         (orig (and (minibufferp) (window-buffer (minibuffer-selected-window))))
         (list (cl-loop for buf in (buffer-list)
                        if (and (not (eq buf cur)) (not (eq buf orig))
                                (funcall pred buf))
                        collect buf)))
    `(,cur ,@(and orig (list orig)) ,@list)))

(defun cape-same-mode-buffers ()
  "Return buffers with same major mode as current buffer."
  (cape--buffer-list
   (lambda (buf) (eq major-mode (buffer-local-value 'major-mode buf)))))

(defun cape-text-buffers ()
  "Return `text-mode' and `prog-mode' buffers."
  (cape--buffer-list
   (lambda (buf)
     (let ((mode (buffer-local-value 'major-mode buf)))
       (or (provided-mode-derived-p mode #'text-mode)
           (provided-mode-derived-p mode #'prog-mode))))))

(defun cape--case-fold-p (fold)
  "Return non-nil if case folding is enabled for FOLD."
  (if (eq fold 'case-fold-search) case-fold-search fold))

(defun cape--case-replace-list (flag input strs)
  "Replace case of STRS depending on INPUT and FLAG."
  (if (and (if (eq flag 'case-replace) case-replace flag)
           (let (case-fold-search) (string-match-p "\\`[[:upper:]]" input)))
      (mapcar (apply-partially #'cape--case-replace flag input) strs)
    strs))

(defun cape--case-replace (flag input str)
  "Replace case of STR depending on INPUT and FLAG."
  (or (and (if (eq flag 'case-replace) case-replace flag)
           (string-prefix-p input str t)
           (let (case-fold-search) (string-match-p "\\`[[:upper:]]" input))
           (save-match-data
             ;; Ensure that single character uppercase input does not lead to an
             ;; all uppercase result.
             (when (and (= (length input) 1) (> (length str) 1))
               (setq input (concat input (substring str 1 2))))
             (and (string-match input input)
                  (replace-match str nil nil input))))
      str))

(defun cape--separator-p (str)
  "Return non-nil if input STR has a separator character.
Separator characters are used by completion styles like Orderless
to split filter words.  In Corfu, the separator is configurable
via the variable `corfu-separator'."
  (string-search (string ;; Support `corfu-separator' and Orderless
                  (or (and (bound-and-true-p corfu-mode)
                           (bound-and-true-p corfu-separator))
                      ?\s))
                 str))

(defmacro cape--silent (&rest body)
  "Silence BODY."
  (declare (indent 0))
  `(cl-letf ((inhibit-message t)
             (message-log-max nil)
             ((symbol-function #'minibuffer-message) #'ignore))
     (ignore-errors ,@body)))

(defun cape--bounds (thing)
  "Return bounds of THING."
  (or (bounds-of-thing-at-point thing) (cons (point) (point))))

(defmacro cape--wrapped-table (wrap body)
  "Create wrapped completion table, handle `completion--unquote'.
WRAP is the wrapper function.
BODY is the wrapping expression."
  (declare (indent 1))
  `(lambda (str pred action)
     (,@body
      (let ((result (complete-with-action action table str pred)))
        (when (and (eq action 'completion--unquote) (functionp (cadr result)))
          (cl-callf ,wrap (cadr result)))
        result))))

(defun cape--accept-all-table (table)
  "Create completion TABLE which accepts all input."
  (cape--wrapped-table cape--accept-all-table
    (or (eq action 'lambda))))

(defun cape--passthrough-table (table)
  "Create completion TABLE disabling any filtering."
  (cape--wrapped-table cape--passthrough-table
    (let (completion-ignore-case completion-regexp-list (_ (setq str ""))))))

(defun cape--noninterruptible-table (table)
  "Create non-interruptible completion TABLE."
  (cape--wrapped-table cape--noninterruptible-table
    (let (throw-on-input))))

(defun cape--silent-table (table)
  "Create a new completion TABLE which is silent (no messages, no errors)."
  (cape--wrapped-table cape--silent-table
    (cape--silent)))

(defun cape--nonessential-table (table)
  "Mark completion TABLE as `non-essential'."
  (let ((dir default-directory))
    (cape--wrapped-table cape--nonessential-table
      (let ((default-directory dir)
            (non-essential t))))))

(defvar cape--debug-length 5
  "Length of printed lists in `cape--debug-print'.")

(defvar cape--debug-id 0
  "Completion table identifier.")

(defun cape--debug-message (&rest msg)
  "Print debug MSG."
  (let ((inhibit-message t))
    (apply #'message msg)))

(defun cape--debug-print (obj &optional full)
  "Print OBJ as string, truncate lists if FULL is nil."
  (cond
   ((symbolp obj) (symbol-name obj))
   ((functionp obj) "#<function>")
   ((proper-list-p obj)
    (concat
     "("
     (string-join
      (mapcar #'cape--debug-print
              (if full obj (take cape--debug-length obj)))
      " ")
     (if (and (not full) (length> obj cape--debug-length)) " ...)" ")")))
   (t (let ((print-level 2))
        (prin1-to-string obj)))))

(defun cape--debug-table (table name beg end)
  "Create completion TABLE with debug messages.
NAME is the name of the Capf, BEG and END are the input markers."
  (lambda (str pred action)
    (let ((result (complete-with-action action table str pred)))
      (if (and (eq action 'completion--unquote) (functionp (cadr result)))
          ;; See `cape--wrapped-table'
          (cl-callf cape--debug-table (cadr result) name beg end)
        (cape--debug-message
         "%s(action=%S input=%s:%s:%S prefix=%S ignore-case=%S%s%s) => %s"
         name
         (pcase action
           ('nil 'try)
           ('t 'all)
           ('lambda 'test)
           (_ action))
         (+ beg 0) (+ end 0) (buffer-substring-no-properties beg end)
         str completion-ignore-case
         (if completion-regexp-list
             (format " regexp=%s" (cape--debug-print completion-regexp-list t))
           "")
         (if pred
             (format " predicate=%s" (cape--debug-print pred))
           "")
         (cape--debug-print result)))
      result)))

(defun cape--dynamic-table (beg end fun)
  "Create dynamic completion table from FUN with caching.
BEG and END are the input bounds.  FUN is the function which
computes the candidates.  FUN must return a pair of a predicate
function function and the list of candidates.  The predicate is
passed new input and must return non-nil if the candidates are
still valid.

It is only necessary to use this function if the set of
candidates is computed dynamically based on the input and not
statically determined.  The behavior is similar but slightly
different to `completion-table-dynamic'.

The difference to the builtins `completion-table-dynamic' and
`completion-table-with-cache' is that this function does not use
the prefix argument of the completion table to compute the
candidates.  Instead it uses the input in the buffer between BEG
and END to FUN to compute the candidates.  This way the dynamic
candidate computation is compatible with non-prefix completion
styles like `substring' or `orderless', which pass the empty
string as first argument to the completion table."
  (let ((beg (copy-marker beg))
        (end (copy-marker end t))
        valid table)
    (lambda (str pred action)
      ;; Bail out early for `metadata' and `boundaries'. This is a pointless
      ;; move because of caching, but we do it anyway in the hope that the
      ;; profiler report looks less confusing, since the weight of the expensive
      ;; FUN computation is moved to the `all-completions' action.  Computing
      ;; `all-completions' must surely be most expensive, so nobody will suspect
      ;; a thing.
      (unless (or (eq action 'metadata) (eq (car-safe action) 'boundaries))
        (let ((input (buffer-substring-no-properties beg end)))
          (unless (and valid
                       (or (cape--separator-p input)
                           (funcall valid input)))
            (let* (;; Reset in case `all-completions' is used inside FUN
                   completion-ignore-case completion-regexp-list
                   ;; Retrieve new state by calling FUN
                   (new (and (< beg end) (funcall fun input)))
                   ;; No interrupt during state update
                   throw-on-input)
              (setq valid (car new) table (cdr new)))))
        (complete-with-action action table str pred)))))

;;;; Capfs

;;;;; cape-history

(declare-function ring-elements "ring")
(declare-function eshell-bol "eshell")
(declare-function comint-line-beginning-position "comint")
(defvar eshell-history-ring)
(defvar comint-input-ring)

(defvar cape--history-properties
  (list :company-kind (lambda (_) 'text)
        :exclusive 'no
        :display-sort-function #'identity
        :cycle-sort-function #'identity
        :category 'cape-history)
  "Completion extra properties for `cape-history'.")

;;;###autoload
(defun cape-history (&optional interactive)
  "Complete from Eshell, Comint or minibuffer history.
See also `consult-history' for a more flexible variant based on
`completing-read'.  If INTERACTIVE is nil the function acts like a Capf."
  (interactive (list t))
  (if interactive
      (cape-interactive #'cape-history)
    (let (history bol)
      (cond
       ((derived-mode-p 'eshell-mode)
        (setq history eshell-history-ring
              bol (static-if (< emacs-major-version 30)
                      (save-excursion (eshell-bol) (point))
                    (line-beginning-position))))
       ((derived-mode-p 'comint-mode)
        (setq history comint-input-ring
              bol (comint-line-beginning-position)))
       ((and (minibufferp) (not (eq minibuffer-history-variable t)))
        (setq history (symbol-value minibuffer-history-variable)
              bol (line-beginning-position))))
      (when (ring-p history)
        (setq history (ring-elements history)))
      (when history
        `(,bol ,(point) ,history ,@cape--history-properties)))))

;;;;; cape-file

(defvar comint-unquote-function)
(defvar comint-requote-function)

(defvar cape--file-properties
  (list :annotation-function (lambda (s) (if (string-suffix-p "/" s) " Dir" " File"))
        :company-kind (lambda (s) (if (string-suffix-p "/" s) 'folder 'file))
        :exclusive 'no
        :category 'file)
  "Completion extra properties for `cape-file'.")

;;;###autoload
(defun cape-file (&optional interactive)
  "Complete file name at point.
See the user option `cape-file-directory-must-exist'.
If INTERACTIVE is nil the function acts like a Capf."
  (interactive (list t))
  (if interactive
      (cape-interactive '(cape-file-directory-must-exist) #'cape-file)
    (pcase-let* ((default-directory (pcase cape-file-directory
                                      ('nil default-directory)
                                      ((pred stringp) cape-file-directory)
                                      (_ (funcall cape-file-directory))))
                 (prefix (and cape-file-prefix
                              (looking-back
                               (concat
                                (regexp-opt (ensure-list cape-file-prefix) t)
                                "[^ \n\t]*")
                               (pos-bol))
                              (match-end 1)))
                 (`(,beg . ,end) (if prefix
                                     (cons prefix (point))
                                   (cape--bounds 'filename)))
                 (non-essential t)
                 (file (buffer-substring-no-properties beg end)))
      (when (or prefix
                (not cape-file-directory-must-exist)
                (and (string-search "/" file)
                     (file-exists-p (file-name-directory
                                     (substitute-in-file-name file)))))
        (unless (boundp 'comint-unquote-function)
          (require 'comint))
        `( ,beg ,end
           ,(cape--nonessential-table
             (completion-table-with-quoting
              #'read-file-name-internal
              comint-unquote-function
              comint-requote-function))
           ,@(when (or prefix (string-match-p "./" file))
               '(:company-prefix-length t))
           ,@cape--file-properties)))))

;;;;; cape-elisp-symbol

(autoload 'elisp--company-kind "elisp-mode")
(autoload 'elisp--company-doc-buffer "elisp-mode")
(autoload 'elisp--company-doc-string "elisp-mode")
(autoload 'elisp--company-location "elisp-mode")

(defvar cape--elisp-symbol-properties
  (list :annotation-function #'cape--elisp-symbol-annotation
        :exit-function #'cape--elisp-symbol-exit
        :predicate #'cape--elisp-symbol-predicate
        :company-kind #'elisp--company-kind
        :company-doc-buffer #'elisp--company-doc-buffer
        :company-docsig #'elisp--company-doc-string
        :company-location #'elisp--company-location
        :exclusive 'no
        :category 'symbol)
  "Completion extra properties for `cape-elisp-symbol'.")

(defun cape--elisp-symbol-predicate (sym)
  "Return t if SYM is bound, fbound or propertized."
  (or (fboundp sym) (boundp sym) (symbol-plist sym)))

(defun cape--elisp-symbol-exit (sym status)
  "Wrap symbol SYM with `cape-elisp-symbol-wrapper' buffers.
STATUS is the exit status."
  (when-let (((not (eq status 'exact)))
             (c (cl-loop for (m . c) in cape-elisp-symbol-wrapper
                         if (derived-mode-p m) return c))
             ((or (not (derived-mode-p 'emacs-lisp-mode))
                  ;; Inside comment or string
                  (let ((s (syntax-ppss))) (or (nth 3 s) (nth 4 s)))))
             (x (if (stringp (car c)) (car c) (string (car c))))
             (y (if (stringp (cadr c)) (cadr c) (string (cadr c)))))
    (save-excursion
      (backward-char (length sym))
      (unless (save-excursion
                (and (ignore-errors (or (backward-char (length x)) t))
                     (looking-at-p (regexp-quote x))))
        (insert x)))
    (unless (looking-at-p (regexp-quote y))
      (insert y))))

(defun cape--elisp-symbol-annotation (sym)
  "Return kind of SYM."
  (setq sym (intern-soft sym))
  (cond
   ((special-form-p sym) " Special")
   ((macrop sym) " Macro")
   ((commandp sym) " Command")
   ((fboundp sym) " Function")
   ((custom-variable-p sym) " Custom")
   ((boundp sym) " Variable")
   ((featurep sym) " Feature")
   ((facep sym) " Face")
   (t " Symbol")))

;;;###autoload
(defun cape-elisp-symbol (&optional interactive)
  "Complete Elisp symbol at point.
If INTERACTIVE is nil the function acts like a Capf."
  (interactive (list t))
  (if interactive
      ;; No cycling since it breaks the :exit-function.
      (let (completion-cycle-threshold)
        (cape-interactive #'cape-elisp-symbol))
    (pcase-let ((`(,beg . ,end) (cape--bounds 'symbol)))
      (when (eq (char-after beg) ?')
        (setq beg (1+ beg) end (max beg end)))
      `(,beg ,end ,obarray ,@cape--elisp-symbol-properties))))

;;;;; cape-elisp-block

(declare-function org-element-context "org-element")
(declare-function markdown-code-block-lang "ext:markdown-mode")

(defun cape--inside-block-p (&rest langs)
  "Return non-nil if inside LANGS code block."
  (when-let ((face (get-text-property (point) 'face))
             (lang (or (and (if (listp face)
                                (memq 'org-block face)
                              (eq 'org-block face))
                            (plist-get (cadr (org-element-context)) :language))
                       (and (if (listp face)
                                (memq 'markdown-code-face face)
                              (eq 'markdown-code-face face))
                            (save-excursion
                              (markdown-code-block-lang))))))
    (member lang langs)))

;;;###autoload
(defun cape-elisp-block (&optional interactive)
  "Complete Elisp in Org or Markdown code block.
This Capf is particularly useful for literate Emacs configurations.
If INTERACTIVE is nil the function acts like a Capf."
  (interactive (list t))
  (cond
   (interactive
    ;; No code block check. Always complete Elisp when command was
    ;; explicitly invoked interactively.
    (cape-interactive #'elisp-completion-at-point))
   ((cape--inside-block-p "elisp" "emacs-lisp")
    (elisp-completion-at-point))))

;;;;; cape-dabbrev

(defvar cape--dabbrev-properties
  (list :annotation-function (lambda (_) " Dabbrev")
        :company-kind (lambda (_) 'text)
        :exclusive 'no
        :category 'cape-dabbrev)
  "Completion extra properties for `cape-dabbrev'.")

(defvar dabbrev-case-replace)
(defvar dabbrev-case-fold-search)
(defvar dabbrev-abbrev-char-regexp)
(defvar dabbrev-abbrev-skip-leading-regexp)
(declare-function dabbrev--find-all-expansions "dabbrev")
(declare-function dabbrev--reset-global-variables "dabbrev")

(defun cape--dabbrev-list (input)
  "Find all Dabbrev expansions for INPUT."
  (cape--silent
    (dlet ((dabbrev-check-other-buffers nil)
           (dabbrev-check-all-buffers nil)
           (dabbrev-backward-only nil)
           (dabbrev-limit nil)
           (dabbrev-search-these-buffers-only
            (ensure-list (funcall cape-dabbrev-buffer-function))))
      (dabbrev--reset-global-variables)
      (cons
       (apply-partially #'string-prefix-p input)
       (cl-loop
        with ic = (cape--case-fold-p dabbrev-case-fold-search)
        for w in (dabbrev--find-all-expansions input ic)
        collect (cape--case-replace (and ic dabbrev-case-replace) input w))))))

(defun cape--dabbrev-bounds ()
  "Return bounds of abbreviation."
  (unless (boundp 'dabbrev-abbrev-char-regexp)
    (require 'dabbrev))
  (let ((re (or dabbrev-abbrev-char-regexp "\\sw\\|\\s_"))
        (limit (minibuffer-prompt-end)))
    (when (or (looking-at re)
              (and (> (point) limit)
                   (save-excursion (forward-char -1) (looking-at re))))
      (cons (save-excursion
              (while (and (> (point) limit)
                          (save-excursion (forward-char -1) (looking-at re)))
                (forward-char -1))
              (when dabbrev-abbrev-skip-leading-regexp
                (while (looking-at dabbrev-abbrev-skip-leading-regexp)
                  (forward-char 1)))
              (point))
            (save-excursion
              (while (looking-at re)
                (forward-char 1))
              (point))))))

;;;###autoload
(defun cape-dabbrev (&optional interactive)
  "Complete with Dabbrev at point.

If INTERACTIVE is nil the function acts like a Capf.  In case you
observe a performance issue with auto-completion and `cape-dabbrev'
it is strongly recommended to disable scanning in other buffers.
See the user option `cape-dabbrev-buffer-function'."
  (interactive (list t))
  (if interactive
      (cape-interactive #'cape-dabbrev)
    (when-let ((bounds (cape--dabbrev-bounds)))
      `(,(car bounds) ,(cdr bounds)
        ,(completion-table-case-fold
          (cape--dynamic-table (car bounds) (cdr bounds) #'cape--dabbrev-list)
          (not (cape--case-fold-p dabbrev-case-fold-search)))
        ,@cape--dabbrev-properties))))

;;;;; cape-dict

(defvar cape--dict-properties
  (list :annotation-function (lambda (_) " Dict")
        :company-kind (lambda (_) 'text)
        :display-sort-function #'identity
        :cycle-sort-function #'identity
        :exclusive 'no
        :category 'cape-dict)
  "Completion extra properties for `cape-dict'.")

(defun cape--dict-list (input)
  "Return all words from `cape-dict-file' matching INPUT."
  (let* ((inhibit-message t)
         (message-log-max nil)
         (default-directory
          (if (and (not (file-remote-p default-directory))
                   (file-directory-p default-directory))
              default-directory
            user-emacs-directory))
         (files (mapcar #'expand-file-name
                        (ensure-list
                         (if (functionp cape-dict-file)
                             (funcall cape-dict-file)
                           cape-dict-file))))
         (words
          (apply #'process-lines-ignore-status
                 "grep"
                 (concat "-Fh"
                         (and (cape--case-fold-p cape-dict-case-fold) "i")
                         (and cape-dict-limit (format "m%d" cape-dict-limit)))
                 input files)))
    (cons
     (apply-partially
      (if (and cape-dict-limit (length= words cape-dict-limit))
          #'equal #'string-search)
      input)
     (cape--case-replace-list cape-dict-case-replace input words))))

;;;###autoload
(defun cape-dict (&optional interactive)
  "Complete word from dictionary at point.
This completion function works best if the dictionary is sorted
by frequency.  See the custom option `cape-dict-file'.  If
INTERACTIVE is nil the function acts like a Capf."
  (interactive (list t))
  (if interactive
      (cape-interactive #'cape-dict)
    (pcase-let ((`(,beg . ,end) (cape--bounds 'word)))
      `( ,beg ,end
         ,(completion-table-case-fold
           (cape--dynamic-table beg end #'cape--dict-list)
           (not (cape--case-fold-p cape-dict-case-fold)))
         ,@cape--dict-properties))))

;;;;; cape-abbrev

(defun cape--abbrev-list ()
  "Abbreviation list."
  (delete "" (cl-loop for x in (abbrev--suggest-get-active-tables-including-parents)
                      nconc (all-completions "" x))))

(defun cape--abbrev-annotation (abbrev)
  "Annotate ABBREV with expansion."
  (concat " "
          (truncate-string-to-width
           (format
            "%s"
            (symbol-value
             (cl-loop for x in (abbrev--suggest-get-active-tables-including-parents)
                      thereis (abbrev--symbol abbrev x))))
           30 0 nil t)))

(defun cape--abbrev-exit (_str status)
  "Expand expansion if STATUS is not exact."
  (unless (eq status 'exact)
    (expand-abbrev)))

(defvar cape--abbrev-properties
  (list :annotation-function #'cape--abbrev-annotation
        :exit-function #'cape--abbrev-exit
        :company-kind (lambda (_) 'snippet)
        :exclusive 'no
        :category 'cape-abbrev)
  "Completion extra properties for `cape-abbrev'.")

;;;###autoload
(defun cape-abbrev (&optional interactive)
  "Complete abbreviation at point.
If INTERACTIVE is nil the function acts like a Capf."
  (interactive (list t))
  (if interactive
      ;; No cycling since it breaks the :exit-function.
      (let (completion-cycle-threshold)
        (cape-interactive #'cape-abbrev))
    (when-let (abbrevs (cape--abbrev-list))
      (let ((bounds (cape--bounds 'symbol)))
        `(,(car bounds) ,(cdr bounds) ,abbrevs ,@cape--abbrev-properties)))))

;;;;; cape-line

(defvar cape--line-properties
  (list :display-sort-function #'identity
        :cycle-sort-function #'identity
        :exclusive 'no
        :category 'cape-line)
  "Completion extra properties for `cape-line'.")

(defun cape--line-list ()
  "Return all lines from buffer."
  (let ((ht (make-hash-table :test #'equal))
        (curr-buf (current-buffer))
        (buffers (funcall cape-line-buffer-function))
        lines)
    (dolist (buf (ensure-list buffers))
      (with-current-buffer buf
        (let ((beg (point-min))
              (max (point-max))
              (pt (if (eq curr-buf buf) (point) -1))
              end)
          (save-excursion
            (while (< beg max)
              (goto-char beg)
              (setq end (pos-eol))
              (unless (<= beg pt end)
                (let ((line (buffer-substring-no-properties beg end)))
                  (unless (or (string-blank-p line) (gethash line ht))
                    (puthash line t ht)
                    (push line lines))))
              (setq beg (1+ end)))))))
    (nreverse lines)))

;;;###autoload
(defun cape-line (&optional interactive)
  "Complete current line from other lines.
The buffers returned by `cape-line-buffer-function' are scanned for lines.
If INTERACTIVE is nil the function acts like a Capf."
  (interactive (list t))
  (if interactive
      (cape-interactive #'cape-line)
    `(,(pos-bol) ,(point) ,(cape--line-list) ,@cape--line-properties)))

;;;; Capf combinators

(defun cape--company-call (&rest app)
  "Apply APP and handle future return values."
  ;; Backends are non-interruptible. Disable interrupts!
  (let ((toi throw-on-input)
        (throw-on-input nil))
    (pcase (apply app)
      ;; Handle async future return values.
      (`(:async . ,fetch)
       (let ((res 'cape--waiting))
         (if toi
             (unwind-protect
                 (progn
                   (funcall fetch
                            (lambda (arg)
                              (when (eq res 'cape--waiting)
                                (push 'cape--done unread-command-events)
                                (setq res arg))))
                   (when (eq res 'cape--waiting)
                     (let ((ev (let ((input-method-function nil)
                                     (echo-keystrokes 0))
                                 (read-event nil t))))
                       (unless (eq ev 'cape--done)
                         (push (cons t ev) unread-command-events)
                         (setq res 'cape--cancelled)
                         (throw toi t)))))
               (setq unread-command-events
                     (delq 'cape--done unread-command-events)))
           (funcall fetch (lambda (arg) (setq res arg)))
           ;; Force synchronization, not interruptible! We use polling
           ;; here and ignore pending input since we don't use
           ;; `sit-for'. This is the same method used by Company itself.
           (while (eq res 'cape--waiting)
             (sleep-for 0.01)))
         res))
      ;; Plain old synchronous return value.
      (res res))))

(defvar-local cape--company-init nil)

;;;###autoload
(defun cape-company-to-capf (backend &optional valid)
  "Convert Company BACKEND function to Capf.
VALID is a function taking the old and new input string.  It should
return nil if the cached candidates became invalid.  The default value
for VALID is `string-prefix-p' such that the candidates are only fetched
again if the input prefix changed."
  (lambda ()
    (when (and (symbolp backend) (not (fboundp backend)))
      (ignore-errors (require backend nil t)))
    (when (bound-and-true-p company-mode)
      (error "`cape-company-to-capf' should not be used with `company-mode', use the Company backend directly instead"))
    (when (and (symbolp backend) (not (alist-get backend cape--company-init)))
      (funcall backend 'init)
      (put backend 'company-init t)
      (setf (alist-get backend cape--company-init) t))
    (when-let ((pre (pcase (cape--company-call backend 'prefix)
                      ((or `(,p ,_s) (and (pred stringp) p)) (cons p (length p)))
                      ((or `(,p ,_s ,l) `(,p . ,l)) (cons p l)))))
      (let* ((end (point)) (beg (- end (length (car pre))))
             (valid (if (cape--company-call backend 'no-cache (car pre))
                        #'equal (or valid #'string-prefix-p)))
             (sort-fun (and (cape--company-call backend 'sorted) #'identity))
             restore-props)
        (list beg end
              (funcall
               (if (cape--company-call backend 'ignore-case)
                   #'completion-table-case-fold
                 #'identity)
               (cape--dynamic-table
                beg end
                (lambda (input)
                  (let ((cands (cape--company-call backend 'candidates input)))
                    ;; The candidate string including text properties should be
                    ;; restored in the :exit-function, unless the UI guarantees
                    ;; this itself, like Corfu.
                    (unless (bound-and-true-p corfu-mode)
                      (setq restore-props cands))
                    (cons (apply-partially valid input) cands)))))
              :category backend
              :exclusive 'no
              :company-prefix-length (cdr pre)
              :company-doc-buffer (lambda (x) (cape--company-call backend 'doc-buffer x))
              :company-location (lambda (x) (cape--company-call backend 'location x))
              :company-docsig (lambda (x) (cape--company-call backend 'meta x))
              :company-deprecated (lambda (x) (cape--company-call backend 'deprecated x))
              :company-kind (lambda (x) (cape--company-call backend 'kind x))
              :display-sort-function sort-fun
              :cycle-sort-function sort-fun
              :annotation-function (lambda (x)
                                     (when-let (ann (cape--company-call backend 'annotation x))
                                       (concat " " (string-trim ann))))
              :exit-function (lambda (x _status)
                               ;; Restore the candidate string including
                               ;; properties if restore-props is non-nil.  See
                               ;; the comment above.
                               (setq x (or (car (member x restore-props)) x))
                               (cape--company-call backend 'post-completion x)))))))

;;;###autoload
(defun cape-interactive (&rest capfs)
  "Complete interactively with the given CAPFS."
  (let* ((ctx (and (consp (car capfs)) (car capfs)))
         (capfs (if ctx (cdr capfs) capfs))
         (completion-at-point-functions
          (if ctx
              (mapcar (lambda (f) `(lambda () (let ,ctx (funcall ',f)))) capfs)
            capfs)))
    (unless (completion-at-point)
      (user-error "%s: No completions"
                  (mapconcat (lambda (fun)
                               (if (symbolp fun)
                                   (symbol-name fun)
                                 "anonymous-capf"))
                             capfs ", ")))))

;;;###autoload
(defun cape-capf-interactive (capf)
  "Create interactive completion function from CAPF."
  (lambda (&optional interactive)
    (interactive (list t))
    (if interactive (cape-interactive capf) (funcall capf))))

;;;###autoload
(defun cape-wrap-super (&rest capfs)
  "Call CAPFS and return merged completion result.
The CAPFS list can contain the keyword `:with' to mark the Capfs
afterwards as auxiliary.  One of the non-auxiliary Capfs before `:with'
must return non-nil for the super Capf to set in and return a non-nil
result.  Such behavior is useful when listing multiple super Capfs in
the `completion-at-point-functions':

  (setq completion-at-point-functions
        (list (cape-capf-super \\='eglot-completion-at-point
                               :with \\='tempel-complete)
              (cape-capf-super \\='cape-dabbrev
                               :with \\='tempel-complete)))"
  (when-let ((results (cl-loop for capf in capfs until (eq capf :with)
                               for res = (funcall capf)
                               if res collect (cons t res))))
    (pcase-let* ((results (nconc results
                                 (cl-loop for capf in (cdr (memq :with capfs))
                                          for res = (funcall capf)
                                          if res collect (cons nil res))))
                 (`((,_main ,beg ,end . ,_)) results)
                 (cand-ht nil)
                 (tables nil)
                 (exclusive nil)
                 (prefix-len nil)
                 (cand-functions
                  '( :company-docsig :company-location :company-kind
                     :company-doc-buffer :company-deprecated
                     :annotation-function :exit-function)))
      (cl-loop for (main beg2 end2 table . plist) in results do
               ;; Note: `cape-capf-super' currently cannot merge Capfs which
               ;; trigger at different beginning positions.  In order to support
               ;; this, take the smallest BEG value and then normalize all
               ;; candidates by prefixing them such that they all start at the
               ;; smallest BEG position.
               (when (= beg beg2)
                 (push (list main (plist-get plist :predicate) table
                             ;; Plist attached to the candidates
                             (mapcan (lambda (f)
                                       (when-let ((v (plist-get plist f)))
                                         (list f v)))
                                     cand-functions))
                       tables)
                 ;; The resulting merged Capf is exclusive if one of the main
                 ;; Capfs is exclusive.
                 (when (and main (not (eq (plist-get plist :exclusive) 'no)))
                   (setq exclusive t))
                 (setq end (max end end2))
                 (let ((plen (plist-get plist :company-prefix-length)))
                   (cond
                    ((eq plen t)
                     (setq prefix-len t))
                    ((and (not prefix-len) (integerp plen))
                     (setq prefix-len plen))
                    ((and (integerp prefix-len) (integerp plen))
                     (setq prefix-len (max prefix-len plen)))))))
      (setq tables (nreverse tables))
      `( ,beg ,end
         ,(lambda (str pred action)
            (pcase action
              ((or `(boundaries . ,_) 'metadata) nil)
              ('t ;; all-completions
               (let ((ht (make-hash-table :test #'equal))
                     (candidates nil))
                 (cl-loop for (main table-pred table cand-plist) in tables do
                          (let* ((pr (if (and table-pred pred)
                                         (lambda (x) (and (funcall table-pred x) (funcall pred x)))
                                       (or table-pred pred)))
                                 (md (completion-metadata "" table pr))
                                 (sort (or (completion-metadata-get md 'display-sort-function)
                                           #'identity))
                                 ;; Always compute candidates of the main Capf
                                 ;; tables, which come first in the tables
                                 ;; list. For the :with Capfs only compute
                                 ;; candidates if we've already determined that
                                 ;; main candidates are available.
                                 (cands (when (or main (or exclusive cand-ht candidates))
                                          (funcall sort (all-completions str table pr)))))
                            ;; Handle duplicates with a hash table.
                            (cl-loop
                             for cand in-ref cands
                             for dup = (gethash cand ht t) do
                             (cond
                              ((eq dup t)
                               ;; Candidate does not yet exist.
                               (puthash cand cand-plist ht))
                              ((not (equal dup cand-plist))
                               ;; Duplicate candidate. Candidate plist is
                               ;; different, therefore disambiguate the
                               ;; candidates.
                               (setf cand (propertize cand 'cape-capf-super
                                                      (cons cand cand-plist))))))
                            (when cands (push cands candidates))))
                 (when (or cand-ht candidates)
                   (setq candidates (apply #'nconc (nreverse candidates))
                         cand-ht ht)
                   candidates)))
              (_ ;; try-completion and test-completion
               (cl-loop for (_main table-pred table _cand-plist) in tables thereis
                        (complete-with-action
                         action table str
                         (if (and table-pred pred)
                             (lambda (x) (and (funcall table-pred x) (funcall pred x)))
                           (or table-pred pred)))))))
         :company-prefix-length ,prefix-len
         :category cape-super
         :display-sort-function identity
         :cycle-sort-function identity
         ,@(and (not exclusive) '(:exclusive no))
         ,@(mapcan
            (lambda (prop)
              (list prop
                    (lambda (cand &rest args)
                      (if-let ((ref (get-text-property 0 'cape-capf-super cand)))
                          (when-let ((fun (plist-get (cdr ref) prop)))
                            (apply fun (car ref) args))
                        (when-let ((plist (and cand-ht (gethash cand cand-ht)))
                                   (fun (plist-get plist prop)))
                          (apply fun cand args))))))
            cand-functions)))))

;;;###autoload
(defun cape-wrap-debug (capf &optional name)
  "Call CAPF and return a completion table which prints trace messages.
If CAPF is an anonymous lambda, pass the Capf NAME explicitly for
meaningful debugging output."
  (unless name
    (setq name (if (symbolp capf) capf "capf")))
  (setq name (format "%s@%s" name (cl-incf cape--debug-id)))
  (pcase (funcall capf)
    (`(,beg ,end ,table . ,plist)
     (let* ((limit (1+ cape--debug-length))
            (pred (plist-get plist :predicate))
            (cands
             ;; Reset regexps for `all-completions'
             (let (completion-ignore-case completion-regexp-list)
               (all-completions
                "" table
                (lambda (&rest args)
                  (and (or (not pred) (apply pred args)) (>= (cl-decf limit) 0))))))
            (plist-str "")
            (plist-elt plist))
       (while (cdr plist-elt)
         (setq plist-str (format "%s %s=%s" plist-str
                                 (substring (symbol-name (car plist-elt)) 1)
                                 (cape--debug-print (cadr plist-elt)))
               plist-elt (cddr plist-elt)))
       (cape--debug-message
        "%s => input=%s:%s:%S table=%s%s"
        name (+ beg 0) (+ end 0) (buffer-substring-no-properties beg end)
        (cape--debug-print cands)
        plist-str))
     `( ,beg ,end
        ,(cape--debug-table
          table name (copy-marker beg) (copy-marker end t))
        ,@(when-let ((exit (plist-get plist :exit-function)))
            (list :exit-function
                  (lambda (cand status)
                    (cape--debug-message "%s:exit(candidate=%S status=%s)"
                                         name cand status)
                    (funcall exit cand status))))
        . ,plist))
    (result
     (cape--debug-message "%s() => %s (No completion)"
                          name (cape--debug-print result)))))

;;;###autoload
(defun cape-wrap-buster (capf &optional valid)
  "Call CAPF and return a completion table with cache busting.
This function can be used as an advice around an existing Capf.
The cache is busted when the input changes.  The argument VALID
can be a function taking the old and new input string.  It should
return nil if the new input requires that the completion table is
refreshed.  The default value for VALID is `equal', such that the
completion table is refreshed on every input change."
  (setq valid (or valid #'equal))
  (pcase (funcall capf)
    (`(,beg ,end ,table . ,plist)
     (setq plist `(:cape--buster t . ,plist))
     `( ,beg ,end
        ,(let* ((beg (copy-marker beg))
                (end (copy-marker end t))
                (input (buffer-substring-no-properties beg end)))
           (lambda (str pred action)
             (let ((new-input (buffer-substring-no-properties beg end)))
               (unless (or (not (eq action t))
                           (cape--separator-p new-input)
                           (funcall valid input new-input))
                 (pcase
                     ;; Reset in case `all-completions' is used inside CAPF
                     (let (completion-ignore-case completion-regexp-list)
                       (funcall capf))
                   ((and `(,new-beg ,new-end ,new-table . ,new-plist)
                         (guard (and (= beg new-beg) (= end new-end))))
                    (let (throw-on-input) ;; No interrupt during state update
                      (setf table new-table
                            input new-input
                            (cddr plist) new-plist))))))
             (complete-with-action action table str pred)))
        ,@plist))))

;;;###autoload
(defun cape-wrap-passthrough (capf)
  "Call CAPF and make sure that no completion style filtering takes place."
  (pcase (funcall capf)
    (`(,beg ,end ,table . ,plist)
     `(,beg ,end ,(cape--passthrough-table table) ,@plist))))

;;;###autoload
(defun cape-wrap-properties (capf &rest properties)
  "Call CAPF and strip or add completion PROPERTIES.
Completion properties include for example :exclusive, :category,
:annotation-function, :display-sort-function and various :company-*
extensions.  The :strip flag means to strip all completion properties."
  (pcase (funcall capf)
    (`(,beg ,end ,table . ,plist)
     `( ,beg ,end ,table
        ,@(and (not (plist-get properties :strip))
               (append properties plist))))))

;;;###autoload
(defun cape-wrap-nonexclusive (capf)
  "Call CAPF and ensure that it is marked as non-exclusive.
This function can be used as an advice around an existing Capf."
  (cape-wrap-properties capf :exclusive 'no))

;;;###autoload
(defun cape-wrap-sort (capf sort)
  "Call CAPF and add SORT function.
This function can be used as an advice around an existing Capf."
  (cape-wrap-properties
   capf
   :display-sort-function sort
   :cycle-sort-function sort))

;;;###autoload
(defun cape-wrap-predicate (capf predicate)
  "Call CAPF and add an additional candidate PREDICATE.
The PREDICATE is passed the candidate symbol or string."
  (pcase (funcall capf)
    (`(,beg ,end ,table . ,plist)
     `( ,beg ,end ,table
        :predicate
        ,(if-let (pred (plist-get plist :predicate))
             ;; First argument is key, second is value for hash tables.
             ;; The first argument can be a cons cell for alists. Then
             ;; the candidate itself is either a string or a symbol. We
             ;; normalize the calling convention here such that PREDICATE
             ;; always receives a string or a symbol.
             (lambda (&rest args)
               (when (apply pred args)
                 (setq args (car args))
                 (funcall predicate (if (consp args) (car args) args))))
           (lambda (key &optional _val)
             (funcall predicate (if (consp key) (car key) key))))
        ,@plist))))

;;;###autoload
(defun cape-wrap-silent (capf)
  "Call CAPF and silence it (no messages, no errors).
This function can be used as an advice around an existing Capf."
  (pcase (cape--silent (funcall capf))
    (`(,beg ,end ,table . ,plist)
     `(,beg ,end ,(cape--silent-table table) ,@plist))))

;;;###autoload
(defun cape-wrap-case-fold (capf &optional nofold)
  "Call CAPF and return a case-insensitive completion table.
If NOFOLD is non-nil return a case sensitive table instead.  This
function can be used as an advice around an existing Capf."
  (pcase (funcall capf)
    (`(,beg ,end ,table . ,plist)
     `(,beg ,end ,(completion-table-case-fold table nofold) ,@plist))))

;;;###autoload
(defun cape-wrap-noninterruptible (capf)
  "Call CAPF and return a non-interruptible completion table.
This function can be used as an advice around an existing Capf."
  (pcase (let (throw-on-input) (funcall capf))
    (`(,beg ,end ,table . ,plist)
     `(,beg ,end ,(cape--noninterruptible-table table) ,@plist))))

;;;###autoload
(defun cape-wrap-prefix-length (capf length)
  "Call CAPF and ensure that prefix length is greater or equal than LENGTH.
If the prefix is long enough, enforce auto completion."
  (pcase (funcall capf)
    (`(,beg ,end ,table . ,plist)
     (when (>= (- end beg) length)
       `(,beg ,end ,table :company-prefix-length t ,@plist)))))

;;;###autoload
(defun cape-wrap-inside-faces (capf &rest faces)
  "Call CAPF only if inside FACES.
This function can be used as an advice around an existing Capf."
  (when-let (((> (point) (point-min)))
             (fs (get-text-property (1- (point)) 'face))
             ((if (listp fs)
                  (cl-loop for f in fs thereis (memq f faces))
                (memq fs faces))))
    (funcall capf)))

;;;###autoload
(defun cape-wrap-inside-code (capf)
  "Call CAPF only if inside code, not inside a comment or string.
This function can be used as an advice around an existing Capf."
  (let ((s (syntax-ppss)))
    (and (not (nth 3 s)) (not (nth 4 s)) (funcall capf))))

;;;###autoload
(defun cape-wrap-inside-comment (capf)
  "Call CAPF only if inside comment.
This function can be used as an advice around an existing Capf."
  (and (nth 4 (syntax-ppss)) (funcall capf)))

;;;###autoload
(defun cape-wrap-inside-string (capf)
  "Call CAPF only if inside string.
This function can be used as an advice around an existing Capf."
  (and (nth 3 (syntax-ppss)) (funcall capf)))

;;;###autoload
(defun cape-wrap-purify (capf)
  "Call CAPF and ensure that it does not illegally modify the buffer.
This function can be used as an advice around an existing
Capf.  It has been introduced mainly to fix the broken
`pcomplete-completions-at-point' function in Emacs versions < 29."
  ;; bug#50470: Fix Capfs which illegally modify the buffer or which illegally
  ;; call `completion-in-region'.  The workaround here was proposed by
  ;; @jakanakaevangeli and is used in his capf-autosuggest package.  In Emacs 29
  ;; the purity bug of Pcomplete has been fixed, such that make
  ;; `cape-wrap-purify' is not necessary anymore.
  (catch 'cape--illegal-completion-in-region
    (condition-case nil
        (let ((buffer-read-only t)
              (inhibit-read-only nil)
              (completion-in-region-function
               (lambda (beg end coll pred)
                 (throw 'cape--illegal-completion-in-region
                        (list beg end coll :predicate pred)))))
          (funcall capf))
      (buffer-read-only nil))))

;;;###autoload
(defun cape-wrap-accept-all (capf)
  "Call CAPF and return a completion table which accepts every input.
This function can be used as an advice around an existing Capf."
  (pcase (funcall capf)
    (`(,beg ,end ,table . ,plist)
     `(,beg ,end ,(cape--accept-all-table table) . ,plist))))

;;;###autoload (autoload 'cape-capf-accept-all "cape")
;;;###autoload (autoload 'cape-capf-buster "cape")
;;;###autoload (autoload 'cape-capf-case-fold "cape")
;;;###autoload (autoload 'cape-capf-debug "cape")
;;;###autoload (autoload 'cape-capf-inside-code "cape")
;;;###autoload (autoload 'cape-capf-inside-comment "cape")
;;;###autoload (autoload 'cape-capf-inside-faces "cape")
;;;###autoload (autoload 'cape-capf-inside-string "cape")
;;;###autoload (autoload 'cape-capf-nonexclusive "cape")
;;;###autoload (autoload 'cape-capf-noninterruptible "cape")
;;;###autoload (autoload 'cape-capf-passthrough "cape")
;;;###autoload (autoload 'cape-capf-predicate "cape")
;;;###autoload (autoload 'cape-capf-prefix-length "cape")
;;;###autoload (autoload 'cape-capf-properties "cape")
;;;###autoload (autoload 'cape-capf-purify "cape")
;;;###autoload (autoload 'cape-capf-silent "cape")
;;;###autoload (autoload 'cape-capf-super "cape")

(dolist (wrapper (list #'cape-wrap-accept-all #'cape-wrap-buster
                       #'cape-wrap-case-fold #'cape-wrap-debug
                       #'cape-wrap-inside-code #'cape-wrap-inside-comment
                       #'cape-wrap-inside-faces #'cape-wrap-inside-string
                       #'cape-wrap-nonexclusive #'cape-wrap-noninterruptible
                       #'cape-wrap-passthrough #'cape-wrap-predicate
                       #'cape-wrap-prefix-length #'cape-wrap-properties
                       #'cape-wrap-purify #'cape-wrap-silent
                       #'cape-wrap-sort #'cape-wrap-super))
  (let ((name (string-remove-prefix "cape-wrap-" (symbol-name wrapper))))
    (defalias (intern (format "cape-capf-%s" name))
      (lambda (capf &rest args) (lambda () (apply wrapper capf args)))
      (format "Create a %s Capf from CAPF.
The Capf calls `%s' with CAPF and ARGS as arguments." name wrapper))))

(defvar-keymap cape-prefix-map
  :doc "Keymap used as completion entry point.
The keymap should be installed globally under a prefix."
  "TAB" #'completion-at-point
  "M-TAB" #'completion-at-point
  "p" #'completion-at-point
  "t" #'complete-tag
  "d" #'cape-dabbrev
  "h" #'cape-history
  "f" #'cape-file
  "s" #'cape-elisp-symbol
  "e" #'cape-elisp-block
  "a" #'cape-abbrev
  "l" #'cape-line
  "w" #'cape-dict
  "k"  'cape-keyword
  ":"  'cape-emoji
  "\\" 'cape-tex
  "_"  'cape-tex
  "^"  'cape-tex
  "&"  'cape-sgml
  "r"  'cape-rfc1345)

;;;###autoload (autoload 'cape-prefix-map "cape" nil t 'keymap)
(defalias 'cape-prefix-map cape-prefix-map)

(provide 'cape)
;;; cape.el ends here
