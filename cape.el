;;; cape.el --- Completion At Point Extensions -*- lexical-binding: t -*-

;; Copyright (C) 2021-2023 Free Software Foundation, Inc.

;; Author: Daniel Mendler <mail@daniel-mendler.de>
;; Maintainer: Daniel Mendler <mail@daniel-mendler.de>
;; Created: 2021
;; Version: 0.12
;; Package-Requires: ((emacs "27.1") (compat "29.1.3.0"))
;; Homepage: https://github.com/minad/cape

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
;; cape-dabbrev: Complete word from current buffers
;; cape-file: Complete file name
;; cape-history: Complete from Eshell, Comint or minibuffer history
;; cape-keyword: Complete programming language keyword
;; cape-symbol: Complete Elisp symbol
;; cape-abbrev: Complete abbreviation (add-global-abbrev, add-mode-abbrev)
;; cape-ispell: Complete word from Ispell dictionary
;; cape-dict: Complete word from dictionary file
;; cape-line: Complete entire line from file
;; cape-tex: Complete unicode char from TeX command, e.g. \hbar.
;; cape-sgml: Complete unicode char from Sgml entity, e.g., &alpha.
;; cape-rfc1345: Complete unicode char using RFC 1345 mnemonics.

;;; Code:

(require 'compat)
(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

(autoload 'thing-at-point-looking-at "thingatpt")

;;;; Customization

(defgroup cape nil
  "Completion At Point Extensions."
  :link '(info-link :tag "Info Manual" "(cape)")
  :link '(url-link :tag "Homepage" "https://github.com/minad/cape")
  :link '(emacs-library-link :tag "Library Source" "cape.el")
  :group 'convenience
  :group 'tools
  :group 'matching
  :prefix "cape-")

(defcustom cape-dict-file "/usr/share/dict/words"
  "Dictionary word list file."
  :type 'string)

(defcustom cape-dabbrev-min-length 4
  "Minimum length of dabbrev expansions.
This setting ensures that words which are too short
are not offered as completion candidates, such that
auto completion does not pop up too aggressively."
  :type 'integer)

(defcustom cape-dabbrev-check-other-buffers t
  "Buffers to check for dabbrev.

If t, check all other buffers (subject to dabbrev ignore rules).
Any other non-nil value only checks some other buffers, as per
`dabbrev-select-buffers-function'."
  :type '(choice (const :tag "off" nil)
                 (const :tag "some" some)
                 (other :tag "all" t)))

(defcustom cape-file-directory nil
  "Base directory used by `cape-file."
  :type '(choice (const nil) string function))

(defcustom cape-file-directory-must-exist t
  "The parent directory must exist for file completion."
  :type 'boolean)

(defcustom cape-line-buffer-function #'cape--buffers-major-mode
  "Function which returns list of buffers.
The buffers are scanned for completion candidates by `cape-line'."
  :type '(choice (const :tag "Current buffer" current-buffer)
                 (const :tag "All buffers" buffer-list)
                 (const :tag "Buffers with same major mode" cape--buffers-major-mode)
                 (function :tag "Custom function")))

(defcustom cape-symbol-wrapper
  '((org-mode ?= ?=)
    (markdown-mode ?` ?`)
    (rst-mode "``" "``")
    (log-edit-mode "`" "'")
    (message-mode "`" "'")
    (rcirc-mode "`" "'"))
  "Wrapper characters for symbols."
  :type '(alist :key-type symbol :value-type (list (choice character string)
                                                   (choice character string))))

;;;; Helpers

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

(cl-defun cape--table-with-properties (table &key category (sort t) &allow-other-keys)
  "Create completion TABLE with properties.
CATEGORY is the optional completion category.
SORT should be nil to disable sorting."
  (if (or (not table) (and (not category) sort))
      table
    (let ((metadata `(metadata
                      ,@(and category `((category . ,category)))
                      ,@(and (not sort) '((display-sort-function . identity)
                                          (cycle-sort-function . identity))))))
      (lambda (str pred action)
        (if (eq action 'metadata)
            metadata
          (complete-with-action action table str pred))))))

(defun cape--input-valid-p (old-input new-input cmp)
  "Return non-nil if the NEW-INPUT is valid in comparison to OLD-INPUT.
The CMP argument determines how the new input is compared to the old input.
- never: Never treat the input as valid.
- prefix/nil: The old input is a prefix of the new input.
- equal: The old input is equal to the new input.
- substring: The old input is a substring of the new input."
  ;; Treat input as not changed if it contains space to allow
  ;; Orderless completion style filtering.
  (or (string-match-p "\\s-" new-input)
      (pcase-exhaustive cmp
        ('never nil)
        ((or 'prefix 'nil) (string-prefix-p old-input new-input))
        ('equal (equal old-input new-input))
        ('substring (string-search old-input new-input)))))

(defun cape--cached-table (beg end fun valid)
  "Create caching completion table.
BEG and END are the input bounds.
FUN is the function which computes the candidates.
VALID is the input comparator, see `cape--input-valid-p'."
  (let ((input 'init)
        (beg (copy-marker beg))
        (end (copy-marker end t))
        (table nil))
    (lambda (str pred action)
      ;; Bail out early for `metadata' and `boundaries'. This is a pointless
      ;; move because of caching, but we do it anyway in the hope that the
      ;; resulting profiler output looks less confusing, since the weight of the
      ;; expensive FUN computation is moved to the `all-completions' action.
      ;; Computing `all-completions' must surely be most expensive, so nobody
      ;; will suspect a thing.
      (unless (or (eq action 'metadata) (eq (car-safe action) 'boundaries))
        (let ((new-input (buffer-substring-no-properties beg end)))
          (when (or (eq input 'init)
                    (not (cape--input-valid-p input new-input valid)))
            ;; We have to make sure that the completion table is interruptible.
            ;; An interruption should not happen between the setqs.
            (setq table (funcall fun new-input)
                  input new-input)))
        (complete-with-action action table str pred)))))

;;;; Capfs

;;;;; cape-history

(declare-function ring-elements "ring")
(declare-function eshell-bol "eshell")
(declare-function comint-bol "comint")
(defvar eshell-history-ring)
(defvar comint-input-ring)

(defvar cape--history-properties
  (list :company-kind (lambda (_) 'text)
        :exclusive 'no)
  "Completion extra properties for `cape-history'.")

;;;###autoload
(defun cape-history (&optional interactive)
  "Complete from Eshell, Comint or minibuffer history.
See also `consult-history' for a more flexible variant based on
`completing-read'. If INTERACTIVE is nil the function acts like a Capf."
  (interactive (list t))
  (if interactive
      (cape-interactive #'cape-history)
    (let (history bol)
      (cond
       ((derived-mode-p 'eshell-mode)
        (setq history eshell-history-ring
              bol (save-excursion (eshell-bol) (point))))
       ((derived-mode-p 'comint-mode)
        (setq history comint-input-ring
              bol (save-excursion (comint-bol) (point))))
       ((and (minibufferp) (not (eq minibuffer-history-variable t)))
        (setq history (symbol-value minibuffer-history-variable)
              bol (line-beginning-position))))
      (when (ring-p history)
        (setq history (ring-elements history)))
      (when history
        `(,bol ,(point)
          ,(cape--table-with-properties history :sort nil)
          ,@cape--history-properties)))))

;;;;; cape-file

(defvar cape--file-properties
  (list :annotation-function (lambda (s) (if (string-suffix-p "/" s) " Dir" " File"))
        :company-kind (lambda (s) (if (string-suffix-p "/" s) 'folder 'file))
        :exclusive 'no)
  "Completion extra properties for `cape-file'.")

;;;###autoload
(defun cape-file (&optional interactive)
  "Complete file name at point.
See the user option `cape-file-directory-must-exist'.
If INTERACTIVE is nil the function acts like a Capf."
  (interactive (list t))
  (if interactive
      (let (cape-file-directory-must-exist)
        (cape-interactive #'cape-file))
    (let* ((default-directory (pcase cape-file-directory
                                ('nil default-directory)
                                ((pred stringp) cape-file-directory)
                                (_ (funcall cape-file-directory))))
           (bounds (cape--bounds 'filename))
           (non-essential t)
           (file (buffer-substring (car bounds) (cdr bounds)))
           ;; Support org links globally, see `org-open-at-point-global'.
           (org (string-prefix-p "file:" file)))
      (when org (setcar bounds (+ 5 (car bounds))))
      (when (or org
                (not cape-file-directory-must-exist)
                (and (string-search "/" file)
                     (file-exists-p (file-name-directory file))))
        `(,(car bounds) ,(cdr bounds)
          ,(cape--nonessential-table #'read-file-name-internal)
          ,@(when (or org (string-match-p "./" file))
              '(:company-prefix-length t))
          ,@cape--file-properties)))))

;;;;; cape-symbol

(defvar cape--symbol-properties
  (append
   (list :annotation-function #'cape--symbol-annotation
         :exit-function #'cape--symbol-exit
         :predicate #'cape--symbol-predicate
         :exclusive 'no)
   (when (eval-when-compile (>= emacs-major-version 28))
     (autoload 'elisp--company-kind "elisp-mode")
     (autoload 'elisp--company-doc-buffer "elisp-mode")
     (autoload 'elisp--company-doc-string "elisp-mode")
     (autoload 'elisp--company-location "elisp-mode")
     (list :company-kind 'elisp--company-kind
           :company-doc-buffer 'elisp--company-doc-buffer
           :company-docsig 'elisp--company-doc-string
           :company-location 'elisp--company-location)))
  "Completion extra properties for `cape-symbol'.")

(defun cape--symbol-predicate (sym)
  "Return t if SYM is bound, fbound or propertized."
  (or (fboundp sym) (boundp sym) (symbol-plist sym)))

(defun cape--symbol-exit (name status)
  "Wrap symbol NAME with `cape-symbol-wrapper' buffers.
STATUS is the exit status."
  (when-let (((not (eq status 'exact)))
             (c (cl-loop for (m . c) in cape-symbol-wrapper
                         if (derived-mode-p m) return c)))
    (save-excursion
      (backward-char (length name))
      (insert (car c)))
    (insert (cadr c))))

(defun cape--symbol-annotation (sym)
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
(defun cape-symbol (&optional interactive)
  "Complete Elisp symbol at point.
If INTERACTIVE is nil the function acts like a Capf."
  (interactive (list t))
  (if interactive
      (cape-interactive #'cape-symbol)
    (pcase-let ((`(,beg . ,end) (cape--bounds 'symbol)))
      (when (eq (char-after beg) ?')
        (setq beg (1+ beg) end (max beg end)))
      `(,beg ,end
        ,(cape--table-with-properties obarray :category 'symbol)
        ,@cape--symbol-properties))))

;;;;; cape-dabbrev

(defvar cape--dabbrev-properties
  (list :annotation-function (lambda (_) " Dabbrev")
        :company-kind (lambda (_) 'text)
        :exclusive 'no)
  "Completion extra properties for `cape-dabbrev'.")

(defvar dabbrev-check-all-buffers)
(defvar dabbrev-check-other-buffers)
(declare-function dabbrev--ignore-case-p "dabbrev")
(declare-function dabbrev--find-all-expansions "dabbrev")
(declare-function dabbrev--reset-global-variables "dabbrev")

;;;###autoload
(defun cape-dabbrev (&optional interactive)
  "Complete with Dabbrev at point.

If INTERACTIVE is nil the function acts like a Capf. In case you
observe a performance issue with autocompletion and `cape-dabbrev'
it is strongly recommended to disable scanning in other buffers.
See the user options `cape-dabbrev-min-length' and
`cape-dabbrev-check-other-buffers'."
  (interactive (list t))
  (if interactive
      (let ((cape-dabbrev-min-length 0))
        (cape-interactive #'cape-dabbrev))
    (when (thing-at-point-looking-at "\\(?:\\sw\\|\\s_\\)+")
      (let ((beg (match-beginning 0))
            (end (match-end 0)))
        `(,beg ,end
          ,(cape--table-with-properties
            (cape--cached-table beg end
                                #'cape--dabbrev-list
                                ;; TODO: Use equal, if candidates must be longer than cape-dabbrev-min-length.
                                ;;(if (> cape-dabbrev-min-length 0) 'equal 'prefix)
                                ;; Problem is that when entering more input, candidates get lost!
                                'prefix)
            :category 'cape-dabbrev)
          ,@cape--dabbrev-properties)))))

(defun cape--dabbrev-list (word)
  "Find all dabbrev expansions for WORD."
  (require 'dabbrev)
  (cape--silent
    (let ((dabbrev-check-other-buffers (not (null cape-dabbrev-check-other-buffers)))
          (dabbrev-check-all-buffers (eq cape-dabbrev-check-other-buffers t)))
      (dabbrev--reset-global-variables))
    (cl-loop with min-len = (+ cape-dabbrev-min-length (length word))
             for w in (dabbrev--find-all-expansions word (dabbrev--ignore-case-p word))
             if (>= (length w) min-len) collect w)))

;;;;; cape-ispell

(defvar cape--ispell-properties
  (list :annotation-function (lambda (_) " Ispell")
        :company-kind (lambda (_) 'text)
        :exclusive 'no)
  "Completion extra properties for `cape-ispell'.")

(declare-function ispell-lookup-words "ispell")
(defun cape--ispell-words (str)
  "Return all words from Ispell matching STR."
  (with-demoted-errors "Ispell Error: %S"
    (require 'ispell)
    (cape--silent (ispell-lookup-words (format "*%s*" str)))))

;;;###autoload
(defun cape-ispell (&optional interactive)
  "Complete word at point with Ispell.
If INTERACTIVE is nil the function acts like a Capf."
  (interactive (list t))
  (if interactive
      (cape-interactive #'cape-ispell)
    (let ((bounds (cape--bounds 'word)))
      `(,(car bounds) ,(cdr bounds)
        ,(cape--table-with-properties
          (cape--cached-table (car bounds) (cdr bounds) #'cape--ispell-words 'substring)
          :category 'cape-ispell)
        ,@cape--ispell-properties))))

;;;;; cape-dict

(defvar cape--dict-properties
  (list :annotation-function (lambda (_) " Dict")
        :company-kind (lambda (_) 'text)
        :exclusive 'no)
  "Completion extra properties for `cape-dict'.")

(defvar cape--dict-words nil)
(defun cape--dict-words ()
  "Dictionary words."
  (or cape--dict-words
      (setq cape--dict-words
            (split-string (with-temp-buffer
                            (insert-file-contents cape-dict-file)
                            (buffer-string))
                          "\n" 'omit-nulls))))

;;;###autoload
(defun cape-dict (&optional interactive)
  "Complete word from dictionary at point.
See the custom option `cape-dict-file'.
If INTERACTIVE is nil the function acts like a Capf."
  (interactive (list t))
  (if interactive
      (cape-interactive #'cape-dict)
    (let ((bounds (cape--bounds 'word)))
      `(,(car bounds) ,(cdr bounds)
        ,(cape--table-with-properties (cape--dict-words) :category 'cape-dict)
        ,@cape--dict-properties))))

;;;;; cape-abbrev

(defun cape--abbrev-tables ()
  "Return list of all active abbrev tables, including parents."
  ;; Emacs 28: See abbrev--suggest-get-active-tables-including-parents.
  (let ((tables (abbrev--active-tables)))
    (append tables (cl-loop for table in tables
                            append (abbrev-table-get table :parents)))))

(defun cape--abbrev-list ()
  "Abbreviation list."
  (delete "" (cl-loop for table in (cape--abbrev-tables)
                      nconc (all-completions "" table))))

(defun cape--abbrev-annotation (abbrev)
  "Annotate ABBREV with expansion."
  (concat " "
          (truncate-string-to-width
           (format
            "%s"
            (symbol-value
             (cl-loop for table in (cape--abbrev-tables)
                      thereis (abbrev--symbol abbrev table))))
           30 0 nil t)))

(defun cape--abbrev-exit (_str status)
  "Expand expansion if STATUS is not exact."
  (unless (eq status 'exact)
    (expand-abbrev)))

(defvar cape--abbrev-properties
  (list :annotation-function #'cape--abbrev-annotation
        :exit-function #'cape--abbrev-exit
        :company-kind (lambda (_) 'snippet)
        :exclusive 'no)
  "Completion extra properties for `cape-abbrev'.")

;;;###autoload
(defun cape-abbrev (&optional interactive)
  "Complete abbreviation at point.
If INTERACTIVE is nil the function acts like a Capf."
  (interactive (list t))
  (if interactive
      ;; NOTE: Disable cycling since abbreviation replacement breaks it.
      (let (completion-cycle-threshold)
        (cape-interactive #'cape-abbrev))
    (when-let (abbrevs (cape--abbrev-list))
      (let ((bounds (cape--bounds 'symbol)))
        `(,(car bounds) ,(cdr bounds)
          ,(cape--table-with-properties abbrevs :category 'cape-abbrev)
          ,@cape--abbrev-properties)))))

;;;;; cape-line

(defvar cape--line-properties nil
  "Completion extra properties for `cape-line'.")

(defun cape--buffers-major-mode ()
  "Return buffers with same major mode as current buffer."
  (cl-loop for buf in (buffer-list)
           if (eq major-mode (buffer-local-value 'major-mode buf))
           collect buf))

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
    `(,(pos-bol) ,(point)
      ,(cape--table-with-properties (cape--line-list) :sort nil)
      ,@cape--line-properties)))

;;;; Capf combinators

;;;###autoload
(defun cape-super-capf (&rest capfs)
  "Merge CAPFS and return new Capf which includes all candidates."
  (lambda ()
    (when-let (results (delq nil (mapcar #'funcall capfs)))
      (pcase-let* ((`((,beg ,end . ,_)) results)
                   (cand-ht (make-hash-table :test #'equal))
                   (extra-fun
                    (lambda (prop)
                      (lambda (cand &rest args)
                        (when-let (fun (plist-get (gethash cand cand-ht) prop))
                          (apply fun cand args)))))
                   (tables nil)
                   (prefix-len nil))
        (cl-loop for (beg2 end2 . rest) in results do
                 (when (and (= beg beg2) (= end end2))
                   (push rest tables)
                   (let ((plen (plist-get (cdr rest) :company-prefix-length)))
                     (cond
                      ((eq plen t)
                       (setq prefix-len t))
                      ((and (not prefix-len) (integerp plen))
                       (setq prefix-len plen))
                      ((and (integerp prefix-len) (integerp plen))
                       (setq prefix-len (max prefix-len plen)))))))
        (setq tables (nreverse tables))
        (list beg end
              (lambda (str pred action)
                (pcase action
                  (`(boundaries . ,_) nil)
                  ('metadata
                   '(metadata (category . cape-super)
                              (display-sort-function . identity)
                              (cycle-sort-function . identity)))
                  ('t
                   (let ((ht (make-hash-table :test #'equal))
                         (candidates nil))
                     (cl-loop for (table . plist) in tables do
                              (let* ((pr (if-let (pr (plist-get plist :predicate))
                                             (if pred
                                                 (lambda (x) (and (funcall pr x) (funcall pred x)))
                                               pr)
                                           pred))
                                     (md (completion-metadata "" table pr))
                                     (sort (or (completion-metadata-get md 'display-sort-function)
                                               #'identity))
                                     (cands (funcall sort (all-completions str table pr))))
                                (cl-loop for cell on cands
                                         for cand = (car cell) do
                                         (if (eq (gethash cand ht t) t)
                                             (puthash cand plist ht)
                                           (setcar cell nil)))
                                (setq candidates (nconc candidates cands))))
                     (setq cand-ht ht)
                     (delq nil candidates)))
                  (_
                   (completion--some
                    (pcase-lambda (`(,table . ,plist))
                      (complete-with-action
                       action table str
                       (if-let (pr (plist-get plist :predicate))
                           (if pred
                               (lambda (x) (and (funcall pr x) (funcall pred x)))
                             pr)
                         pred)))
                    tables))))
              :exclusive 'no
              :company-prefix-length prefix-len
              :company-doc-buffer (funcall extra-fun :company-doc-buffer)
              :company-location (funcall extra-fun :company-location)
              :company-docsig (funcall extra-fun :company-docsig)
              :company-deprecated (funcall extra-fun :company-deprecated)
              :company-kind (funcall extra-fun :company-kind)
              :annotation-function (funcall extra-fun :annotation-function)
              :exit-function (funcall extra-fun :exit-function))))))

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

;;;###autoload
(defun cape-company-to-capf (backend &optional valid)
  "Convert Company BACKEND function to Capf.
VALID is the input comparator, see `cape--input-valid-p'.
This feature is experimental."
  (let ((init (make-variable-buffer-local (make-symbol "cape--company-init"))))
    (lambda ()
      (when (and (symbolp backend) (not (fboundp backend)))
        (ignore-errors (require backend nil t)))
      (unless (symbol-value init)
        (cape--company-call backend 'init)
        (set init t))
      (when-let ((prefix (cape--company-call backend 'prefix))
                 (initial-input (if (stringp prefix) prefix (car-safe prefix))))
        (let* ((end (point)) (beg (- end (length initial-input)))
               (dups (cape--company-call backend 'duplicates))
               candidates)
          (list beg end
                (funcall
                 (if (cape--company-call backend 'ignore-case)
                     #'completion-table-case-fold
                   #'identity)
                 (cape--table-with-properties
                  (cape--cached-table
                   beg end
                   (lambda (input)
                     (setq candidates (cape--company-call backend 'candidates input))
                     (when dups (setq candidates (delete-dups candidates)))
                     candidates)
                   (if (cape--company-call backend 'no-cache initial-input)
                       'never valid))
                  :category backend
                  :sort (not (cape--company-call backend 'sorted))))
                :exclusive 'no
                :company-prefix-length (cdr-safe prefix)
                :company-doc-buffer (lambda (x) (cape--company-call backend 'doc-buffer x))
                :company-location (lambda (x) (cape--company-call backend 'location x))
                :company-docsig (lambda (x) (cape--company-call backend 'meta x))
                :company-deprecated (lambda (x) (cape--company-call backend 'deprecated x))
                :company-kind (lambda (x) (cape--company-call backend 'kind x))
                :annotation-function (lambda (x)
                                       (when-let (ann (cape--company-call backend 'annotation x))
                                         (if (string-match-p "^[ \t]" ann)
                                             ann
                                           (concat " " ann))))
                :exit-function
                (lambda (x _status)
                  (cape--company-call backend 'post-completion
                                      (or (car (member x candidates)) x)))))))))

;;;###autoload
(defun cape-interactive (&rest capfs)
  "Complete interactively with the given CAPFS."
  (let ((completion-at-point-functions capfs))
    (unless (completion-at-point)
      (user-error "%s: No completions"
                  (mapconcat (lambda (fun)
                               (if (symbolp fun)
                                   (symbol-name fun)
                                 "anonymous-capf"))
                             capfs ", ")))))

;;;###autoload
(defun cape-interactive-capf (capf)
  "Create interactive completion function from CAPF."
  (lambda (&optional interactive)
    (interactive (list t))
    (if interactive (cape-interactive capf) (funcall capf))))

;;;###autoload
(defun cape-wrap-buster (capf &optional valid)
  "Call CAPF and return a completion table with cache busting.
The cache is busted when the input changes, where VALID is the input
comparator, see `cape--input-valid-p'."
    (pcase (funcall capf)
      (`(,beg ,end ,table . ,plist)
       `(,beg ,end
              ,(let* ((beg (copy-marker beg))
                      (end (copy-marker end t))
                      (input (buffer-substring-no-properties beg end)))
                 (lambda (str pred action)
                   (let ((new-input (buffer-substring-no-properties beg end)))
                     (unless (cape--input-valid-p input new-input valid)
                       (pcase (funcall capf)
                         (`(,_beg ,_end ,new-table . ,_plist)
                          ;; NOTE: We have to make sure that the completion table is interruptible.
                          ;; An interruption should not happen between the setqs.
                          (setq table new-table input new-input)))))
                   (complete-with-action action table str pred)))
              ,@plist))))

;;;###autoload
(defun cape-wrap-properties (capf &rest properties)
  "Call CAPF and add additional completion PROPERTIES.
Completion properties include for example :exclusive, :annotation-function and
the various :company-* extensions. Furthermore a boolean :sort flag and a
completion :category symbol can be specified."
  (pcase (funcall capf)
    (`(,beg ,end ,table . ,plist)
     `(,beg ,end
            ,(apply #'cape--table-with-properties table properties)
            ,@properties ,@plist))))

;;;###autoload
(defun cape-wrap-nonexclusive (capf)
  "Call CAPF and ensure that it is marked as non-exclusive."
  (cape-wrap-properties capf :exclusive 'no))

;;;###autoload
(defun cape-wrap-predicate (capf predicate)
  "Call CAPF and add an additional candidate PREDICATE.
The PREDICATE is passed the candidate symbol or string."
  (pcase (funcall capf)
    (`(,beg ,end ,table . ,plist)
     `(,beg ,end ,table
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
  "Call CAPF and silence it (no messages, no errors)."
  (pcase (cape--silent (funcall capf))
    (`(,beg ,end ,table . ,plist)
     `(,beg ,end ,(cape--silent-table table) ,@plist))))

;;;###autoload
(defun cape-wrap-case-fold (capf &optional dont-fold)
  "Call CAPF and return a case insenstive completion table.
If DONT-FOLD is non-nil return a case sensitive table instead."
  (pcase (funcall capf)
    (`(,beg ,end ,table . ,plist)
     `(,beg ,end ,(completion-table-case-fold table dont-fold) ,@plist))))

;;;###autoload
(defun cape-wrap-noninterruptible (capf)
  "Call CAPF and return a non-interruptible completion table."
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
       `(,beg ,end ,table
         :company-prefix-length t
         ,@plist)))))

;;;###autoload
(defun cape-wrap-inside-comment (capf)
  "Call CAPF only if inside comment."
  (and (nth 4 (syntax-ppss)) (funcall capf)))

;;;###autoload
(defun cape-wrap-inside-string (capf)
  "Call CAPF only if inside string."
  (and (nth 3 (syntax-ppss)) (funcall capf)))

;;;###autoload
(defun cape-wrap-purify (capf)
  "Call CAPF and ensure that it does not modify the buffer."
  ;; bug#50470: Fix Capfs which illegally modify the buffer or which
  ;; illegally call `completion-in-region'. The workaround here has been
  ;; proposed @jakanakaevangeli in bug#50470 and is used in
  ;; @jakanakaevangeli's capf-autosuggest package.
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
  "Call CAPF and return a completion table which accepts every input."
  (pcase (funcall capf)
    (`(,beg ,end ,table . ,plist)
     `(,beg ,end ,(cape--accept-all-table table) . ,plist))))

(defmacro cape--capf-wrapper (wrapper)
  "Create a capf transformer for WRAPPER."
  `(defun ,(intern (format "cape-capf-%s" wrapper)) (&rest args)
     (lambda () (apply #',(intern (format "cape-wrap-%s" wrapper)) args))))

;;;###autoload (autoload 'cape-capf-accept-all "cape")
(cape--capf-wrapper accept-all)
;;;###autoload (autoload 'cape-capf-buster "cape")
(cape--capf-wrapper buster)
;;;###autoload (autoload 'cape-capf-case-fold "cape")
(cape--capf-wrapper case-fold)
;;;###autoload (autoload 'cape-capf-inside-comment "cape")
(cape--capf-wrapper inside-comment)
;;;###autoload (autoload 'cape-capf-inside-string "cape")
(cape--capf-wrapper inside-string)
;;;###autoload (autoload 'cape-capf-noninterruptible "cape")
(cape--capf-wrapper noninterruptible)
;;;###autoload (autoload 'cape-capf-nonexclusive "cape")
(cape--capf-wrapper nonexclusive)
;;;###autoload (autoload 'cape-capf-predicate "cape")
(cape--capf-wrapper predicate)
;;;###autoload (autoload 'cape-capf-prefix-length "cape")
(cape--capf-wrapper prefix-length)
;;;###autoload (autoload 'cape-capf-properties "cape")
(cape--capf-wrapper properties)
;;;###autoload (autoload 'cape-capf-purify "cape")
(cape--capf-wrapper purify)
;;;###autoload (autoload 'cape-capf-silent "cape")
(cape--capf-wrapper silent)

(provide 'cape)
;;; cape.el ends here
