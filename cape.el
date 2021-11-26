;;; cape.el --- Completion At Point Extensions -*- lexical-binding: t -*-

;; Author: Daniel Mendler
;; Created: 2021
;; License: GPL-3.0-or-later
;; Version: 0.2
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

;; Let your completions fly! This package provides additional completion
;; backends in the form of capfs.

;;; Code:

(eval-when-compile (require 'cl-lib))

(defgroup cape nil
  "Completion At Point Extensions."
  :group 'convenience
  :prefix "cape-")

(defcustom cape-dict-file "/etc/dictionaries-common/words"
  "Dictionary word list file."
  :type 'string)

(defcustom cape-company-async-timeout 1.0
  "Company asynchronous timeout."
  :type 'float)

(defcustom cape-company-async-wait 0.02
  "Company asynchronous busy waiting time."
  :type 'float)

(defcustom cape-dabbrev-min-length 4
  "Minimum length of dabbrev expansions."
  :type 'integer)

(defcustom cape-file-directory-must-exist t
  "The parent directory must exist for file completion."
  :type 'integer)

(defcustom cape-keywords
  ;; This variable was taken from company-keywords.el.
  ;; Please contribute corrections or additions to both Cape and Company.
  '((c++-mode ;; https://en.cppreference.com/w/cpp/keyword
     "alignas" "alignof" "and" "and_eq" "asm" "atomic_cancel" "atomic_commit"
     "atomic_noexcept" "auto" "bitand" "bitor" "bool" "break" "case" "catch"
     "char" "char16_t" "char32_t" "char8_t" "class" "co_await" "co_return"
     "co_yield" "compl" "concept" "const" "const_cast" "consteval" "constexpr"
     "constinit" "continue" "decltype" "default" "delete" "do" "double"
     "dynamic_cast" "else" "enum" "explicit" "export" "extern" "false" "final"
     "float" "for" "friend" "goto" "if" "import" "inline" "int" "long" "module"
     "mutable" "namespace" "new" "noexcept" "not" "not_eq" "nullptr" "operator"
     "or" "or_eq" "override" "private" "protected" "public" "reflexpr" "register"
     "reinterpret_cast" "requires" "return" "short" "signed" "sizeof" "static"
     "static_assert" "static_cast" "struct" "switch" "synchronized" "template"
     "this" "thread_local" "throw" "true" "try" "typedef" "typeid" "typename"
     "union" "unsigned" "using" "virtual" "void" "volatile" "wchar_t" "while"
     "xor" "xor_eq")
    (c-mode ;; https://en.cppreference.com/w/c/keyword
     "_Alignas" "_Alignof" "_Atomic" "_Bool" "_Complex" "_Generic" "_Imaginary"
     "_Noreturn" "_Static_assert" "_Thread_local"
     "auto" "break" "case" "char" "const" "continue" "default" "do"
     "double" "else" "enum" "extern" "float" "for" "goto" "if" "inline"
     "int" "long" "register" "restrict" "return" "short" "signed" "sizeof"
     "static" "struct" "switch" "typedef" "union" "unsigned" "void" "volatile"
     "while")
    (csharp-mode
     "abstract" "add" "alias" "as" "base" "bool" "break" "byte" "case"
     "catch" "char" "checked" "class" "const" "continue" "decimal" "default"
     "delegate" "do" "double" "else" "enum" "event" "explicit" "extern"
     "false" "finally" "fixed" "float" "for" "foreach" "get" "global" "goto"
     "if" "implicit" "in" "int" "interface" "internal" "is" "lock" "long"
     "namespace" "new" "null" "object" "operator" "out" "override" "params"
     "partial" "private" "protected" "public" "readonly" "ref" "remove"
     "return" "sbyte" "sealed" "set" "short" "sizeof" "stackalloc" "static"
     "string" "struct" "switch" "this" "throw" "true" "try" "typeof" "uint"
     "ulong" "unchecked" "unsafe" "ushort" "using" "value" "var" "virtual"
     "void" "volatile" "where" "while" "yield")
    (d-mode ;; http://www.digitalmars.com/d/2.0/lex.html
     "abstract" "alias" "align" "asm"
     "assert" "auto" "body" "bool" "break" "byte" "case" "cast" "catch"
     "cdouble" "cent" "cfloat" "char" "class" "const" "continue" "creal"
     "dchar" "debug" "default" "delegate" "delete" "deprecated" "do"
     "double" "else" "enum" "export" "extern" "false" "final" "finally"
     "float" "for" "foreach" "foreach_reverse" "function" "goto" "idouble"
     "if" "ifloat" "import" "in" "inout" "int" "interface" "invariant"
     "ireal" "is" "lazy" "long" "macro" "mixin" "module" "new" "nothrow"
     "null" "out" "override" "package" "pragma" "private" "protected"
     "public" "pure" "real" "ref" "return" "scope" "short" "static" "struct"
     "super" "switch" "synchronized" "template" "this" "throw" "true" "try"
     "typedef" "typeid" "typeof" "ubyte" "ucent" "uint" "ulong" "union"
     "unittest" "ushort" "version" "void" "volatile" "wchar" "while" "with")
    (f90-mode ;; f90.el
     "abs" "abstract" "achar" "acos" "adjustl" "adjustr" "aimag" "aint"
     "align" "all" "all_prefix" "all_scatter" "all_suffix" "allocatable"
     "allocate" "allocated" "and" "anint" "any" "any_prefix" "any_scatter"
     "any_suffix" "asin" "assign" "assignment" "associate" "associated"
     "asynchronous" "atan" "atan2" "backspace" "bind" "bit_size" "block"
     "btest" "c_alert" "c_associated" "c_backspace" "c_bool"
     "c_carriage_return" "c_char" "c_double" "c_double_complex" "c_f_pointer"
     "c_f_procpointer" "c_float" "c_float_complex" "c_form_feed" "c_funloc"
     "c_funptr" "c_horizontal_tab" "c_int" "c_int16_t" "c_int32_t" "c_int64_t"
     "c_int8_t" "c_int_fast16_t" "c_int_fast32_t" "c_int_fast64_t"
     "c_int_fast8_t" "c_int_least16_t" "c_int_least32_t" "c_int_least64_t"
     "c_int_least8_t" "c_intmax_t" "c_intptr_t" "c_loc" "c_long"
     "c_long_double" "c_long_double_complex" "c_long_long" "c_new_line"
     "c_null_char" "c_null_funptr" "c_null_ptr" "c_ptr" "c_short"
     "c_signed_char" "c_size_t" "c_vertical_tab" "call" "case" "ceiling"
     "char" "character" "character_storage_size" "class" "close" "cmplx"
     "command_argument_count" "common" "complex" "conjg" "contains" "continue"
     "copy_prefix" "copy_scatter" "copy_suffix" "cos" "cosh" "count"
     "count_prefix" "count_scatter" "count_suffix" "cpu_time" "cshift"
     "cycle" "cyclic" "data" "date_and_time" "dble" "deallocate" "deferred"
     "digits" "dim" "dimension" "distribute" "do" "dot_product" "double"
     "dprod" "dynamic" "elemental" "else" "elseif" "elsewhere" "end" "enddo"
     "endfile" "endif" "entry" "enum" "enumerator" "eoshift" "epsilon" "eq"
     "equivalence" "eqv" "error_unit" "exit" "exp" "exponent" "extends"
     "extends_type_of" "external" "extrinsic" "false" "file_storage_size"
     "final" "floor" "flush" "forall" "format" "fraction" "function" "ge"
     "generic" "get_command" "get_command_argument" "get_environment_variable"
     "goto" "grade_down" "grade_up" "gt" "hpf_alignment" "hpf_distribution"
     "hpf_template" "huge" "iachar" "iall" "iall_prefix" "iall_scatter"
     "iall_suffix" "iand" "iany" "iany_prefix" "iany_scatter" "iany_suffix"
     "ibclr" "ibits" "ibset" "ichar" "ieee_arithmetic" "ieee_exceptions"
     "ieee_features" "ieee_get_underflow_mode" "ieee_set_underflow_mode"
     "ieee_support_underflow_control" "ieor" "if" "ilen" "implicit"
     "import" "include" "independent" "index" "inherit" "input_unit"
     "inquire" "int" "integer" "intent" "interface" "intrinsic" "ior"
     "iostat_end" "iostat_eor" "iparity" "iparity_prefix" "iparity_scatter"
     "iparity_suffix" "ishft" "ishftc" "iso_c_binding" "iso_fortran_env"
     "kind" "lbound" "le" "leadz" "len" "len_trim" "lge" "lgt" "lle" "llt"
     "log" "log10" "logical" "lt" "matmul" "max" "maxexponent" "maxloc"
     "maxval" "maxval_prefix" "maxval_scatter" "maxval_suffix" "merge"
     "min" "minexponent" "minloc" "minval" "minval_prefix" "minval_scatter"
     "minval_suffix" "mod" "module" "modulo" "move_alloc" "mvbits" "namelist"
     "ne" "nearest" "neqv" "new" "new_line" "nint" "non_intrinsic"
     "non_overridable" "none" "nopass" "not" "null" "nullify"
     "number_of_processors" "numeric_storage_size" "only" "onto" "open"
     "operator" "optional" "or" "output_unit" "pack" "parameter" "parity"
     "parity_prefix" "parity_scatter" "parity_suffix" "pass" "pause"
     "pointer" "popcnt" "poppar" "precision" "present" "print" "private"
     "procedure" "processors" "processors_shape" "product" "product_prefix"
     "product_scatter" "product_suffix" "program" "protected" "public"
     "pure" "radix" "random_number" "random_seed" "range" "read" "real"
     "realign" "recursive" "redistribute" "repeat" "reshape" "result"
     "return" "rewind" "rrspacing" "same_type_as" "save" "scale" "scan"
     "select" "selected_char_kind" "selected_int_kind" "selected_real_kind"
     "sequence" "set_exponent" "shape" "sign" "sin" "sinh" "size" "spacing"
     "spread" "sqrt" "stop" "subroutine" "sum" "sum_prefix" "sum_scatter"
     "sum_suffix" "system_clock" "tan" "tanh" "target" "template" "then"
     "tiny" "transfer" "transpose" "trim" "true" "type" "ubound" "unpack"
     "use" "value" "verify" "volatile" "wait" "where" "while" "with" "write")
    (go-mode ;; https://golang.org/ref/spec#Keywords, https://golang.org/pkg/builtin/
     "append" "bool" "break" "byte" "cap" "case" "chan" "close" "complex" "complex128"
     "complex64" "const" "continue" "copy" "default" "defer" "delete" "else" "error"
     "fallthrough" "false" "float32" "float64" "for" "func" "go" "goto" "if" "imag"
     "import" "int" "int16" "int32" "int64" "int8" "interface" "len" "make"
     "map" "new" "nil" "package" "panic" "print" "println" "range" "real" "recover"
     "return" "rune" "select" "string" "struct" "switch" "true" "type" "uint" "uint16"
     "uint32" "uint64" "uint8" "uintptr" "var")
    (java-mode
     "abstract" "assert" "boolean" "break" "byte" "case" "catch" "char" "class"
     "continue" "default" "do" "double" "else" "enum" "extends" "final"
     "finally" "float" "for" "if" "implements" "import" "instanceof" "int"
     "interface" "long" "native" "new" "package" "private" "protected" "public"
     "return" "short" "static" "strictfp" "super" "switch" "synchronized"
     "this" "throw" "throws" "transient" "try" "void" "volatile" "while")
    (javascript-mode ;; https://tc39.github.io/ecma262/
     "async" "await" "break" "case" "catch" "class" "const" "continue"
     "debugger" "default" "delete" "do" "else" "enum" "export" "extends" "false"
     "finally" "for" "function" "if" "import" "in" "instanceof" "let" "new"
     "null" "return" "static" "super" "switch" "this" "throw" "true" "try"
     "typeof" "undefined" "var" "void" "while" "with" "yield")
    (kotlin-mode
     "abstract" "annotation" "as" "break" "by" "catch" "class" "companion"
     "const" "constructor" "continue" "data" "do" "else" "enum" "false" "final"
     "finally" "for" "fun" "if" "import" "in" "init" "inner" "interface"
     "internal" "is" "lateinit" "nested" "null" "object" "open" "out" "override"
     "package" "private" "protected" "public" "return" "super" "this" "throw"
     "trait" "true" "try" "typealias" "val" "var" "when" "while")
    (lua-mode ;; https://www.lua.org/manual/5.3/manual.html
     "and" "break" "do" "else" "elseif" "end" "false" "for" "function" "goto" "if"
     "in" "local" "nil" "not" "or" "repeat" "return" "then" "true" "until" "while")
    (objc-mode
     "@catch" "@class" "@encode" "@end" "@finally" "@implementation"
     "@interface" "@private" "@protected" "@protocol" "@public"
     "@selector" "@synchronized" "@throw" "@try" "alloc" "autorelease"
     "bycopy" "byref" "in" "inout" "oneway" "out" "release" "retain")
    (perl-mode ;; cperl.el
     "AUTOLOAD" "BEGIN" "CHECK" "CORE" "DESTROY" "END" "INIT" "__END__"
     "__FILE__" "__LINE__" "abs" "accept" "alarm" "and" "atan2" "bind"
     "binmode" "bless" "caller" "chdir" "chmod" "chomp" "chop" "chown" "chr"
     "chroot" "close" "closedir" "cmp" "connect" "continue" "cos"
     "crypt" "dbmclose" "dbmopen" "defined" "delete" "die" "do" "dump" "each"
     "else" "elsif" "endgrent" "endhostent" "endnetent" "endprotoent"
     "endpwent" "endservent" "eof" "eq" "eval" "exec" "exists" "exit" "exp"
     "fcntl" "fileno" "flock" "for" "foreach" "fork" "format" "formline"
     "ge" "getc" "getgrent" "getgrgid" "getgrnam" "gethostbyaddr"
     "gethostbyname" "gethostent" "getlogin" "getnetbyaddr" "getnetbyname"
     "getnetent" "getpeername" "getpgrp" "getppid" "getpriority"
     "getprotobyname" "getprotobynumber" "getprotoent" "getpwent" "getpwnam"
     "getpwuid" "getservbyname" "getservbyport" "getservent" "getsockname"
     "getsockopt" "glob" "gmtime" "goto" "grep" "gt" "hex" "if" "index" "int"
     "ioctl" "join" "keys" "kill" "last" "lc" "lcfirst" "le" "length"
     "link" "listen" "local" "localtime" "lock" "log" "lstat" "lt" "map"
     "mkdir" "msgctl" "msgget" "msgrcv" "msgsnd" "my" "ne" "next" "no"
     "not" "oct" "open" "opendir" "or" "ord" "our" "pack" "package" "pipe"
     "pop" "pos" "print" "printf" "push" "q" "qq" "quotemeta" "qw" "qx"
     "rand" "read" "readdir" "readline" "readlink" "readpipe" "recv" "redo"
     "ref" "rename" "require" "reset" "return" "reverse" "rewinddir" "rindex"
     "rmdir" "scalar" "seek" "seekdir" "select" "semctl" "semget" "semop"
     "send" "setgrent" "sethostent" "setnetent" "setpgrp" "setpriority"
     "setprotoent" "setpwent" "setservent" "setsockopt" "shift" "shmctl"
     "shmget" "shmread" "shmwrite" "shutdown" "sin" "sleep" "socket"
     "socketpair" "sort" "splice" "split" "sprintf" "sqrt" "srand" "stat"
     "study" "sub" "substr" "symlink" "syscall" "sysopen" "sysread" "system"
     "syswrite" "tell" "telldir" "tie" "time" "times" "tr" "truncate" "uc"
     "ucfirst" "umask" "undef" "unless" "unlink" "unpack" "unshift" "untie"
     "until" "use" "utime" "values" "vec" "wait" "waitpid"
     "wantarray" "warn" "while" "write" "x" "xor" "y")
    (php-mode
     "__CLASS__" "__DIR__" "__FILE__" "__FUNCTION__" "__LINE__" "__METHOD__"
     "__NAMESPACE__" "_once" "abstract" "and" "array" "as" "break" "case"
     "catch" "cfunction" "class" "clone" "const" "continue" "declare"
     "default" "die" "do" "echo" "else" "elseif" "empty" "enddeclare"
     "endfor" "endforeach" "endif" "endswitch" "endwhile" "eval" "exception"
     "exit" "extends" "final" "for" "foreach" "function" "global"
     "goto" "if" "implements" "include" "instanceof" "interface"
     "isset" "list" "namespace" "new" "old_function" "or" "php_user_filter"
     "print" "private" "protected" "public" "require" "require_once" "return"
     "static" "switch" "this" "throw" "try" "unset" "use" "var" "while" "xor")
    (python-mode ;; https://docs.python.org/3/reference/lexical_analysis.html#keywords
     "False" "None" "True" "and" "as" "assert" "break" "class" "continue" "def"
     "del" "elif" "else" "except" "exec" "finally" "for" "from" "global" "if"
     "import" "in" "is" "lambda" "nonlocal" "not" "or" "pass" "print" "raise"
     "return" "try" "while" "with" "yield")
    (ruby-mode
     "BEGIN" "END" "alias" "and"  "begin" "break" "case" "class" "def" "defined?"
     "do" "else" "elsif"  "end" "ensure" "false" "for" "if" "in" "module"
     "next" "nil" "not" "or" "redo" "rescue" "retry" "return" "self" "super"
     "then" "true" "undef" "unless" "until" "when" "while" "yield")
    (rust-mode ;; https://doc.rust-lang.org/grammar.html#keywords
     "Self" "as" "box" "break" "const" "continue" "crate" "else" "enum" "extern"
     "false" "fn" "for" "if" "impl" "in" "let" "loop" "macro" "match" "mod"
     "move" "mut" "pub" "ref" "return" "self" "static" "struct" "super"
     "trait" "true" "type" "unsafe" "use" "where" "while")
    (scala-mode
     "abstract" "case" "catch" "class" "def" "do" "else" "extends" "false"
     "final" "finally" "for" "forSome" "if" "implicit" "import" "lazy" "match"
     "new" "null" "object" "override" "package" "private" "protected"
     "return" "sealed" "super" "this" "throw" "trait" "true" "try" "type" "val"
     "var" "while" "with" "yield")
    (swift-mode
     "Protocol" "Self" "Type" "and" "as" "assignment" "associatedtype"
     "associativity" "available" "break" "case" "catch" "class" "column" "continue"
     "convenience" "default" "defer" "deinit" "didSet" "do" "dynamic" "dynamicType"
     "else" "elseif" "endif" "enum" "extension" "fallthrough" "false" "file"
     "fileprivate" "final" "for" "func" "function" "get" "guard" "higherThan" "if"
     "import" "in" "indirect" "infix" "init" "inout" "internal" "is" "lazy" "left"
     "let" "line" "lowerThan" "mutating" "nil" "none" "nonmutating" "open"
     "operator" "optional" "override" "postfix" "precedence" "precedencegroup"
     "prefix" "private" "protocol" "public" "repeat" "required" "rethrows" "return"
     "right" "selector" "self" "set" "static" "struct" "subscript" "super" "switch"
     "throw" "throws" "true" "try" "typealias" "unowned" "var" "weak" "where"
     "while" "willSet")
    (julia-mode
     "abstract" "break" "case" "catch" "const" "continue" "do" "else" "elseif"
     "end" "eval" "export" "false" "finally" "for" "function" "global" "if"
     "ifelse" "immutable" "import" "importall" "in" "let" "macro" "module"
     "otherwise" "quote" "return" "switch" "throw" "true" "try" "type"
     "typealias" "using" "while")
    (thrift-mode ;; https://github.com/apache/thrift/blob/master/contrib/thrift.el
     "binary" "bool" "byte" "const" "double" "enum" "exception" "extends"
     "i16" "i32" "i64" "include" "list" "map" "oneway" "optional" "required"
     "service" "set" "string" "struct" "throws" "typedef" "void")
    ;; Aliases
    (js2-mode javascript-mode)
    (js2-jsx-mode javascript-mode)
    (espresso-mode javascript-mode)
    (js-mode javascript-mode)
    (js-jsx-mode javascript-mode)
    (rjsx-mode javascript-mode)
    (cperl-mode perl-mode)
    (jde-mode java-mode)
    (ess-julia-mode julia-mode)
    (enh-ruby-mode ruby-mode))
  "Alist of major modes and keywords."
  :type 'alist)

(defmacro cape--silent (&rest body)
  "Silence BODY."
  `(cl-letf ((inhibit-message t)
             (message-log-max nil)
             ((symbol-function #'minibuffer-message) #'ignore))
     (ignore-errors ,@body)))

(defun cape--bounds (thing)
  "Return bounds of THING."
  (or (bounds-of-thing-at-point thing) (cons (point) (point))))

(defun cape--interactive (capf)
  "Complete with CAPF."
  (pcase (funcall capf)
    (`(,beg ,end ,table . ,extra)
     (let ((completion-extra-properties extra))
       (completion-in-region beg end table (plist-get extra :predicate))))
    (_ (user-error "%s: No completions" capf))))

(cl-defun cape--table-with-properties (table &key category (sort t))
  "Create completion TABLE with properties.
CATEGORY is the optional completion category.
SORT should be nil to disable sorting."
  (let ((metadata `(metadata
                    ,@(and category `((category . ,category)))
                    ,@(and (not sort) '((display-sort-function . identity)
                                        (cycle-sort-function . identity))))))
    (lambda (str pred action)
      (if (eq action 'metadata)
          metadata
        (complete-with-action action table str pred)))))

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
        ('substring (string-match-p (regexp-quote old-input) new-input)))))

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
      (let ((new-input (buffer-substring-no-properties beg end)))
        (when (or (eq input 'init) (not (cape--input-valid-p input new-input valid)))
          (setq table (funcall fun new-input) input new-input)))
      (complete-with-action action table str pred))))

(defvar cape--file-properties
  (list :annotation-function (lambda (s) (if (string-suffix-p "/" s) " Folder" " File"))
        :company-kind (lambda (s) (if (string-suffix-p "/" s) 'folder 'file)))
  "Completion extra properties for `cape-file'.")

;;;###autoload
(defun cape-file (&optional interactive)
  "Complete file name at point.
If INTERACTIVE is nil the function acts like a capf."
  (interactive (list t))
  (if interactive
      (let (cape-file-directory-must-exist)
        (cape--interactive #'cape-file))
    (let* ((bounds (cape--bounds 'filename))
           (file (buffer-substring (car bounds) (cdr bounds))))
      (when (or (not cape-file-directory-must-exist)
                (and (string-match-p "/" file) (file-exists-p (file-name-directory file))))
        `(,(car bounds) ,(cdr bounds) ,#'read-file-name-internal
          :company-prefix-length ,(and (not (equal file "/")) (string-suffix-p "/" file))
          :exclusive no ,@cape--file-properties)))))

(defvar cape--symbol-properties
  (list :annotation-function #'cape--symbol-annotation
        :company-kind #'cape--symbol-kind)
  "Completion extra properties for `cape-symbol'.")

(defun cape--symbol-kind (sym)
  "Return kind of SYM."
  (setq sym (intern-soft sym))
  (cond
   ((or (macrop sym) (special-form-p sym)) 'keyword)
   ((fboundp sym) 'function)
   ((boundp sym) 'variable)
   ((featurep sym) 'module)
   ((facep sym) 'color)
   (t 'text)))

(defun cape--symbol-annotation (sym)
  "Return kind of SYM."
  (setq sym (intern-soft sym))
  (cond
   ((or (macrop sym) (special-form-p sym)) " Macro")
   ((fboundp sym) " Function")
   ((boundp sym) " Variable")
   ((featurep sym) " Feature")
   ((facep sym) " Face")
   (t " Symbol")))

;;;###autoload
(defun cape-symbol (&optional interactive)
  "Complete symbol at point.
If INTERACTIVE is nil the function acts like a capf."
  (interactive (list t))
  (if interactive
      (cape--interactive #'cape-symbol)
    (let ((bounds (cape--bounds 'symbol)))
      `(,(car bounds) ,(cdr bounds)
        ,(cape--table-with-properties obarray :category 'symbol)
        :exclusive no ,@cape--symbol-properties))))

(defvar cape--dabbrev-properties
  (list :annotation-function (lambda (_) " Dabbrev")
        :company-kind (lambda (_) 'text))
  "Completion extra properties for `cape-dabbrev'.")

(defvar dabbrev-check-all-buffers)
(defvar dabbrev-check-other-buffers)
(declare-function dabbrev--ignore-case-p "dabbrev")
(declare-function dabbrev--find-all-expansions "dabbrev")
(declare-function dabbrev--reset-global-variables "dabbrev")
(declare-function dabbrev--abbrev-at-point "dabbrev")

;;;###autoload
(defun cape-dabbrev (&optional interactive)
  "Complete with Dabbrev at point.
If INTERACTIVE is nil the function acts like a capf."
  (interactive (list t))
  (if interactive
      (let ((cape-dabbrev-min-length 0))
        (cape--interactive #'cape-dabbrev))
    (require 'dabbrev)
    (cape--dabbrev-reset)
    (let ((abbrev (ignore-errors (dabbrev--abbrev-at-point))) beg end)
      (when (and abbrev (not (string-match-p "\\s-" abbrev)))
        (save-excursion
          (search-backward abbrev)
          (setq beg (point))
          (search-forward abbrev)
          (setq end (point)))
        `(,beg ,end
               ,(cape--table-with-properties
                 ;; Use equal, if candidates must be longer than cape-dabbrev-min-length.
                 (cape--cached-table beg end #'cape--dabbrev-list
                                     (if (> cape-dabbrev-min-length 0)
                                         'equal 'prefix))
                 :category 'cape-dabbrev)
               :exclusive no ,@cape--dabbrev-properties)))))

(defun cape--dabbrev-reset ()
  "Reset dabbrev state."
  (let ((dabbrev-check-all-buffers nil)
        (dabbrev-check-other-buffers nil))
    (dabbrev--reset-global-variables)))

(defun cape--dabbrev-list (word)
  "Find all dabbrev expansions for WORD."
  (cape--silent
   (cape--dabbrev-reset)
   (cl-loop with min-len = (+ cape-dabbrev-min-length (length word))
            for w in (dabbrev--find-all-expansions word (dabbrev--ignore-case-p word))
            if (>= (length w) min-len) collect w)))

(defvar cape--ispell-properties
  (list :annotation-function (lambda (_) " Ispell")
        :company-kind (lambda (_) 'text))
  "Completion extra properties for `cape-ispell'.")

(declare-function ispell-lookup-words "ispell")
(defun cape--ispell-words (str)
  "Return all words from Ispell matching STR."
  (with-demoted-errors "Ispell Error: %S"
    (require 'ispell)
    (cape--silent (ispell-lookup-words (format "*%s*" str)))))

;;;###autoload
(defun cape-ispell (&optional interactive)
  "Complete with Ispell at point.
If INTERACTIVE is nil the function acts like a capf."
  (interactive (list t))
  (if interactive
      (cape--interactive #'cape-ispell)
    (let ((bounds (cape--bounds 'word)))
      `(,(car bounds) ,(cdr bounds)
        ,(cape--table-with-properties
          (cape--cached-table (car bounds) (cdr bounds) #'cape--ispell-words 'substring)
          :category 'cape-ispell)
        :exclusive no ,@cape--ispell-properties))))

(defvar cape--dict-properties
  (list :annotation-function (lambda (_) " Dict")
        :company-kind (lambda (_) 'text))
  "Completion extra properties for `cape-dict'.")

(defvar cape--dict-words nil)
(defun cape--dict-words ()
  "Dictionary words."
  (or cape--dict-words
      (setq cape--dict-words
             (split-string (with-temp-buffer
                             (insert-file-contents-literally cape-dict-file)
                             (buffer-string))
                           "\n" 'omit-nulls))))

;;;###autoload
(defun cape-dict (&optional interactive)
  "Complete word at point.
If INTERACTIVE is nil the function acts like a capf."
  (interactive (list t))
  (if interactive
      (cape--interactive #'cape-dict)
    (let ((bounds (cape--bounds 'word)))
      `(,(car bounds) ,(cdr bounds)
        ,(cape--table-with-properties (cape--dict-words) :category 'cape-dict)
        :exclusive no ,@cape--dict-properties))))

(defun cape--abbrev-list ()
  "Abbreviation list."
  (delete "" (nconc (all-completions "" global-abbrev-table)
                    (all-completions "" local-abbrev-table))))

(defun cape--abbrev-annotation (abbrev)
  "Annotate ABBREV with expansion."
  (concat " "
          (truncate-string-to-width
           (symbol-value
            (or (abbrev--symbol abbrev local-abbrev-table)
                (abbrev--symbol abbrev global-abbrev-table)))
           30 0 nil t)))

(defvar cape--abbrev-properties
  (list :annotation-function #'cape--abbrev-annotation
        :exit-function (lambda (&rest _) (expand-abbrev))
        :company-kind (lambda (_) 'snippet))
  "Completion extra properties for `cape-abbrev'.")

;;;###autoload
(defun cape-abbrev (&optional interactive)
  "Complete abbreviation at point.
If INTERACTIVE is nil the function acts like a capf."
  (interactive (list t))
  (if interactive
      (cape--interactive #'cape-abbrev)
    (when-let (abbrevs (cape--abbrev-list))
      (let ((bounds (cape--bounds 'symbol)))
        `(,(car bounds) ,(cdr bounds)
          ,(cape--table-with-properties abbrevs :category 'cape-abbrev)
          :exclusive no ,@cape--abbrev-properties)))))

(defun cape--keyword-list ()
  "Return keywords for current major mode."
  (when-let (kw (alist-get major-mode cape-keywords))
    (if (symbolp (cadr kw)) (alist-get (cadr kw) cape-keywords) kw)))

(defvar cape--keyword-properties
  (list :annotation-function (lambda (_) " Keyword")
        :company-kind (lambda (_) 'keyword))
  "Completion extra properties for `cape-keyword'.")

;;;###autoload
(defun cape-keyword (&optional interactive)
  "Complete word at point.
If INTERACTIVE is nil the function acts like a capf."
  (interactive (list t))
  (if interactive
      (cape--interactive #'cape-keyword)
    (when-let (keywords (cape--keyword-list))
      (let ((bounds (cape--bounds 'symbol)))
        `(,(car bounds) ,(cdr bounds)
          ,(cape--table-with-properties keywords :category 'cape-keyword)
          :exclusive no ,@cape--keyword-properties)))))

(defun cape--super-function (ht prop)
  "Return merged function for PROP given HT."
  (lambda (x)
    (when-let (fun (plist-get (gethash x ht) prop))
      (funcall fun x))))

;;;###autoload
(defun cape-super-capf (&rest capfs)
  "Merge CAPFS and return new Capf which includes all candidates."
  (lambda ()
    (when-let (results (delq nil (mapcar #'funcall capfs)))
      (pcase-let ((`((,beg ,end . ,_)) results)
                  (candidates 'init)
                  (ht (make-hash-table :test #'equal))
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
              (cape--table-with-properties
               (lambda (str pred action)
                 (when (eq candidates 'init)
                   (clrhash ht)
                   (setq candidates
                         (cl-loop for (table . plist) in tables nconc
                                  (let* ((pred (plist-get plist :predicate))
                                         (metadata (completion-metadata "" table pred))
                                         (sort (or (completion-metadata-get metadata 'display-sort-function)
                                                   #'identity))
                                         (cands (funcall sort (all-completions "" table pred))))
                                    (cl-loop for cand in cands do (puthash cand plist ht))
                                    cands))))
                 (complete-with-action action candidates str pred))
               :sort nil :category 'cape-super)
              :exclusive 'no
              :company-prefix-length prefix-len
              :company-doc-buffer (cape--super-function ht :company-doc-buffer)
              :company-location (cape--super-function ht :company-location)
              :company-docsig (cape--super-function ht :company-docsig)
              :company-deprecated (cape--super-function ht :company-deprecated)
              :company-kind (cape--super-function ht :company-kind)
              :annotation-function (cape--super-function ht :annotation-function)
              :exit-function (lambda (x _status) (funcall (cape--super-function ht :exit-function) x)))))))

(defun cape--company-call (backend &rest args)
  "Call Company BACKEND with ARGS."
  (pcase (apply backend args)
    (`(:async . ,fetcher)
     (let ((res 'trash))
       ;; Force synchronization
       (funcall fetcher (lambda (arg) (setq res arg)))
       (with-timeout (cape-company-async-timeout (setq res nil))
         (while (eq res 'trash)
           (sleep-for cape-company-async-wait)))
       res))
    (res res)))

;;;###autoload
(defun cape-company-to-capf (backend &optional valid)
  "Convert Company BACKEND function to Capf.
VALID is the input comparator, see `cape--input-valid-p'.
This feature is experimental."
  (unless (symbolp backend)
    (error "Backend must be a symbol"))
  (let ((init (intern (format "cape--company-init:%s" backend))))
    (lambda ()
      (unless (boundp init)
        (make-variable-buffer-local init))
      (unless (symbol-value init)
        (cape--company-call backend 'init)
        (set init t))
      (when-let* ((prefix (cape--company-call backend 'prefix))
                  (initial-input (if (stringp prefix) prefix (car-safe prefix))))
        ;; TODO When fetching candidates, support asynchronous operation. If a
        ;; future is returned, the capf should fail first. As soon as the future
        ;; callback is called, remember the result, refresh the UI and return the
        ;; remembered result the next time the capf is called.
        (let* ((end (point)) (beg (- end (length initial-input))))
          (list beg end
                (cape--table-with-properties
                 (cape--cached-table beg end
                                     (if (cape--company-call backend 'duplicates)
                                         (lambda (input)
                                           (delete-dups (cape--company-call backend 'candidates input)))
                                       (apply-partially #'cape--company-call backend 'candidates))
                                     (if (cape--company-call backend 'no-cache initial-input) 'never valid))
                 :category backend
                 :sort (not (cape--company-call backend 'sorted)))
                :exclusive 'no
                :company-prefix-length (cdr-safe prefix)
                :company-doc-buffer (lambda (x) (cape--company-call backend 'doc-buffer x))
                :company-location (lambda (x) (cape--company-call backend 'location x))
                :company-docsig (lambda (x) (cape--company-call backend 'meta x))
                :company-deprecated (lambda (x) (cape--company-call backend 'deprecated x))
                :company-kind (lambda (x) (cape--company-call backend 'kind x))
                :annotation-function (lambda (x) (cape--company-call backend 'annotation x))
                :exit-function (lambda (x _status) (cape--company-call backend 'post-completion x))))))))

;;;###autoload
(defun cape-capf-buster (capf &optional valid)
  "Return transformed CAPF where the cache is busted on input change.
VALID is the input comparator, see `cape--input-valid-p'."
  (lambda ()
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
                          (setq table new-table input new-input)))))
                   (complete-with-action action table str pred)))
              ,@plist)))))

(defvar cape--line-properties nil
  "Completion extra properties for `cape-line'.")

(defun cape--line-list ()
  "Return all lines from buffer."
  (let ((beg (point-min))
        (max (point-max))
        (pt (point))
        (ht (make-hash-table :test #'equal))
        end lines)
    (save-excursion
      (while (< beg max)
        (goto-char beg)
        (setq end (line-end-position))
        (unless (<= beg pt end)
          (let ((line (buffer-substring-no-properties beg end)))
            (unless (or (string-blank-p line) (gethash line ht))
              (puthash line t ht)
              (push line lines))))
        (setq beg (1+ end))))
    (nreverse lines)))

;;;###autoload
(defun cape-line (&optional interactive)
  "Complete current line from other lines in buffer.
If INTERACTIVE is nil the function acts like a capf."
  (interactive (list t))
  (if interactive
      (cape--interactive #'cape-line)
    `(,(line-beginning-position) ,(point)
      ,(cape--table-with-properties (cape--line-list) :sort nil)
      ,@cape--line-properties)))

;;;###autoload
(defun cape-capf-with-properties (capf &rest properties)
  "Return a new CAPF with additional completion PROPERTIES.
Completion properties include for example :exclusive, :annotation-function
and the various :company-* extensions."
  (lambda ()
    (pcase (funcall capf)
      (`(,beg ,end ,table . ,plist)
       `(,beg ,end ,table ,@properties ,@plist)))))

;;;###autoload
(defun cape-silent-capf (capf)
  "Return a new CAPF which is silent (no messages, no errors)."
  (lambda ()
    (pcase (cape--silent (funcall capf))
      (`(,beg ,end ,table . ,plist)
       `(,beg ,end
              ,(lambda (str pred action)
                 (cape--silent (complete-with-action action table str pred)))
              ,@plist)))))

;;;###autoload
(defun cape-interactive-capf (capf)
  "Create interactive completion function from CAPF."
  (lambda (&optional interactive)
    (interactive (list t))
    (if interactive (cape--interactive capf) (funcall capf))))

(provide 'cape)
;;; cape.el ends here
