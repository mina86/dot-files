;;                                -*- mode: emacs-lisp; lexical-binding: t -*-
;; .emacs  -- Emacs configuration file
;; Copyright 2004-2012 by Michal Nazarewicz (mina86@mina86.com)
;; Some parts of the code may be © by their respective authors.
;;
;;{{{ Compatibility with old Emacses

(unless (boundp 'user-emacs-directory)
  (defvar user-emacs-directory (expand-file-name "~/.emacs.d/")
    "Directory beneath which additional per-user Emacs-specific files are placed.
Various programs in Emacs store information in this directory.
Note that this should end with a directory separator."))

(unless (fboundp 'use-region-p)
  (defun use-region-p ()
    "Return t if certain commands should apply to the region.
Certain commands normally apply to text near point,
but in Transient Mark mode when the mark is active they apply
to the region instead.  Such commands should use this subroutine to
test whether to do that."
    (and (region-active-p) (> (region-end) (region-beginning)))))

(unless (fboundp 'region-active-p)
  (defun region-active-p ()
    "Return t if Transient Mark mode is enabled and the mark is active.
This is NOT the best function to use to test whether a command should
operate on the region instead of the usual behavior -- for that,
use `use-region-p'."
    (and transient-mark-mode mark-active)))

;;}}}
;;{{{ System dependend data and directories

;; Make sure user-emacs-directory is defined
(if (string-equal user-emacs-directory "")
    (setq user-emacs-directory (expand-file-name "~/.emacs.d")))

;; Add  ~/.emacs.d/elisp  to load path
(when (eval-when-compile
        (file-directory-p (concat user-emacs-directory "elisp")))
  (setq load-path (cons (concat user-emacs-directory "elisp") load-path))
  (eval-when-compile
    (setq load-path (cons (concat user-emacs-directory "elisp") load-path))))

;; notmuch
(autoload 'notmuch (concat user-emacs-directory "mail.el") "notmuch mail" t)

;; Packages repositories
(eval-when-compile (require 'package))
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

;;}}}
;;{{{ Utilities

(defmacro setq-if-bound (name value)
  "Set value of variable NAME to VALUE if its bound.
This may be especially useful when dealing with code that needs
to run on variety of systems and its not always know whether
certain feature, and thus variable, will be present."
  `(when (boundp (quote ,name))
     (setq ,name ,value)))

(defun mn--current-local-map ()
  "Return current local map creating one if not set yet."
  (let ((map (current-local-map)))
    (unless map
      (use-local-map (setq map (make-sparse-keymap))))
    map))

(defmacro set-key (key &rest body)
  "(set-key [KEYMAP] KEY BODY...)

KEYMAP can be either :local or a symbol.  :local will use local
keymap (and create it if necessary).  A symbol must be a variable
name holding the keymap to modify.  If omitted, global keymap is
used.

KEY may be anything other than a symbol describing the key.
The following won't work:
	(let ((key \"a\")) (set-key key self-insert-command)).

If BODY is just a single value which is not a list it will
be quoted and passed as argument to `define-key' so in the
simplest case it's just an unquoted name of the function:
	(set-key \"a\" self-insert-command)
Using a variable holding a symbol won't work:
	(let ((callback 'self-insert-command))
	   (set-key \"a\" callback))

Alternatively, BODY is a body of a lambda that is to be bound to
the key.  The body can be preceded by \":args (ARGS)
INTERACTIVE\" where ARGS is the list of lambda's arguments and
INTERACTIVE is argument of the `interactive' command."
  (let ((keymap '(current-global-map)))
    (cond
     ((eq :local key)
      (setq keymap '(mn--current-local-map)
            key    (car body)
            body   (cdr body)))
     ((symbolp key)
      (setq keymap key
            key    (car body)
            body   (cdr body))))

    (list 'define-key keymap key
          (if (and (null (cdr body)) (not (listp (car body))))
              `(quote ,(car body))
            (let (args (int (list 'interactive)))
              (when (eq :args (car body))
                (setq body (cdr body))
                (setq args (car body)
                      int  (list 'interactive (cadr body))
                      body (cddr body)))
              `(function (lambda ,args ,int ,@body)))))))

(defmacro add-lambda-hook (hook &rest body)
  "Add a lambda to a hook.
HOOK is either a hook (just as with `add-hook' function) or a list of
hooks in which case lambda will be added to all the hooks in HOOK
list.  BODY is body of the lambda to be added."
  (declare (indent 1))
  (if (and (listp hook) (eq (car hook) 'quote) (listp (cadr hook)))
      (let ((func (make-symbol "func")))
        `(let
          ((,func (function (lambda () ,@body))))
          . ,(mapcar (lambda (h) `(add-hook (quote ,h) ,func))
                     (cadr hook))))
    `(add-hook ,hook (function (lambda () ,@body)))))

;;}}}
;;{{{ Auto-byte-compile

(defvar auto-byte-compile-files-list
  (eval-when-compile
    (append
     (when user-init-file
       (if (string-match "\\.elc$" user-init-file)
           (list
            (substring user-init-file 0 -4)
            (substring user-init-file 0 -1))
         (list user-init-file)))
     (list (concat user-emacs-directory "/init.el")
           (concat user-emacs-directory "/mail.el"))))
  "List of files to auto compile.")

(defun auto-byte-compile-file (&optional files match regexp)
  "Byte compiles file matching criteria.
FILES can be
- nil in which case value of the `buffer-file-name' variable will be
  used unless it returns nil in which case no action will be taken;
- a string which is equivalent to passing list with that string as the
  only element;
- a list of strings representing file names; or
- anything else which is equivalent to passing
  `auto-byte-compile-files-list'.

Entries equal to \".\", \"..\" or ending with \"/.\" or \"/..\"
are ignored.  Directories starting with a dot will be ignored.
If element is a directory it will be processed recursively but if
REGEXP is nil only files ending with \".el\" will be processed.

MATCH can be
- nil which is equivalent to passing `auto-byte-compile-files-list';
- a string which is equivalent to passing list with that string as the
  only element;
- a list in which case file have to be in that list to be processed; or
- anything else in which case file will be processed regardless of name.

If any element of MATCH is a string ending with a slash ('/') it
is treated as directory name (no checking is done if it is really
a directory or even if it exists) and file is said to match such
entry if it begins with it thus all files in given directory will
match.

If called interacivelly without prefix arg will behave as with
MATCH equal t.  With prefix arg will behave as with MATCH equal
nil.

REGEXP must be nil which is equivalent with passing a list
containing only empty string or a list of regular expressions
which file have to match to be processed.

So the default is to auto-compile the current file iff it exists
in `auto-byte-compile-files-list'.

Non-string elements in list will be ignored.

Auto-compilation means that file will be byte-compiled iff the
compiled version does not exits or is older then the file
itself."
  (interactive (list (read-file-name "Auto byte compile file:" nil nil t)
                     (not current-prefix-arg)))

  (when (or files (setq files buffer-file-name))
    (setq files (cond ((stringp files) (list files))
                      ((listp   files) files)
                      (t               auto-byte-compile-files-list))
          match (mapcar 'expand-file-name
                        (cond ((null match)    auto-byte-compile-files-list)
                              ((stringp match) (list match))
                              ((listp match)   match))))

    (let (f)
      (while files
        (setq f     (expand-file-name (car files))
              files (cdr files))
        (cond
         ((string-match "\\(?:^\\|/\\)\\.\\.?$" f)
          "Ignored")
         ((file-directory-p f)
          (unless (string-match f "\\(?:^\\|/\\)\\.")
            (if regexp
                (setq files (append (directory-files f t nil t) files))
              (auto-byte-compile-file (directory-files f t nil t)
                                      (or match t) '("\\.el$")))))
         ((and (file-newer-than-file-p f (byte-compile-dest-file f))
               (or (not match)
                   (catch 'found
                     (dolist (m match)
                       (if (string= m (if (string-match "/$" m)
                                          (substring f 0 (length m)) f))
                           (throw 'found t)))))
               (or (not regexp)
                   (catch 'found (dolist (r regexp)
                                   (if (string-match r f) (throw 'found t)))))
               (byte-compile-file f))))))))

(defun auto-byte-compile-buffer (&optional match buffer)
  "Auto compiles file in given buffer if in Lisp mode.
MATCH has the same meaning as in `auto-byte-compile-file' function.
If BUFFER is nil, current buffer is used.  Buffer is compiled only if
major mode of BUFFER is 'lisp-mode or 'emacs-lisp-mode.

If called interacivelly will behave as with MATCH equal t and
BUFFER equal nil unless prefix argument was given in which case
MATCH will equal nil."
  (interactive (list (not current-prefix-arg) nil))
  (and (buffer-file-name buffer)
       (memq (if buffer (with-current-buffer buffer major-mode) major-mode)
             '(lisp-mode emacs-lisp-mode))
       (auto-byte-compile-file (buffer-file-name buffer) match)))

(add-hook 'kill-buffer-hook 'auto-byte-compile-buffer)
(add-lambda-hook 'kill-emacs-hook (auto-byte-compile-file t))

;;}}}
;;{{{ Bindings

;;{{{   Sequence commands

;; Sequence commands  1.4  by me, Michal Nazarewicz ;)
;; http://www.emacswiki.org/cgi-bin/wiki/DoubleKeyBinding
(defvar seq-times 0
  "Number of times command was executed.
Contains random data before `seq-times' macro is called.")

(defmacro seq-times (&optional name max &rest body)
  "Returns number of times command NAME was executed.
Updates `seq-times' variable accordingly to keep track.  If NAME is
Nil `this-command' will be used.  If MAX is specified the counter will
wrap around at the value of MAX never reaching it.  If body is given
it will be evaluated if the command is run for the first time in
a sequence."
  (declare (indent 2))
  (let ((next (if (or (null max) (eq max 'nil))
                  '(1+ seq-times)
                `(% (1+ seq-times) ,max)))
        (cnd (cond ((or (eq name 'last-command) (eq name 't)) t)
                   ((or (null name) (eq name 'nil)) 'this-command)
                   (`(or ,name this-command)))))
    `(setq seq-times
           ,(if (eq cnd t)
                next
              `(if (eq last-command ,cnd)
                   ,next
                 ,@body
                 0)))))

(defmacro seq-times-nth (name body &rest list)
  "Return element of the LIST depending on number of times command was called.
NAME and BODY has the same meaning as in `seq-times' function.  LIST
is a list of values to return.  Depending how many times command was
called, element with that index will be returned.  The counter will
wrap around."
  (declare (indent 2))
  `(nth (seq-times ,name ,(length list) ,body) ',list))

(defmacro seq-times-do (name body &rest commands)
  "Evaluates command depending on number of times command was called.
NAME and BODY has the same meaning as in `seq-times' function.
COMMANDS is a list of sexps to evaluate.  Depending how many times
command was called, sexp with that index will be evaluated.  The
counter will wrap around."
  (declare (indent 2))
  `(eval (nth (seq-times ,name ,(length commands) ,body) ',commands)))

;;}}}
;;{{{   Home/End

;; My home
(defvar my-home-end--point 0)
(defun my-home ()
   "Go to beginning of line, indent or buffer.
When called once move point to beginning of line, twice - indent,
three times - beginning of the buffer, four times - back to where it
was at the beginning."
   (interactive)
   (seq-times-do nil (setq my-home-end--point (point))
     (beginning-of-line)
     (back-to-indentation)
     (goto-char (point-min))
     (goto-char my-home-end--point)))

(substitute-key-definition 'move-beginning-of-line 'my-home
                           (current-global-map))

;; My end
(defun my-end ()
  "Go to end of line or buffer.
When called once move point to end of line, twice - end of buffer,
three times - back to where it was at the beginning."
  (interactive)
  (seq-times-do nil (setq my-home-end--point (point))
    (if (bound-and-true-p folding-mode)
        (folding-end-of-line) (end-of-line))
    (forward-paragraph)
    (goto-char (point-max))
    (goto-char my-home-end--point)))

(substitute-key-definition 'move-end-of-line 'my-end (current-global-map))

;;}}}
;;{{{   Pager/Scrolling

(setq-if-bound scroll-error-top-bottom t)
(setq-if-bound scroll-preserve-screen-position t)

;; Makes paging functions work the way god intended
;; http://www.docs.uu.se/~mic/emacs.html
;; pager.el was modified by me
(when (eval-when-compile (load "pager" t))
  (require 'pager)
  (setq pager-goto-edge t)
  (substitute-key-definition 'scroll-down 'pager-page-up
                             (current-global-map))
  (substitute-key-definition 'scroll-down-command 'pager-page-up
                             (current-global-map))
  (substitute-key-definition 'scroll-up 'pager-page-down
                             (current-global-map))
  (substitute-key-definition 'scroll-up-command 'pager-page-down
                             (current-global-map)))

;;}}}
;;{{{   Save with no blanks

;; Save with no trailing whitespaces
(defun save-no-blanks (&optional no-strip)
  "Save file stripping all trailing white space and empty lines from EOF.
When called with prefix argument (or NO-STRIP being non-nill) does not
perform stripping and behaves as plain `save-buffer'."
  (interactive "P")
  (unless no-strip
    (save-restriction
      (delete-trailing-whitespace)))
  (save-buffer))

(substitute-key-definition 'save-buffer 'save-no-blanks (current-global-map))

;;}}}
;;{{{   Misc

(set-key "\C-x\C-c" :args (arg) "P"
         (when (yes-or-no-p "Do you really want to quit? ")
           (save-buffers-kill-emacs arg))) ; Never kill by mistake


(set-key "\C-h"          [(backspace)])
(set-key [(backspace)]   delete-backward-char)
(set-key [(delete)]      delete-char)


(defvar mn-window-skip-modes '(compilation-mode)
  "List of major modes `mn-window-skip-p' treats as uninteresting.")

(defvar mn-window-skip-names '("*Compile-Log*" "*Backtrace*" "*compilation*")
  "List of buffer names `mn-window-skip-p' treats as uninteresting.
This comes as an addition to `mn-window-skip-regexp'.")

(defvar mn-window-skip-regexp nil
  "A regexp matching buffer names `mn-window-skip-p' treats as uninteresting.
This comes as an addition to `mn-window-skip-names'.")

(defun mn-window-skip-p (window)
  "Check whether WINDOW is uninteresting to `mn-other-window'."
  (catch 'done
    ; catch is ugly, I know, but it saves on some indention
    (let ((b (window-buffer window)) n)
      (unless b
        (throw 'done nil))
      (when (memq (with-current-buffer b major-mode) mn-window-skip-modes)
        (throw 'done t))
      (unless (or mn-window-skip-names mn-window-skip-regexp)
        (throw 'done nil))
      (unless (setq n (buffer-name b))
        (throw 'done nil))
      (when (member n mn-window-skip-names)
        (throw 'done t))
      (when (and mn-window-skip-regexp (string-match mn-window-skip-regexp n))
        (throw 'done t))
      nil)))

(defun mn-other-window (count &optional all-frames)
  "Select another interesting window in cyclic ordering of windows.
COUNT and ALL-FRAMES has the same meaning as in `other-window' function.

Window is not interesting id buffer it contains has one of the
major modes from `mn-window-skip-modes' list, its name is on the
`mn-window-skip-names' list or matches `mn-window-skip-regexp'."
  (interactive "p")
  (let ((func (cond ((> count 0) 'next-window)
                    ((< count 0) (setq count (- count)) 'previous-window))))
    (when func
      (catch 'loop
        (let* ((wnd (selected-window))
               (w   wnd))
          (while (> count 0)
            (setq w (funcall func w nil all-frames))
            (when (eq wnd w)
              (throw (quote loop) w))
            (unless (mn-window-skip-p w)
              (setq count (1- count))))
          (select-window w))))))

(substitute-key-definition 'other-window 'mn-other-window (current-global-map))
(set-key "\C-xO"         other-window)
(set-key "\C-xp"         "\C-u-1\C-xo")          ; C-x p  prev win
(set-key "\C-xP"         "\C-u-1\C-xO")

(when (fboundp 'windmove-right)
  (set-key "\M-F"          windmove-right)
  (set-key "\M-B"          windmove-left)
  (set-key "\M-P"          windmove-up)
  (set-key "\M-N"          windmove-down))

; don't ask which buffer to kill
(set-key "\C-xk" kill-this-buffer)

(set-key "\C-cr"         revert-buffer)         ; Reload buffer
(set-key "\C-x\C-b" (switch-to-buffer (other-buffer))) ; C-x C-b switch
(set-key [(control ";")] comment-dwim)          ; C-; comments

;; Jump
(require 'ffap)
(defun my-jump () "Jump to the thing at point." (interactive)
  (let ((thing (ffap-guesser))) (if thing (ffap thing)) t))

(set-key [(control return)] my-jump)
(set-key [(control shift mouse-1)] ffap-at-mouse)
(set-key "\C-x\C-f"      ffap)


;; Make l behave as it should in help-mode
;; http://www.emacswiki.org/cgi-bin/wiki/EmacsNiftyTricks
(defvar help-mode-map)  ; silence compiler warning
(add-lambda-hook 'help-mode-hook (set-key help-mode-map "l" help-go-back))

;;}}}
;;{{{   Killing, yanking, X selection, etc

;; Regions, selections and marks
(setq mouse-yank-at-point t       ;mouse yank at point not at cursor   (X-win)
      kill-read-only-ok   t       ;be silent when killing text from RO buffer
      set-mark-command-repeat-pop t
      kill-do-not-save-duplicates t)
(delete-selection-mode 1)         ;deleting region by typing or del (like Win)

(set-key [(shift insert)]
  (let ((mouse-yank-at-point nil))
    (mouse-yank-primary nil)))

;;}}}
;;{{{   Just one space

(substitute-key-definition 'just-one-space
                           (lambda () (interactive) (cycle-spacing -1 t))
                           (current-global-map))

;;}}}
;;{{{   Tab - indent or complete

(eval-when-compile (require 'hippie-exp))
(setq hippie-expand-try-functions-list
      '(
;        try-expand-all-abbrevs
;        try-expand-list
;        try-expand-line
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol)
      hippie-expand-verbose nil)


;; Indent or complete
(defvar indent-or-complete-complete-function 'hippie-expand
  "Function to complete the word when using `indent-or-complete'.
It is called with one argument - nil.")

(defvar indent-or-complete--last-was-complete nil)

(defun indent-or-complete ()
  "Indent or complete depending on context.
In minibuffer run `minibuffer-complete', if `use-region-p' run
`indent-region', if point is at end of a word run
`inent-or-complete-complete-function', else run
`indent-for-tab-command'."
  (interactive)
  (cond ((and (fboundp 'minibufferp) (minibufferp)) (minibuffer-complete))
        ((use-region-p) (indent-region (region-beginning) (region-end)))
        ((setq indent-or-complete--last-was-complete
              (or (and (char-before)
                       (= ?w (char-syntax (char-before)))
                       (or (not (char-after))
                           (/= ?w (char-syntax (char-after)))))
                  (and (eq last-command this-command)
                       indent-or-complete--last-was-complete)))
         (funcall indent-or-complete-complete-function nil))
        ((indent-for-tab-command))))

(when (eval-when-compile (load "auto-complete-config" t))
  (require 'auto-complete-config)
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/elisp/ac-dict")
  (ac-config-default)

  (setq ac-auto-start 3
        ac-delay 0.1
        ac-auto-show-menu 2
        ac-ignore-case nil
        ac-use-comphist t
        indent-or-complete-complete-function 'auto-complete)

  (set-key ac-completing-map "\M-o" ac-previous)
  (set-key ac-completing-map "\M-p" ac-previous)
  (set-key ac-completing-map "\M-n" ac-next)

  (define-key (current-global-map) "\M-/"
    indent-or-complete-complete-function))

;; (set-key "\t"    indent-or-complete)
;; (set-key [(tab)] indent-or-complete)

(add-lambda-hook 'find-file-hooks
  (unless (eq major-mode 'org-mode)
    (set-key :local [(tab)] indent-or-complete)))

;;}}}
;;{{{   Filling

;; Alt+q - Fill
(defun my-fill ()
  "Fills paragraph (or region) using a cyclic order alignment.
If called once fills the paragraph to the left, twice - justifies,
three times - to the right, four times - centers."
  (interactive)
  (fill-paragraph (seq-times-nth () () left full right center) t))

(set-key "\M-q"          my-fill)

;;}}}
;;{{{   Fkeys

;;{{{     F1 - Help

(defun my-help ()
  "Show context dependent help.
If function given tries to `describe-function' otherwise uses
`manual-entry' to display manpage of a `current-word'."
  (interactive)
  (let ((var (variable-at-point)))
    (if (symbolp var)
        (describe-variable var)
      (let ((fn (function-called-at-point)))
        (if fn
            (describe-function fn)
          (man (current-word)))))))

(set-key [(f1)]      my-help)

;;}}}
;;{{{     F2 - find configuration files

(set-key [(f2)]         (find-file user-init-file))
(set-key [(control f2)] (find-file (concat user-emacs-directory "/mail.el")))
(set-key [(meta f2)]    (find-file custom-file))
(set-key [(shift f2)]   (find-file "~/.bashrc"))

;;}}}
;;{{{     F5 - Mail

(set-key [(f5)] notmuch)

;;}}}
;;{{{     F7 - spell checking

(require 'ispell)

(setq-default ispell-program-name "aspell")
(ispell-change-dictionary "british" t)

(defvar mn-spell-dictionaries '("polish" "british")
  "List of dictionaries to cycle through with `mn-spell-switch-dictionary'.")

(defun mn-spell (&optional lang start end)
  "Spell check region, buffer or from point.
If LANG is not-nil sets Ispell dictionary to LANG, then checks
region from START to END for spelling errors.  The default values
for START and END are `region-beginning' and `region-end' if
`use-region-p' or `point-min' and `point-max' otherwise.

If LANG is an empty string local dictionary is set to
nil (ie. the global dictionary is used).

If START >= END this function only sets the dictionary and
returns nil.  Otherwise it returns whatever `ispell-region'
returned."
  (interactive
   (list (completing-read
          "Use new dictionary (RET for current, SPC to complete): "
          (if (fboundp 'ispell-valid-dictionary-list)
              (mapcar 'list (ispell-valid-dictionary-list)))
          nil t)))
  (if lang (ispell-change-dictionary (if (string= lang "") nil lang)))
  (let* ((rp (use-region-p))
         (s  (or start (if rp (region-beginning) (point-min))))
         (e  (or end   (if rp (region-end)       (point-max)))))
    (if (< s e) (ispell-region s e))))

(defun mn-spell-switch-dictionary (&optional global)
  "Switche dictionary to the next dictionary from `mn-spell-dictionaries'.
If GLOBAL is non-nil, or with a prefix argument set global dictionary."
  (interactive "P")
  (ispell-change-dictionary
   (let ((dic (or (and (not global) ispell-local-dictionary) ispell-dictionary))
         (list mn-spell-dictionaries))
     (while (and list (not (string= (car list) dic))) (setq list (cdr list)))
     (or (cadr list) (car mn-spell-dictionaries)))
   global))


(set-key [(f7)]         (mn-spell))
(set-key [(control f7)] (mn-spell nil (point)))
(set-key [(meta f7)]    mn-spell-switch-dictionary)
(set-key [(shift f7)]   ispell-word)


(define-globalized-minor-mode global-flyspell-mode
  flyspell-mode mn-turn-flyspell-on)

(defun mn-turn-flyspell-on ()
  (cond ((memq major-mode '(c-mode c++-mode php-mode python-mode perl-mode
                            cperl-mode emacs-lisp-mode lisp-mode scheme-mode
                            diff-mode))
         (flyspell-prog-mode))
        ((memq major-mode '(notmuch-hello-mode notmuch-search-mode
                            notmuch-show-mode)))
        ((flyspell-mode t))))

(global-flyspell-mode 1)

;;}}}
;;{{{     F9 - Compilation

(defconst -mn-compile-common
  " -Wall -Wextra -Wfloat-equal -Wshadow -Wwrite-strings -Winline -Wdisabled-optimization -Wstrict-aliasing=2 -pedantic -DMINA86 -ggdb -O0 -Wpointer-arith -funit-at-a-time")

(defvar mn-compile-vars
  `((("CFLAGS"   "-std=c99 -Werror-implicit-function-declaration -Wunreachable-code -Wstrict-prototypes -Wold-style-definition")
     ("CXXFLAGS" "-std=c++98")
     ("CPPFLAGS" ,-mn-compile-common)
     ("LDFLAGS"  nil))

    (("CFLAGS"   "-std=c89 -Werror-implicit-function-declaration -Wunreachable-code -Wstrict-prototypes -Wold-style-definition")
     ("CXXFLAGS" "-std=c++98 -Wstrict-null-sentinel -Wctor-dtor-privacy -Woverloaded-virtual")
     ("CPPFLAGS" ,-mn-compile-common)
     ("LDFLAGS"  nil)))

  "Enviroment variables set by `mn-compile' priory to compilation.
The car of the list is a list of default enviroment variables to be
set and cadr is a list is a list of alternative enviroment variables.
Each list is a list of two element lists which car is a enviroment
variables name and cadr is value.")

(autoload 'recompile "compile"
  "Re-compile the program including the current buffer.
If this is run in a Compilation mode buffer, re-use the arguments from the
original use.  Otherwise, recompile using `compile-command'.
If the optional argument `edit-command' is non-nil, the command can be edited." t)

(defun mn-compile (&optional alt recompile touch)
  "Compile current file.
If ALT is omited or nil sets CFLAGS, CXXFLAGS, CPPFLAGS and
LDFLAGS enviroment variables to `mn-cflags', `mn-cxxflags',
`mn-cppflags', `mn-ldflags' respectively.

If ALT is non-nil (or when called interactive with any prefix
argument) sets CFLAGS, CXXFLAGS, CPPFLAGS and LDFLAGS enviroment
variables to `mn-alt-cflags', `mn-alt-cxxflags',
`mn-alt-cppflags', `mn-alt-ldflags' respectively.

Executes `recompile' function if RECOMPILE is non-nill or
`compile' otherwise.  If TOUCH is non-nil marks buffer as
modified beforehand."
  (interactive "P")
  (if touch (set-buffer-modified-p t))
  (save-buffer)
  (if (or (eq major-mode 'lisp-mode) (eq major-mode 'emacs-lisp-mode))
      (auto-byte-compile-file nil t)
    (let (v (vars (if alt (cadr mn-compile-vars) (car mn-compile-vars))))
      (while (set 'v (pop vars)) (setenv (car v) (cadr v))))
    (if recompile (recompile) (call-interactively 'compile))))


(set-key [(f9)]         mn-compile)
(set-key [(control f9)] :args (a) "P" (mn-compile a t))
(set-key [(meta f9)]    :args (a) "P" (mn-compile (not a) t t))
(set-key [(shift f9)]   next-error)

(eval-when-compile (require 'compile))
(setq compilation-scroll-output 'first-error  ; scroll until first error
      compilation-window-height 12)           ; keep it readable

;;}}}

;;}}}
;;{{{   ISearch mode

(add-hook 'isearch-mode-hook (lambda ()
 (set-key isearch-mode-map [(f1)]      isearch-mode-help)
 (set-key isearch-mode-map "\C-t"      isearch-toggle-regexp)
 (set-key isearch-mode-map "\C-c"      isearch-toggle-case-fold)
 (set-key isearch-mode-map "\C-j"      isearch-edit-string)
 (set-key isearch-mode-map "\C-h"      isearch-del-char)
 (set-key isearch-mode-map [backspace] isearch-del-char)))

;;}}}
;;{{{   Copy/Kill

;; Make C-w, M-w work on word if no selection
(set-key "\C-w" (call-interactively
                 (if (use-region-p) 'kill-region 'kill-word)))
(set-key "\M-w"
  (if (use-region-p)
      (call-interactively 'kill-ring-save)
    (kill-ring-save (point) (progn (forward-word 1) (point)))
    (setq this-command 'kill-region)))

;;}}}

;;}}}
;;{{{ Syntax highlighting

;; Font lock
(require 'font-lock)
(global-font-lock-mode t)

;; Let customize keep config there
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load-file custom-file))
(setq-if-bound auto-byte-compile-files-list
               (cons custom-file auto-byte-compile-files-list))

;; Other
(show-paren-mode t)               ;show matching parenthesis.

(defface my-tab-face
  '((((class color) (min-colors 216)) (:background "#666"))
    (t (:background "yellow")))
  "Face used to show TAB characters."
  :group 'whitespace)

(defface my-big-indent-face
  '((((class color) (min-colors 216)) (:background "#966"))
    (t (:background "red")))
  "Face used to show a sequence of 4-5 tabs at the beginning of a line."
  :group 'whitespace)

(defface my-huge-indent-face
  '((((class color) (min-colors 216)) (:background "#C66"))
    (t (:background "magenta")))
  "Face used to show a sequence of 6 or more tabs at the beginning of a line."
  :group 'whitespace)

(defface my-fixme-face
  '((t :background "red" :foreground "white" :weight bold))
  "Face use to show FIXME and XXX markers in the text."
  :group 'whitespace)

(defface my-todo-face
  '((t :foreground "red" :weight bold))
  "Face used to show TODO markers in the text."
  :group 'whitespace)

;; Show blanks and FIXME
;; http://www.emacswiki.org/cgi-bin/wiki/EightyColumnRule
(add-lambda-hook 'font-lock-mode-hook
  (unless (eq 'diff-mode major-mode)
    (font-lock-add-keywords nil
     '(("\t+" 0 'my-tab-face t)
       ("^\t\\{4,5\\}" 0 'my-big-indent-face t)
       ("^\t\\{6,\\}" 0 'my-huge-indent-face t)
       ("\\<\\(TODO:?\\)\\>" 1 'my-todo-face t)
       ("\\<\\(FIXME:?\\|XXX\\)\\>" 1 'my-fixme-face t)))))


;;}}}
;;{{{ Misc small config

(setq use-dialog-box nil)         ;never use dialog boxes

;; Frame apperence
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode)   (tool-bar-mode   -1))
(if (fboundp 'menu-bar-mode)   (menu-bar-mode   -1))
(setq default-frame-alist '((width . 80)
                            (right-fringe . 4)
                            (left-fringe  . 4)
                            (menu-bar-lines . 0)
                            (tool-bar-lines . 0)
                            (vertical-scroll-bars)
                            (foreground-color . "gray")
                            (background-color . "black")
                            (background-mode . dark)
                            (wait-for-wm . nil)))

;; Modeline
(setq line-number-mode t          ;show line number in modeline
      column-number-mode t)       ;show column number in modeline

(setq mode-line-format
      '("%e"
        mode-line-mule-info
        mode-line-modified
        mode-line-frame-identification
        mode-line-buffer-identification
        "  "
        mode-line-position
        vc-mode
        " "
        mode-line-modes
        (which-func-mode ("" which-func-format))
        (global-mode-string ("" global-mode-string))
        "%-")

      mode-line-position
      '(:eval (let* ((min (point-min)) (max (point-max))
                     (wide (and (= min 1) (= max (1+ (buffer-size))))))
                (concat
                 (if wide "(" "[")
                 (if column-number-mode "%2c, " "")
                 (if line-number-mode "%2l/" "")
                 (number-to-string (1+ (count-lines min max)))
                 (if wide ")" "]"))))

      mode-line-modes
      '("("
        (:propertize
         ("" mode-name mode-line-process minor-mode-alist)
         help-echo "mouse-1: major mode, mouse-2: major mode help, mouse-3: toggle minor modes"
         mouse-face mode-line-highlight
         local-map (keymap
                    (header-line keymap
                                 (down-mouse-3 . mode-line-mode-menu-1))
                    (mode-line keymap
                               (down-mouse-3 . mode-line-mode-menu-1)
                               (mouse-2 . describe-mode)
                               (down-mouse-1 . mouse-major-mode-menu))))
        ") "))

(add-lambda-hook 'after-init-hook
  ;; Highlight groups of three digits
  (when (fboundp 'global-num3-mode)
    (global-num3-mode t))
  ;; Dim buffers which are not (current-buffer)
  (when (fboundp 'auto-dim-other-buffers-mode)
    (auto-dim-other-buffers-mode t)))

;; Other
(require 'icomplete)
(icomplete-mode 1)                ;nicer completion in minibuffer
(setq icomplete-prospects-height 2) ; don't spam my minibuffer
(set-key minibuffer-local-map "\C-c"  ; C-c clears minibuffer
  (delete-minibuffer-contents))
(setq suggest-key-bindings 3)     ;suggestions for shortcut keys for 3 seconds
(setq frame-title-format "Emacs") ;frame title format
(setq history-delete-duplicates t)
(setq inhibit-splash-screen   t   ;don't show splash screen
      inhibit-startup-buffer-menu t) ;don't show buffer menu when oppening
                                  ; many files               (EMACS 21.4+)
(setq sentence-end-base "[.?!…][]\"'”)}]*" ; "…" also ends a sentence
      sentence-end-double-space 1 ;sentance end with double space
      paragraph-start    " *\\([*+-]\\|\\([0-9]+\\|[a-zA-Z]\\)[.)]\\|$\\)"
      paragraph-separate "$"
      require-final-newline t)    ;always end file with NL
(fset 'yes-or-no-p 'y-or-n-p)     ;make yes/no be y/n
(set-default 'indicate-empty-lines t) ;show empty lines at the end of file
(set-default 'indicate-buffer-boundaries t) ;show buffer boundries on fringe
(setq x-alt-keysym 'meta)         ;treat Alt as Meta even if real Meta found
(setq gc-cons-threshold 4000000)  ;bytes before garbage collection
(eval-when-compile (require 'ange-ftp))
(setq ange-ftp-try-passive-mode t);passive FTP
(blink-cursor-mode -1)            ;do not blink cursor
(setq blink-cursor-alist '((t      . box)  ;seriously, don't blink,
                           (box    . box)) ;blink-cursor-mode does not
      cursor-type 'box)                    ;work for me.
(setq-if-bound compilation-auto-jump-to-first-error t)
(setq-if-bound line-move-visual nil) ;move by logical lines not screen lines

;; Saving etc
(when (fboundp recentf-mode)
  (recentf-mode -1))           ;no recent files
(setq backup-by-copying-when-linked t) ;preserve hard links
(auto-compression-mode 1)         ;automatic compression
(setq make-backup-files nil)      ;no backup
(global-auto-revert-mode 1)       ;automaticly reload buffer when changed


;; Indention
(defun set-tab (tab &optional stop-list)
  "Adjust `tab-width', `tab-stop-list' and indent level in current buffer.
`tab-width' is set to absolute value of TAB, while `tab-stop-list' to
STOP-LIST (if given and not-nil) or a list of all positive from
smallest to largest which are multiplications of TAB and are lower or
equal 120.

If called interactively user will be prompted for desired width.  With
prefix argument, `indent-tabs-mode' will also be set to t.

If TAB is negative, `indent-tabs-mode' will be set to nil and an
absolute value will be taken.

This function also tries to set indent-level for current buffer's
major mode.  This is not very inteligent nor has complete list of
rules so it is likely not to work."
  (interactive "nTab-width: ")
  (let ((negative (< tab 0)))
    (setq tab-width (abs tab))
    (set (make-local-variable 'tab-stop-list)
         (or stop-list (number-sequence tab 120 tab-width)))
    (mapc
     (lambda (var)
       (when (boundp var)
         (set (make-local-variable var) tab-width)))
     '(c-basic-offset perl-indent-level cperl-indent-level  js-indent-level
       sh-basic-offset sh-indentation))
    (cond
     (negative (setq indent-tabs-mode nil))
     (prefix-arg (setq indent-tabs-mode t)))))

(setq indent-tabs-mode t)         ;indent using tabs
(set-tab 8)                       ;tab width and stop list
(eval-when-compile (require 'tabify))
(setq tabify-regexp "^\t* [ \t]+");tabify only at the beginning of line

;; Scrolling/moving
(setq scroll-step 1               ;scroll one line
      hscroll-step 1              ;scroll one column
      next-line-add-newlines nil) ;no new lines with down arrow key


;; Do not break line after single character when filling
(defun fill-single-char-nobreak-p ()
  "Don't break line after a single character."
  (save-excursion
    (skip-chars-backward " \t")
    (backward-char 2)
    (looking-at "[[:space:]][a-zA-Z]")))

(add-to-list 'fill-nobreak-predicate 'fill-single-char-nobreak-p)


;;}}}
;;{{{ Major Modes

;;{{{   CC Mode

(add-lambda-hook 'c-initialization-hook
  (c-add-style
   "mina86"
   '((c-basic-offset . 8)             ; 8-char wide indention...
     (tab-width . 8)                  ; ...which equals single tab...
     (indent-tabs-mode . t)           ; ...so use tabs
     (c-comment-only-line-offset . 0)  ; XXX no idea what it does
     (c-label-minimum-indentation . 1) ; no min. indention for labels

     (c-cleanup-list                 ; Clean ups
      brace-else-brace                 ; "} else {" in one line
      brace-elseif-brace               ; "} else if (...) {" in one line
      brace-catch-brace                ; "} catch (...) {" in one line
      defun-close-semi                 ; "};" together
      )

     (c-offsets-alist                ; Indention levels:
      ; Don't indent inside namespaces, extern, etc
      (incomposition          . 0)
      (inextern-lang          . 0)
      (inmodule               . 0)
      (innamespace            . 0)

      ; Preprocessor macros
      (cpp-define-intro       c-lineup-cpp-define +)
      (cpp-macro              . [ 0 ])
      (cpp-macro-cont         . +)

      ; Brace after newline newer indents
      (block-open             . 0)
      (brace-entry-open       . 0)
      (brace-list-open        . 0)
      (class-open             . 0)
      (composition-open       . 0)
      (defun-open             . 0)
      (extern-lang-open       . 0)
      (inline-open            . 0)
      (module-open            . 0)
      (namespace-open         . 0)
      (statement-case-open    . 0)
      (substatement-open      . 0)

      ; Obviously, do not indent closing brace
      (arglist-close          . 0)
      (block-close            . 0)
      (brace-list-close       . 0)
      (class-close            . 0)
      (composition-close      . 0)
      (defun-close            . 0)
      (extern-lang-close      . 0)
      (inline-close           . 0)
      (module-close           . 0)
      (namespace-close        . 0)

      ; Obviously, indent next line after opening brace and single statements
      (defun-block-intro      . +)
      (statement-block-intro  . +)
      (substatement           . +)

      ; Handle nicely multi line argument lists
      (arglist-close c-lineup-arglist 0)
      (arglist-cont          c-lineup-gcc-asm-reg +)
      (arglist-cont-nonempty c-lineup-gcc-asm-reg c-lineup-arglist +)
      (arglist-intro          . +)

      ; Misc
      (brace-list-intro       . +)     ; Indent elements in brace-lists
      (brace-list-entry       . 0)
      (c      . c-lineup-C-comments)   ; Indent comments nicely
      (comment-intro          . c-lineup-comment)
      (catch-clause           . 0)     ; catch/finally where try
      (do-while-closure       . 0)     ; while (...) where do
      (else-clause            . 0)     ; else where if
      (func-decl-cont         . +)     ; Indent stuff after function
      (friend                 . 0)     ; friend need no additional indention
      (inclass                . +)     ; Indent stuff inside class...
      (access-label           . -)     ; ...expect for access labels
      (inexpr-statement       . 0)     ; No unneeded indent in ({ ... })...
      (inexpr-class           . 0)     ; ...& anonymous classes
      (inher-intro            . +)     ; ndent & lineup inheritance list
      (inher-cont             . c-lineup-multi-inher)
      (member-init-intro      . +)     ; Indent & lineup initialisation list
      (member-init-cont       . c-lineup-multi-inher)
      (label                  . [ 0 ]) ; Labels always on first column
      (substatement-label     . [ 0 ])
      (statement              . 0)     ; Statement same as line above...
      (statement-cont c-lineup-cascaded-calls +) ; ...but indent cont.
      (statement-case-intro   . +)     ; Indent statements in switch...
      (case-label             . 0)     ; ...but not the labels
      (stream-op . c-lineup-streamop)  ; Lineup << operators in C++
      (string . c-lineup-dont-change)  ; Do not touch strings!
      (template-args-cont c-lineup-template-args +) ; Lineup template args
      (topmost-intro          . 0)     ; Topmost stay topmost
      (topmost-intro-cont     c-lineup-topmost-intro-cont 0)

      ; Other stuff I don't really care about
      ; I keep it here for the sake of having all symbols specified.
      (inlambda               . c-lineup-inexpr-block)
      (knr-argdecl            . 0)
      (knr-argdecl-intro      . +)
      (lambda-intro-cont      . +)
      (objc-method-args-cont  . c-lineup-ObjC-method-args)
      (objc-method-call-cont
       ;c-lineup-ObjC-method-call-colons
       c-lineup-ObjC-method-call +)
      (objc-method-intro      . [0])
      )

     ; I don't care about anything that is below -- not using any
     ; automagick -- but for the sake of having everything set I'll keep
     ; it here.

     (c-hanging-braces-alist         ; Auto new lines around braces
      ; In most cases new line after open brace and both before and
      ; after close brace.  The "before" is however ommited
      ; from *-close symbols because when editing normally we will
      ; be on the new line already -- if we're not, user probably
      ; knows better.
      (defun-open             after)
      (defun-close            after)
      (class-open             after)
      (class-close            after)
      (inline-open            after)
      (inline-close           after)
      (extern-lang-open       after)
      (extern-lang-close      after)
      (namespace-open         after)
      (namespace-close        after)
      (module-open            after)
      (module-close           after)
      (composition-open       after)
      (composition-close      after)

      ; No new line after closing brace if it matches do { or if (...) {
      (block-open             after)
      (substatement-open      after)
      (block-close            . c-snug-do-while)

      ; With brace-lists however, do nothing automatically -- user knows
      ; better
      (brace-list-open        )
      (brace-list-close       )
      (brace-list-intro       )
      (brace-entry-open       )

      ; Others
      (statement-cont         )
      (statement-case-open    after)
      (inexpr-class-open      )
      (inexpr-class-close     )
      (arglist-cont-nonempty  )
      )

     (c-hanging-colons-alist
      ; Add new line after labels
      (case-label             after)
      (label                  after)
      (access-label           after)
      ; But nothing else
      (member-init-intro      )
      (inher-intro            )

      (c-hanging-semi&comma-criteria
       (mn-c-semi&comma-no-newlines-if-open-brace
        c-semi&comma-no-newlines-before-nonblanks
        c-semi&comma-inside-parenlist))
      )))


  (defun mn-c-semi&comma-no-newlines-if-open-brace ()
    "Prevents newline after semicolon if there is an open brace
on the same line.  Function is a bit stupid and does not check if
the open brace was real open brace or part of comment/string."
    (when (let ((p (point))) (save-excursion (forward-line 0)
                                             (search-forward "{" p t)))
      'stop))


  (eval-when-compile (require 'cc-mode))
  (setq c-default-style '((awk-mode . "awk")
                          (other    . "mina86")))

  (add-lambda-hook 'c-common-mode-hook
    (when buffer-file-name
      (let ((fn (file-name-nondirectory buffer-file-name)))
        (set (make-local-variable 'compile-command)
             (concat "make -k "
                     (substring
                      fn 0 (string-match "\\.[^\\.]*$" fn 1)))))))

  (when (and (load "google-c-style" t)
             (boundp 'google-c-style))
    (c-add-style "google" google-c-style)
    (add-lambda-hook 'c-mode-hook
      (and buffer-file-name
           (string-match-p "/google3/" buffer-file-name)
           (c-set-style "google")))))

;;}}}
;;{{{   HTML/XML & comapny Mode

;; Create a link out of the preceeding word if it appears to be a URL
(defun mn-linkify-maybe ()
  "Maybe turn URL before cursor into a link.
If the word before cursor appears to be an URL wrap it around in a <a
href=\"...\">...</a>.  Returns whether it happend.

This is however a stub implementation (because thingatpt could not be
loaded) which does nothing and returns nil."
  nil)

(when (load "thingatpt" t)
  (let ((url-regexp
         (or (bound-and-true-p thing-at-point-url-regexp)
             (bound-and-true-p ffap-url-regexp))))
    (when url-regexp
      (defun mn-linkify-maybe ()
        "If the word before cursor appears to be an URL wrap it around
in a <a href=\"...\">...</a>.  Returns whether it happend."
        (interactive "*")
        (when (save-excursion
                (save-restriction
                  (let ((end (point)))
                    (beginning-of-line)
                    (narrow-to-region (point) end)
                    (looking-at
                     (concat ".*\\(" url-regexp "\\)\\'")))))
          (let ((url (delete-and-extract-region (match-beginning 1)
                                                (match-end 1))))
            (insert "<a href=\"" url "\">" url "</a>")))))))


(defun mn-magick-self-insert-command (spec &optional default when-prefix)
  "If prefix argument is not nil calls WHEN-PREFIX with single
argument being a `prefix-numeric-value' or the `prefix-arg'.

Otherwise looks through SPEC which is a list of 3-element
lists (called rules):
  (regexp length action)
Rule is said to match if `point' is at least LENGTH characters from
`point-min' and REGEXP matches starting at position LENGTH characters
earlier then `point'.  ACTION of first rule that matches is performed.
If no rule matches DEFAULT is treated as action to perform.

If ACTION to perform is a string (DEFAULT may not be a string)
then `replace-match' is called otherwise it is assumed ACTION is
a function and it is called with single argument 1.

DEFAUTL and WHEN-PREFIX defaults to `self-insert-command'."
  (if prefix-arg
      (funcall (or when-prefix  'self-insert-command)
               (prefix-numeric-value prefix-arg))
    (let ((action (catch 'done
                    (let ((dist (- (point) (point-min))))
                      (dolist (rule spec)
                        (when (>= dist (cadr rule))
                          (save-excursion
                            (backward-char (cadr rule))
                            (when (looking-at (car rule))
                              (throw 'done (cadr (cdr rule))))))))
                    default)))
      (if (stringp action) (replace-match action)
        (funcall (or action 'self-insert-command) 1)))))

(defsubst mn-magick-self-insert-define (map key spec
                                        &optional default when-prefix)
  (define-key map key
    (function (lambda () (interactive "*")
                (mn-magick-self-insert-command spec default when-prefix)))))

(defun mn-xml-configure-bindings (mode-map)
  (mapc
   (function (lambda (args)
               (apply 'mn-magick-self-insert-define mode-map args)))
   (let* ((en-dash "–") (em-dash "—") (nbsp    " ") (thin-sp " ")
          (nbsp-en      (concat nbsp en-dash))
          (nbsp-en-nbsp (concat nbsp en-dash nbsp))
          (thin-em      (concat thin-sp em-dash))
          (thin-em-thin (concat thin-sp em-dash thin-sp)))
     `((" "  (("[^<]\\b\\w"             2 ,(concat "\\&" nbsp))
              ("^\\w"                   1 ,(concat "\\&" nbsp))
              ("&nbsp;"                 6 " ")
              ("&#160;"                 6 " ")
              (,nbsp                    1 " ")
              (,thin-sp                 1 " ")
              (,nbsp-en                 2 ,nbsp-en-nbsp)
              (,thin-em                 2 ,thin-em-thin))
             ,(lambda (n) (mn-linkify-maybe) (self-insert-command n)))
       ("<"  (("<"                      1 "&lt;")))
       (">"  (("&#160;&#8211;"         13 " -->")
              ("&nbsp;&#8211;"         13 " -->")
              (,nbsp-en                 2 " -->")
              (">"                      1 "&gt;")))
       ("&"  (("&"                      1 "&amp;")))
       ("."  (("\\.\\."                 2 "…")))
       ("\"" (("\""                     1 "&quot;")))
       ("~"  (("~"                      1 ,nbsp)))
       ("-"  (("[-!]-"                  2 nil)
              (" -"                     2 ,nbsp-en)
              ("-"                      1 ,nbsp-en)
              (,nbsp-en                 2 ,thin-em)
              (,(concat nbsp "&#8211;") 8 ,thin-em)
              (,en-dash                 1 ,em-dash)
              ("&#8211;"                7 ,em-dash)))
       ([(backspace)]
             (("&lt;"                   4 "<")
              ("&gt;"                   4 ">")
              ("&amp;"                  5 "&")
              ("&quot;"                 6 "\"")
              ("&#8230;"                7 "..")
              ("…"                      1 "..")
              ("&.....;"                7 "")
              ("&....;"                 6 "")
              ("&...;"                  5 "")
              ("&..;"                   4 ""))
             delete-backward-char
             delete-backward-char))))
  (setq indent-tabs-mode nil
        next-error-function 'nxml-next-error))

;; http://github.com/nelhage/elisp/blob/master/dot-emacs
(declare-function rng-first-error "rng-valid")
(declare-function rng-next-error "rng-valid")
(defun nxml-next-error (arg reset)
  (if reset (rng-first-error))
  (rng-next-error arg))

(eval-when-compile (require 'sgml-mode))
(declare-function sgml-close-tag "sgml-mode")
(eval-after-load "sgml-mode"
  (add-lambda-hook 'sgml-mode-hook
    (mn-xml-configure-bindings sgml-mode-map)
    (set-key sgml-mode-map "/"
             (if (and (not prefix-arg)
                      (eq (char-before) ?<))
                 (let (delete-active-region)
                   (delete-char -1)
                   (sgml-close-tag))
               (funcall 'self-insert-command
                        (prefix-numeric-value prefix-arg))))))

(when (fboundp 'nxml-mode)
  (eval-when-compile (require 'nxml-mode))
  (setq nxml-slash-auto-complete-flag t
        nxml-auto-insert-xml-declaration-flag t
        nxml-bind-meta-tab-to-complete-flag t)
  (add-to-list 'auto-mode-alist '("\\.xml\\'" . nxml-mode))
  (eval-after-load "nxml-mode"
    (add-lambda-hook 'nxml-mode-hook
      (mn-xml-configure-bindings nxml-mode-map))))

(defun replace-string-pairs (list)
  (let* ((mark  (use-region-p))
         (start (if mark (region-beginning) (point)))
         (end   (if mark (region-end) (point-max))))
    (save-excursion
      (dolist (pair list)
        (let ((diff (- (length (cdr pair)) (length (car pair)))))
          (goto-char start)
          (while (search-forward (car pair) end t)
            (setq end (+ end diff))
            (replace-match (cdr pair) nil t)))))))

(defun html-escape ()
  "Replace HTML special characters with HTML entities.

In Transient Mark mode, if the mark is active, operate on the contents
of the region.  Otherwise, operate from point to the end of the buffer."
  (interactive)
  (replace-string-pairs
   '(("&" . "&amp;") ("<" . "&lt;") (">" . "&gt;") ("\"" . "&quot;"))))

(defun html-unescape ()
  "Replace (some) HTML entities with characters.

In Transient Mark mode, if the mark is active, operate on the contents
of the region.  Otherwise, operate from point to the end of the buffer."
  (interactive)
  (replace-string-pairs
   '(("&amp;" . "&") ("&lt;" . "<") ("&gt;" . ">") ("&quot;" . "\""))))

;;}}}
;;{{{   (La)TeX and nroff mode

;; Helper for tex-space
(defmacro my-tex-looking-back (regexp len)
  "Return non-nil if REGEXP prefixed with \\b matches LEN chars backward."
  `(save-excursion
     (backward-char ,len)
     (looking-at ,(concat "\\b" regexp))))

;; insert '~' or '\ ' instead of ' ' in LaTeX when needed
;; Also removes '~' when 2nd space added
;; http://www.debianusers.pl/article.php?aid=39
(defun tex-space (arg)
  "Insert \"~\", \"\\ \" or just a space depending on context.

If point follows a tilde or \"\\ \" sequence, replace it with a space
\(or ARG spaces); otherwise if ARG is specified (or called with prefix
argument), insert ARG spaces; otherwise if point follows a one-letter
word, insert \"~\"; otherwise if point follows something that looks
like an abbreviation with a dot, insert \"\\ \"; otherwise just insert
space.

Function actually uses `self-insert-command' to insert spaces so if
it's not bound to space, the results may be somehow surprising."
  (interactive "P")
  (cond
   ((re-search-backward "\\\\ " (- (point) 2) t)
    (delete-char 2) (self-insert-command arg))
   ((re-search-backward "\\~" (- (point) 1) t)
    (delete-char 1) (self-insert-command (prefix-numeric-value arg)))
   (arg (self-insert-command (prefix-numeric-value arg)))
   ((my-tex-looking-back "[a-z]" 1)
    ;    (my-tex-looking-back "[a-z][a-z]" 2))
    (insert-char ?~ 1))
   ((or (my-tex-looking-back "[a-z][a-z]\\." 3)
        (my-tex-looking-back "\\(?:tz[wn]\\|it[pd]\\)\\." 4))
    (insert "\\ "))
   (t (self-insert-command 1))))

(eval-when-compile (require 'tex-mode))
(add-lambda-hook '(tex-mode-hook latex-mode-hook)
  (set-key tex-mode-map " " tex-space))


;; Insert '\ ' instead of ' ' in nroff when needed
;; Also removes '\' when 2nd space added
(defun nroff-space (arg)
  "Insert \"\\ \" or just a space depending on context.
If point follows a \"\\ \" sequence, replace it with a space (or ARG
spaces); otherwise if ARG is specified (or called with prefix
argument), insert ARG spaces; otherwise if point follows a one-letter
word, insert \"\\ \"; otherwise if just insert space.

Function actually uses `self-insert-command' to insert spaces so if
it's not bound to space, the results may be somehow surprising."
  (interactive "p")
  (cond
   ((re-search-backward "\\\\ " (- (point) 2) t)
    (delete-char 2) (self-insert-command arg))
   (arg (self-insert-command arg))
   ((my-tex-looking-back "[a-z]" 1)
;        (my-tex-looking-back "[a-z][a-z]" 2))
;        (my-tex-search-back "do\\|na\\|od\\|po\\|za\\|we\\|to\\|co" 2))
    (insert-char ?\\ 1) (self-insert-command 1))
   (t (self-insert-command 1))))

(eval-when-compile (require 'nroff-mode))
(add-lambda-hook 'nroff-mode-hook
  (set-key nroff-mode-map " " nroff-space))

;;}}}
;;{{{   Generate serialVersionUID in Java

(defun mn-serialVersionUID (&optional insert)
  "Generate serialVersionUID and possibly insert it to current buffer.
When called interactivly or with a non nil INSERT argument insert
a random 64-bit hexadecimal integer prefixed with \"0x\" and suffixed
with \"L\".  When called with nil argument or with argument ommited
returns that number."
  (interactive (list t))
  (if insert
      (insert (mn-serialVersionUID))
    (format "0x%04x%04x%04x%04xL"
            (random 65536) (random 65536)
            (random 65536) (random 65536))))

(add-lambda-hook 'java-mode-hook
  (add-lambda-hook 'write-contents-functions
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (while (re-search-forward "serialVersionUID\s+=\s+[0-9a-fA-FxX]+L;"
                                  nil t)
          (replace-match
           (concat "serialVersionUID = " (mn-serialVersionUID) ";")
           nil t))))))

;;}}}
;;{{{   Misc

;; Text mode
(add-lambda-hook 'text-mode-hook
  (auto-fill-mode t)
  (set-tab 8)
  (setq-if-bound word-wrap t))

;; Assembler mode
(add-lambda-hook 'asm-mode-hook
  (set-tab 8 16)
  (setq comment-column 40))

;; Org mode
(eval-when-compile (require 'org))
(setq org-insert-mode-line-in-empty-file t)

;; Lisp/Scheme mode
;; No tabs! and if opening file with tabs, assume 8-char wide
(add-lambda-hook '(emacs-lisp-mode-hook lisp-mode-hook scheme-mode-hook)
  (setq indent-tabs-mode nil) (set-tab 8))

;; Sawfish mode
(autoload 'sawfish-mode "sawfish" "Mode for editing Sawfish config files")
(add-to-list 'auto-mode-alist '("sawfish/?rc\\'" . sawfish-mode))
(add-to-list 'auto-mode-alist '("\\.jl\\'" . sawfish-mode))

;; Bison mode
(when (eval-when-compile (load "bison-mode.el" t))
  (autoload 'bison-mode "bison-mode.el")
  (add-to-list 'auto-mode-alist '("\\.y$" . bison-mode))
  (setq bison-decl-type-column   8)
  (setq bison-decl-token-column 16))

;; Flex
(autoload 'flex-mode "flex-mode")
(add-to-list 'auto-mode-alist '("\\.l$" . flex-mode))

;; Use cperl-mode for Perl
(eval-when-compile (require 'cperl-mode))
(mapc
 (lambda (list)
   (mapc
    (lambda (pair)
      (if (eq (cdr pair) 'perl-mode)
          (setcdr pair 'cperl-mode)))
    list))
 (list auto-mode-alist interpreter-mode-alist))
(setq cperl-invalid-face nil  ; don't highlight trailing white-space
      cperl-highlight-variables-indiscriminately t
      cperl-electric-backspace-untabify nil)

;; shell-script-mode
(eval-when-compile (require 'sh-script))
(setq sh-indent-for-case-label 0
      sh-indent-for-case-alt '+)

;;}}}

;;}}}
;;{{{ Various features

;; uniquify
(when (eval-when-compile (load "uniquify" t))
  (require 'uniquify)
  (setq uniquify-buffer-name-style 'reverse
        uniquify-strip-common-suffix t))

;; HTMLize
;; http://fly.srk.fer.hr/~hniksic/emacs/htmlize.el
(autoload 'htmlize-buffer "htmlize" "Convert buffer to HTML" t)
(autoload 'htmlize-region "htmlize" "Convert region to HTML" t)

;;{{{   Folding

(when (eval-when-compile (load "folding" t))
  (require 'folding)
  (defconst folding-default-keys-function
    '(folding-bind-backward-compatible-keys))

  (set-key folding-mode-map "\C-cf"                    folding-toggle-show-hide)
  (set-key folding-mode-map [(control ?c) (return)]    folding-shift-in)
  (set-key folding-mode-map [(control ?c) (delete)]    folding-shift-out)
  (set-key folding-mode-map [(control ?c) (backspace)] folding-shift-out)
  (set-key folding-mode-map "\C-c\C-f"                 folding-open-buffer)
  (set-key folding-mode-map "\C-cF"                    folding-whole-buffer)
  (set-key folding-mode-map "\C-e"                     my-end)

  (folding-add-to-marks-list 'php-mode        "// {{{" "// }}}" nil t)
  (folding-add-to-marks-list 'sawfish-mode    ";;{{{" ";;}}}" nil t)
  (folding-add-to-marks-list 'javascript-mode "// {{{" "// }}}" nil t)
  (folding-add-to-marks-list 'css-mode        "/* {{{" "/* }}}" " */" t)

  (folding-mode-add-find-file-hook))

;;}}}
;;{{{   Different cursor color depending on mode

;; http://www.emacswiki.org/cgi-bin/wiki/EmacsNiftyTricks
(defvar hcz-set-cursor-color-color "")
(defvar hcz-set-cursor-color-buffer "")
(defun hcz-set-cursor-color-according-to-mode ()
  "Change cursor color according to some minor modes."
  ;; set-cursor-color is somewhat costly, so we only call it when needed:
  (let ((color (cond (buffer-read-only "blue")
                     (overwrite-mode "red")
                     (t "yellow"))))
    (unless (and
             (string= color hcz-set-cursor-color-color)
             (string= (buffer-name) hcz-set-cursor-color-buffer))
      (set-cursor-color (setq hcz-set-cursor-color-color color))
      (setq hcz-set-cursor-color-buffer (buffer-name)))))

(add-hook 'post-command-hook 'hcz-set-cursor-color-according-to-mode)

(set-cursor-color (setq hcz-set-cursor-color-color "yellow"))

;;}}}
;;{{{   Notes in *scratch*

;; Notes in *scratch*  v. 0.3
;; Copyright (c) 2006 by Michal Nazarewicz (mina86@mina86.com)
;; Released under GNU GPL

(defconst scratch-file (concat user-emacs-directory "scratch")
  "File where content of *scratch* buffer will be read from and saved to.")
(defconst scratch-file-autosave (concat scratch-file ".autosave")
  "File where to autosave content of *scratch* buffer.")

(with-current-buffer (get-buffer-create "*scratch*")
  (if (file-readable-p scratch-file)
      (if (and (file-readable-p scratch-file-autosave)
               (file-newer-than-file-p scratch-file-autosave scratch-file))
;               (y-or-n-p "Recover scratch file? "))
          (insert-file-contents scratch-file-autosave nil nil nil t)
        (insert-file-contents scratch-file nil nil nil t)
        (set-buffer-modified-p nil)))
  (auto-save-mode 1)
  (setq buffer-auto-save-file-name scratch-file-autosave)
  (set (make-local-variable 'revert-buffer-function) 'scratch-revert)
  (fundamental-mode))

(defun scratch-revert (_ignore-auto _noconfirm)
  (when (file-readable-p scratch-file)
    (insert-file-contents scratch-file nil nil nil t)
    (set-buffer-modified-p nil)))

(defun kill-scratch-buffer ()
  (not (when (string-equal (buffer-name (current-buffer)) "*scratch*")
         (delete-region (point-min) (point-max))
         (set-buffer-modified-p nil)
         (next-buffer)
         t)))

(defun kill-emacs-scratch-save ()
  (with-current-buffer (get-buffer-create "*scratch*")
    (write-region nil nil scratch-file)
    (unless (string-equal scratch-file buffer-auto-save-file-name)
      (delete-auto-save-file-if-necessary t))))

(add-hook 'kill-buffer-query-functions 'kill-scratch-buffer)
(add-hook 'kill-emacs-hook 'kill-emacs-scratch-save)

;;}}}

;; Start server
(unless (and (fboundp 'daemonp) (daemonp))
  (server-start))

;;}}}

;; Local
(load (concat user-emacs-directory "local.el") t)

;;; init.el ends here