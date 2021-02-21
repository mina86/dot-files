;; init.el  -- Emacs configuration file             -*- lexical-binding: t -*-

;; Copyright 2004-2020 by Michal Nazarewicz (mina86@mina86.com)
;; Some parts of the code may be © by their respective authors.

;;; Code:

;; Mitigate Bug#28350 (security) in Emacs 25.2 and earlier.
(with-eval-after-load "enriched"
  (defun enriched-decode-display-prop (start end &optional _param)
    (list start end)))

(let ((dir (expand-file-name "~/.local/share/emacs/site-lisp")))
  (when (file-name-directory dir)
    (push dir load-path)))

;; Configure and activate packages
(require 'package)

;; Emacs 26.1 does this automatically but older don’t.
(unless package--initialized
  (package-initialize))

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/"))
      package-menu-hide-low-priority t
      package-archive-priorities '(("gnu" . 100)))

(unless package-archive-contents
  (package-refresh-contents))
;; This is similar to ‘package-install-selected-packages’ except it doesn’t ask
;; any questions.
(dolist (pkg package-selected-packages)
  (unless (package-installed-p pkg)
    (condition-case err
        (package-install pkg t)
      (error (message (error-message-string err))))))

(when (fboundp 'auto-compile-on-save-mode)
  (auto-compile-on-save-mode))
(setq load-prefer-newer t)

;; Utilities

(defun set-key--current-local-map ()
  "Return current local map creating one if not set yet."
  (or (current-local-map)
      (let ((map (make-sparse-keymap)))
        (use-local-map map)
        map)))

(defmacro set-key (keymap key &rest def)
  "(set-key [KEYMAP] KEY . DEF)

In KEYMAP, define key sequence KEY as DEF.

KEYMAP can be :global (to mean global keymap, the default), :local (to mean
the local keymap) or an unquoted symbol (to mean a keymap in given variable).

KEY is anything `define-key' accepts as a key except that if KEYMAP was not
given, KEY cannot be an unquoted symbol, i.e.:
    (let ((key \"a\"))
      (set-key         key self-insert-command)  ; will *not* work
      (set-key :global key self-insert-command)) ; will work

If DEF is a single unquoted symbol it will be quoted, otherwise if it is
a single non-cons value it will not be quoted, otherwise it will be processed
as a lambda (see below).  Thus the following do what one might expect:
    (set-key \"a\" self-insert-command)
        ;; same as (global-set-key \"a\" 'self-insert-command)
    (set-key \"\\C-h\" [(backspace)])
        ;; same as (global-set-key \"\\C-h\" [(backspace)])
    (set-key \"\\C-d\" ())
        ;; same as (global-set-key \"\\C-h\" ())
However, the following will not work:
    (let ((callback 'self-insert-command))
      (set-key \"a\" callback))
        ;; same as (global-set-key \"a\" 'callback)

If DEF is a cons value, it's format is:
    ([:args ARGS INTERACTIVE] . BODY)
and results in the following lambda:
    (lambda ARGS (interactive INTERACTIVE) . BODY)
or if :args is not given (at which point DEF == BODY):
    (lambda () (interactive) . BODY)
For example:
    (set-key \"\\C-B\" (goto-char (- (point) 2)))
        ;; same as (global-set-key \"\\C-B\"
        ;;           (lambda () (interactive) (goto-char (- (point) 2))))
    (set-key \"\\C-B\" :args (n) \"P\" (goto-char (- (point) (* 2 n))))
        ;; same as (global-set-key \"\\C-B\"
        ;;           (lambda (n) (interactive \"P\")
        ;;             (goto-char (- (point) (* 2 n)))))

This macro is not designed to be a complete replacement for `define-key',
`global-set-key' or `local-set-key', since it is not capable of dealing with
some forms of DEFs that those functions accept, but instead it is meant as
a helper to use in user configuration file to save on typing especially when
lambdas are used."
  (setq keymap (cond ((eq :local keymap)  '(set-key--current-local-map))
                     ((eq :global keymap) '(current-global-map))
                     ((symbolp keymap) keymap)
                     (t
                      (setq def (cons key def) key keymap) ; shift args
                      '(current-global-map))))
  (unless def
    (error "DEF argument missing"))
  (list
   'define-key keymap key
   (cond ((or (cdr def) (consp (car def)))
          (let ((args        (if (eq :args (car def)) (cadr def)))
                (interactive (if (eq :args (car def)) (list (car (cddr def)))))
                (body        (if (eq :args (car def)) (cdr (cddr def)) def)))
            `(lambda ,args (interactive . ,interactive) ,@body)))
         ((symbolp (car def)) `#',(car def))
         ((car def)))))

(defmacro add-lambda-hook (hook &rest body)
  "Add a lambda to a hook.
HOOK is either a hook (just as with `add-hook' function) or a list of
hooks in which case lambda will be added to all the hooks in HOOK
list.  BODY is body of the lambda to be added."
  (declare (indent 1))
  (if (and (listp hook) (eq (car hook) 'quote) (listp (cadr hook)))
      (let ((func (make-symbol "func")))
        `(let
          ((,func (lambda () ,@body)))
          . ,(mapcar (lambda (h) `(add-hook (quote ,h) ,func))
                     (cadr hook))))
    `(add-hook ,hook (lambda () ,@body))))

;; Bindings

;;   Sequence commands

(defvar seq-times 0
  "Number of times command was executed.
Contains random data before `seq-times' macro is called.")

(defmacro seq-times (&optional max &rest body)
  "Returns number of times ‘this-command’ was executed.
Updates `seq-times' variable accordingly to keep track.  If MAX
is specified the counter will wrap around at the value of MAX
never reaching it.  If body is given it will be evaluated if the
command is run for the first time in a sequence."
  (declare (indent 1))
  `(setq seq-times
         (if (eq last-command this-command)
             ,(if (or (null max) (eq max 'nil))
                  '(1+ seq-times)
                `(% (1+ seq-times) ,max))
           ,@body
           0)))

(defmacro seq-times-nth (body &rest list)
  "Return element of the LIST depending on number of ‘times-command’ was called.
BODY has the same meaning as in `seq-times' function.  LIST is
a list of values from which to choose value to return.  Depending
how many times command was called, element with that index will
be returned.  The counter will wrap around."
  (declare (indent 1))
  `(nth (seq-times ,(length list) ,body) ',list))

(defmacro seq-times-do (body &rest commands)
  "Evaluates command depending on number of times command was called.
BODY has the same meaning as in `seq-times' function.  COMMANDS
is a list of sexps to evaluate.  Depending how many times command
was called, sexp with that index will be evaluated.  The counter
will wrap around."
  (declare (indent 1))
  `(eval (nth (seq-times ,(length commands) ,body) ',commands)))

;;   Home/End

;; My home
(defvar my-home-end--point 0)
(defun my-home ()
   "Go to beginning of line, indent or buffer.
When called once move point to beginning of line, twice - beginning of
the buffer, thrice - back to where it was at the beginning."
   (interactive)
   (seq-times-do (setq my-home-end--point (point))
     (beginning-of-line)
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
  (seq-times-do (setq my-home-end--point (point))
    (end-of-line)
    (goto-char (point-max))
    (goto-char my-home-end--point)))

(substitute-key-definition 'move-end-of-line 'my-end (current-global-map))

;;   Save with no blanks

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

(defun save-no-blanks-done (&optional no-strip)
  "Save file and mark it done if there are any server clients."
  (interactive "P")
  (save-no-blanks no-strip)
  (and (boundp 'server-buffer-clients)
       (fboundp 'server-buffer-done)
       server-buffer-clients
       (server-buffer-done (current-buffer) t))
  (kill-buffer (current-buffer)))

(global-set-key [remap save-buffer] #'save-no-blanks)
(set-key "\C-c\C-c" save-no-blanks-done)

;;   Misc

(when (fboundp 'save-buffers-kill-terminal)
  (set-key "\C-x\C-c"      save-buffers-kill-emacs)
  (set-key "\C-xc"         save-buffers-kill-terminal))

(set-key "\C-h"          [(backspace)])
(set-key [(backspace)]   delete-backward-char)
(set-key [(delete)]      delete-forward-char)
(set-key "\C-d"          [(delete)])

(set-key "\C-x1"         delete-other-windows-vertically)
(set-key "\C-x3"
         (split-window-right)
         ;; Balance horizontally.  This is copied from balance-windows
         (let* ((window (frame-root-window))
                (frame  (window-frame window)))
           (window--resize-reset (window-frame window) t)
           (balance-windows-1 window t)
           (when (window--resize-apply-p frame t)
             (window-resize-apply frame t)
             (window--pixel-to-total frame t))))

(when (fboundp 'windmove-right)
  (set-key "\M-F"          windmove-right)
  (set-key "\M-B"          windmove-left)
  (set-key "\M-P"          windmove-up)
  (set-key "\M-N"          windmove-down))

(set-key "\C-xk"         kill-this-buffer)  ; don't ask which buffer to kill
(set-key "\C-cr"         revert-buffer)     ; Reload buffer
(set-key "\C-x\C-b"      (switch-to-buffer (other-buffer))) ; C-x C-b switch

(when (fboundp 'shift-number-up)
  (with-no-warnings (set-key "\M-+" shift-number-up)
                    (set-key "\M--" shift-number-down)))

;; Jump

(require 'ffap)
(defun my-jump () "Jump to the thing at point." (interactive)
  (let ((thing (ffap-guesser))) (if thing (ffap thing)) t))

(set-key [(control return)]        my-jump)
(set-key [(control shift mouse-1)] ffap-at-mouse)
(set-key "\C-x\C-f"                ffap)

;; Make q close current buffer if it's read-only
(set-key "q" :args (n) "p"
         (if (and buffer-read-only (not (= n 0)))
             (kill-buffer (current-buffer))
           (self-insert-command n)))

;; Minibuffer

(set-key minibuffer-local-map "\C-c"  ; C-c clears minibuffer
         delete-minibuffer-contents)
(set-key minibuffer-local-map "\C-p" previous-history-element)
(set-key minibuffer-local-map "\C-n" next-history-element)

;;   Killing, yanking, X selection, etc

;; Regions, selections and marks
(setq mouse-yank-at-point t       ;mouse yank at point not at cursor   (X-win)
      kill-read-only-ok   t       ;be silent when killing text from RO buffer
      set-mark-command-repeat-pop t
      kill-do-not-save-duplicates t)
(delete-selection-mode 1)         ;deleting region by typing or del (like Win)

(set-key esc-map "Y" (yank-pop -1))     ;move back in kill ring

(set-key [(shift insert)]
  (let ((mouse-yank-at-point nil))
    (mouse-yank-primary nil)))

;; Use browse-kill-ring
(when (fboundp 'browse-kill-ring-default-keybindings)
  (setq-default browse-kill-ring-display-duplicates nil
                browse-kill-ring-highlight-current-entry t
                browse-kill-ring-highlight-inserted-item t
                browse-kill-ring-separator "\x0C")
  (add-hook 'browse-kill-ring-hook 'form-feed-mode)
  (browse-kill-ring-default-keybindings))

;;   Just one space

(substitute-key-definition 'just-one-space
                           (lambda () (interactive) (cycle-spacing -1))
                           (current-global-map))

;;   Tab - indent or complete

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

;; (set-key "\t"    indent-or-complete)
;; (set-key [(tab)] indent-or-complete)

(add-lambda-hook 'find-file-hook
  (unless (eq major-mode 'org-mode)
    (set-key :local [(tab)] indent-or-complete)))

;;   Fkeys

;;     F1 - Help

(defun my-help ()
  "Show context dependent help.
If function given tries to `describe-function' otherwise uses
`manual-entry' to display manpage of a `current-word'."
  (interactive)
  (or (let ((var (variable-at-point)))
        (when (symbolp var) (describe-variable var) t))
      (let ((fn (function-called-at-point)))
        (when fn (describe-function fn) t))
      (man (current-word))))

(global-set-key [(f1)] help-map)
(set-key help-map [(f1)] my-help)


;;     F2 - find configuration files

(defvar mpn-find-file-map
  (let ((map (make-sparse-keymap))
        (lst `(("i" . ,(if (string-match "\\.elc$" user-init-file)
                           (substring user-init-file 0 -1)
                         user-init-file))
                ("e" . ,(concat user-emacs-directory "/early-init.el"))
                ("m" . ,(concat user-emacs-directory "/mail.el"))
                ("c" . ,(concat user-emacs-directory "/customize.el"))
                ("b" . "~/.bashrc")
                ("S" . "~/.shellrc")
                ("s" . "~/.sawfish/rc"))))
    (while lst
      (when (file-exists-p (cdar lst))
        (define-key map (caar lst)
          (let ((path (cdar lst))) (lambda () (interactive) (find-file path))))
        (setq lst (cdr lst))))
    map)
  "Keymap for characters following the F2 key.")

(global-set-key [(f2)] mpn-find-file-map)

;;     F3/F4 - keyboard macros

(eval-when-compile (require 'kmacro))
(defun kmacro-end-or-call-possibly-on-region-lines (arg &optional no-repeat)
  "End keyboard macro or call it.
End defining a keyboard macro if one is being defined and if not call
last keyboard macro ARG times or on region if `use-region-p'.
Optional argument NO-REPEAT is passed to `kmacro-call-macro' function."
  (interactive "P")
  (cond
   (defining-kbd-macro
     (if kmacro-call-repeat-key
         (kmacro-call-macro arg no-repeat t)
       (kmacro-end-macro arg)))
   ((and (eq this-command 'kmacro-view-macro)  ;; We are in repeat mode!
         kmacro-view-last-item)
    (kmacro-exec-ring-item (car kmacro-view-last-item) arg))
   ((and arg (listp arg))
    (with-no-warnings (kmacro-call-ring-2nd 1)))
   ((use-region-p)
    (apply-macro-to-region-lines (region-beginning) (region-end)))
   (t
    (kmacro-call-macro arg no-repeat))))

(set-key [(f3)] kmacro-start-macro-or-insert-counter)
(set-key [(f4)] kmacro-end-or-call-possibly-on-region-lines)

;;     F5 - Mail

(when (file-exists-p (concat user-emacs-directory "mail.el"))
  (set-key [(f5)]
           (load (concat user-emacs-directory "mail.el"))
           (with-no-warnings
             (set-key [(f5)] notmuch)
             (notmuch))))

;;     F6 - notes

(require 'remember)
(when (fboundp 'remember-notes)
  (defun remember-notes-initial-buffer ()
    (if-let ((buf (find-buffer-visiting remember-data-file)))
        ;; If notes are already open, simply return the buffer.  No further
        ;; processing necessary.  This case is needed because with daemon mode,
        ;; ‘initial-buffer-choice’ function can be called multiple times.
        buf
      (if-let ((buf (get-buffer remember-notes-buffer-name)))
          (kill-buffer buf))
      (save-current-buffer
        (remember-notes t)
        (condition-case nil
            (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
              (recover-this-file))
          (error)
          (user-error))
        (current-buffer))))
  (setq remember-notes-buffer-name "*scratch*"
        initial-buffer-choice #'remember-notes-initial-buffer)
  (set-key [(f6)] remember-notes))

;;     F7 - spell checking

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
  flyspell-mode mn-turn-flyspell-on
  :group 'flyspell)

(defun mn-turn-flyspell-on ()
  "Turn `flyspell-mode' or `flyspell-prog-mode' depending on major mode."
  (cond ((or (string-prefix-p " *" (buffer-name))
             (string-prefix-p "*" (buffer-name))
             (minibufferp)
             (derived-mode-p 'notmuch-hello-mode 'notmuch-search-mode
                             'notmuch-show-mode 'package-menu-mode
                             'dired-mode)))
        ((derived-mode-p 'prog-mode 'diff-mode) (flyspell-prog-mode))
        (t (flyspell-mode t))))

(global-flyspell-mode 1)

;;     F9 - Compilation

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
  (if (derived-mode-p 'emacs-lisp-mode)
      (byte-compile-file (buffer-file-name))
    (let (v (vars (if alt (cadr mn-compile-vars) (car mn-compile-vars))))
      (while (set 'v (pop vars)) (setenv (car v) (cadr v))))
    (if (and recompile (fboundp 'recompile))
        (recompile)
      (call-interactively 'compile))))

(set-key [(f9)]         mn-compile)
(set-key [(control f9)] :args (a) "P" (mn-compile a t))
(set-key [(meta f9)]    :args (a) "P" (mn-compile (not a) t t))
(set-key [(shift f9)]   next-error)

(require 'compile)
(setq compilation-scroll-output 'first-error  ; scroll until first error
      compilation-window-height 12            ; keep it readable
      compilation-auto-jump-to-first-error t)

(require 'ansi-color)
(add-lambda-hook 'compilation-filter-hook
  (ansi-color-apply-on-region compilation-filter-start (point)))

;;   ISearch mode ace-jump-mode

(add-hook 'isearch-mode-hook (lambda ()
 (set-key isearch-mode-map [(f1)]      isearch-mode-help)
 (set-key isearch-mode-map "\C-t"      isearch-toggle-regexp)
 (set-key isearch-mode-map "\C-c"      isearch-toggle-case-fold)
 (set-key isearch-mode-map "\C-j"      isearch-edit-string)
 (set-key isearch-mode-map "\C-h"      isearch-del-char)
 (set-key isearch-mode-map [backspace] isearch-del-char)))

(setq search-whitespace-regexp "[ \t\r]+")

(autoload 'ace-jump-mode "ace-jump-mode" "Emacs quick move minor mode" t)
(set-key "\M-s" ace-jump-mode)
(set-key [insert] ace-jump-mode)
(setq-default
 ace-jump-mode-scope 'window
 ace-jump-mode-case-fold nil
 ace-jump-mode-move-keys
 (string-to-list "htnsueoagcrlp.,;mwvzkjq'difybx/-\\@#)(+}]{![*=&$"))

;;   Copy/Kill

;; Make C-w, M-w work on word if no selection
(set-key "\C-w" (call-interactively
                 (if (use-region-p) 'kill-region 'kill-word)))
(set-key "\M-w"
  (if (use-region-p)
      (call-interactively 'kill-ring-save)
    (kill-ring-save (point) (progn (forward-word 1) (point)))
    (setq this-command 'kill-region)))

;; Syntax highlighting

;; Font lock
(require 'font-lock)
(global-font-lock-mode t)
(setq font-lock-global-modes '(not notmuch-search-mode notmuch-hello-mode))

;; Let customize keep config there
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load-file custom-file))

;; Other
(show-paren-mode t)               ;show matching parenthesis.

(defface my-fixme-face
  '((t :background "red" :foreground "white" :weight bold))
  "Face use to show FIXME and XXX markers in the text."
  :group 'whitespace)

(defface my-todo-face
  '((t :foreground "red" :weight bold))
  "Face used to show TODO markers in the text."
  :group 'whitespace)

;; Show TODO and FIXME
;; http://www.emacswiki.org/cgi-bin/wiki/EightyColumnRule
(add-lambda-hook 'font-lock-mode-hook
  (unless (eq 'diff-mode major-mode)
    (font-lock-add-keywords nil
     '(("\\<\\(TODO:?\\)\\>" 1 'my-todo-face t)
       ("\\<\\(FIXME:?\\|XXX\\)\\>" 1 'my-fixme-face t)))))

;; 'lines-tail would be great but it does not really work with tabs ≠ 8
;; characters.
(setq-default whitespace-style '(face
                                 space-before-tab::tab
                                 tab-mark
                                 tabs
                                 big-indent
                                 trailing)
              whitespace-big-indent-regexp "^\\(\t\\{4,\\}\\)")
(global-whitespace-mode)

;; Modeline
(defface mode-line-de-em
  '((t (:foreground "#696")))
  "Face used for de-emphasised elements on mode-line."
  :group 'mode-line-faces)

(defface mode-line-modified-buffer-id
  '((t (:slant italic :inherit (mode-line-buffer-id))))
  "Face used for buffer id part of the mode line when the buffer is modified."
  :group 'mode-line-faces)

(setq-default line-number-mode   t
              column-number-mode t

              mode-line-format
              '((buffer-read-only (:propertize "» " face mode-line-de-em) "  ")
                (:eval (concat
                        (propertize
                         "%14b  " 'face (if (buffer-modified-p)
                                            'mode-line-modified-buffer-id
                                          'mode-line-buffer-id))
                        (if (> (current-column) 80)
                            (propertize "%2c" 'face 'warning)
                          "%2c")
                        (propertize ":" 'face 'mode-line-de-em)
                        "%l"))
                (:eval (let ((min (point-min)) (max (point-max)))
                         (and (= min 1)
                              (= max (1+ (buffer-size)))
                              (concat
                               (propertize "/" 'face 'mode-line-de-em)
                               (save-excursion
                                 (goto-char (point-max))
                                 (format-mode-line "%l"))))))
                vc-mode
                "  "
                mode-name
                " "
                mode-line-process
                minor-mode-alist
                (:propertize " %-" face mode-line-de-em)))

;; Highlight groups of three digits
(when (fboundp 'global-num3-mode)
  (global-num3-mode t))

(when (fboundp 'mc/mark-next-like-this)
  (with-no-warnings
    (set-key "\M-." mc/mark-next-like-this)
    (set-key "\M-," mc/unmark-next-like-this)))

;; Other
(require 'icomplete)
(icomplete-mode 1)                ;nicer completion in minibuffer
(setq icomplete-prospects-height 2) ; don't spam my minibuffer
(setq suggest-key-bindings 3)     ;suggestions for shortcut keys for 3 seconds
(setq frame-title-format "Emacs") ;frame title format
(setq history-delete-duplicates t)
(setq inhibit-splash-screen   t   ;don't show splash screen
      inhibit-startup-buffer-menu t) ;don't show buffer menu when oppening
                                  ; many files               (EMACS 21.4+)
(setq paragraph-start    " *\\([*+-]\\|\\([0-9]+\\|[a-zA-Z]\\)[.)]\\|$\\)"
      require-final-newline t)    ;always end file with NL
(fset 'yes-or-no-p 'y-or-n-p)     ;make yes/no be y/n
(setq-default indicate-empty-lines t) ;show empty lines at the end of file
(setq-default indicate-buffer-boundaries t) ;show buffer boundries on fringe
(setq x-alt-keysym 'meta)         ;treat Alt as Meta even if real Meta found
(eval-when-compile (require 'ange-ftp))
(setq ange-ftp-try-passive-mode t);passive FTP
(blink-cursor-mode -1)            ;do not blink cursor
(setq blink-cursor-alist '((t      . box)  ;seriously, don't blink,
                           (box    . box)) ;blink-cursor-mode does not
      cursor-type 'box)                    ;work for me.
(setq-default cursor-type 'box)
(setq visible-bell nil)           ;no visual bell
(setq ring-bell-function (lambda ()
                           (invert-face 'mode-line)
                           (run-with-timer 0.05 nil 'invert-face 'mode-line)))
(setq line-move-visual nil) ;move by logical lines not screen lines
(setq byte-count-to-string-function
      (lambda (size) (file-size-human-readable size 'si " ")))
(when (fboundp 'describe-char-eldoc)
  (if (boundp 'eldoc-documentation-functions)
      (add-hook 'eldoc-documentation-functions #'describe-char-eldoc -50)
    (setq-default eldoc-documentation-function #'describe-char-eldoc)))

;; Saving etc
(when (fboundp recentf-mode)
  (recentf-mode -1))           ;no recent files
(setq backup-by-copying-when-linked t) ;preserve hard links
(auto-compression-mode 1)         ;automatic compression
(setq make-backup-files nil)      ;no backup
(global-auto-revert-mode 1)       ;automaticly reload buffer when changed
(setq vc-handled-backends nil)    ;I don't use vc-mode


;; Indention
(defconst set-tab--variables
  '(c-basic-offset perl-indent-level cperl-indent-level js-indent-level
    sh-basic-offset python-indent-offset css-indent-offset
    typescript-indent-level)
  "List of variables which specify indent level in various modes.")

(defun set-tab (tab)
  "Adjust `tab-width' indent level in current buffer.
`tab-width' is set to absolute value of TAB.

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
    (mapc (lambda (var)
            (when (boundp var)
              (set (make-local-variable var) tab-width)))
          set-tab--variables)
     (cond
      (negative (setq indent-tabs-mode nil))
      (prefix-arg (setq indent-tabs-mode t)))))

(setq indent-tabs-mode t
      tab-width 8)
;; Don’t set-default 'c-basic-offset.  We want to take value from c-style and
;; setting the default to something other than 'set-from-style prevents
;; dir-local c-file-style variable to work.
(dolist (var (cdr set-tab--variables))
  (set-default var 8))
(eval-when-compile (require 'tabify))
(setq tabify-regexp "^\t* [ \t]+");tabify only at the beginning of line

;; Scrolling/moving
(setq scroll-step 1               ;scroll one line
      hscroll-step 1              ;scroll one column
      next-line-add-newlines nil  ;no new lines with down arrow key
      scroll-error-top-bottom t
      scroll-preserve-screen-position t)

;; Major Modes

;;   CC Mode

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
      defun-close-semi)                ; "};" together

     (c-offsets-alist                ; Indention levels:
      ;; Don't indent inside namespaces, extern, etc
      (incomposition          . 0)
      (inextern-lang          . 0)
      (inmodule               . 0)
      (innamespace            . 0)

      ;; Preprocessor macros
      (cpp-define-intro       c-lineup-cpp-define +)
      (cpp-macro              . [ 0 ])
      (cpp-macro-cont         . +)

      ;; Brace after newline newer indents
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

      ;; Obviously, do not indent closing brace
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

      ;; Obviously, indent next line after opening brace and single statements
      (defun-block-intro      . +)
      (statement-block-intro  . +)
      (substatement           . +)

      ;; Handle nicely multi line argument lists
      (arglist-close c-lineup-arglist 0)
      (arglist-cont          c-lineup-gcc-asm-reg +)
      (arglist-cont-nonempty c-lineup-gcc-asm-reg c-lineup-arglist +)
      (arglist-intro          . +)

      ;; Misc
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

      ;; Other stuff I don't really care about
      ;; I keep it here for the sake of having all symbols specified.
      (inlambda               . c-lineup-inexpr-block)
      (knr-argdecl            . 0)
      (knr-argdecl-intro      . +)
      (lambda-intro-cont      . +)
      (objc-method-args-cont  . c-lineup-ObjC-method-args)
      (objc-method-call-cont
       ;;c-lineup-ObjC-method-call-colons
       c-lineup-ObjC-method-call +)
      (objc-method-intro      . [0]))

     ;; I don't care about anything that is below -- not using any
     ;; automagick -- but for the sake of having everything set I'll keep
     ;; it here.

     (c-hanging-braces-alist         ; Auto new lines around braces
      ;; In most cases new line after open brace and both before and
      ;; after close brace.  The "before" is however ommited
      ;; from *-close symbols because when editing normally we will
      ;; be on the new line already -- if we're not, user probably
      ;; knows better.
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

      ;; No new line after closing brace if it matches do { or if (...) {
      (block-open             after)
      (substatement-open      after)
      (block-close            . c-snug-do-while)

      ;; With brace-lists however, do nothing automatically -- user knows
      ;; better
      (brace-list-open        )
      (brace-list-close       )
      (brace-list-intro       )
      (brace-entry-open       )

      ;; Others
      (statement-cont         )
      (statement-case-open    after)
      (inexpr-class-open      )
      (inexpr-class-close     )
      (arglist-cont-nonempty  )
      )

     (c-hanging-colons-alist
      ;; Add new line after labels
      (case-label             after)
      (label                  after)
      (access-label           after)
      ;; But nothing else
      (member-init-intro      )
      (inher-intro            )

      (c-hanging-semi&comma-criteria
       (c-semi&comma-no-newlines-before-nonblanks
        c-semi&comma-inside-parenlist)))))

  (eval-when-compile (require 'cc-mode))
  (setq c-default-style '((awk-mode . "awk")
                          (other    . "mina86"))))

(setq-default python-fill-docstring-style 'pep-257-nn)

;;   HTML/XML & comapny Mode

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


(require 'sgml-mode)
(setq-default sgml-quick-keyss 'close
              sgml-specials ())

(defvar mpn-sgml-never-close-regexp
  (concat "\\`" (regexp-opt '("p" "tbody" "tr" "td" "th" "li" "dd" "dt") nil)
          "\\'"))

(defun mpn-sgml-get-context-for-close ()
  "Return context of a tag to be closed.
This is like ‘sgml-get-context’ except it omits elements with
optional closing tags.  For example, if buffer is

    <div><p>

return context for \"div\" tag rather than \"p\" since p’s close
tag is optional."
  (when-let ((ctx (save-excursion (sgml-get-context t))))
    (setq ctx (nreverse ctx))
    (while (and ctx (string-match-p mpn-sgml-never-close-regexp
                                    (sgml-tag-name (car ctx))))
      (setq ctx (cdr ctx)))
    (car ctx)))

(defun mpn-sgml-magic-self-insert-command (prefix char)
  "Adds special handling when inserting <, >, & or / character.
If called with a prefix argument (even if it’s just C-u 1), or
for a character other than <, >, & or /, act like
‘self-insert-command’.  Otherwise perform context-dependent
action for each of the characters.  If no special action applies,
simply insert the character."
  (interactive (list current-prefix-arg last-command-event))
  (when (cond (prefix)

              ((eq char ?&) (insert "&amp;"))

              ((and (eq char ?<) (eq (preceding-char) ?<))
               (delete-char -1)
               (insert "&lt;"))
              ((and (eq char ?>) (not (eq 'tag (car (sgml-lexical-context)))))
               (insert "&gt;"))

              ((and (eq char ?/)
                    (eq (preceding-char) ?<)
                    (setq char (mpn-sgml-get-context-for-close)))
               (delete-char -1)
               (insert "</" (sgml-tag-name char) ">")
               (indent-according-to-mode))

              (t))
    (self-insert-command (prefix-numeric-value prefix) char)))

(defun mpn-sgml-magic-delete-backward-char (prefix)
  (interactive "P")
  (let* ((p (point))
         (n (- p (point-min)))
         (undo (lambda (str chr)
                 (when (and (>= n (length str))
                            (string-equal
                             (buffer-substring (- p (length str)) p)
                             str))
                   (delete-region (- p (length str)) p)
                   (insert chr)))))
    (when (cond (prefix)
                ((and (use-region-p) delete-active-region))
                ((not (eq (preceding-char) ?\;)))
                ((funcall undo "&amp;" ?&))
                ((funcall undo "&lt;" ?<))
                ((funcall undo "&gt;" ?>))
                (t))
      (with-suppressed-warnings ((interactive-only delete-backward-char))
        (delete-backward-char (prefix-numeric-value prefix))))))

(define-key sgml-mode-map " " nil)
(define-key sgml-mode-map "&" #'mpn-sgml-magic-self-insert-command)
(define-key sgml-mode-map "<" #'mpn-sgml-magic-self-insert-command)
(define-key sgml-mode-map ">" #'mpn-sgml-magic-self-insert-command)
(define-key sgml-mode-map "/" #'mpn-sgml-magic-self-insert-command)
(define-key sgml-mode-map "\C-h" #'mpn-sgml-magic-delete-backward-char)
(define-key sgml-mode-map [(backspace)]
  #'mpn-sgml-magic-delete-backward-char)


;; http://github.com/nelhage/elisp/blob/master/dot-emacs
(declare-function rng-first-error "rng-valid")
(declare-function rng-next-error "rng-valid")
(defun nxml-next-error (arg reset)
  (if reset (rng-first-error))
  (rng-next-error arg))

;;   (La)TeX and nroff mode

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

;;   Misc

;; Alt+q - Fill
(defun my-fill ()
  "Fills paragraph (or region) using a cyclic order alignment.
If called once fills the paragraph to the left, twice - justifies,
three times - to the right, four times - centers."
  (interactive)
  (fill-paragraph (seq-times-nth () left full right center) t))

(set-key "\M-q"          my-fill)

(when (fboundp 'fill-single-char-nobreak-p)
  (add-hook 'fill-nobreak-predicate 'fill-single-char-nobreak-p))

(setq-default tildify-pattern "\\<[a-zA-Z]\\([ \t\n]+\\)")
(setq-default tildify-space-pattern "")

(defun mn-tildify-space-needs-hard-space-p ()
  (not (let ((ch (char-before (- (point) 2))))
         (and ch (or (eq ?’ ch) (eq ?w (char-syntax ch)))))))

(add-hook 'tildify-space-predicates #'mn-tildify-space-needs-hard-space-p)

(add-hook 'prog-mode-hook (lambda () (setq fill-column 80)))
(add-hook 'csv-mode-hook 'turn-off-auto-fill)
(add-hook 'wdired-mode-hook 'turn-off-auto-fill)

(setq-default display-fill-column-indicator t
              display-fill-column-indicator-character ?│
              auto-fill-function 'do-auto-fill
              comment-auto-fill-only-comments t)

;; Text mode
(add-lambda-hook 'text-mode-hook
  (setq word-wrap t))

;; Org mode
(setq-default
 org-insert-mode-line-in-empty-file t
 org-hide-leading-stars t
 org-startup-indented t
 org-src-fontify-natively t
 org-catch-invisible-edits 'smart
 org-agenda-start-with-follow-mode t
 org-agenda-window-setup 'current-window
 org-agenda-restore-windows-after-quit t
 org-blank-before-new-entry '((heading . t) (plain-list-item . auto))
 org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("+" . "*")))
(eval-when-compile (require 'org))
(with-eval-after-load "org"
  (with-no-warnings
    (define-key org-mode-map "\M-p" #'org-backward-element)
    (define-key org-mode-map "\M-n" #'org-forward-element))
  (define-key org-mode-map "\C-a" nil)
  (define-key org-mode-map "\C-e" nil)
  (when (fboundp 'form-feed-mode)
    (add-lambda-hook 'org-mode (form-feed-mode 1)))
  (add-lambda-hook 'org-agenda-mode-hook (hl-line-mode 1)))

(eval-when-compile (require 'org-agenda))
(set-key [(control f6)]
         (require 'org-agenda)
         (if (equal (buffer-name) org-agenda-buffer-name)
             (message "You're already in the agenda view!")
           (let ((buffer (get-buffer org-agenda-buffer-name)))
             (if buffer
                 (let ((window (get-buffer-window buffer)))
                   (if window
                       (select-window window)
                     (switch-to-buffer buffer)))
               (org-agenda nil "t")))))

;; Lisp/Scheme mode
(add-lambda-hook '(emacs-lisp-mode-hook lisp-mode-hook scheme-mode-hook)
  (setq indent-tabs-mode nil)
  ;; Show ^L as a line
  (if (fboundp 'form-feed-mode) (form-feed-mode)))

;; Sawfish mode
(autoload 'sawfish-mode "sawfish" "Mode for editing Sawfish config files")
(add-to-list 'auto-mode-alist '("sawfish/?rc\\'" . sawfish-mode))
(add-to-list 'auto-mode-alist '("\\.jl\\'" . sawfish-mode))

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
(setq-default sh-indent-for-case-label 0
              sh-indent-for-case-alt '+)

;; Various features

;; uniquify
(when (eval-when-compile (load "uniquify" t))
  (require 'uniquify)
  (setq uniquify-buffer-name-style 'reverse
        uniquify-strip-common-suffix t))

(unless (and (fboundp 'daemonp) (daemonp))
  (server-start))

(provide 'init)

;; Local
(load (concat user-emacs-directory "local.el") t)

;;; init.el ends here
