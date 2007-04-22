;;                                 -*- mode: lisp; indent-tabs-mode: nil -*-
;;{{{ Header
;;
;; RISC Mode  v. 0.3
;; $Id: risc-mode.el,v 1.3 2007/04/22 10:16:31 mina86 Exp $
;; Copyright (c) 2006 by Michal Nazarewicz (mina86/AT/mina86.com)
;;
;; This software is OSI Certified Open Source Software.
;; OSI Certified is a certification mark of the Open Source Initiative.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
;;
;;}}}
;;{{{ Commentary
;;
;;{{{ Idea
;;
;; All actions which  one may do in a text editor  can be divided into
;; two groups: typing  and doing other stuff.  In  EMACS it is trivial
;; to type  as it's enought to type  a character you want  to put into
;; a buffer whereas 'other stuff'  require key sequences some of which
;; are pretty  long or hard  to type.  That's  OK as long as  you type
;; mostly but what if you need to  kill a lot of lines and your finger
;; hurts of holding a control key?
;;
;; RISC Mode provides  another mode in which a  certain actions (other
;; then inserting a characters) are assigned to each letter.  This way
;; you  don't need  to hold  a control  key to  perform tasks  such as
;; killing lines or  words.  You still need to hold  shift key in some
;; situations but it's rather rare. ;)
;;
;; Personally I find RISC Mode very  useful as I don't need to move my
;; hand to the parto of the  keyboard with arrows as 'ijkl' works just
;; as good.
;;
;; Where  did  the  name  come  from?  CISC  CPUs  provide  a  lot  of
;; instructions encoded  in a variable length (posibly  very long) bit
;; sequences.  RISC  CPUs provide a  small set of commands  encoded in
;; a fixed,  short bit sequences.  Now  imagine that a bit  is one key
;; stroke  immidietly you  see that  EMACS is  a "CISC  editor"  as it
;; provides a  lot of  commands with a  variable length  key bindings.
;; RISC Mode provides  a short key bindings for  only the basic editor
;; commands.
;;
;;}}}
;;{{{ Installatin
;;
;; Put  'risc-mode.el'  somewhere `load'  will  find  it  and put  the
;; fallowing lines to your '.emacs' file:
;;
;; (autoload 'risc-mode        "risc-mode" "Toggles RISC Mode on or off" t)
;; (autoload 'risc-mode-toggle "risc-mode" "Toggles RISC Mode on or off" t)
;; (global-set-key "\C-c\C-c"     'risc-mode-toggle)
;; (global-set-key (kbd "<menu>") 'risc-mode-toggle)
;;
;; If you are used to using  menu key for executing commands or set it
;; to do something else you may want to comment out the last line.
;;
;; I also added the fallowing to my .Xmodmap file:
;;
;;   remove Lock = Caps_Lock
;;   keysym Caps_Lock = Menu Help
;;
;; so it's enought  to press Caps Lock, which is  qutie easy to press,
;; to toggle RISC Mode.
;;
;;}}}
;;{{{ ModeLine
;;
;; When RISC  Mode is enabled  EMACS behaves completly  different then
;; usually so  a easy way  to dystinguish whether  it is on or  off is
;; needed.  Putting  'RISC' in  ModeLine does not  work as it  is quit
;; small and easy to overlook.   Because of that, when RISC Mode (Poor
;; RISC Mode) is enabled a  background color of ModeLine is changed to
;; red  (yellow).   This  is  controlled  by  `risc-mode-modeline-bg',
;; `poor-risc-mode-modeline-bg'     and    `risc-mode-off-modeline-bg'
;; varaibles.
;;
;;}}}
;;{{{ Poor RISC Mode
;;
;; Poor RISC Mode  is enabled by presing either Q or  '.  In this mode
;; EMACS  behaves  almost  "normal".   The  only  difference  is  that
;; pressing RET  or ESC ESC  turns RISC Mode  on again (and  Poor RISC
;; Mode off).  This is ment to be an easy way to insert few words when
;; in RISC Mode.
;;
;;}}}
;;{{{ Key Bindings
;;
;;  i j k l  -- moves by one char/line in an obvious manner
;;  I J K L  -- moves by one word/paragraph in an obvious manner
;;
;;  h a      -- moves to the beginning of line [1]
;;  ; e      -- moves to the end of line [2]
;;  A        -- moves to the beginning of buffer
;;  E        -- moves to the end of buffer
;;
;;  M-i M-k  -- scrolls window down/up one line w/o moving the cursor
;;  n p      -- scrolls one screen down/up [3]
;;  /        -- recenteres window [4]
;;
;;  N P      -- moves cursor to the next/previous window in a frame
;;  H :      -- changes current buffer to the previous/next
;;
;;  x        -- deletes char
;;  X        -- deletes char backward
;;  D        -- kills line
;;
;;  di dk    -- kills previous/next line
;;  dj dl    -- deletes previous/next char
;;  dI dK    -- kills previous/next paragraph
;;  dJ dL    -- kills previous/next word
;;
;;  dh da    -- kills characters from the beginning of line
;;  d; de    -- kills characters till the end of line
;;  dd       -- kills whole line
;;
;;  q        -- inserts the next typed character
;;  Q '      -- enables Poor RISC Mode
;;  dq       -- toogles overwrite mode
;;
;;  o O      -- inserts new line before the current (and turns Poor RISC Mode)
;;  m M      -- inserts new line after the current (and turns Poor RISC Mode)
;;
;;  t        -- transposes chars
;;  T        -- transposes words
;;  dt       -- transposes lines
;;
;;  v        -- sets mark
;;  dv       -- kills selection
;;  VV       -- exchanges point and mark [5]
;;
;;  b        -- reads pressed char and saves bookmark [6]
;;  B        -- reads pressed char and jumps to saved bookmark [6]
;;  db       -- reads pressed char and deletes saved bookmark [6]
;;  SPC      -- alias of "b "
;;  C-SPC    -- alias of "B "
;;
;;  G        -- if defining a macro stops defining;
;;              else if defined macro exists calls it;
;;              else starts defining a macro [7]
;;  dG       -- if defining a macro inserts counter, deletes macro otehrwise
;;  .        -- repeats last command
;;
;;  s S      -- isearches forward/backward
;;  f F      -- isearches forward/backward regexp
;;  w W      -- (query) replaces a string
;;  r R      -- (query) replaces a regexp
;;
;;  g        -- `keyboard-quit'
;;  u        -- `universal-argument'
;;  y        -- `yank'
;;  Y        -- `yank' or `yank-pop' if last command was `yank'
;;  z        -- undos
;;  Z        -- undos with limit to changes within the current region
;;  ?        -- whatever M-/ was bound to.
;;
;;  c        -- an alias to C-x
;;  cc       -- disables RISC Mode
;;  C        -- reserved for user
;;
;; *** Notes
;;
;; [1] "h" and "a"  are set to the same thing "C-a"  was set when RISC
;;     Mode was loaded.
;; [2] ";" and "e"  are set to the same thinf "C-e"  was set when RISC
;;     Mode was loaded.
;; [3] "n" and "p"  are set to the same thing "C-v"  and "M-v" was set
;;     when RISC Mode was loaded.
;; [4] "/" is set to the same thing "C-l" was set when RISC Mode was
;;     loaded.
;; [5]  "V" is ment  to be  a prefix  character so  in the  future new
;;     "V<x>" bindings may be added.
;; [6] Any  key other then  SPC is used  verbatim as a  bookmark name.
;;     SPC is  treated specialy and evaluated either  to last bookmark
;;     name used or buffer name if no bookmark was set yet.
;; [7] Note that "dGG" will always start a new macro even if one is
;;     already defined.
;;
;;}}}
;;
;;}}}
;;{{{ Variables

;; Toggle variables
(defcustom risc-mode nil
  "Non-nil if RISC Mode is enabled.
See the command `risc-mode' for a description of this minor-mode.
Setting this variable directly does not take effect; use either
\\[customize] or the function `risc-mode'."
  :set        'custom-set-minor-mode
  :initialize 'custom-initialize-default
  :group      'risc-mode
  :type       'boolean)

(defcustom poor-risc-mode nil
  "Non-nil if Poor RISC Mode is enabled.
See the command `risc-mode' for a description of this minor-mode.
Setting this variable directly does not take effect; use either
\\[customize] or the function `poor-risc-mode'."
  :set        'custom-set-minor-mode
  :initialize 'custom-initialize-default
  :group      'poor-risc-mode
  :type       'boolean)


; We need to disable RISC mode in minibuffer
(defvar risc-mode-map-enabled nil)
(defvar poor-risc-mode-map-enabled nil)

(add-hook 'minibuffer-setup-hook
          (lambda () (setq risc-mode-map-enabled        nil
                           poor-risc-mode-map-enabled   nil)))

(add-hook 'minibuffer-exit-hook
          (lambda () (setq risc-mode-map-enabled        risc-mode
                           poor-risc-mode-map-enabled   poor-risc-mode)))


;; Keymaps
(defvar risc-mode-map (make-keymap) "Keymap for `risc-mode'.")
(defvar poor-risc-mode-map (make-keymap) "Keymap for `poor-risc-mode'.")

;; Macro for easier key setting
(defmacro risc-set-key (key func) "Defines a key in `risc-mode-map'."
  (list 'define-key 'risc-mode-map key func))

;; Poor RISC mode
(define-key poor-risc-mode-map "\n"             'risc-mode)
(define-key poor-risc-mode-map "\e\e"           'risc-mode)
(define-key poor-risc-mode-map (kbd "<ret>")    'risc-mode)
(define-key poor-risc-mode-map (kbd "<return>") 'risc-mode)


;; Modeline backgrounds
(defcustom risc-mode-modeline-bg "red"
  "`modeline' background when `risc-mode' is enabled."
  :set        'custom-set-minor-mode
  :initialize 'custom-initialize-default
  :group      'risc-mode
  :type       'color)

(defcustom poor-risc-mode-modeline-bg "darkgreen"
  "`modeline' background when `poor-risc-mode' is enabled."
  :set        'custom-set-minor-mode
  :initialize 'custom-initialize-default
  :group      'risc-mode
  :type       'color)

(defcustom risc-mode-off-modeline-bg "blue"
  "`modeline' background when `risc-mode' is disabled."
  :set        'custom-set-minor-mode
  :initialize 'custom-initialize-default
  :group      'risc-mode
  :type       'color)


;; Add minor modes
(add-minor-mode 'risc-mode        " RISC")
(add-minor-mode 'poor-risc-mode   " risc")
(setq minor-mode-map-alist
      (cons (cons 'risc-mode-map-enabled risc-mode-map)
            (assq-delete-all 'risc-mode-map-enabled  minor-mode-map-alist)))
;; poor-risc-mode-map is added by risc-mode

;;}}}
;;{{{ Functions

;; RISC Mode
(defun risc-mode (&optional arg msg)
  "Toggle RISC Mode and Poor RISC Mode on or off.
Interactively, with no prefix argument, toggle the RISC Mode.
With universal prefix ARG turn RISC Mode on.
With zero or negative ARG turn RISC and Poor Risc Modes off.
With `arg' being \"p\" turns Poor RISC Mode on.
Only one of the modes RISC Mode and Poor RISC Mode
can be enabled at the same time.

If `msg' is no-nill prints message in echo area even if called
noninteractively unless it is -1 in which case the message won't
be displayed.

RISC Mode Povides short, 1- and 2-character long key bindings for
many common operations.  This way it is much easier to move
around the text and transform it's content then when the mode is
disabled however it is much harder to insert any text.

Poor RISC Mode adds a key bindings for RET and ESC ESC which turn
RISC Mode on as well as binding for C-c C-c which turns Poor RISC
Mode off (and does not enable RISC Mode)."
  (interactive "P")
  (if (eq arg 'toggle) (setq arg nil))
  (let ((prm poor-risc-mode) (rm risc-mode))

    ;; Turn modes on/off
    (setq risc-mode (if (not arg) (not risc-mode)
                        (and (not (equal arg "p"))
                             (> (prefix-numeric-value arg) 0))))
    (setq poor-risc-mode               (equal arg "p"))
    (setq risc-mode-map-enabled        risc-mode
          poor-risc-mode-map-enabled   poor-risc-mode)

    ;; Set modeline background
    (set-face-background
     'modeline (cond (risc-mode        risc-mode-modeline-bg)
                     (poor-risc-mode   poor-risc-mode-modeline-bg)
                     (t                risc-mode-off-modeline-bg)))

    ;; Move Poor RISC Mode keymap at the beginnig
    (unless (or (not poor-risc-mode)
                (eq (caar minor-mode-map-alist) 'poor-risc-mode-map-enabled))
      (setq minor-mode-map-alist
            (cons (cons 'poor-risc-mode-map-enabled poor-risc-mode-map)
                  (assq-delete-all 'poor-risc-mode-map-enabled
                                   minor-mode-map-alist))))

    ;; 'c' as alias of C-x
    (unless (or (not risc-mode) rm)
      (risc-set-key "c" (copy-keymap (lookup-key (current-global-map) "\C-x")))
      (risc-set-key "cc" 'risc-mode-toggle)
      (risc-set-key "c\C-c" (lambda () (interactive) (error "Use C-x C-c"))))

    ;; On/off hooks
    (if (and rm  (not risc-mode))
        (run-hooks 'risc-mode-hook 'risc-mode-off-hook))
    (if (and prm (not poor-risc-mode))
        (run-hooks 'poor-risc-mode-hook 'poor-risc-mode-off-hook))
    (if (and (not rm ) risc-mode)
        (run-hooks 'risc-mode-hook 'risc-mode-on-hook))
    (if (and (not prm) poor-risc-mode)
        (run-hooks 'poor-risc-mode-hook 'poor-risc-mode-on-hook))

    ;; Customize
    (unless (not (called-interactively-p))
      (customize-mark-as-set risc-mode)
      (customize-mark-as-set poor-risc-mode))

    ;; Message
    (if (and (not (current-message)) (not (eq msg -1))
             (or msg (called-interactively-p)))
        (cond
         (risc-mode        (message "RISC Mode enabled"))
         (poor-risc-mode   (message "Poor RISC Mode enabled"))
         (rm               (message "RISC Mode disabled"))
         (prm              (message "Poor RISC Mode disabled"))))

    ;; Final stuff
    (force-mode-line-update)
    risc-mode))


;; Poor RISC Mode
(defun poor-risc-mode (&optional arg msg)
  "Toggle Poor RISC Mode and RISC Mode on or off.
Interactively, with no prefix argument, toggle the Poor RISC Mode and
sets RISC Mode to the state opposite to Poor RISC mode.
With universal prefix ARG turn Poor RISC Mode on and RISC Mode off.
With zero or negative ARG turn Poor RISC Mode and RISC mode off.
Fore more information see `risc-mode'.

If `msg' is no-nill prints message in echo area even if called
noninteractively unless it is -1 in which case the message won't
be displayed."
  (interactive (list (or current-prefix-arg nil) nil))
  (if (eq arg 'toggle) (setq arg nil))
  (setq msg (or msg (called-interactively-p)))
  (cond
   ((and (eq arg nil) (not poor-risc-mode))   (risc-mode "p" msg))
   ((and (eq arg nil)      poor-risc-mode )   (risc-mode 1   msg))
   ((> (prefix-numeric-value arg) 0)          (risc-mode "p" msg))
   (t                                         (risc-mode 0   msg)))
  poor-risc-mode)


;; Toggles RISC Mode taking Poor RISC mode into account
(defun risc-mode-toggle (&optional arg msg)
  "Toggle Poor RISC Mode and RISC Mode on or off.
Interactively, with no prefix argument, disables RISC and Poor RISC
mode if any of those modes is enabled otherwise enables RISC Mode.
With universal prefix ARG turn RISC Mode on.
With zero or negative ARG turn RISC Mode and Poor RISC mode off.
Fore more information see `risc-mode'.

If `msg' is no-nill prints message in echo area even if called
noninteractively unless it is -1 in which case the message won't
be displayed."
  (interactive (list (or current-prefix-arg nil) nil))
  (if (eq arg 'toggle) (setq arg nil))
  (risc-mode (if arg arg (if (or risc-mode poor-risc-mode) -1 1))
             (or msg (called-interactively-p))))

;;}}}
;;{{{ Moving

;; Basic movement
(risc-set-key "l"   'forward-char)
(risc-set-key "L"   'forward-word)
(risc-set-key "j"   'backward-char)
(risc-set-key "J"   'backward-word)
(risc-set-key "k"   'next-line)
(risc-set-key "K"   'forward-paragraph)
(risc-set-key "i"   'previous-line)
(risc-set-key "I"   'backward-paragraph)

;; Home & End
(risc-set-key "h"   (lookup-key (current-global-map) "\C-a"))
(risc-set-key "a"   (lookup-key (current-global-map) "\C-a"))
(risc-set-key ";"   (lookup-key (current-global-map) "\C-e"))
(risc-set-key "e"   (lookup-key (current-global-map) "\C-e"))
(risc-set-key "A"   'beginning-of-buffer)
(risc-set-key "E"   'end-of-buffer)

;; Scroll screen and page up/down
(risc-set-key "\ei" (lambda () (interactive) (scroll-down 1)))
(risc-set-key "\ek" (lambda () (interactive) (scroll-up   1)))
(risc-set-key "p"   (lookup-key (current-global-map) "\ev"))
(risc-set-key "n"   (lookup-key (current-global-map) "\C-v"))

;; Window/frame/buffer manpipulation
(risc-set-key "N"   'other-window)
(risc-set-key "P"   (lambda (arg) (interactive "p") (other-window (- 0 arg))))
(risc-set-key "/"   (lookup-key (current-global-map) "\C-l"))
(risc-set-key "H"   'previous-buffer)
(risc-set-key ":"   'next-buffer)

;;}}}
;;{{{ Modification

;; Delete
(risc-set-key "x"   'delete-char)
(risc-set-key "X"   'backward-delete-char)

(risc-set-key "dl"  'delete-char)
(risc-set-key "dj"  'backward-delete-char)
(risc-set-key "dL"  'kill-word)
(risc-set-key "dJ"  'backward-kill-word)

(defun kill-line-backwards (&optional arg)
  "Kill characters backwards till the beginning of the current line;
if no nonblanks there, kill thru newline.
With prefix argument, kill that many lines from point.
Negative arguments kill lines forward.
With zero argument, kills the text after point on the current line.

When calling from a program, nil means \"no arg\",
a number counts as a prefix arg.

To kill a whole line, when point is not at the beginning, type \
\\[end-of-line] \\[kill-line-backwards] \\[kill-line-backwards].

If `kill-whole-line' is non-nil, then this command kills the whole line
plus previous line's terminating newline, when used at the end of a line
with no argument.  As a consequence, you can always kill a whole line
by typing \\[end-of-line] \\[kill-line-backwards].

If you want to append the killed line to the last killed text,
use \\[append-next-kill] before \\[kill-line-backwards].

If the buffer is read-only, Emacs will beep and refrain from deleting
the line, but put the line in the kill ring anyway.  This means that
you can use this command to copy text from a read-only buffer.
\(If the variable `kill-read-only-ok' is non-nil, then this won't
even beep.)"
  (interactive "P")
  (kill-region
   (point)
   (progn
     (if arg
         (if (= arg 0)
             (end-of-line)
           (forward-visible-line (- 0 (prefix-numeric-value arg))))
       (if (bobp) (signal 'beginning-of-buffer nil))
       (if (eobp) (signal 'end-of-buffer       nil))
       (skip-chars-backward " \t")
       (if (or kill-whole-line (bolp))
           (progn (forward-visible-line -1) (end-of-line))
         (forward-visible-line 0)))
     (point))))


(risc-set-key "dk"  'kill-line)
(risc-set-key "D"   'kill-line)
(risc-set-key "di"  'kill-line-backwards)
(risc-set-key "dI"
  (lambda (arg) (interactive "p")
    (kill-region (point) (save-excursion (backward-paragraph arg) (point)))))
(risc-set-key "dK"
  (lambda (arg) (interactive "p")
    (kill-region (point) (save-excursion (forward-paragraph arg) (point)))))

(risc-set-key "dd"  'kill-whole-line)

(risc-set-key "dh"  "u0dk")
(risc-set-key "d;"  "u0di")
(risc-set-key "da"  "u0dk")
(risc-set-key "de"  "u0di")

;; Insert
(risc-set-key "q"    (lambda (arg) (interactive "*p")
                       (setq last-command-char (read-char))
                       (self-insert-command arg)))
(risc-set-key "Q"    'poor-risc-mode)
(risc-set-key "'"    'poor-risc-mode)
(risc-set-key "dq"   'overwrite-mode)

;; Insert line
(risc-set-key "o"
  (lambda () (interactive) (beginning-of-line)(call-interactively 'open-line)))
(risc-set-key "O"
  (lambda () (interactive) (beginning-of-line)(call-interactively 'open-line)
    (poor-risc-mode)))

(risc-set-key "m"
  (lambda () (interactive) (end-of-line) (call-interactively 'newline)))
(risc-set-key "M"
  (lambda () (interactive) (end-of-line) (call-interactively 'newline)
    (poor-risc-mode)))

;; Transpose
(risc-set-key "t"    'transpose-chars)
(risc-set-key "T"    'transpose-words)
(risc-set-key "dt"   'transpose-lines)

;; Selecting
(risc-set-key "v"    'set-mark-command)
(risc-set-key "dv"   'kill-region)
(risc-set-key "VV"   'exchange-point-and-mark)

;;}}}
;;{{{ Bookmarks

(require 'bookmark)

(defun risc-bookmark-read-name ()
  (let ((char (read-char)))
    (if (eq char 32)
        (or bookmark-current-bookmark (bookmark-buffer-name))
      (format "%c"
              (if (and enable-multibyte-characters
                       (>=
                  (unibyte-char-to-multibyte char)
                char)))))))

(risc-set-key "b"   (lambda (a) (interactive "p")
                      (bookmark-set    (risc-bookmark-read-name) a)))
(risc-set-key "B"   (lambda () (interactive)
                      (bookmark-jump   (risc-bookmark-read-name))))
(risc-set-key "db"  (lambda () (interactive)
                      (bookmark-delete (risc-bookmark-read-name))))

(risc-set-key " "   "b ")
(risc-set-key (kbd "C-SPC") "B ")

;;}}}
;;{{{ Macros

(defun risc-macro (arg)
  "If `defining-kbd-macro' function will `kmacro-end-macro'
otherwise if no macro is defined will `kmacro-start-macro' otherwise
will `call-last-kbd-macro'."
  (interactive "P")
  (if defining-kbd-macro (if arg (end-kbd-macro arg) (end-kbd-macro))
    (if last-kbd-macro (call-last-kbd-macro arg) (start-kbd-macro arg))))

(defun risc-macro-delete-or-insert-counter (a)
  "If `defining-kbd-macro' or `executing-kbd-macro' will
`kmacro-insert-counter' otherwise will set `last-kbd-macro' to
nil."
  (interactive "P")
  (if (or defining-kbd-macro executing-kbd-macro)
      (kmacro-insert-counter) (setq last-kbd-macro ())))

(risc-set-key "G"   'risc-macro)
(risc-set-key "dG"  'risc-macro-delete-or-insert-counter)

(risc-set-key "."   'repeat)

;;}}}
;;{{{ Other

;; Search and replace
(risc-set-key "s"    'isearch-forward)
(risc-set-key "S"    'isearch-backward)
(risc-set-key "f"    'isearch-forward-regexp)
(risc-set-key "F"    'isearch-backward-regexp)
(risc-set-key "w"    'replace-string)
(risc-set-key "W"    'query-replace)
(risc-set-key "r"    'replace-regexp)
(risc-set-key "R"    'query-replace-regexp)

;; Misc
(risc-set-key "g"    'keyboard-quit)
(risc-set-key "u"    'universal-argument)
(risc-set-key "y"    'yank)
(risc-set-key "Y"    (lambda () (interactive)
                       (call-interactively
                        (if (eq last-command 'yank) 'yank-pop 'yank))))
(risc-set-key "z"    'undo)
(risc-set-key "Z"    "uz")
(risc-set-key (kbd "?")   (lookup-key (current-global-map) "\M-/"))


;; Add "u" to universal universal-argument-map
(defun risc-universal-argument-more ()
  "Calls `universal-argument-more' if in `risc-mode' or
`universal-argument-other-key' otherwise."
  (interactive)
  (call-interactively (if risc-mode 'universal-argument-more
                        'universal-argument-other-key)))

(unless (assq 117 universal-argument-map)
  (setcdr universal-argument-map
          (cons
           (cons 117 'risc-universal-argument-more)
           (cdr universal-argument-map))))

;;}}}
