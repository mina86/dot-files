;;                                -*- mode: emacs-lisp; lexical-binding: t -*-
;; mail.el  -- Mail configuration file
;; Copyright 2006-2020 by Michal Nazarewicz (mina86@mina86.com)
;;

(eval-when-compile (setq load-path (cons user-emacs-directory load-path)))
(require 'init)
(require 'message)

;;{{{ Identify

(defconst alt-mail-address
  (eval-when-compile (rot13-string "zanmnerjvpm@tznvy.pbz")))

(setq user-full-name "Michal Nazarewicz"
      user-mail-address (eval-when-compile (rot13-string "zvan86@zvan86.pbz"))
      message-user-fqdn "mina86.com"

      message-alternative-emails (regexp-opt (list user-mail-address
                                                   alt-mail-address)
                                             nil)
      message-dont-reply-to-names message-alternative-emails

      message-subject-trailing-was-query t
      message-subject-trailing-was-regexp
      "[ \t]*\\((*[Ww][Aa][Ss]:.*)\\|\\[*[Ww][Aa][Ss]:.*\\]\\)"
      message-subject-re-regexp
      (eval-when-compile (concat "^[ \t]*\\("
                                   "\\(" "[Ff][Ww][Dd]?"
                                   "\\|" "[Oo][Dd][Pp]"
                                   "\\|" "[Rr][Ee]"
                                   "\\)"
                                   "\\(\\[[0-9]*\\]\\)*:[ \t]*"
                                   "\\)*[ \t]*"))

      message-kill-buffer-on-exit t
      message-citation-line-function 'message-insert-formatted-citation-line
      message-citation-line-format   "On %a, %b %d %Y, %N wrote:"
      send-mail-function    'message-smtpmail-send-it)

(setq-default smtpmail-smtp-server  "smtp.gmail.com"
              smtpmail-smtp-service 587)

;; Get domain from the From address when generating Message-IDs
(defun mn-message-make-fqdn ()
  (if-let* ((addr (message-fetch-field "from"))
            (addr (cadr (mail-extract-address-components addr))))
      (and (string-match "@\\(.*\\)\\'" addr)
           (match-string 1 addr))))
(advice-add #'message-make-fqdn :before-until #'mn-message-make-fqdn)

;; Use random left part of the Message-ID
(defun mn-message-unique-id ()
  (random t)
  (let ((chars "0123456789abcdefghijklmnopqrstuvwxyz+") str)
    (while (< (length str) 24)
      (setq str (cons (aref chars (random (length chars))) str)))
    (apply #'string str)))
(advice-add #'message-unique-id :before-until #'mn-message-unique-id)


;; Since 27.1 message-from-style is obsolete; suppress warning.
(with-no-warnings (setq message-from-style 'angels))

(defun message-narrow-to-body ()
  (widen)
  (goto-char (point-min))
  (re-search-forward
   (concat "^" (regexp-quote mail-header-separator) "\n") nil t)
  (narrow-to-region
   (point)
   (if (re-search-forward message-signature-separator)
       (match-beginning 0) (point-max))))

(defun mn-ack-patch ()
  (interactive)
  (save-restriction
    (message-narrow-to-body)
    (let ((case-fold-search t)
          (prefix (concat "^" message-cite-prefix-regexp "[[:blank:]]*"))
          (p (point)))
      (goto-char (point-min))
      (beginning-of-line
       (cond
        ;; Insert Acked-by just below ‘> Cc: User’ if found.
        ((re-search-forward (concat prefix "Cc:[[:blank:]]*"
                                    (regexp-quote user-full-name))
                            nil t) 2)
        ;; Insert Acked-by above --- line separating message from diff.
        ((re-search-forward (concat prefix "---$") nil t) 1)
        ;; Insert Acked-by below diff stats line.
        ((re-search-forward (concat prefix "[0-9]+ files? changed") nil t) 2)
        ;; Lastly, try ‘diff <file-name>’ line in front of first hunk.
        ((re-search-forward (concat prefix "diff ") nil t) 1)
        ;; If even that fails, insert where we are (were).
        ((goto-char p) 2))))
    (insert "\nAcked-by: " user-full-name " <" user-mail-address ">\n\n")))

(set-key message-mode-map "\C-ca" mn-ack-patch)

(defun mn-load-face (face-png)
  "Returns Face header encoding specified face image or nil on error."
  (if (not (file-exists-p face-png))
      (message "%s.png missing; no Face header will be used" face-png)
    (with-temp-buffer
      (insert-file-contents face-png)
      (if (> (base64-encode-region (point-min) (point-max) t) 967)
          (message "%s > 966 chars after encoding; no Face header will be used"
                   face-png)
        (cons "Face" (buffer-string))))))

(require 'gnus-alias)
(let ((signature (expand-file-name "~/.mail/signature.txt"))
      (headers (if-let ((face-png (expand-file-name "~/.mail/face.png"))
                        (face-hdr (mn-load-face face-png)))
                   (cons face-hdr nil)))
      (user-from (concat user-full-name " <" user-mail-address ">")))
  (setq gnus-alias-identity-alist
        `(("priv" nil ,user-from nil       ,headers "\n" ,signature))
        gnus-alias-default-identity (caar gnus-alias-identity-alist)))
(add-hook 'message-setup-hook 'gnus-alias-determine-identity)

;;}}}
;;{{{ Message mode

(defvar mn-message-home--point 0)
(defun mn-message-home ()
  "‘message-mode’-aware ‘my-home’.

When point is on a header line, the point goes to 1. beginning of
the header, 2. beginning and 3. wraps to it’s original position.

Otherwise (when it’s in message body), the point goes
to 1. beginning of line, 2. beginning of the message body
and 3. wrap to it’s original position."
  (interactive)
  (seq-times-do (setq mn-message-home--point (point))
    ;; First time:
    (progn
      (beginning-of-line)
      (when (message-point-in-header-p)
        (re-search-forward ": *" (point-at-eol) t)))
    ;; Second time:
    (if (message-point-in-header-p)
        (beginning-of-line)
      (message-goto-body))
    ;; Third time:
    (goto-char mn-message-home--point)))

(define-key message-mode-map [remap message-beginning-of-line]
  #'mn-message-home)

(add-lambda-hook 'message-mode-hook (flyspell-mode 1))
;(add-hook 'message-setup-hook 'mml-secure-sign-pgpmime)

;(autoload 'pgg-encrypt-region "pgg"
;  "Encrypt the current region." t)
;(autoload 'pgg-encrypt-symmetric-region "pgg"
;  "Encrypt the current region with symmetric algorithm." t)
;(autoload 'pgg-decrypt-region "pgg"
;  "Decrypt the current region." t)
;(autoload 'pgg-sign-region "pgg"
;  "Sign the current region." t)
;(autoload 'pgg-verify-region "pgg"
;  "Verify the current region." t)
;(autoload 'pgg-insert-key "pgg"
;  "Insert the ASCII armored public key." t)
;(autoload 'pgg-snarf-keys-region "pgg"
;  "Import public keys in the current region." t)
;
;(setq pgg-scheme 'gpg
;      pgg-gpg-user-id "mina86"
;      pgg-gpg-program "gpg2"
;;      pgg-gpg-use-agent nil
;;      pgg-cache-passphrase nil
;      gnus-treat-x-pgp-sig t
;      mm-verify-option 'known
;      mm-decrypt-option 'known)

;;}}}
;;{{{ Notmuch

(require 'notmuch)

(setq notmuch-show-logo nil

      notmuch-search-oldest-first nil
      notmuch-search-result-format
      '(("subject" . " %-69.69s")
        ("count"   . "  %7s")
        ("tags"    . "  (%s)\n")
        ("authors" . "  %-63.63s")
        ("date"    . "  %12s"))

      notmuch-show-mark-read-tags nil
      notmuch-show-all-multipart/alternative-parts nil
      notmuch-show-relative-dates nil
      notmuch-show-insert-text/plain-hook '(notmuch-wash-tidy-citations
                                            notmuch-wash-elide-blank-lines
                                            notmuch-wash-excerpt-citations)

      notmuch-message-replied-tags (list "+replied" "-unread")
      notmuch-message-headers '("Subject" "To" "Cc" "Bcc" "Date")

      notmuch-hello-sections
      '(notmuch-hello-insert-saved-searches
        notmuch-hello-insert-search notmuch-hello-insert-recent-searches
        notmuch-hello-insert-alltags)

      notmuch-saved-searches
      '(("to me"   . "is:unread and -is:asm and  is:me and -is:foss")
        ("me+foss" . "is:unread and -is:asm and  is:me and  is:foss")
        ("foss"    . "is:unread and -is:asm and -is:me and  is:foss and -is:hrt")
        ("rest"    . "is:unread and -is:asm and -is:me and -is:foss and -is:hrt"))

      notmuch-tag-formats
      '(("unread" (propertize tag 'face '(:foreground "red")))
        ("flagged" (notmuch-tag-format-image-data tag (notmuch-tag-star-icon))))
      notmuch-search-line-faces
      '(("unread" :weight bold)))

(setq-default notmuch-mua-cite-function
              'message-cite-original-without-signature)

(add-lambda-hook 'notmuch-hello-refresh-hook
  (if (and (eq (point) (point-min))
           (search-forward "Saved searches:" nil t))
      (progn
        (forward-line)
        (widget-forward 1))
    (if (eq (widget-type (widget-at)) 'editable-field)
        (beginning-of-line))))

;(define-key notmuch-hello-mode-map [tab] 'widget-forward)

(set-key notmuch-show-mode-map "h" (unless (notmuch-show-next-open-message)
                                     (notmuch-show-next-thread t)))
(set-key notmuch-show-mode-map "H" (notmuch-show-next-message t))
(set-key notmuch-show-mode-map "t" (unless (notmuch-show-previous-open-message)
                                     (notmuch-show-previous-thread-show)))
(set-key notmuch-show-mode-map "T" (notmuch-show-previous-message))

(define-key notmuch-search-mode-map "h" 'notmuch-search-next-thread)
(define-key notmuch-search-mode-map "t" 'notmuch-search-previous-thread)

(define-key notmuch-show-mode-map "\C-t" 'notmuch-show-view-raw-message)
(define-key notmuch-show-mode-map "q"    'notmuch-bury-or-kill-this-buffer)

(define-key notmuch-show-mode-map "f" 'notmuch-show-reply)
(define-key notmuch-show-mode-map "F" 'notmuch-show-reply-sender)
(define-key notmuch-search-mode-map "f" 'notmuch-search-reply-to-thread)
(define-key notmuch-search-mode-map "F" 'notmuch-search-reply-to-thread-sender)

(define-key notmuch-show-mode-map "s" 'notmuch-search)
(define-key notmuch-show-mode-map "w" 'notmuch-show-save-attachments)

(dolist (x '(("V"    t   "+mute" "-unread")
             ("v"    t   "-unread")
             ("b"    nil "+trash" "-unread")
             ("u"    nil "+unread")
             ("\M-u" nil "-trash" "+unread")
             ("d"    nil "-unread")))
  (let ((key  (car  x))
        (all  (cadr x))
        (tags (cddr x)))
   (set-key notmuch-show-mode-map key
            (if all
                (notmuch-show-tag-all tags)
              (notmuch-show-tag tags))
            (if (or all (not (notmuch-show-next-open-message)))
              (notmuch-show-next-thread t)))
   (set-key notmuch-search-mode-map key
            (notmuch-search-tag tags)
            (notmuch-search-next-thread))))

;; (add-lambda-hook '(notmuch-hello-mode-hook notmuch-search-hook)
;;   (if (fboundp 'turn-off-fci-mode)
;;      (turn-off-fci-mode)))

(defun mn-prettify-subject (subject)
  (save-match-data
    (setq subject (if subject (string-trim subject) "(no subject)"))
    (let ((start 0))
      (while (string-match (eval-when-compile
                             (let ((chrs "[:cntrl:]\x7f\u2028\u2029"))
                               (concat "[ " chrs "]\\{2,\\}\\|[" chrs "]+")))
                           subject start)
        (setq subject (replace-match " " t t subject)
              start (1+ (match-beginning 0)))))
    (if (string-match
         (concat "^\\[Differential] \\[[^]]+] \\(?:\\[[-+ ]+] \\)?\\|"
                 "^\\[JIRA] Updates for ")
                      subject)
        (substring subject (match-end 0))
      subject)))

(defun mn-notmuch-search-insert-field-advice (orig field format-string result)
  (if (string-equal field "subject")
      (insert (propertize
               (format format-string
                       (mn-prettify-subject (plist-get result :subject)))
               'face 'notmuch-search-subject))
    (funcall orig field format-string result)))

(add-function :around (symbol-function #'notmuch-search-insert-field)
              #'mn-notmuch-search-insert-field-advice)
(add-lambda-hook 'notmuch-show-hook
  (when header-line-format
    (setq header-line-format (mn-prettify-subject header-line-format))))

;;}}}
