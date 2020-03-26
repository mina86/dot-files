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
(defconst corp-mail-address
  (eval-when-compile (rot13-string "zca@uhqfba-genqvat.pbz")))

(setq user-full-name "Michal Nazarewicz"
      user-mail-address (eval-when-compile (rot13-string "zvan86@zvan86.pbz"))
      message-user-fqdn "mina86.com"

      message-alternative-emails (regexp-opt (list user-mail-address
                                                   alt-mail-address
                                                   corp-mail-address)
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

(defun mn-determine-gnus-alias-identity ()
  (let (has-priv has-corp has-non-corp)
    (message-with-reply-buffer
      (save-restriction
        (mail-narrow-to-head)
        (let ((priv-re (concat "\\<" (regexp-opt (list user-mail-address
                                                       alt-mail-address)
                                                 nil) "\\>"))
              (corp-re "@hudson-trading\\.com\\>")
              emails)
          (setq emails (mapcar #'message-fetch-field
                               '("from" "to" "cc" "bcc"))
                emails (delq nil emails)
                emails (mapcar #'message-tokenize-header emails)
                emails (apply #'nconc emails)
                emails (mapcar #'mail-strip-quoted-names emails))
          (dolist (email emails)
            (cond ((string-match priv-re email) (setq has-priv t))
                  ((string-match corp-re email) (setq has-corp t))
                  ((setq has-non-corp t)))))))
    (gnus-alias-use-identity
     (cond (has-corp (if (or has-priv has-non-corp) "ext" "corp"))
           (has-priv "priv")
           ((with-no-warnings gnus-alias-default-identity))))))

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

(when (eval-and-compile (load "gnus-alias" t))
  (let ((signature (expand-file-name "~/.mail/signature.txt"))
        (ext-sign (concat "Best regards\nMichał Nazarewicz"))
        (headers (if-let ((face-png (expand-file-name "~/.mail/face.png"))
                          (face-hdr (mn-load-face face-png)))
                     (cons face-hdr nil)))
        (corp-org "Hudson River Trading")
        (user-from (concat user-full-name " <" user-mail-address ">"))
        (corp-from (concat user-full-name " <" corp-mail-address ">")))
    (setq gnus-alias-identity-alist
          `(("corp" nil ,corp-from ,corp-org ,headers "\n" ,signature)
            ("ext"  nil ,corp-from ,corp-org nil      "\n" ,ext-sign)
            ("priv" nil ,user-from nil       ,headers "\n" ,signature))
          gnus-alias-default-identity (caar gnus-alias-identity-alist)))
  (add-hook 'message-setup-hook #'mn-determine-gnus-alias-identity))

;;}}}
;;{{{ Message mode

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
      '(("asm"     . "is:unread and  is:asm and  is:me")
        ("to me"   . "is:unread and -is:asm and  is:me and -is:foss")
        ("me+foss" . "is:unread and -is:asm and  is:me and  is:foss")
        ("corp"    . "is:unread and -is:asm and -is:me and -is:foss and  is:corp")
        ("foss"    . "is:unread and -is:asm and -is:me and  is:foss and -is:corp")
        ("rest"    . "is:unread and -is:asm and -is:me and -is:foss and -is:corp"))

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

;;}}}
