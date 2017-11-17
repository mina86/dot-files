;;                                -*- mode: emacs-lisp; lexical-binding: t -*-
;; mail.el  -- Mail configuration file
;; Copyright 2006-2013 by Michal Nazarewicz (mina86@mina86.com)
;;

(eval-when-compile (setq load-path (cons user-emacs-directory load-path)))
(require 'init)
(require 'message)

;;{{{ Identify

(setq
 user-full-name "Michal Nazarewicz"
 user-mail-address (eval-when-compile (rot13-string "zvan86@zvan86.pbz"))
 message-from-style 'angels
 message-user-fqdn "mina86.com"

 message-subject-trailing-was-query t
 message-subject-trailing-was-regexp
 "[ 	]*\\((*[Ww][Aa][Ss]:.*)\\|\\[*[Ww][Aa][Ss]:.*\\]\\)"

 message-dont-reply-to-names
 (eval-when-compile
   ;; This matches way more than it should but it’s easier to write that way.
   (concat "\\<m\\(?:ina86\\|n86\\|pn\\|nazarewicz\\)@"
           "\\(?:\\(?:g\\(?:oogle\\|mail\\)\\|mina86\\)\\.com\\|"
           "\\(?:g\\?o2\\|tlen\\)\\.pl\\)\\>"))

 notmuch-mua-cite-function 'message-cite-original-without-signature)

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
  (let ((p (point)))
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
      (insert "\nAcked-by: " user-full-name " <" user-mail-address ">\n\n"))))

(set-key message-mode-map "\C-ca" mn-ack-patch)

(defun mn-email-to-dresscode-pl ()
  "Returns whether email message is being sent to dresscode-pl@google.com."
  (save-restriction
    (message-narrow-to-headers)
    (cl-some (lambda (hdr-name)
               (let ((val (message-fetch-field hdr-name)))
                 (and val (string-match "dresscode-pl@google.com" val))))
             '("to" "cc" "bcc"))))

(defun mn-email-to-non-google ()
  "Returns whether email message is being sent to some non-@google.com address."
  (save-restriction
    (message-narrow-to-headers)
    (catch 'break
      (mapc (lambda (hdr-name)
              (let ((val (message-fetch-field hdr-name)))
                (or (not val)
                    (string-match "@\\(google\\.com\\|x\\.team\\)\\>" val)
                    (throw 'break t))))
            '("to" "cc" "bcc"))
      nil)))

(when (eval-when-compile (load "gnus-alias" t))
  (let ((has-corp (string-match "^mpn-glaptop" (system-name)))
        ;; Yeah, I know the RFC describing OpenGPG header never came to
        ;; fruition, but I might just as well use it instead of any other X-PGP
        ;; headers which are not standardised.
        (headers '(("OpenPGP" . "id=AC1F5F5CD41888F8CC8458582060401250751FF4; url=https://mina86.com/mina86.pub")))
        (signature (expand-file-name "~/.mail/signature.txt")))

    (let ((face-png (expand-file-name "~/.mail/face.png")))
      (if (not (file-exists-p face-png))
        (message "~/.mail/face.png missing; no Face header will be used")
        (with-temp-buffer
          (insert-file-contents face-png)
          (if (<= (base64-encode-region (point-min) (point-max) t) 967)
              (push (cons "Face" (buffer-string)) headers)
            (message "~/.mail/face.png > 966 chars after encoding; no Face header will be used")))))

    (setq gnus-alias-identity-alist
        (list (list "priv" nil
                    (concat user-full-name " <" user-mail-address ">")
                    "https://mina86.com/" headers "\n" signature)
              (list "dc" nil
                    (concat user-full-name " <" user-mail-address ">")
                    "Google Inc"
                    (cons (cons "Bcc" "coding-challenge-2015@googlegroups.com")
                          headers) "\n" signature)))
    (when has-corp
      (push (list "corp" nil
                  (concat user-full-name " <mpn@google.com>")
                  "Google Inc" headers "\n" signature)
            gnus-alias-identity-alist))

    (setq gnus-alias-identity-rules
          (cons '("dresscode-pl" mn-email-to-dresscode-pl "dc")
                (when has-corp
                  '(("non-google-address" mn-email-to-non-google "priv"))))))

  (setq gnus-alias-default-identity (caar gnus-alias-identity-alist))
  (add-hook 'message-setup-hook 'gnus-alias-determine-identity))

;;}}}
;;{{{ Message mode

(setq message-directory          (expand-file-name "~/.mail/Mail")
      nndraft-directory          (expand-file-name "~/.mail/Drafts")
      nnml-use-compressed-files  t

      message-kill-buffer-on-exit t

      message-citation-line-function 'message-insert-formatted-citation-line
      message-citation-line-format   "On %a, %b %d %Y, %N wrote:"

      send-mail-function    'message-smtpmail-send-it
      smtpmail-smtp-server  "smtp.gmail.com"
      smtpmail-smtp-service 587)

(add-lambda-hook 'message-mode-hook (flyspell-mode 1))
(add-hook 'message-setup-hook 'mml-secure-sign-pgpmime)

;; Fix Subject in outgoing messages
;; http://www.emacswiki.org/cgi-bin/wiki/JorgenSchaefersGnusConfig
(add-hook 'message-header-setup-hook 'mn-fix-re-subject)
(defun mn-fix-re-subject ()
  (while (re-search-forward "^Subject: \\(\\([Oo][Dd][Pp]\\|[Rr][Ee]\\)\\(\\[[0-9]+\\]\\)?: \\)+" nil t)
    (replace-match "Subject: Re: ")))

(autoload 'pgg-encrypt-region "pgg"
  "Encrypt the current region." t)
(autoload 'pgg-encrypt-symmetric-region "pgg"
  "Encrypt the current region with symmetric algorithm." t)
(autoload 'pgg-decrypt-region "pgg"
  "Decrypt the current region." t)
(autoload 'pgg-sign-region "pgg"
  "Sign the current region." t)
(autoload 'pgg-verify-region "pgg"
  "Verify the current region." t)
(autoload 'pgg-insert-key "pgg"
  "Insert the ASCII armored public key." t)
(autoload 'pgg-snarf-keys-region "pgg"
  "Import public keys in the current region." t)

(setq pgg-scheme 'gpg
      pgg-gpg-user-id "mina86"
      pgg-gpg-program "gpg2"
;      pgg-gpg-use-agent nil
;      pgg-cache-passphrase nil
      gnus-treat-x-pgp-sig t
      mm-verify-option 'known
      mm-decrypt-option 'known)

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
      notmuch-show-insert-text/plain-hook
      '(notmuch-wash-wrap-long-lines notmuch-wash-tidy-citations
        notmuch-wash-elide-blank-lines notmuch-wash-excerpt-citations)

      notmuch-message-replied-tags '("replied" "-unread")
      notmuch-message-headers '("Subject" "To" "Cc" "Bcc" "Date")

      notmuch-hello-sections
      '(notmuch-hello-insert-saved-searches
        notmuch-hello-insert-search notmuch-hello-insert-recent-searches
        notmuch-hello-insert-alltags)

      notmuch-saved-searches
      '(("to me"    . "tag:unread and not tag:linux and                      tag:me")
        ("me+linux" . "tag:unread and     tag:linux and                      tag:me")
        ("goog"     . "tag:unread and not tag:linux and     tag:goog and not tag:me")
        ("linux"    . "tag:unread and     tag:linux and not tag:goog and not tag:me")
        ("rest"     . "tag:unread and not tag:linux and not tag:goog and not tag:me"))

      notmuch-tag-formats
      '(("unread" (propertize tag 'face '(:foreground "red")))
        ("flagged" (notmuch-tag-format-image-data tag (notmuch-tag-star-icon))))
      notmuch-search-line-faces
      '(("unread" :weight bold)))

(add-hook 'notmuch-hello-refresh-hook
          (lambda ()
            (if (and (eq (point) (point-min))
                     (search-forward "Saved searches:" nil t))
                (progn
                  (forward-line)
                  (widget-forward 1))
              (if (eq (widget-type (widget-at)) 'editable-field)
                  (beginning-of-line)))))

;(define-key notmuch-hello-mode-map [tab] 'widget-forward)

(set-key notmuch-show-mode-map "h"
         (unless (notmuch-show-next-open-message)
           (notmuch-show-next-thread t)))
(set-key notmuch-show-mode-map "H" (notmuch-show-next-message t))
(set-key notmuch-show-mode-map "t"
         (unless (notmuch-show-previous-open-message)
           (notmuch-show-previous-thread t)))
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
             ("b"    nil "+deleted" "-unread")
             ("u"    nil "+unread")
             ("\M-u" nil "-deleted" "+unread")
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

(add-lambda-hook '(notmuch-hello-mode-hook notmuch-search-hook)
  (if (fboundp 'turn-off-fci-mode)
      (turn-off-fci-mode)))

;;}}}
