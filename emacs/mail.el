;;                                -*- mode: emacs-lisp; lexical-binding: t -*-
;; mail.el  -- Mail configuration file
;; Copyright 2006-2013 by Michal Nazarewicz (mina86@mina86.com)
;;

(eval-when-compile (setq load-path (cons user-emacs-directory load-path)))
(require 'init)

;;{{{ Identify

(setq
 user-full-name "Michal Nazarewicz"
 user-mail-address (eval-when-compile (rot13-string "zvan86@zvan86.pbz"))
 message-from-style 'angels
 message-user-fqdn "mina86.com")

(when (eval-when-compile (load "gnus-alias" t))
  (let ((has-corp (string-match "^mpn-glaptop" (system-name)))
        (headers '(("X-PGP" . "50751FF4")
                   ("X-PGP-FP" . "AC1F 5F5C D418 88F8 CC84 5858 2060 4012 5075 1FF4")
                   ("X-Face" . "PbkBB1w#)bOqd`iCe\"Ds{e+!C7`pkC9a|f)Qo^BMQvy\\q5x3?vDQJeN(DS?|-^$uMti[3D*#^_Ts\"pU$jBQLq~Ud6iNwAw_r_o_4]|JO?]}P_}Nc&\"p#D(ZgUb4uCNPe7~a[DbPG0T~!&c.y$Ur,=N4RT>]dNpd;KFrfMCylc}gc??'U2j,!8%xdD")
                   ("Face" . "iVBORw0KGgoAAAANSUhEUgAAADAAAAAwBAMAAAClLOS0AAAAJFBMVEWbfGlUPDDHgE57V0jUupKjgIObY0PLrom9mH4dFRK4gmjPs41MxjOgAAACQElEQVQ4jW3TMWvbQBQHcBk1xE6WyALX1069oZBMlq+ouUwpEQQ6uRjttkWP4CmBgGM0BQLBdPFZYPsyFUo6uEtKDQ7oy/U96XR2Ux8ehH/89Z6enqxBcS7Lg81jmSuujrfCZcLI/TYYvbGj+jbgFpHJ/bqQAUISj8iLyu4LuFHJTosxsucO4jSDNE0Hq3hwK/ceQ5sx97b8LcUDsILfk+ovHkOIsMbBfg43VuQ5Ln9YAGCkUdKJoXR9EclFBhixy3EGVz1K6eEkhxCAkeMMnqoAhAKwhoUJkDrCqvbecaYINlFKSRS1i12VKH1XpUd4qxL876EkMcDvHj3s5RBajHHMlA5iK32e0C7VgG0RlzFPvoYHZLRmAC0BmNcBruhkE0KsMsbEc62ZwUJDxWUdMsMhVqovoT96i/DnX/ASvz/6hbCabELLk/6FF/8PNpPCGqcZTGFcBhhAaZZDbQPaAB3+KrWWy2XgbYDNIinkdWAFcCpraDE/knwe5DBqGmgzESl1p2E4MWAz0VUPgYYzmfWb9yS4vCvgsxJriNTHoIBz5YteBvg+VGISQWUqhMiByPIPpygeDBE6elD973xWwKkEiHZAHKjhuPsFnBuArrzxtakRcISv+XMIPl4aGBUJm8Emk7qBYU8IlgNEIpiJhk/No24jHwkKTFHDWfPniR4iw5vJaw2nzSjfq2zffcE/GDjRC2dn0J0XwPAbDL84TvaFCJEU4Oml9pRyEUhR3Cl2t01AoEjRbs0sYugp14/4X5n4pU4EHHnMAAAAAElFTkSuQmCC")))
        (signature (expand-file-name "~/.mail/signature.txt")))

    (setq gnus-alias-identity-alist
        (cons `("priv" nil
                ,(concat user-full-name " <" user-mail-address ">")
                "http://mina86.com/"
                ,headers
                "\n"
                ,signature)
              (and has-corp `(("corp" nil
                               ,(concat user-full-name " <mpn@google.com>")
                               "Google Inc"
                               ,headers
                               "\n"
                               ,signature))))

        gnus-alias-identity-rules
        (when has-corp
          `(("non-google-address"
             ,(lambda ()
                (save-restriction
                  (message-narrow-to-headers)
                  (catch 'break
                    (let ((val (mapconcat 'message-fetch-field
                                          '("to" "cc" "bcc") " ")))
                      (set-match-data (list 0 0))
                      (while (string-match "@[-A-Za-z0-9.]+" val (match-end 0))
                        (unless (string= "@google.com"
                                         (downcase (match-string 0 val)))
                          (throw 'break t)))))))
             "priv")))

        gnus-alias-default-identity
        (if has-corp "corp" "priv")))

  (add-hook 'message-setup-hook 'gnus-alias-determine-identity))

;;}}}
;;{{{ Message mode

(setq message-directory          (expand-file-name "~/.mail/Mail")
      nndraft-directory          (expand-file-name "~/.mail/Drafts")
      nnml-use-compressed-files  t

      message-kill-buffer-on-exit t

      message-citation-line-function 'message-insert-formatted-citation-line
      message-citation-line-format   "On %a, %b %d %Y, %N wrote:"

      message-generate-hashcash 'opportunistic

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
        ("rest"     . "tag:unread and not tag:linux and not tag:goog and not tag:me")))

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
(define-key notmuch-show-mode-map "q"    'notmuch-kill-this-buffer)

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
