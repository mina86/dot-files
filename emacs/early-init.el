;; early-init.el  -- Emacs early configuration file   -*- lexical-binding: t -*-

;; Disable GC while Emacs is initialising.  We get it back to normal after
;; initialisation is done.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)
(add-hook 'after-init-hook (lambda ()
                             (setq gc-cons-threshold (* 64 1024 1024)
                                   gc-cons-percentage 0.1)
                             (garbage-collect))
          t)

;; Set frame parameters.  We do it in early-init to avoid flickering of the
;; initial frame as the settings are applied after its displayed.  We could do
;; this by setting the options via X resources but that would spread Emacs
;; configuration into multiple places.  It’s cleaner to do it here.
(let ((font "-adobe-courier-medium-r-normal-*-10-*-*-*-*-*-iso10646-1"))
  (setq default-frame-alist `((font             . ,font)
                              (left             . 0)
                              (top              . 0)
                              (fullscreen       . fullheight)
                              (width            . 80)
                              (right-fringe     . 4)
                              (left-fringe      . 4)
                              (menu-bar-lines   . 0)
                              (tool-bar-lines   . 0)
                              (vertical-scroll-bars)
                              (foreground-color . "gray")
                              (background-color . "black")
                              (background-mode  . dark)
                              (wait-for-wm      . nil))
        initial-frame-alist default-frame-alist)
  (set-face-attribute 'default nil :font font))

(setq frame-resize-pixelwise       t
      frame-inhibit-implied-resize t)

;; Ignore X resources.  Everything we care about is already set above.
(advice-add #'x-apply-session-resources :override #'ignore)

;; Ignore site-start and default initialisation files if they exist.  Whatever
;; they include, we’re overriding or don’t care about.
;(setq site-run-file        nil
;      inhibit-default-init t)

(provide 'early-init)
