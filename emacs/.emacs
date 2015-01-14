;; Initializing path vars to the local bin directory, to the mu4e source files and to my org-agenda dir.
(defvar paul/path-localbin)
(defvar paul/path-mu4esource)
(defvar paul/path-org-agenda-files "~/org/")

(if (eq system-type 'darwin)
    (setq
     paul/path-localbin "/usr/local/bin/"
     paul/path-mu4esource "/usr/local/Cellar/mu/0.9.10/share/emacs/site-lisp/mu4e"
     )
  (setq
   paul/path-localbin "~/.local/bin/"
   paul/path-mu4esource "~/.local/share/emacs/site-lisp/mu4e"
   )
  )

;; Add the local bin directory to the exec path so that emacs find all needed binaries.
(add-to-list 'exec-path paul/path-localbin)

;; Enable package manager
(require 'package)
(package-initialize)
(setq package-enable-at-startup nil)

;; Add custom lisp code to path and add melpa and org package dirs
(add-to-list 'load-path (concat user-emacs-directory "lisp"))
(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                         ("org" . "http://orgmode.org/elpa/")
			 ("gnu" . "http://elpa.gnu.org/packages/")))

;; Install and activate use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; Show line numbers
(global-linum-mode t)

;; Wrap text after 79 chars
(setq fill-column 79)

;; Set the color-theme to solarized-dark
(use-package solarized-theme
  :pre-load
  (setq solarized-high-contrast-mode-line t)
  :defer t
  :ensure solarized-theme
  :init
  (load-theme 'solarized-dark t)
  )

;; Setting font face and font size according to OS.
(if (eq system-type 'darwin)
    (set-default-font "PragmataPro for Powerline 14")
  (set-default-font "PragmataPro for Powerline 12")
  )

;; Borrowed from http://ionrock.org/emacs-email-and-mu.html
;; Choose account label to feed msmtp -a option based on From header
;; in Message buffer; This function must be added to
;; message-send-mail-hook for on-the-fly change of From address before
;; sending message since message-send-mail-hook is processed right
;; before sending message.
(defun paul/choose-msmtp-account ()
  (if (message-mail-p)
      (save-excursion
        (let*
            ((from (save-restriction
                     (message-narrow-to-headers)
                     (message-fetch-field "from")))
             (account
              (cond
               ((string-match "paul.bubenzer@gmail.com" from) "Gmail")
               ((string-match "paul.bubenzer@googlemail.com" from) "Gmail")
               ((string-match "p.bubenzer@uni-muenster.de" from) "WWU")
               ((string-match "p.bubenzer@wwu.de" from) "WWU")
               ((string-match "paul.bubenzer@uni-muenster.de" from) "WWU")
               ((string-match "paul.bubenzer@wwu.de" from) "WWU")
               ((string-match "p_bube02@uni-muenster.de" from) "WWU")
               ((string-match "p_bube02@wwu.de" from) "WWU"))))
          (setq message-sendmail-extra-arguments (list '"-a" account))))))

(defun paul/set-from-address ()
  "Set the From address based on the To address of the original."
  (let ((msg mu4e-compose-parent-message)) ;; msg is shorter...
    (if msg
	(setq user-mail-address
	      (cond
	       ((mu4e-message-contact-field-matches msg :to "@wwu")
		"p.bubenzer@wwu.de")
	       ((mu4e-message-contact-field-matches msg :to "@uni-muenster")
		"p.bubenzer@uni-muenster.de")
	       ((mu4e-message-contact-field-matches msg :to "@gmail")
		"paul.bubenzer@gmail.com")
	       ((mu4e-message-contact-field-matches msg :to "@googlemail")
		"paul.bubenzer@googlemail.com")
	       (t "paul.bubenzer@gmail.com"))))))

;; Adding mu4e mail support
(add-to-list 'load-path paul/path-mu4esource)
(use-package mu4e
  :commands mu4e
  :config
  (progn
    (setq
     mu4e-mu-binary (concat paul/path-localbin "mu")
     mu4e-maildir "~/.mail"
     mu4e-sent-folder "/Gmail/sent"
     mu4e-drafts-folder "/Gmail/drafts"
     mu4e-trash-folder "/Gmail/Trash"
     mu4e-refile-folder "/Gmail/archive"
     mu4e-get-mail-command "offlineimap"
     mu4e-update-interval 300
     mu4e-headers-results-limit 100
     mu4e-user-mail-address-list '("p.bubenzer@wwu.de" "paul.bubenzer@gmail.com")
     mu4e-sent-messages-behavior 'sent
     mu4e-compose-signature-auto-include nil
     user-mail-address "p.bubenzer@wwu.de"
     user-full-name "Paul Bubenzer"
     message-kill-buffer-on-exit t
     message-send-mail-function 'message-send-mail-with-sendmail
     message-sendmail-envelope-from 'header
     sendmail-program (concat paul/path-localbin "msmtp")
     )
    (add-hook 'message-send-mail-hook 'paul/choose-msmtp-account)
    (add-hook 'mu4e-compose-pre-hook 'paul/set-from-address)
    (add-to-list 'mu4e-bookmarks '("maildir:/Gmail/Uni" "University Inbox" ?a))
    (add-to-list 'mu4e-bookmarks '("maildir:/Gmail/INBOX" "Regular Gmail-Inbox" ?b))
    (evil-set-initial-state 'mu4e-headers-mode 'normal)
    (evil-set-initial-state 'mu4e-view-mode 'normal)
    (evil-define-key 'normal 'mu4e-headers-mode-map
      "k" 'mu4e-headers-prev
      "j" 'mu4e-headers-next
      "y" 'mu4e~headers-jump-to-maildir
      "w" 'mu4e-select-other-view
      "o" 'mu4e-update-mail-and-index
      "/" 'mu4e-headers-search
      "?" 'mu4e-headers-search-edit
      "l" 'mu4e-headers-search-narrow
      (kbd "<")  'mu4e-headers-query-prev
      (kbd ">") 'mu4e-headers-query-next
      "b" 'mu4e-headers-search-bookmark
      "B" 'mu4e-headers-search-bookmark-edit
      "O" 'mu4e-headers-change-sorting
      "P" 'mu4e-headers-toggle-threading
      "Q" 'mu4e-headers-toggle-full-search
      "W" 'mu4e-headers-toggle-include-related
      "V" 'mu4e-headers-toggle-skip-duplicates
      "q" 'mu4e~headers-quit-buffer
      "%" 'mu4e-headers-mark-pattern
      "t" 'mu4e-headers-mark-subthread
      "T" 'mu4e-headers-mark-thread
      (kbd "<backspace>")  'mu4e-headers-mark-for-trash
      (kbd "d")            'mu4e-headers-mark-for-trash
      (kbd "<delete>")     'mu4e-headers-mark-for-delete
      (kbd "<deletechar>") 'mu4e-headers-mark-for-delete
      (kbd "D")            'mu4e-headers-mark-for-delete
      (kbd "m")            'mu4e-headers-mark-for-move
      (kbd "r")            'mu4e-headers-mark-for-refile
      (kbd "!")            'mu4e-headers-mark-for-read
      (kbd "u")            'mu4e-headers-mark-for-unmark
      (kbd "+")            'mu4e-headers-mark-for-flag
      (kbd "-")            'mu4e-headers-mark-for-unflag
      (kbd "=")            'mu4e-headers-mark-for-untrash
      (kbd "&")            'mu4e-headers-mark-custom
      (kbd "*")              'mu4e-headers-mark-for-something
      (kbd "#")   'mu4e-mark-resolve-deferred-marks
      "U" 'mu4e-mark-unmark-all
      "x" 'mu4e-mark-execute-all
      "a" 'mu4e-headers-action
      "R" 'mu4e-compose-reply
      "F" 'mu4e-compose-forward
      "C" 'mu4e-compose-new
      "E" 'mu4e-compose-edit
      (kbd "RET") 'mu4e-headers-view-message
      [mouse-2]   'mu4e-headers-view-message
      "$" 'mu4e-show-log
      "H" 'mu4e-display-manual
      )
    (evil-define-key 'normal 'mu4e-view-mode-map
      "k" 'mu4e-view-headers-prev
      "j" 'mu4e-view-headers-next
      "y" 'mu4e~headers-jump-to-maildir
      "w" 'mu4e-select-other-view
      "o" 'mu4e-update-mail-and-index
      "/" 'mu4e-headers-search
      "?" 'mu4e-view-search-edit
      "l" 'mu4e-view-search-narrow
      (kbd "<")  'mu4e-headers-query-prev
      (kbd ">") 'mu4e-headers-query-next
      "q" 'mu4e~view-quit-buffer
      "v" 'mu4e-view-open-attachment
      "b" 'mu4e-headers-search-bookmark
      "B" 'mu4e-headers-search-bookmark-edit
      "%" 'mu4e-view-mark-pattern
      "t" 'mu4e-view-mark-subthread
      "T" 'mu4e-view-mark-thread
      (kbd "C-b") 'mu4e-view-go-to-url
      "F" 'mu4e-compose-forward
      "R" 'mu4e-compose-reply
      "C" 'mu4e-compose-new
      "E" 'mu4e-compose-edit
      "." 'mu4e-view-raw-message
      "|" 'mu4e-view-pipe
      "a" 'mu4e-view-action
      "O" 'mu4e-headers-change-sorting
      "P" 'mu4e-headers-toggle-threading
      "Q" 'mu4e-headers-toggle-full-search
      "W" 'mu4e-headers-toggle-include-related
      (kbd "SPC") 'mu4e-view-scroll-up-or-next
      (kbd "<backspace>") 'mu4e-scroll-down
      "e" 'mu4e-view-save-attachment
      "A" 'mu4e-view-attachment-action
      "d" 'mu4e-view-mark-for-trash
      (kbd "<delete>") 'mu4e-view-mark-for-delete
      (kbd "<deletechar>") 'mu4e-mark-for-delete
      (kbd "D") 'mu4e-view-mark-for-delete
      (kbd "m") 'mu4e-view-mark-for-move
      (kbd "r") 'mu4e-view-mark-for-refile
      (kbd "&") 'mu4e-view-mark-custom
      (kbd "+") 'mu4e-view-mark-for-flag
      (kbd "-") 'mu4e-view-mark-for-unflag
      (kbd "=") 'mu4e-view-mark-for-untrash
      (kbd "*")             'mu4e-view-mark-for-something
      (kbd "<kp-multiply>") 'mu4e-view-mark-for-something
      (kbd "<insert>")     'mu4e-view-mark-for-something
      (kbd "<insertchar>") 'mu4e-view-mark-for-something
      (kbd "#") 'mu4e-mark-resolve-deferred-marks
      "h" 'mu4e-view-toggle-hide-cited
      (kbd "M-q") 'mu4e-view-fill-long-lines
      "u" 'mu4e-view-unmark
      "U" 'mu4e-view-unmark-all
      "x" 'mu4e-view-marked-execute
      "$" 'mu4e-show-log
      "H" 'mu4e-display-manual
      )
    )
  )

;; No long answering anymore
(fset 'yes-or-no-p 'y-or-n-p)

;; Fuzzy search buffer and file names
(use-package projectile
  :ensure projectile
  :init
  (projectile-global-mode)
  )

;; Fuzzy search command names
(use-package smex
  :commands (smex)
  :ensure smex
  :init
  (smex-initialize)
  )

;; auto-complete
(use-package popup
  :ensure popup
  )
(use-package auto-complete
  :ensure auto-complete
  :init
  (ac-config-default)
  )
;; (use-package auto-complete-auctex
  ;; :ensure auto-complete-auctex
  ;; )

;; Snippets
(use-package yasnippet
  :ensure yasnippet
  :init
  (yas-global-mode t)
  )

;; Adding markdown support
(use-package markdown-mode
  :ensure markdown-mode
  :init
  (progn
    (add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
    (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
    (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
    )
  :config
  (use-package markdown-mode+
    :ensure markdown-mode+
    )
  )

;; Adding on-the-fly syntax checking via flycheck
(use-package flycheck
  :ensure flycheck
  )

;; Adding pass support
(use-package password-store
  :ensure password-store
  :init
  (setq password-store-executable (concat paul/path-localbin "pass"))
  )

;; This is needed for password-store and sets up gpg-agent environment vars
(use-package keychain-environment
  :ensure keychain-environment
  :init
  (keychain-refresh-environment))

;; Activate evil-mode + convenient subpackages
(use-package evil-leader
  :pre-load
  (setq evil-search-module 'evil-search
	evil-want-C-u-scroll t
	evil-want-C-w-in-emacs-state t)
  :ensure evil-leader
  :commands (evil-leader-mode)
  :demand evil-leader
  :init
  (global-evil-leader-mode)
  :config
  (progn
    (evil-leader/set-key
      "," 'ibuffer
      "\\" 'pp-eval-last-sexp
      "a" 'add-parantheses
      "b" (lambda() (interactive) (add-parantheses "big"))
      "ce" 'LaTeX-environment
      "d" 'dired-jump
      "e" (lambda() (interactive) (find-file "~/.emacs"))
      "f" 'projectile-find-file
      "h" 'help-for-help
      "m" 'mu4e
      "oa" 'org-agenda
      "ob" 'org-iswitchb
      "oc" 'org-capture
      "oj" (lambda() (interactive) (org-capture nil "j"))
      "ol" 'org-store-link
      "oo" 'org-open-at-point
      "os" 'org-schedule
      "ot" 'org-todo
      "pc" 'password-store-copy
      "x" (lambda() (interactive) (TeX-command "LatexMk" 'TeX-master-file -1))
      )
    )
  )

(use-package evil
  :ensure evil
  :demand evil
  :init
  (evil-mode t)
  :config
  (progn
    (use-package evil-indent-textobject
      :ensure evil-indent-textobject
      )
    (use-package evil-tabs
      :ensure evil-tabs
      )
    (use-package evil-visualstar
      :ensure evil-visualstar
      )
    (use-package evil-surround
      :ensure evil-surround
      :init
      (global-evil-surround-mode 1)
      )
    (setq sentence-end-double-space nil)
    )
  )

;; Evil keybindings
;;; Use SPC to execute commands via smex
(define-key evil-normal-state-map (kbd "SPC") 'smex)
;;; Fold-simulation
(define-key evil-normal-state-map (kbd "zM") 'hide-body)
(define-key evil-normal-state-map (kbd "zm") 'hide-subtree)
(define-key evil-normal-state-map (kbd "zR") 'show-all)
(define-key evil-normal-state-map (kbd "zr") 'show-subtree)
(define-key evil-normal-state-map (kbd "zj") 'outline-next-visible-heading)
(define-key evil-normal-state-map (kbd "zk") 'outline-previous-visible-heading)
;;; Use Us to get the magit-status window
;;; vim-unimpaired emulation
(define-key evil-normal-state-map (kbd "]a") 'next-buffer)
(define-key evil-normal-state-map (kbd "[a") 'previous-buffer)
(define-key evil-normal-state-map (kbd "]b") 'next-buffer)
(define-key evil-normal-state-map (kbd "[b") 'previous-buffer)
;;; Use Q instead of gq
(define-key evil-normal-state-map (kbd "Q") 'evil-fill-and-move)
;;; Function for the parantheses macro
(defun add-parantheses (&optional size)
  (interactive)
  (if (eq size nil)
      (setq left-par "\\left" right-par "\\right")
    (setq left-par (concat "\\" size "l") right-par (concat "\\" size "r"))
    )
  (evil-jump-item)
  (insert right-par)
  (evil-jump-item)
  (insert left-par)
  )

(defun paul/org-save-all-org-buffers ()
  "Save all current org-buffers, switch into the org-directory and make a git commit with the current date and time as the commit msg."
  (interactive)
  (org-save-all-org-buffers)
  (cd paul/path-org-agenda-files)
  (start-process "git" nil "git" "commit" "-am" (format-time-string "%Y-%m-%d %H:%M")) 
  )

(defun paul/org-push ()
  "Retrieves the password for rep/bit using password-store and runs magit-push."
  (interactive)
  (cd paul/path-org-agenda-files)
  (password-store-copy "rep/bit")
  (magit-push)
  )

(defun paul/find-table-location ()
  "Find the right table location using the current year and month."
  (let ((year (string-to-number (format-time-string "%Y")))
	(month (string-to-number (format-time-string "%m"))))
    (org-datetree-find-month-create year month)
    )
  )

(use-package org
  :config
  (progn
    (use-package org-datetree
      :commands (org-datetree-find-month-create)
      )
    (setq
     org-todo-keywords '((sequence "TODO(t)" "APPT(a)" "|" "DONE(d)"))
     org-todo-keyword-faces '(("APPT"  . (:foreground "sienna" :weight bold)))
     org-log-done 'time
     org-agenda-files (list (concat paul/path-org-agenda-files "work.org") (concat paul/path-org-agenda-files "home.org"))
     org-capture-templates
     '(("t" "Todo" entry (file+headline (concat paul/path-org-agenda-files "work.org") "Inbox")
	"* TODO %?")
       ("j" "Journal" plain (file+datetree (concat paul/path-org-agenda-files "thesis_diary.org"))
	)
       ("m" "Money entry" table-line (file+function (concat paul/path-org-agenda-files "money.org") paul/find-table-location)
	"| | %? | |" :table-line-pos "III-1")
       ("n" "General note" entry (file+datetree (concat paul/path-org-agenda-files "notes.org")))
       )
     org-latex-to-pdf-process (list "latexmk %f")
     org-src-fontify-natively t
     )
    (evil-set-initial-state 'org-agenda-mode 'normal)
    (evil-define-key 'normal org-agenda-mode-map
      (kbd "C-m") 'org-agenda-switch-to
      "i" 'org-agenda-clock-in
      "j" 'org-agenda-next-item
      "k" 'org-agenda-previous-item
      "l" 'org-agenda-log-mode
      "o" 'org-agenda-clock-out
      "p" 'paul/org-push
      "q" 'org-agenda-Quit
      "r" 'org-agenda-redo
      "s" 'paul/org-save-all-org-buffers
      "t" 'org-agenda-todo
      )
    )
  )

;; Adding git support via magit
(use-package magit
  :commands (magit-status magit-diff-unstaged)
  :ensure magit
  :init
  (progn
    (define-key evil-normal-state-map (kbd "Us") 'magit-status)
    (define-key evil-normal-state-map (kbd "Ud") 'magit-diff-unstaged)
    )
  :config
  (progn
    (setq magit-commit-all-when-nothing-staged t)
    (evil-set-initial-state 'magit-mode 'normal)
    (evil-set-initial-state 'magit-status-mode 'normal)
    (evil-set-initial-state 'magit-diff-mode 'normal)
    (evil-set-initial-state 'magit-log-mode 'normal)
    (evil-define-key 'normal magit-mode-map
        "j" 'magit-goto-next-section
        "k" 'magit-goto-previous-section
        "c" 'magit-key-mode-popup-committing
        "p" 'magit-key-mode-popup-pushing
        "f" 'magit-key-mode-popup-fetching
        "F" 'magit-key-mode-popup-pulling
        "s" 'magit-stage-item
        "S" 'magit-stage-all
	"u" 'magit-unstage-item
	"U" 'magit-unstage-all
	"q" 'magit-mode-quit-window
	)
    (evil-define-key 'normal magit-log-mode-map
        "j" 'magit-goto-next-section
        "k" 'magit-goto-previous-section)
    (evil-define-key 'normal magit-diff-mode-map
        "j" 'magit-goto-next-section
        "k" 'magit-goto-previous-section)
    )
  )

;; iBuffer bindings
(eval-after-load 'ibuffer
  '(progn
     (evil-set-initial-state 'ibuffer-mode 'normal)
     (evil-define-key 'normal ibuffer-mode-map
       (kbd "j") 'evil-next-line
       (kbd "k") 'evil-previous-line
       (kbd "J") 'ibuffer-jump-to-buffer
       (kbd "l") 'ibuffer-visit-buffer
       (kbd "v") 'ibuffer-toggle-marks
       )
     )
  )

;; Dired-x bindings and config
(defun my-dired-up-directory ()
  "Take dired up one directory, but behave like dired-find-alternate-file"
  (interactive)
  (let ((old (current-buffer)))
    (dired-up-directory)
    (kill-buffer old)
    ))

(use-package dired-x
  :commands (dired-jump)
  :config
  (progn
    (put 'dired-find-alternate-file 'disabled nil)
    (evil-set-initial-state 'dired-mode 'normal)
    (evil-define-key 'normal dired-mode-map
      "h" 'my-dired-up-directory
      "l" 'dired-find-alternate-file
      "o" 'dired-sort-toggle-or-edit
      "v" 'dired-toggle-marks
      "m" 'dired-mark
      "u" 'dired-unmark
      "U" 'dired-unmark-all-marks
      "c" 'dired-create-directory
      "n" 'evil-search-next
      "N" 'evil-search-previous
      "q" 'kill-this-buffer)
    )
  )

;; LaTeX stuff

;; Smart-parens mode for automatically pairing parentheses
;; (use-package smartparens
;;:ensure smartparens
;;)
;; (add-hook 'LaTeX-mode-hook 'smartparens-mode)
;;; AucTex
(use-package tex
  :ensure auctex
  :config
  (progn
    (setq TeX-command-default "latexmk")
    (use-package auctex-latexmk
      :ensure auctex-latexmk
      :init
      (auctex-latexmk-setup)
      )
    (use-package latex-preview-pane
      :ensure latex-preview-pane
      :init
      (latex-preview-pane-enable)
      )
    )
  )

(defun return-indent-in-latex ()
  (local-set-key (kbd "C-m") 'newline-and-indent)
  )
;; Enable cdlatex in LaTeX-mode
(use-package cdlatex
  :ensure cdlatex
  :config
  (setq cdlatex-paired-parens "$[{(")
  )
(add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)
;; Enable reftex in LaTeX-mode
(add-hook 'LaTeX-mode-hook 'reftex-mode)
;; Fix Enter-behaviour in LaTeX-mode and org-mode with Evil
(add-hook 'LaTeX-mode-hook 'return-indent-in-latex)
(add-hook 'org-mode-hook 'return-indent-in-latex)

;; Enable bibretrieve to quickly retrieve biblatex entries.
(use-package bibretrieve
  :commands (bibretrieve)
  :ensure bibretrieve
  :config
  (setq bibretrieve-backends '(("msn" . 10) ("arxiv" . 5) ("zbm" . 5)))
  )

;; Enable flyspell
(use-package flycheck
  :commands (flycheck-mode)
  :ensure flycheck
  )

(use-package flyspell
  :commands (flyspell-mode flyspell-buffer)
  :config
  (progn
    (setq ispell-program-name (concat paul/path-localbin "aspell"))
    (setq ispell-dictionary "english")
    )
  )

(add-hook 'LaTeX-mode-hook 'flyspell-mode)
;(add-hook 'LaTeX-mode-hook 'flyspell-buffer)

;; Enable cdlatex in org-mode
(add-hook 'org-mode-hook 'org-cdlatex-mode)

;; Enable autofill in all text-modes
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook
	  '(lambda() (set-fill-column 80)))

;; Enable reftex in org-mode
(add-hook 'org-mode-hook 'reftex-mode)


;; Tell reftex the path to my default bib file
(setq reftex-default-bibliography '("~/latex-docs/thesis/thesis_literature.bib"))

;; Powerline integration
(use-package powerline
  :demand t
  :ensure powerline
  :config
  (progn
    (powerline-evil-center-color-theme)
    (display-time-mode t)
    (setq powerline-evil-tag-style 'verbose)
    )
  )
(use-package powerline-evil
  :ensure powerline-evil
  )

(setq org-confirm-babel-evaluate nil)
(setq org-src-preserve-indentation t)

      
(use-package ox-latex
  :config
  (progn
    (setq org-latex-packages-alist
	  '(
	    ("article,loadthm" "myStyle" nil)
	    ("" "mathtools" nil)
	    ("" "csquotes" nil)
	    ("autolang=other,natbib=true,firstinits=true" "biblatex" nil)
	    )
	  )
    (add-to-list 'org-latex-classes
		 '("mytemplate"
		   "\\documentclass[intlimits,english,a4paper]{scrartcl}

[PACKAGES]
\\addbibresource{~/latex-docs/thesis/thesis_literature.bib}

% Pagestyling
\\pagestyle{headings}

[NO-DEFAULT-PACKAGES]
[EXTRA]"
               ("\\section{%s}" . "\\section*{%s}")
	       ("\\subsection{%s}" . "\\subsection*{%s}")
	       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	       ("\\paragraph{%s}" . "\\paragraph*{%s}")
	       ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
)
    )
)


(setq preview-gs-options '("-q" "-dNOSAFER" "-dNOPAUSE" "-DNOPLATFONTS" "-dPrinted" "-dTextAlphaBits=4" "-dGraphicsAlphaBits=4"))

;; Virtualenvwrapper settings
(use-package virtualenvwrapper
  :ensure virtualenvwrapper
  :init
  (venv-initialize-interactive-shells)
  :config
  (setq venv-location "~/myhp/")
  )

;; Django stuff
(use-package django-mode
  :ensure django-mode
  )
(require 'django-html-mode)
(require 'django-mode)
;; (yas/load-directory "~/.emacs.d/elpa/django-snippets-20131229.811/snippets")
(add-to-list 'auto-mode-alist '("\\.djhtml$" . django-html-mode))

;; Python Jedi autocomplete setup
(use-package jedi
  :ensure jedi
  )
(require 'jedi)
(add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'django-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

(desktop-save-mode 1)
;; Save session including tabs
;; http://stackoverflow.com/questions/22445670/save-and-restore-elscreen-tabs-and-split-frames
(defun session-save ()
  "Store the elscreen tab configuration."
  (interactive)
  (if (desktop-save emacs-configuration-directory)
      (with-temp-file elscreen-tab-configuration-store-filename
	(insert (prin1-to-string (elscreen-get-screen-to-name-alist))))))

;; Load session including tabs
(defun session-load ()
  "Restore the elscreen tab configuration."
  (interactive)
  (if (desktop-read)
      (let ((screens (reverse
		      (read
		       (with-temp-buffer
			 (insert-file-contents elscreen-tab-configuration-store-filename)
			 (buffer-string))))))
	(while screens
	  (setq screen (car (car screens)))
	  (setq buffers (split-string (cdr (car screens)) ":"))
	  (if (eq screen 0)
	      (switch-to-buffer (car buffers))
	    (elscreen-find-and-goto-by-buffer (car buffers) t t))
	  (while (cdr buffers)
	    (switch-to-buffer-other-window (car (cdr buffers)))
	    (setq buffers (cdr buffers)))
	  (setq screens (cdr screens))))))

;; clipboard
(setq x-select-enable-clipboard t)
