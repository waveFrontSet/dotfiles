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
  )

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
      "e" (lambda() (interactive) (find-file "~/.emacs"))
      "f" 'projectile-find-file
      "h" 'dired-jump
      "oa" 'org-agenda
      "ob" 'org-iswitchb
      "oc" 'org-capture
      "oj" (lambda() (interactive) (org-capture nil "j"))
      "ol" 'org-store-link
      "oo" 'org-open-at-point
      "os" 'org-schedule
      "ot" 'org-todo
      "x" (lambda() (interactive) (TeX-command "LatexMk" 'TeX-master-file' -1))
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
(define-key evil-normal-state-map (kbd "Us") 'magit-status)
(define-key evil-normal-state-map (kbd "Ud") 'magit-diff-unstaged)
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

;; Adding git support via magit
(use-package magit
  :ensure magit
  :config
  (setq magit-commit-all-when-nothing-staged t)
  (progn
    (evil-set-initial-state 'magit-mode 'normal)
    (evil-set-initial-state 'magit-status-mode 'normal)
    (evil-set-initial-state 'magit-diff-mode 'normal)
    (evil-set-initial-state 'magit-log-mode 'normal)
    (evil-define-key 'normal magit-mode-map
        "j" 'magit-goto-next-section
        "k" 'magit-goto-previous-section)
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

;; Dired bindings
(require 'dired-x)
(put 'dired-find-alternate-file 'disabled nil)
(defun my-dired-up-directory ()
  "Take dired up one directory, but behave like dired-find-alternate-file"
  (interactive)
  (let ((old (current-buffer)))
    (dired-up-directory)
    (kill-buffer old)
    ))

(progn
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
  :ensure bibretrieve
  :config
  (setq bibretrieve-backends '(("msn" . 10) ("arxiv" . 5) ("zbm" . 5)))
  )

;; Enable flyspell
(use-package flycheck
  :ensure flycheck
  :config
  (progn
    (setq ispell-program-name "aspell")
    (setq ispell-dictionary "english")
    )
  )
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
;(add-hook 'LaTeX-mode-hook 'flyspell-buffer)

;; Enable cdlatex in org-mode
(add-hook 'org-mode-hook 'org-cdlatex-mode)

;; org-mode config
(setq org-log-done 'time)
(setq org-agenda-files (list "~/org/work.org"
			     "~/org/home.org"))
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/org/work.org" "Inbox")
	 "* TODO %?")
	("j" "Journal" entry (file "~/latex-docs/thesis/thesis_diary.org")
	 "* %<%d.%m.%Y> \n %?")))
;; Enable autofill in all text-modes
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook
	  '(lambda() (set-fill-column 80)))

;; Enable reftex in org-mode
(add-hook 'org-mode-hook 'reftex-mode)

;; Export from latex to pdf via latexmk in org-mode
(setq org-latex-to-pdf-process (list "latexmk %f"))

;; Enable code syntax highlighting in org-mode
(setq org-src-fontify-natively t)

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

(require 'ob-latex)
(setq org-confirm-babel-evaluate nil)
(setq org-src-fontify-natively t)
(setq org-src-preserve-indentation t)

(require 'ox-latex)
(add-to-list 'org-latex-classes
             '("mytemplate"
	       "\\documentclass[intlimits,english,a4paper]{amsbook}

\\usepackage[loadthm]{myStyle}
\\usepackage{mathtools}

% Literaturverzeichnis-Pakete
\\usepackage{csquotes}
\\usepackage[babel=other,backend=biber,natbib=true,firstinits=true]{biblatex}
\\addbibresource{thesis_literature.bib}
\\bibliography{thesis_literature}

% Pagestyling
\\pagestyle{headings}

               [NO-DEFAULT-PACKAGES]
               [NO-PACKAGES]
               [EXTRA]"
	       ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\section{%s}" . "\\section*{%s}")
	       ("\\subsection{%s}" . "\\subsection*{%s}")
	       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	       ("\\paragraph{%s}" . "\\paragraph*{%s}")
	       ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

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
