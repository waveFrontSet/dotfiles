;; from purcell/emacs.d
(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))

;; Enable package manager and add MELPA repository.
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;; Add manually installed packages to load-path
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; Show line numbers
(global-linum-mode t)

;; Wrap text after 79 chars
(setq fill-column 79)

;; Color-theme
(require-package 'solarized-theme)
(setq solarized-high-contrast-mode-line t)
(load-theme 'solarized-light t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-command "latexmk")
 '(TeX-PDF-mode t)
 '(TeX-source-correlate-start-server t)
 '(TeX-view-program-list (quote (("okular" "okular -unique %o#src:%n%b"))))
 '(TeX-view-program-selection (quote ((output-pdf "Okular") ((output-dvi style-pstricks) "dvips and gv") (output-dvi "xdvi") (output-pdf "Evince") (output-html "xdg-open"))))
 '(cdlatex-paired-parens "$[{(")
 '(exec-path (quote ("/usr/local/bin" "/usr/bin" "/bin" "/usr/sbin" "/sbin" "/usr/local/Cellar/emacs/24.3/libexec/emacs/24.3/x86_64-apple-darwin13.1.0" "/usr/local/Cellar/ghostscript/9.10/bin" "/usr/local/texlive/2013/bin/x86_64-darwin")))
 '(pdf-latex-command "lualatex")
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "PragmataPro for Powerline" :foundry "unknown" :slant normal :weight normal :height 120 :width normal)))))
(if (eq system-type 'darwin)
(custom-set-faces
 '(default ((t (:family "PragmataPro for Powerline" :foundry "unknown" :slant normal :weight normal :height 140 :width normal)))))
(setq LaTeX-enable-toolbar nil)
)
;; Fuzzy search buffer and file names
(require-package 'projectile)
(projectile-global-mode)

;; Fuzzy search command names
(require-package 'smex)
(require 'smex)
(smex-initialize)

;; auto-complete
(require-package 'popup)
(require-package 'auto-complete)
(require-package 'auto-complete-auctex)
;(require 'auto-complete-auctex)
(ac-config-default)

;; Snippets
(require-package 'yasnippet)
(require 'yasnippet)
(yas-global-mode t)

;; Adding markdown support
(require-package 'markdown-mode)
(require-package 'markdown-mode+)
(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; Adding git support via magit
(require-package 'magit)
;;; If nothing is staged and 'c c' is invoked, everything will be commited.
(setq magit-commit-all-when-nothing-staged t)

;; Adding on-the-fly syntax checking via flycheck
(require-package 'flycheck)

;; Adding pass support
(require-package 'password-store)

;; Activate evil-mode + convenient subpackages
(require-package 'evil)
(require-package 'evil-indent-textobject)
(require-package 'evil-leader)
(require-package 'evil-tabs)
(require-package 'evil-visualstar)
(require-package 'surround)

(setq evil-search-module 'evil-search
      evil-want-C-u-scroll t
      evil-want-C-w-in-emacs-state t)

(require 'evil-leader)
(global-evil-leader-mode)
(require 'evil)
(evil-mode t)

(require 'evil-visualstar)

(require 'surround)
(global-surround-mode 1)

;; Evil customizations
(setq sentence-end-double-space nil)

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
;;; Testing
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
(defun add-big-parantheses ()
  (interactive)
  (add-parantheses "big")
)
(defun open-emacs ()
  (interactive)
  (find-file "~/.emacs")
)
(evil-leader/set-key "a" 'add-parantheses)
(evil-leader/set-key "b" 'add-big-parantheses)
(evil-leader/set-key "ce" 'LaTeX-environment)
(evil-leader/set-key "e" 'open-emacs)
(evil-leader/set-key "f" 'projectile-find-file)

;; LaTeX stuff

;; Smart-parens mode for automatically pairing parentheses
;; (require-package 'smartparens)
;; (add-hook 'LaTeX-mode-hook 'smartparens-mode)
;;; AucTex
(require-package 'auctex)
(require 'tex-site)
(getenv "PATH")
(setenv "PATH"
        (concat "/usr/local/texlive/2013/bin/x86_64-darwin" ":"
                (getenv "PATH")))
(require-package 'auctex-latexmk)
(require 'auctex-latexmk)
(auctex-latexmk-setup)
(require-package 'latex-preview-pane)
(latex-preview-pane-enable)
(setq TeX-command-default "latexmk")
;; Assign newline-and-indent to Enter
(defun return-indent-in-latex ()
  (local-set-key (kbd "C-m") 'newline-and-indent)
)
;; Enable cdlatex in LaTeX-mode
(load "cdlatex")
(add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)
;; Enable reftex in LaTeX-mode
(add-hook 'LaTeX-mode-hook 'reftex-mode)
;; Fix Enter-behaviour in LaTeX-mode and org-mode with Evil
(add-hook 'LaTeX-mode-hook 'return-indent-in-latex)
(add-hook 'org-mode-hook 'return-indent-in-latex)

;; Enable bibretrieve to quickly retrieve biblatex entries.
(require-package 'bibretrieve)
(setq bibretrieve-backends '(("msn" . 10) ("arxiv" . 5) ("zbm" . 5)))

;; Enable flyspell
(setq ispell-program-name "aspell")
(setq ispell-dictionary "english")
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

;; Export from latex to pdf via latexmk in org-mode
(setq org-latex-to-pdf-process (list "latexmk %f"))

;; Enable code syntax highlighting in org-mode
(setq org-src-fontify-natively t)

;; Tell reftex the path to my default bib file
(setq reftex-default-bibliography '("~/latex-docs/thesis/thesis_literature.bib"))

;; Powerline integration
(require-package 'powerline)
(require-package 'powerline-evil)
(require 'powerline)
(powerline-evil-center-color-theme)
(display-time-mode t)
(setq powerline-evil-tag-style 'verbose)

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
(require-package 'virtualenvwrapper)
(require 'virtualenvwrapper)
(venv-initialize-interactive-shells)
(setq venv-location "~/myhp/")

;; Django stuff
(require-package 'django-mode)
(require 'django-html-mode)
(require 'django-mode)
;; (yas/load-directory "~/.emacs.d/elpa/django-snippets-20131229.811/snippets")
(add-to-list 'auto-mode-alist '("\\.djhtml$" . django-html-mode))

;; Python Jedi autocomplete setup
(require-package 'jedi)
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
