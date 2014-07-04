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
(require-package 'color-theme-solarized)
(load-theme 'solarized-light t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(exec-path (quote ("/usr/local/bin" "/usr/bin" "/bin" "/usr/sbin" "/sbin" "/usr/local/Cellar/emacs/24.3/libexec/emacs/24.3/x86_64-apple-darwin13.1.0" "/usr/local/Cellar/ghostscript/9.10/bin" "/usr/local/texlive/2013/bin/x86_64-darwin" )))
 '(pdf-latex-command "lualatex")
 '(solarized-broken-srgb t))
(custom-set-faces
 '(default ((t (:family "PragmataPro for Powerline" :foundry "unknown" :slant normal :weight normal :height 140 :width normal)))))
(if (eq system-type 'darwin)
(custom-set-faces
 '(default ((t (:family "PragmataPro for Powerline" :foundry "unknown" :slant normal :weight normal :height 180 :width normal)))))
)
;; Fuzzy search buffer and file names
(require-package 'projectile)
(projectile-global-mode)

;; Fuzzy search command names
(require-package 'smex)
(require 'smex)
(smex-initialize)

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

;; LaTeX stuff

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

;; Enable cdlatex in LaTeX-mode
(load "cdlatex")
(add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)
;; Enable reftex in LaTeX-mode
(add-hook 'LaTeX-mode-hook 'reftex-mode)
;; Enable TeX-fold-mode in LaTeX-mode
(add-hook 'LaTeX-mode-hook 'TeX-fold-mode)
(add-hook 'TeX-fold-mode-hook 'TeX-fold-buffer)


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
(setq reftex-default-bibliography '("~/thesis/thesis_literature.bib"))

;; change mode-line color by evil state
(lexical-let ((default-color (cons (face-background 'mode-line)
				   (face-foreground 'mode-line))))
  (add-hook 'post-command-hook
	    (lambda ()
	      (let ((color (cond ((minibufferp) default-color)
				 ((evil-insert-state-p) '("#e80000" . "#ffffff"))
				 ((evil-emacs-state-p)  '("#444488" . "#ffffff"))
				 ((buffer-modified-p)   '("#006fa0" . "#ffffff"))
				 (t default-color))))
		(set-face-background 'mode-line (car color))
		(set-face-foreground 'mode-line (cdr color))))))
;; org-mode LaTeX export / preview
(require 'org-latex)
(setq org-latex-create-formula-image-program 'imagemagick)

(add-to-list 'org-latex-packages-alist '("" "mathtools" t))
(add-to-list 'org-latex-packages-alist '("loadthm, article" "myStyle" t))

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
