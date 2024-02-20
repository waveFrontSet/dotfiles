;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; Solarized Dark theme.
(setq doom-theme 'doom-solarized-dark)

;; Modeline configuration
(setq doom-modeline-major-mode-icon t)
(setq doom-modeline-enable-word-count t)

;; Generally, don't format with lsp
(setq +format-with-lsp nil)
;; Don't watch all files with lsp
(setq lsp-enable-file-watchers nil)

;; If I'm on my mac, the font size seems to be a little smaller
(if IS-MAC
    (progn
      (setq
       doom-font (font-spec :family "Fira Code" :size 18)
       ns-use-thin-smoothing t
       )
      (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
      (add-to-list 'default-frame-alist '(ns-appearance . dark))
      )
  (setq doom-font (font-spec :family "Fira Code" :size 16))
  )

;; Evil keybindings
;;; Fold-simulation
(map! :n "zM" 'outline-hide-body)
(map! :n "zm" 'outline-hide-subtree)
(map! :n "zR" 'outline-show-all)
(map! :n "zr" 'outline-show-subtree)
(map! :n "zj" 'outline-next-visible-heading)
(map! :n "zk" 'outline-previous-visible-heading)
;;; Use Q instead of gq
(map! :n "Q" 'evil-fill-and-move)
;;; Shortcut for muting search highlighting
(map! :n "C-l" (progn 'redraw-frame 'evil-ex-nohighlight))

;; Python config: Only use pylint.
(defun my-flycheck-setup ()
  (progn
    (flycheck-select-checker 'python-flake8)
    (flycheck-add-next-checker 'python-flake8 'python-mypy)
    ))
(after! flycheck
  (add-hook 'python-mode-local-vars-hook #'my-flycheck-setup 'append)
  )
(setq flycheck-python-mypy-config '("mypy.ini" "setup.cfg" "pyproject.toml"))
(require 'dap-python)
(after! dap-mode
  (setq dap-python-debugger 'debugpy))
(set-formatter! 'ruff '("ruff" "format" "-") :modes '(python-mode))

;; Org configuration
(setq org-directory "~/org/")

(defun paul/org-save-all-org-buffers ()
  "Save all current org-buffers, switch into the org-directory and make a git commit with the current date and time as the commit msg."
  (interactive)
  (org-save-all-org-buffers)
  (cd org-directory)
  (start-process "git-add" nil "git" "add" "roam")
  (start-process "git" nil "git" "commit" "-am" (format-time-string "%Y-%m-%d %H:%M"))
  )

(after! org
  (setq
   org-todo-keywords '((sequence
                        "TODO(t)"
                        "NEXT(n)"
                        "WAIT(w@)"
                        "|"
                        "DONE(d!)"
                        "CANCELED(c@)")
                       (sequence
                        "STORY(s)"
                        "|"
                        "DONE(d!)"
                        "CANCELED(c@)")
                       (sequence
                        "[ ](T)"
                        "[-](S)"
                        "[?](W)"
                        "|"
                        "[X](D)")
                       (sequence
                        "APPT(a)"
                        "|"
                        "TELE(e)"
                        "GESP(g)"
                        "DONE(d!)"
                        "CANCELED(c@)"))
   org-todo-keyword-faces '(("APPT"  . (:foreground "sienna" :weight bold))
                            ("[-]"  . +org-todo-active)
                            ("NEXT" . +org-todo-active)
                            ("[?]"  . +org-todo-onhold)
                            ("WAIT" . +org-todo-onhold)
                            ("CANCELED" . (:foreground "red" :weight bold)))
   org-log-done 'time
   org-agenda-files (list (concat org-directory "work.org")
                          (concat org-directory "home.org")
                          (concat org-directory "inbox.org")
                          (concat org-directory "tickler.org")
                          (concat org-directory "notes.org")
                          (concat org-directory "diary.org")
                          (concat org-directory "daily.org")
                          (concat org-directory "weekly.org")
                          )
   org-latex-to-pdf-process (list "latexmk %f")
   org-src-fontify-natively t
   org-highlight-latex-and-related '(latex script entities)
   org-refile-targets '((org-agenda-files :maxlevel . 3)
                        ("~/org/someday.org" :level . 1))
   org-confirm-babel-evaluate nil
   org-src-preserve-indentation t
   org-agenda-span 'day
   org-agenda-start-day "0d"
   org-agenda-start-with-log-mode t
   org-agenda-start-with-clockreport-mode t
   org-agenda-repeating-timestamp-show-all nil
   org-agenda-custom-commands '(
                                ("w" "Work agenda"
                                 (
                                  (agenda "" ((org-agenda-span 1)))
                                  (tags-todo "@work/NEXT"
                                             ((org-agenda-overriding-header
                                               (concat (all-the-icons-faicon "bolt" :v-adjust 0.01) " Next Tasks")))
                                             )
                                  (tags-todo "@work/TODO"
                                             ((org-agenda-overriding-header
                                               (concat (all-the-icons-faicon "tasks" :v-adjust 0.01) " Tasks")))
                                             )
                                  (tags-todo "@work/STORY"
                                             ((org-agenda-overriding-header
                                               (concat (all-the-icons-faicon "play-circle" :v-adjust 0.01) " Storys")))
                                             )
                                  (tags-todo "@work/WAIT"
                                             ((org-agenda-overriding-header
                                               (concat (all-the-icons-faicon "hourglass" :v-adjust 0.01) " Waiting")))
                                             )
                                  )
                                 )
                                ("h" "Home agenda"
                                 (
                                  (tags-todo "@home/NEXT"
                                             ((org-agenda-overriding-header
                                               (concat (all-the-icons-faicon "bolt" :v-adjust 0.01) " Next Tasks")))
                                             )
                                  (tags-todo "@home/TODO"
                                             ((org-agenda-overriding-header
                                               (concat (all-the-icons-faicon "tasks" :v-adjust 0.01) " Tasks")))
                                             )
                                  (tags-todo "@home/WAIT"
                                             ((org-agenda-overriding-header
                                               (concat (all-the-icons-faicon "hourglass" :v-adjust 0.01) " Waiting")))
                                             )
                                  (tags "+LEVEL=1|TODO=\"TODO\""
                                        ((org-agenda-overriding-header
                                          (concat (all-the-icons-faicon "book" :v-adjust 0.01) " Notes"))
                                         (org-agenda-files (file-expand-wildcards "~/org/notes/*.org")))
                                        )
                                  )
                                 (
                                  (org-agenda-sorting-strategy '((category-up todo-state-down)))
                                  )
                                 )
                                ("g" "Global search" search ""
                                 ((org-agenda-files '("~/org" "~/org/notes" "~/org/roam"))))
                                ("i" "Inbox" todo ""
                                 ((org-agenda-files '("~/org/todo.org"))
                                  (org-agenda-sorting-strategy '(todo-state-down)))
                                 )
                                ("o" "Someday" todo ""
                                 ((org-agenda-files '("~/org/someday.org"))
                                  (org-agenda-sorting-strategy '(todo-state-down)))
                                 )
                                )
   org-agenda-clockreport-parameter-plist '(
                                            :maxlevel 2
                                            :block today
                                            :scope agenda
                                            :formula "$5=$3+$4;t::$6=round(4*$5)/4;%.2f"
                                            :fileskip0 t
                                            :stepskip0 t
                                            :narrow 80!
                                            )
   appt-message-warning-time 5
   appt-display-interval 5
   org-agenda-block-separator ?\u2015
   org-clock-persist t
   org-clock-history-length 23
   org-clock-in-resume t
   org-drawers (quote ("PROPERTIES" "LOGBOOK"))
   org-log-into-drawer t
   org-clock-into-drawer t
   org-clock-out-remove-zero-time-clocks t
   org-clock-out-when-done t
   org-clock-persist-query-resume nil
   org-clock-report-include-clocking-task t
   org-clock-auto-clock-resolution (quote when-no-clock-is-running)
   org-clock-mode-line-total 'today
   org-html-doctype "html5"
   org-html-html5-fancy t
   )
  (set-face-attribute 'org-agenda-structure nil :inherit 'default :height 1.50)
  (org-agenda-to-appt)
  (auto-fill-mode)
  (org-clock-persistence-insinuate)
  (add-to-list 'org-modules 'org-habit)
  (require 'org-habit)
  (map! :leader
        :desc "Org Capture"
        "C" 'org-capture
        )
  (map! :localleader
        :mode 'org-mode
        "2" 'org2blog-user-interface
        )
  (map! :localleader
        :mode 'org-agenda-mode
        "S" 'paul/org-save-all-org-buffers
        )
  (use-package! doct
    :config
    (setq org-capture-templates
          (doct '(("Todo"
                   :keys "t"
                   :file +org-capture-todo-file
                   :headline "Inbox"
                   :template ("* TODO %i%?")
                   :prepend t)
                  ("Story"
                   :keys "s"
                   :file +org-capture-todo-file
                   :template ("* STORY %^{ticket id} %? [/]\n:PROPERTIES:\n:CATEGORY: %\\1\n:END:"))
                  ("Tickler"
                   :keys "T"
                   :file +org-capture-todo-file
                   :headline "Tickler"
                   :template ("* %i%? \n %^{Appointed Time}T"))
                  ("Appointment"
                   :keys "a"
                   :file +org-capture-todo-file
                   :headline "General Meetings"
                   :template ("* APPT %? \n %^{Appointed Time}T"))
                  ("Short Appointment"
                   :keys "m"
                   :file +org-capture-todo-file
                   :headline "Short distractions"
                   :template ("* GESP %?")
                   :clock-in t
                   :clock-keep nil
                   :clock-resume t)
                  ("Protocol"
                   :keys "p"
                   :file +org-capture-todo-file
                   :headline "Inbox"
                   :template ("* TODO %^{Title}\nSource: %u, %c\n#+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?"))
                  ("Protocol Link"
                   :keys "L"
                   :file +org-capture-todo-file
                   :headline "Inbox"
                   :template ("* TODO [[%:link][%:description]] \nCaptured On: %U")
                   :immediate-finish t)
                  ("Reviews"
                   :keys "r"
                   :datetree t
                   :time-prompt t
                   :immediate-finish t
                   :children (("Daily Review"
                               :keys "d"
                               :file "daily.org"
                               :template-file "templates/daily_review.org")
                              ("Weekly Review"
                               :keys "w"
                               :file "weekly.org"
                               :template-file "templates/weekly_review.org")))
                  ("General quick note"
                   :keys "n"
                   :file +org-capture-notes-file
                   :datetree t
                   :template ("* %^{Headline}\n%?")
                   :prepare-finalize (lambda() (unless org-note-abort (org-set-tags-command))))
                  ("Journal"
                   :keys "j"
                   :file +org-capture-journal-file
                   :datetree t
                   :template ("* %^{Headline}\n%?")
                   :prepare-finalize (lambda() (unless org-note-abort (org-set-tags-command))))))))
  )

(after! flyspell
  (setq ispell-dictionary "en_US")
  (if IS-WINDOWS (progn
                   (add-to-list 'exec-path "~/hunspell/bin")
                   (setq ispell-program-name (locate-file "hunspell" exec-path exec-suffixes 'file-executable-p))
                   )
    )
  )

(setq ob-mermaid-cli-path "/opt/homebrew/bin/mmdc")

(add-hook 'vue-mode-hook #'lsp!)
