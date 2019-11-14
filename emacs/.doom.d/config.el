;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; Solarized Dark theme.
(setq doom-theme 'doom-solarized-dark)

;; If I'm on my mac, the font size seems to be a little smaller
(if (eq system-type 'darwin)
    (progn
      (mac-auto-operator-composition-mode)
      (setq doom-font (font-spec :family "Fira Code" :size 18))
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

;; Org2Blog configuration for writing Wordpress posts in org mode.
(use-package! org2blog
  :commands 'org2blog-user-interface
  :config (progn
            (require 'auth-source)
            (add-to-list 'gnutls-trustfiles "/usr/local/etc/openssl/cert.pem")
            (add-to-list 'auth-sources "~/.netrc")
            (setq org2blog/wp-show-post-in-browser t)
            (setq org2blog/wp-use-wp-latex nil)
            (setq org2blog/wp-use-sourcecode-shortcode t)
            (let* ((credentials (auth-source-user-and-password "wp-blog"))
                   (username (nth 0 credentials))
                   (password (nth 1 credentials))
                   (config `(("wordpress"
                              :url "https://paul-grillenberger.de/xmlrpc.php"
                              :username ,username
                              :password ,password))))
              (setq org2blog/wp-blog-alist config))
            )
  )

;; Invoke black automatically after saving a file opened in python-mode
(add-hook 'python-mode-hook #'blacken-mode)

;; Org configuration
(defvar paul/path-org-agenda-files "~/org/")

(defun paul/org-save-all-org-buffers ()
  "Save all current org-buffers, switch into the org-directory and make a git commit with the current date and time as the commit msg."
  (interactive)
  (org-save-all-org-buffers)
  (cd paul/path-org-agenda-files)
  (start-process "git" nil "git" "commit" "-am" (format-time-string "%Y-%m-%d %H:%M"))
  )

(defun paul/org-push ()
  "Retrieves the password for rep/bit using password-store and runs git-push."
  (interactive)
  (cd paul/path-org-agenda-files)
  (start-process "git-push" nil "git" "push")
  )

(defun paul/find-table-location ()
  "Find the right table location using the current year and month."
  (let ((year (string-to-number (format-time-string "%Y")))
        (month (string-to-number (format-time-string "%m"))))
    (org-datetree-find-month-create year month)
    )
  )

(after! org
  (setq
   org-todo-keywords '((sequence "TODO(t)" "WAIT(w@)" "|" "DONE(d!)" "CANCELED(c@)")
                       (sequence "APPT(a)" "|" "TELE(e)" "GESP(g)" "DONE(d!)" "CANCELED(c@)"))
   org-todo-keyword-faces '(("APPT"  . (:foreground "sienna" :weight bold)) ("CANCELED" . (:foreground "red" :weight bold)))
   org-log-done 'time
   org-agenda-files (list (concat paul/path-org-agenda-files "work.org")
                          (concat paul/path-org-agenda-files "home.org")
                          (concat paul/path-org-agenda-files "inbox.org")
                          (concat paul/path-org-agenda-files "tickler.org")
                          (concat paul/path-org-agenda-files "thesis_diary.org")
                          (concat paul/path-org-agenda-files "notes.org")
                          (concat paul/path-org-agenda-files "userstorys.org"))
   org-capture-templates
   '(("t" "Todo" entry (file+headline "~/org/inbox.org" "Inbox")
      "* TODO %i%?")
     ("T" "Tickler" entry (file+headline "~/org/tickler.org" "Tickler")
      "* %i%? \n %U")
     ("a" "Appointment" entry (file+headline (concat paul/path-org-agenda-files "work.org") "General Meetings")
      "* APPT %?")
     ("p" "Phonecall" entry (file+headline (concat paul/path-org-agenda-files "work.org") "Short distractions")
      "* TELE Telefonanruf von %?" :clock-in t :clock-resume t)
     ("g" "Gespräch" entry (file+headline (concat paul/path-org-agenda-files "work.org") "Short distractions")
      "* GESP Gespräch mit %?" :clock-in t :clock-resume t)
     ("n" "General note" entry (file+datetree (concat paul/path-org-agenda-files "notes.org")))
     )
   org-latex-to-pdf-process (list "latexmk %f")
   org-src-fontify-natively t
   org-highlight-latex-and-related '(latex script entities)
   org-refile-targets '((org-agenda-files :maxlevel . 3))
   org-confirm-babel-evaluate nil
   org-src-preserve-indentation t
   org-agenda-span 'day
   org-agenda-start-with-log-mode t
   org-agenda-start-with-clockreport-mode t
   org-agenda-repeating-timestamp-show-all nil
   org-agenda-custom-commands '(
                                ("w" "Work agenda" tags-todo "@work"
                                 ((org-agenda-sorting-strategy '(todo-state-up)))
                                 )
                                ("h" "Home agenda" tags-todo "@home"
                                 ((org-agenda-sorting-strategy '(todo-state-up)))
                                 )
                                ("i" "Inbox" todo ""
                                 ((org-agenda-files '("~/org/inbox.org"))
                                  (org-agenda-sorting-strategy '(todo-state-up)))
                                 )
                                )
   org-agenda-clockreport-parameter-plist '(
                                            :maxlevel 2 :block today :scope agenda :formula "$5=$3+$4;t::$6=round(4*$5)/4;%.2f"
                                            :fileskip0 t :stepskip0 t :narrow 80!
                                            )
   appt-message-warning-time 5
   appt-display-interval 5
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
   )
  (org-agenda-to-appt)
  (org-clock-persistence-insinuate)
  (add-to-list 'org-modules 'org-habit)
  (require 'org-habit)
  (map! :localleader
        :mode 'org-mode
        "2" 'org2blog-user-interface
        )
  (map! :localleader
        :mode 'org-agenda-mode
        "p" 'paul/org-push
        "S" 'paul/org-save-all-org-buffers)
  )

(after! flyspell
  (setq ispell-dictionary "english")
  )

(setenv "WORKON_HOME" "~/anaconda/envs")
