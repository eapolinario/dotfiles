;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Eduardo Apolinario"
      user-mail-address "curupa@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "FiraCode" :size 12 :weight 'light)
(setq doom-font (font-spec :family "Fira Code" :size 14)
      doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-one-light)
;; (setq doom-theme 'doom-tomorrow-night)
;; (setq doom-theme 'doom-acario-light)
;; (setq doom-theme 'doom-solarized-light)
;; (setq doom-theme 'doom-dark+)
;; (setq doom-theme 'modus-vivendi)
(setq doom-theme 'doom-gruvbox)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")
;; TODO maybe there is a Doom variable that contains this already
(setq doom-config-directory "~/.doom.d/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(use-package! magit
  :init
  (map! :leader :prefix "g"
        "s" #'magit-status))

(setq-default evil-escape-key-sequence "fd")

;; counsel should search for hidden files too.
;; https://github.com/hlissner/doom-emacs/issues/3190#issuecomment-631932638
(after! counsel
  (setq counsel-find-file-ignore-regexp
        (concat
         ;; File names beginning with #
         "\\(?:\\`[#]\\)"
         ;; File names ending with # or ~
         "\\|\\(?:\\`.+?[#~]\\'\\)"))
  (setq counsel-rg-base-command
        "rg -M 240 --hidden --with-filename --no-heading --line-number --color never %s || true"))

;; Golang
(add-hook! go-mode
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save))

(use-package! org-superstar
  :config
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))
  (setq org-superstar-remove-leading-stars t)
  (setq org-superstar-headline-bullets-list '("◉" "⁖" "○" "✸" "✿"))

  (setq org-superstar-prettify-item-bullets t
        org-hide-leading-stars t)
)

;; org-related configuration

;; better list management
(after! org
  (add-hook 'org-mode-hook 'org-autolist-mode))

;; TODO Not sure why those are configured. After figuring it out, leave a comment
(after! org
  (setq org-log-note-clock-out t)
  (setq org-log-done 'note)
  (setq org-log-into-drawer t))

;; Disable linum mode in org-mode
(after! org
  (add-hook 'org-mode-hook (lambda() (display-line-numbers-mode -1))))

;; I have started using org-clock to track time I spend on tasks. Often I restart Emacs for different reasons in
;; the middle of a session, so I want to persist all the running clocks and their history.
(after! org-clock
  (setq org-clock-persist t)
  (org-clock-persistence-insinuate))

;; Disable linum mode in org-mode
(after! org
  (add-hook 'org-mode-hook (lambda() (display-line-numbers-mode -1))))

;; I have started using org-clock to track time I spend on tasks. Often I restart Emacs for different reasons in
;; the middle of a session, so I want to persist all the running clocks and their history.
(after! org-clock
  (setq org-clock-persist t)
  (org-clock-persistence-insinuate))

(after! org
  (setq org-clock-out-switch-to-state "WAITING"  ;; Change the state of a task to "WAITING" after clocking out.
        org-clock-in-switch-to-state "NEXT"  ;; Change the state of a task to "NEXT" after clocking in.

        my-org-work-journal-file (concat org-directory "/work_journal.org")
        my-org-personal-journal-file (concat org-directory "/personal_journal.org")
        my-org-notes-file (concat org-directory "/notes.org")
        my-org-links-file (concat org-directory "/links.org")
        org-default-notes-file (concat org-directory "/inbox.org")
        org-todo-keywords (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d@!)")
                                  (sequence "INTR(i@!)" "|" "DONE(d)")
                                  (sequence "WAITING(w@!/!)" "|" "CANCELLED(c@/!)")))
        org-todo-keyword-faces (quote (("TODO" :foreground "red" :weight bold)
                                       ("INTR" :foreground "brown" :weight bold)
                                       ("NEXT" :foreground "purple" :weight bold)
                                       ("DONE" :foreground "forest green" :weight bold)
                                       ("WAITING" :foreground "orange" :weight bold)
                                       ("CANCELLED" :foreground "forest green" :weight bold)))
        org-todo-state-tags-triggers (quote (("CANCELLED" ("CANCELLED" . t))
                                             ("WAITING" ("WAITING" . t))
                                             (done ("WAITING"))
                                             ("TODO" ("WAITING") ("CANCELLED"))
                                             ("NEXT" ("WAITING") ("CANCELLED"))
                                             ("DONE" ("WAITING") ("CANCELLED"))))
        org-agenda-files (list org-directory)
        ;; In order to use files for the org-capture templates we have to the backquote list form and force
        ;; the evaluation of the function that describes the template. The solution is described in
        ;; https://stackoverflow.com/a/50875947/205787.
        org-capture-templates `(
                                ("u"
                                 "URL"
                                 entry
                                 (file+function my-org-links-file org-reverse-datetree-goto-date-in-file)
                                 (file ,(concat doom-config-directory "org-templates/links.template")))
                                ("i"
                                 "INTR"
                                 entry
                                 (file+headline org-default-notes-file "Tasks")
                                 (file ,(concat doom-config-directory "org-templates/interruption.template")))
                                ("t"
                                 "TODO"
                                 entry
                                 (file+headline org-default-notes-file "Tasks")
                                 (file ,(concat doom-config-directory "org-templates/todo.template")))
                                ("n"
                                 "Notes"
                                 entry
                                 (file+headline my-org-notes-file "Notes")
                                 (file ,(concat doom-config-directory "org-templates/notes.template")))
                                ("j" "Journals")
                                ("jw"
                                 "Work Journal"
                                 entry
                                 (file+datetree my-org-work-journal-file)
                                 (file ,(concat doom-config-directory "org-templates/journal.template"))
                                 :tree-type week)
                                ("jp"
                                 "Personal Journal"
                                 entry
                                 (file+datetree my-org-personal-journal-file)
                                 (file ,(concat doom-config-directory "org-templates/journal.template"))
                                 :tree-type week))
        org-refile-targets '((org-agenda-files :maxlevel . 2))
        org-agenda-todo-ignore-scheduled t
        org-outline-path-complete-in-steps nil
        org-refile-use-outline-path t

        ;; org-pomodoro settings
        ;; cribbed from https://gist.github.com/bravosierrasierra/1d98a89a7bcb618ef70c6c4a92af1a96
        ;; org-pomodoro-ticking-sound-p t
  )
)

(after! org-roam
  (setq org-roam-directory (file-truename "~/org/org-roam-v2"))
  (setq org-roam-completion-everywhere t)
  (setq org-roam-mode-section-functions
        (list #'org-roam-backlinks-section
              #'org-roam-reflinks-section
              #'org-roam-unlinked-references-section))
)

;; end of org-related configuration

;; org-roam-ui configuration as described in the manual: https://github.com/org-roam/org-roam-ui#doom
(use-package! websocket
    :after org-roam)

(use-package! org-roam-ui
    :after org-roam ;; or :after org
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))


;; Disable flycheck in org-mode
(setq flycheck-global-modes '(not org-mode))
