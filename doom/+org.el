;;; +org.el --- Private org doom emacs configuration -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Eduardo Apolinario
;;
;; Author: Eduardo Apolinario <curupa@gmail.com>
;; Maintainer: Eduardo Apolinario <curupa@gmail.com>
;; Created: May 13, 2025
;; Modified: May 13, 2025
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/eduardo/+org
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; Disable flycheck in org-mode
(setq flycheck-global-modes '(not org-mode))

;; better list management
(after! org
  (add-hook 'org-mode-hook 'org-autolist-mode))

;; Disable linum mode in org-mode
(after! org
  (add-hook 'org-mode-hook (lambda() (display-line-numbers-mode -1))))

;; I have started using org-clock to track time I spend on tasks. Often I restart Emacs for different reasons in
;; the middle of a session, so I want to persist all the running clocks and their history.
(after! org-clock
  (setq org-clock-persist t)
  (org-clock-persistence-insinuate))

;; Configure org-roam
;; ============================
;;
(after! org-roam
  (setq org-roam-directory (file-truename (concat org-directory "org-roam-v2")))
  (setq org-roam-completion-everywhere t)
  (setq org-roam-mode-section-sections
        (list #'org-roam-backlinks-section
              #'org-roam-reflinks-section
              #'org-roam-unlinked-references-section))
  )

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
;; End of org-roam configuration

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
                                 (file ,(concat doom-user-dir "org-templates/links.template")))
                                ("i"
                                 "INTR"
                                 entry
                                 (file+headline org-default-notes-file "Tasks")
                                 (file ,(concat doom-user-dir "org-templates/interruption.template")))
                                ("t"
                                 "TODO"
                                 entry
                                 (file+headline org-default-notes-file "Tasks")
                                 (file ,(concat doom-user-dir "org-templates/todo.template"))
                                 :prepend t)
                                ("n"
                                 "Notes"
                                 entry
                                 (file+headline my-org-notes-file "Notes")
                                 (file ,(concat doom-user-dir "org-templates/notes.template")))
                                ("j" "Journals")
                                ("jw"
                                 "Work Journal"
                                 entry
                                 (file+datetree my-org-work-journal-file)
                                 (file ,(concat doom-user-dir "org-templates/journal.template"))
                                 :tree-type week)
                                ("jp"
                                 "Personal Journal"
                                 entry
                                 (file+datetree my-org-personal-journal-file)
                                 (file ,(concat doom-user-dir "org-templates/journal.template"))
                                 :tree-type week)
                                ("o" "Centralized templates for projects")
                                ("ot" "Project todo" entry #'+org-capture-central-project-todo-file
                                 "* TODO %?\n %i\n %a" :heading "Tasks" :prepend nil)
                                ("on" "Project notes" entry #'+org-capture-central-project-notes-file
                                 "* %U %?\n %i\n %a" :heading "Notes" :prepend t)
                                ("oc" "Project changelog" entry #'+org-capture-central-project-changelog-file
                                 "* %U %?\n %i\n %a" :heading "Changelog" :prepend t)
                                )
        org-refile-targets '((org-agenda-files :maxlevel . 3))
        org-agenda-todo-ignore-scheduled t
        org-outline-path-complete-in-steps nil
        org-refile-use-outline-path t
        org-log-note-clock-out t
        org-log-done 'note
        org-log-into-drawer t

        ;; org-pomodoro settings
        ;; cribbed from https://gist.github.com/bravosierrasierra/1d98a89a7bcb618ef70c6c4a92af1a96
        org-pomodoro-ticking-sound-p nil
        org-pomodoro-finished-sound-p nil
        )
  )


;; ;; Misc
;; ;; ===========================
;; ;;
;; ;; Setup org-download
(after! org-download
  (setq org-download-method 'directory)
  (setq org-download-image-dir (concat (file-name-sans-extension (buffer-file-name)) "-img"))
  (setq org-download-image-org-width 600)
  (setq org-download-link-format "[[file:%s]]\n"
        org-download-abbreviate-filename-function #'file-relative-name)
  (setq org-download-link-format-function #'org-download-link-format-function-default))
;; ;; End of Misc
