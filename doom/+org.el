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

(after! org
  (setq org-yank-image-save-method "images/"))

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
                                 (file ,(concat doom-user-dir "org-templates/todo.template")))
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
                                 :tree-type week))
        org-refile-targets '((org-agenda-files :maxlevel . 2))
        org-agenda-todo-ignore-scheduled t
        org-outline-path-complete-in-steps nil
        org-refile-use-outline-path t

        ;; org-pomodoro settings
        ;; cribbed from https://gist.github.com/bravosierrasierra/1d98a89a7bcb618ef70c6c4a92af1a96
        org-pomodoro-ticking-sound-p nil
        org-pomodoro-finished-sound-p nil
  )
)

(after! org-roam
  (setq org-roam-directory (file-truename "~/org/org-roam-v2"))
  (setq org-roam-completion-everywhere t)
  (setq org-roam-mode-section-sections
        (list #'org-roam-backlinks-section
              #'org-roam-reflinks-section
              #'org-roam-unlinked-references-section))
  ;; (setq org-roam-capture-templates
  ;;       '(("a" "Article" plain (function org-roam-capture--get-point)
  ;;          "%?"
  ;;          :file-name "articles/${slug}"
  ;;          :head "#+title: ${title}\n#+roam_key: ${url}\n#+roam_tags: \n#+roam_alias: \n#+date: %<%Y-%m-%d>\n#+author: \n#+source: \n\n* Metadata\n  :PROPERTIES:\n  :Custom_ID: %(org-id-new)\n  :URL: \n  :Author: \n  :Date: %<%Y-%m-%d>\n  :Tags: \n  :END:\n\n* Summary\n\n* Quotes\n\n* Reflections\n\n* Actionable Items\n\n* Links\n"
  ;;          :unnarrowed t)))
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

;; Setup org-download
(after! org-download
  (setq org-download-method 'directory)
  (setq org-download-image-dir (concat (file-name-sans-extension (buffer-file-name)) "-img"))
  (setq org-download-image-org-width 600)
  (setq org-download-link-format "[[file:%s]]\n"
        org-download-abbreviate-filename-function #'file-relative-name)
  (setq org-download-link-format-function #'org-download-link-format-function-default))

(use-package consult-org-roam
   :ensure t
   :after org-roam
   :init
   (require 'consult-org-roam)
   ;; Activate the minor mode
   (consult-org-roam-mode 1)
   :custom
   ;; Use `ripgrep' for searching with `consult-org-roam-search'
   (consult-org-roam-grep-func #'consult-ripgrep)
   ;; Configure a custom narrow key for `consult-buffer'
   (consult-org-roam-buffer-narrow-key ?r)
   ;; Display org-roam buffers right after non-org-roam buffers
   ;; in consult-buffer (and not down at the bottom)
   (consult-org-roam-buffer-after-buffers t)
   :config
   ;; Eventually suppress previewing for certain functions
   (consult-customize
    consult-org-roam-forward-links
    :preview-key "M-.")
   :bind
   ;; Define some convenient keybindings as an addition
   ("C-c n e" . consult-org-roam-file-find)
   ("C-c n b" . consult-org-roam-backlinks)
   ("C-c n B" . consult-org-roam-backlinks-recursive)
   ("C-c n l" . consult-org-roam-forward-links)
   ("C-c n r" . consult-org-roam-search))

(use-package! org-excalidraw
  :config
  (setq org-excalidraw-directory (concat org-directory "excalidraw")))
(after! org (org-excalidraw-initialize))



;; Disable flycheck in org-mode
(setq flycheck-global-modes '(not org-mode))

;;; +org.el ends here
