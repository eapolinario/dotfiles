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
(setq doom-font (font-spec :family "monospace" :size 14))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one-light)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded

;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

;; Maximize on startup
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;;
(use-package! magit
  :init
  (map! :leader :prefix "g"
        "s" #'magit-status))

(setq-default evil-escape-key-sequence "fd")

(after! org
  (setq org-log-done t)
  (setq org-log-into-drawer t))

;; Disable linum mode in org-mode
(after! org
  (add-hook 'org-mode-hook (lambda() (display-line-numbers-mode -1))))

(after! org
  (setq org-clock-out-switch-to-state "WAITING"  ;; Change the state of a task to "WAITING" after clocking out.
        org-clock-in-switch-to-state "NEXT"  ;; Change the state of a task to "NEXT" after clocking in.
        org-directory "~/org"
        my-org-work-journal-file (concat org-directory "/work_journal.org")
        my-org-personal-journal-file (concat org-directory "/personal_journal.org")
        my-org-notes-file (concat org-directory "/notes.org")
        my-org-links-file (concat org-directory "/links.org")
        org-default-notes-file (concat org-directory "/inbox.org")
        org-todo-keywords (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                                  (sequence "WAITING(w@/!)" "|" "CANCELLED(c@/!)")))
        org-todo-keyword-faces (quote (("TODO" :foreground "red" :weight bold)
                                       ("NEXT" :foreground "blue" :weight bold)
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
                                 (file ,(concat org-directory "/org-templates/links.template")))
                                ("t"                                                            ; hotkey
                                 "TODO"                                                         ; name
                                 entry                                                          ; type
                                 (file+headline org-default-notes-file "Tasks")                 ; target
                                 (file ,(concat org-directory "/org-templates/todo.template"))) ; template
                                ("n"                                                             ; hotkey
                                 "Notes"                                                         ; name
                                 entry                                                           ; type
                                 (file+headline my-org-notes-file "Notes")                       ; target
                                 (file ,(concat org-directory "/org-templates/notes.template"))) ; template
                                ("j" "Journals")
                                ("jw"                                                              ; hotkey
                                 "Work Journal"                                                   ; name
                                 entry                                                            ; type
                                 (file+datetree my-org-work-journal-file)                         ; target
                                 (file ,(concat org-directory "/org-templates/journal.template")) ; template
                                 :tree-type week)                                                 ; properties
                                ("jp"                                                              ; hotkey
                                 "Personal Journal"                                               ; name
                                 entry                                                            ; type
                                 (file+datetree my-org-personal-journal-file)                     ; target
                                 (file ,(concat org-directory "/org-templates/journal.template")) ; template
                                 :tree-type week))                                                 ; properties
        org-refile-targets '((org-agenda-files :maxlevel . 1))
        org-agenda-todo-ignore-scheduled t
        org-outline-path-complete-in-steps nil
        org-refile-use-outline-path t

        ;; org-pomodoro settings
        ;; cribbed from https://gist.github.com/bravosierrasierra/1d98a89a7bcb618ef70c6c4a92af1a96
        ;; org-pomodoro-ticking-sound-p t
  )
)

;; FIXME: this should respect the variables defined above and not stay floating.
(setq org-roam-directory "~/org/org-roam")

(after! org-roam
      (setq org-roam-capture-ref-templates
            '(("r" "ref" plain (function org-roam-capture--get-point)
               "%?"
               :file-name "websites/${slug}"
               :head "#+TITLE: ${title}
    #+ROAM_KEY: ${ref}
    - source :: ${ref}"
               :unnarrowed t))))

;; better list management
(after! org
  (add-hook 'org-mode-hook 'org-autolist-mode))

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
        "rg -M 240 --hidden --with-filename --no-heading --line-number --color never %s"))

;; Golang
(add-hook! go-mode
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save))

;; FIXME: this is not the right way of using add-hook!
(after! emojify
  (add-hook! 'after-init-hook #'global-emojify-mode))

(use-package! org-superstar
  :config
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))
  (setq org-superstar-remove-leading-stars t)
  (setq org-superstar-headline-bullets-list '("◉" "⁖" "○" "✸" "✿"))

  (setq org-superstar-prettify-item-bullets t
        org-hide-leading-stars t)
)

(use-package! elfeed
  :config
  (add-hook 'elfeed-show-mode-hook 'elfeed-setup-hook)
  (setq elfeed-feeds
        '(("https://planet.emacslife.com/atom.xml"))))

