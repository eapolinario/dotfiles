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

(after! org
  (setq org-clock-out-switch-to-state "WAITING"  ;; Change the state of a task to "WAITING" after clocking out.
        org-clock-in-switch-to-state "NEXT"  ;; Change the state of a task to "NEXT" after clocking in.

        org-directory "~/org"
        my-org-work-journal-file (concat org-directory "/work_journal.org")
        my-org-personal-journal-file (concat org-directory "/personal_journal.org")
        my-org-notes-file (concat org-directory "/notes.org")
        my-org-links-file (concat org-directory "/links.org")
        org-default-notes-file (concat org-directory "/inbox.org")
        org-todo-keywords (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d@!)")
                                  (sequence "INTR(i@!)" "|" "DONE(d)")
                                  (sequence "WAITING(w@!/!)" "|" "CANCELLED(c@/!)")))
        org-todo-keyword-faces (quote (("TODO" :foreground "red" :weight bold)
                                       ("INTR" :foreground "black" :weight bold)
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
                                ("i"                                                                    ; hotkey
                                 "INTR"                                                                 ; name
                                 entry                                                                  ; type
                                 (file+headline org-default-notes-file "Tasks")                         ; target
                                 (file ,(concat org-directory "/org-templates/interruption.template"))) ; template
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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-safe-themes
   '("79278310dd6cacf2d2f491063c4ab8b129fee2a498e4c25912ddaa6c3c5b621e" "e074be1c799b509f52870ee596a5977b519f6d269455b84ed998666cf6fc802a" "3d3807f1070bb91a68d6638a708ee09e63c0825ad21809c87138e676a60bda5d" "71e5acf6053215f553036482f3340a5445aee364fb2e292c70d9175fb0cc8af7" "7a994c16aa550678846e82edc8c9d6a7d39cc6564baaaacc305a3fdc0bd8725f" "54cf3f8314ce89c4d7e20ae52f7ff0739efb458f4326a2ca075bf34bc0b4f499" "425cf02839fa7c5ebd6cb11f8074f6b8463ae6ed3eeb4cf5a2b18ffc33383b0b" "bc836bf29eab22d7e5b4c142d201bcce351806b7c1f94955ccafab8ce5b20208" "3c2f28c6ba2ad7373ea4c43f28fcf2eed14818ec9f0659b1c97d4e89c99e091e" "2cdc13ef8c76a22daa0f46370011f54e79bae00d5736340a5ddfe656a767fddf" default))
 '(fci-rule-color "#555556")
 '(jdee-db-active-breakpoint-face-colors (cons "#1B2229" "#fd971f"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1B2229" "#b6e63e"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1B2229" "#525254"))
 '(objed-cursor-color "#e74c3c")
 '(pdf-view-midnight-colors (cons "#d6d6d4" "#1c1e1f"))
 '(rustic-ansi-faces
   ["#1c1e1f" "#e74c3c" "#b6e63e" "#e2c770" "#268bd2" "#fb2874" "#66d9ef" "#d6d6d4"])
 '(safe-local-variable-values
   '((cider-figwheel-main-default-options . ":dev")
     (cider-clojure-cli-global-options . "-R:nrepl:deps:cljs")
     (cider-preferred-build-tool . clojure-cli)
     (cider-default-cljs-repl . figwheel-main)))
 '(vc-annotate-background "#1c1e1f")
 '(vc-annotate-color-map
   (list
    (cons 20 "#b6e63e")
    (cons 40 "#c4db4e")
    (cons 60 "#d3d15f")
    (cons 80 "#e2c770")
    (cons 100 "#ebb755")
    (cons 120 "#f3a73a")
    (cons 140 "#fd971f")
    (cons 160 "#fc723b")
    (cons 180 "#fb4d57")
    (cons 200 "#fb2874")
    (cons 220 "#f43461")
    (cons 240 "#ed404e")
    (cons 260 "#e74c3c")
    (cons 280 "#c14d41")
    (cons 300 "#9c4f48")
    (cons 320 "#77504e")
    (cons 340 "#555556")
    (cons 360 "#555556")))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
