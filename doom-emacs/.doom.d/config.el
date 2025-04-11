;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Eduardo Apolinario"
      user-mail-address "curupa@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
(setq doom-font (font-spec :family "Hasklig" :size 12 :weight 'semi-light)
     doom-variable-pitch-font (font-spec :family "Hasklig" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-opera)
;; (setq doom-theme 'catppuccin)
;; (setq catppuccin-flavor 'frappuccino)
;; (setq doom-theme 'doom-nord)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
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
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; magit prefix
(use-package! magit
  :init
  (map! :leader :prefix "g"
        "s" #'magit-status))

;; A more ergonomic escape sequence for evil
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

;; TODO: remove this as doom moved away from org-superstar in favor of org-modern
;; Improve org bullet list
(after! org-superstar
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))
  (setq org-superstar-remove-leading-stars t)
  (setq org-superstar-headline-bullets-list '("◉" "⁖" "○" "✸" "✿"))
  (setq org-superstar-prettify-item-bullets t
        org-hide-leading-stars t))

;; org-related configuration
(setq doom-config-directory "~/.doom.d/")

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

;; Disable flycheck in org-mode
(setq flycheck-global-modes '(not org-mode))

;; Github copilot
;; accept completion from copilot and fallback to company
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (("C-TAB" . 'copilot-accept-completion-by-word)
         ("C-<tab>" . 'copilot-accept-completion-by-word)
         :map copilot-completion-map
         ("<tab>" . 'copilot-accept-completion)
         ("TAB" . 'copilot-accept-completion))
  :custom
  (copilot-max-char -1)
  (copilot-indent-offset-warning-disable t))

(use-package! protobuf-mode
  :defer-incrementally t)

;; Enable which-function-mode in prog-mode
(add-hook 'prog-mode-hook 'which-function-mode)

;; Setup org-download
(after! org-download
  (setq org-download-method 'directory)
  (setq org-download-image-dir (concat (file-name-sans-extension (buffer-file-name)) "-img"))
  (setq org-download-image-org-width 600)
  (setq org-download-link-format "[[file:%s]]\n"
        org-download-abbreviate-filename-function #'file-relative-name)
  (setq org-download-link-format-function #'org-download-link-format-function-default))


;; Enable rainbow-mode in prog-mode
;; (add-hook 'prog-mode-hook 'rainbow-mode)
;; (add-hook! prog-mode 'rainbow-mode)

(use-package flycheck-golangci-lint
  :ensure t
  :hook (go-mode . flycheck-golangci-lint-setup))

;; Ensure output of `make compile' scrolls to the bottom
;; Learned this on https://www.youtube.com/watch?v=6oeE52bIFyE
;; tbh I don't understand why this is not the default.
(setq compilation-scroll-output t)

;; All the LLMs!
(use-package! gptel
  :config
  ;; TODO: Do not commit these keys to git!
  (setq! gptel-api-key "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")
  (gptel-make-gemini "Gemini"
    :key "AIzaSyCU6-erLCeVMH5-vkkOpQL45lA0YQciVzE"
    :stream t)
  (setq
   gptel-model "llama3.1:8b"
   gptel-backend (gptel-make-ollama "Ollama"
                   :host "localhost:11434"
                   :stream t
                   :models '("llama3.1:8b")))
  )

(use-package! gptel-extensions
  :after gptel
  :config
  (map!
   :leader
   (:prefix "y"
    :desc "Send Buffer gptel" :n "b" #'gptel-ext-send-whole-buffer
    :desc "Question Document" :n "q" #'gptel-ext-ask-document
    :desc "Rewrite Region" :n "R" #'gptel-ext-rewrite-and-replace
    :desc "Refactor Region" :n "r" #'gptel-ext-refactor)))

(use-package! whisper
  :config
  (setq whisper-install-directory (concat doom-data-dir "whisper")
        ;; TODO whisper-install-whispercpp nil
        whisper-model "medium"
        whisper-language "en"
        ;; whisper-translate nil
        ;; whisper-enable-speed-up nil ;; FIXME this just fails
        ;; whisper-use-threads 8
        ;; whisper--ffmpeg-input-format "pulse"
        ;; whisper--ffmpeg-input-device "default"
        )
  (map! :leader
        (:prefix "y"
         :desc "Whisper" :n "w" #'whisper-run
         :desc "Whisper File" :n "W" #'whisper-file)))

;; End of all the LLMs! (for now)

;; Better visualization of backlinks in org-roam
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

(use-package! lsp-mode
  :defer t
  :custom
  (gc-cons-threshold (* 400 1024 1024))      ; increase GC threshold to improve perf in LSP mode
  (read-process-output-max (* 1 1024 1024))  ; handle large LSP responses
  (add-hook 'lsp-after-open-hook 'lsp-lens-mode)
  :config (setq lsp-lens-enable t
                lsp-auto-execute-action nil
                lsp-verify-signature t)
  )

(after! lsp-ui
  :defer t
  :config (setq lsp-ui-doc-enable t
                lsp-ui-doc-show-with-cursor t
                lsp-ui-doc-show-with-mouse t))

(use-package! git-link
  :defer t
  :custom
  (git-link-use-commit t)
  (git-link-use-single-line-number t)
  :commands (git-link git-link-commit git-link-homepage)
  :bind
  ("C-c v &" . git-link))

(use-package! rainbow-delimiters
  :defer t
  :hook
  (prog-mode . rainbow-delimiters-mode))

;; ruff-lsp
(defcustom lsp-ruff-executable "ruff-lsp"
  "Command to start the Ruff language server."
  :group 'lsp-python
  :risky t
  :type 'file)

;; Register ruff-lsp with the LSP client.
(after! lsp-mode
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection (lambda () (list lsp-ruff-executable)))
    :activation-fn (lsp-activate-on "python")
    :add-on? t
    :server-id 'ruff)))

(use-package! beacon
  :defer t
  :config
  (beacon-mode 1))

(after! dap-mode
  (require 'dap-python)
  ;; if you installed debugpy, you need to set this
  ;; https://github.com/emacs-lsp/dap-mode/issues/306
  (setq dap-python-debugger 'debugpy)

  (dap-ui-mode)
  (dap-ui-controls-mode 1)
  )

;; Find all references in a project using lsp
;;
(map! :leader
      :desc "Find all references" :n "r" #'lsp-find-references)

;; we recommend using use-package to organize your init.el
(use-package codeium
    :init
    (add-to-list 'completion-at-point-functions #'codeium-completion-at-point)
    ;; or on a hook
    ;; (add-hook 'python-mode-hook
    ;;     (lambda ()
    ;;         (setq-local completion-at-point-functions '(codeium-completion-at-point))))

    ;; if you want multiple completion backends, use cape (https://github.com/minad/cape):
    ;; (add-hook 'python-mode-hook
    ;;     (lambda ()
    ;;         (setq-local completion-at-point-functions
    ;;             (list (cape-super-capf #'codeium-completion-at-point #'lsp-completion-at-point)))))
    ;; an async company-backend is coming soon!

    ;; codeium-completion-at-point is autoloaded, but you can
    ;; optionally set a timer, which might speed up things as the
    ;; codeium local language server takes ~0.2s to start up
    ;; (add-hook 'emacs-startup-hook
    ;;  (lambda () (run-with-timer 0.1 nil #'codeium-init)))

    ;; :defer t ;; lazy loading, if you want
    :config
    (setq use-dialog-box nil) ;; do not use popup boxes

    ;; if you don't want to use customize to save the api-key
    ;; (setq codeium/metadata/api_key "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx")

    ;; get codeium status in the modeline
    (setq codeium-mode-line-enable
        (lambda (api) (not (memq api '(CancelRequest Heartbeat AcceptCompletion)))))
    (add-to-list 'mode-line-format '(:eval (car-safe codeium-mode-line)) t)
    ;; alternatively for a more extensive mode-line
    ;; (add-to-list 'mode-line-format '(-50 "" codeium-mode-line) t)

    ;; use M-x codeium-diagnose to see apis/fields that would be sent to the local language server
    (setq codeium-api-enabled
        (lambda (api)
            (memq api '(GetCompletions Heartbeat CancelRequest GetAuthToken RegisterUser auth-redirect AcceptCompletion))))
    ;; you can also set a config for a single buffer like this:
    ;; (add-hook 'python-mode-hook
    ;;     (lambda ()
    ;;         (setq-local codeium/editor_options/tab_size 4)))

    ;; You can overwrite all the codeium configs!
    ;; for example, we recommend limiting the string sent to codeium for better performance
    (defun my-codeium/document/text ()
        (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (min (+ (point) 1000) (point-max))))
    ;; if you change the text, you should also change the cursor_offset
    ;; warning: this is measured by UTF-8 encoded bytes
    (defun my-codeium/document/cursor_offset ()
        (codeium-utf8-byte-length
            (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (point))))
    (setq codeium/document/text 'my-codeium/document/text)
    (setq codeium/document/cursor_offset 'my-codeium/document/cursor_offset))
