;;DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

(load! "+bindings")
(load! "+org")

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
;; (setq doom-font (font-spec :family "Iosevka" :size 12)
;;      doom-variable-pitch-font (font-spec :family "Iosevka" :size 13))
(setq doom-font (font-spec :family "IosevkaTerm Nerd Font" :size 12 :weight medium)
     doom-variable-pitch-font (font-spec :family "IosevkaTerm Nerd Font" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function.
(setq doom-theme 'catppuccin)
(setq catppuccin-flavor 'mocha) ; latte, frappe, macchiato, mocha

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

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

(use-package! eglot-booster
  :after eglot
  :config (eglot-booster-mode))

;; All the LLMs!
(after! gptel
  (setq gptel-log-level 'info) ;; help in debugging
  ;; (require 'gptel-integrations) ;; TODO
  ;; (setf (alist-get 'org-mode gptel-prompt-prefix-alist) "@user\n")
  ;; (setf (alist-get 'org-mode gptel-response-prefix-alist) "@assistant\n")

  ;; TODO: indent and figure out if this is overriding other important values in that variable
   ;; (setq gptel-prompt-prefix-alist '((markdown-mode . "\n** ### ") (org-mode . "-----\n*** ")
   ;;                               (text-mode . "------\n### ")))
   ;;  (setq gptel-response-prefix-alist '((markdown-mode . "\n")
   ;;                                 (org-mode . "-----\n")
   ;;                                 (text-mode . "------\n")))
  ;; Configure providers
  (gptel-make-gh-copilot "Copilot")
  (gptel-make-deepseek "DeepSeek" :stream t :key (auth-source-pick-first-password :host "api.deepseek.com"))
  (gptel-make-gemini "Gemini" :stream t :key (auth-source-pick-first-password :host "generativelanguage.googleapis.com")))
