;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Personal bindings
(load! "+bindings")

;; Who doesn't love org?
(load! "+org")

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Eduardo Apolinario"
      user-mail-address "curupa@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
(setq doom-font (font-spec :family "FiraCode Nerd Font" :size 13 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "FiraCode Nerd Font" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-molokai)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; A more ergonomic escape sequence for evil
(setq-default evil-escape-key-sequence "fd")
;; Ensure output of `make compile' scrolls to the bottom
;; Learned this on https://www.youtube.com/watch?v=6oeE52bIFyE
;; tbh I don't understand why this is not the default.
(setq compilation-scroll-output t)

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
;; I grew accustomed to this for whatever reason
(after! magit
  (map! :leader :prefix "g"
        "s" #'magit-status)
  (setq magit-diff-refine-hunk 'all))

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

;; Enable which-function-mode in prog-mode
(add-hook 'prog-mode-hook 'which-function-mode)

;; git-link :heart:
(use-package! git-link
  :defer t
  :custom
  (git-link-use-commit t)
  (git-link-use-single-line-number t)
  :commands (git-link git-link-commit git-link-homepage)
  :bind
  ("C-c v &" . git-link))

;; LLM stuff
(after! gptel
  (setq gptel-log-level 'info ;; help in debugging
        gptel-include-reasoning t)
  (require 'gptel-integrations) ;; TODO
  ;; (setf (alist-get 'org-mode gptel-prompt-prefix-alist) "@user\n")
  ;; (setf (alist-get 'org-mode gptel-response-prefix-alist) "@assistant\n")

  ;; Configure providers
  (gptel-make-gh-copilot "Copilot")
  ;; N.B.: the key is the prefix in the list of models in gptel.
  ;; There's a default list prefixed by "ChatGPT" which is not controlled by this.
  ;; TODO: figure out what happens if we override the name "ChatGPT" here.
  (gptel-make-openai "OpenAI"
    :stream t
    :key (auth-source-pick-first-password :host "api.openai.com")
    :models '((gpt-5         . "gpt-5")
              (gpt-5-mini    . "gpt-5-mini")
              (gpt-4o-mini   . "gpt-4o-mini")
              (gpt-4o        . "gpt-4o")
              (gpt-4.1       . "gpt-4.1")
              (gpt-4.1-mini  . "gpt-4.1-mini"))
    )
  (gptel-make-deepseek "DeepSeek"
    :stream t
    :key (auth-source-pick-first-password :host "api.deepseek.com"))
  (gptel-make-gemini "Gemini"
    :stream t
    :key (auth-source-pick-first-password :host "generativelanguage.googleapis.com"))

  ;; Set default model. Right now this is very OpenAI-centric, i.e. might change in the future.
  (setq gptel-model 'gpt-5))

;; Magit integration for gptel 
(after! gptel-magit
  (setq gptel-magit-model 'gpt-4.1-mini))

;; Let me write longer commit messages
(after! git-commit
  (setq git-commit-summary-max-length 120))

;; Enable ast-grep
(use-package! ast-grep
  :ensure t)

(use-package! org-web-tools
  :after (org))
