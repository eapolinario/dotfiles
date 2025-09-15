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

;; (use-package! dap-mode
;;   :after go-mode
;;   :config
;;   (require 'dap-dlv-go))

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


;; (use-package! dape
;;   :init
;;   ;; Put the info buffers on the right like gud (optional, pick one)
;;   ;; (setq dape-buffer-window-arrangement 'gud)
;;   (setq dape-buffer-window-arrangement 'right)

;;   :config
;;   ;; Recommended by dape for nicer keymaps in-session
;;   (repeat-mode 1)

;;   ;; Persist breakpoints across sessions (optional)
;;   ;; (add-hook 'kill-emacs-hook #'dape-breakpoint-save)
;;   ;; (add-hook 'emacs-startup-hook #'dape-breakpoint-load)

;;   ;; ------- Go (Delve) adapter -------
;;   ;; Requires `dlv` (Delve) installed:
;;   ;;   go install github.com/go-delve/delve/cmd/dlv@latest
;;   ;; Dape prefers explicit adapter configs; this one runs `dlv dap ...`
;;   (add-to-list
;;    'dape-configs
;;    `(delve
;;      modes (go-mode go-ts-mode)
;;      command "dlv"
;;      command-args ("dap" "--listen" "127.0.0.1:0") ; pick a free port
;;      command-cwd dape-cwd-fn
;;      host "127.0.0.1"
;;      port 0
;;      :type "debug"           ; <- required: adapter id for Delve
;;      :request "launch"
;;      :cwd dape-cwd-fn
;;      :program dape-cwd-fn))  ; default run = package dir

;;   ;; ------- Helper: get current Go test name -------
;;   (defun my/go-test-name-at-point ()
;;     "Return the nearest enclosing `Test*` function name."
;;     (save-excursion
;;       (or (when (re-search-backward "^func \\(Test[^( ]+\\)" nil t)
;;             (match-string 1))
;;           (user-error "Cursor is not inside a `func Test*` test"))))

;;   ;; For dape, values can be functions; they are resolved to their return values
;;   ;; right before the DAP 'launch' is sent. We'll compute :args dynamically.
;;   (defun my/dape-go-test-args ()
;;     (vector "-test.run" (format "^%s$" (my/go-test-name-at-point))))

;;   ;; A dedicated config to "debug this test at point"
;;   (add-to-list
;;    'dape-configs
;;    `(go-test@point
;;      modes (go-mode go-ts-mode)
;;      command "dlv"
;;      command-args ("dap" "--listen" "127.0.0.1:0")
;;      command-cwd dape-cwd-fn
;;      host "127.0.0.1"
;;      port 0
;;      :type "debug"
;;      :request "launch"
;;      :mode "test"            ; <-- IMPORTANT for Delve test mode
;;      :cwd dape-cwd-fn
;;      :program dape-cwd-fn    ; run the current package's tests
;;      :args my/dape-go-test-args))

;;   (add-to-list
;;    'dape-configs
;;    (cons 'go-test@point
;;        '(modes (go-mode go-ts-mode)
;;          command "dlv"
;;          command-args ("dap" "--listen" "127.0.0.1:0")
;;          command-cwd dape-cwd-fn
;;          host "127.0.0.1"
;;          port 0
;;          :type "debug"
;;          :request "launch"
;;          :mode "test"
;;          :cwd dape-cwd-fn
;;          :program dape-cwd-fn
;;          :args my/dape-go-test-args)))


;;   (add-to-list
;;    'dape-configs
;;    `(delve-unit-test
;;      modes (go-mode go-ts-mode)
;;      ensure dape-ensure-command
;;      fn dape-config-autoport
;;      command "dlv"
;;      command-args ("dap" "--listen" "127.0.0.1::autoport")
;;      command-cwd dape-cwd-fn
;;      port :autoport
;;      :type "debug"
;;      :request "launch"
;;      :mode (lambda ()
;;              (if (string-suffix-p "_test.go" (buffer-name))
;;                  "test"
;;                "debug"))
;;      :cwd dape-cwd-fn
;;      :program (lambda ()
;;                 (if (string-suffix-p "_test.go" (buffer-name))
;;                     (concat "./" (file-relative-name default-directory (funcall dape-cwd-fn)))
;;                   (funcall dape-cwd-fn)))
;;      :args (lambda ()
;;              (require 'which-func)
;;              (if (string-suffix-p "_test.go" (buffer-name))
;;                  (when-let* ((test-name (which-function))
;;                              (test-regexp (concat "^" test-name "$")))
;;                    (if test-name `["-test.run" ,test-regexp]
;;                      (error "No test selected"))))
;;              [])))

;;   ;; Doom-friendly leader keys
;;   (map! :leader
;;         (:prefix ("d" . "debug")
;;          :desc "Start dape (choose config)" "d" #'dape
;;          :desc "Toggle breakpoint"         "b" #'dape-breakpoint-toggle
;;          ;; :desc "Debug Go test at point"    "t" (cmd! (dape 'go-test@point))
;;          :desc "Debug Go test at point"    "t" (cmd! (dape "go-test@point"))
;;          ;; :desc "Debug Go test at point - attempt2"    "u" (cmd! (dape 'delve-unit-test))
;;          :desc "Debug Go test at point - attempt2"    "u" #'delve-unit-test)
;;          )))

;; (after! dape
;;   ;; --- Enhanced helper: build a -test.run regex for the test at point -----
;;   (defun my/dape--go-test-run-args ()
;;     "Return a vector suitable for :args to run the test at point.
;; Includes nearest subtest name if point is inside a t.Run/s.Run block."
;;     (save-excursion
;;       (let* ((orig (point))
;;              ;; Find enclosing `func TestXxx(t *testing.T)` or `func (suite *TestSuite) TestXxx()`
;;              (test (progn
;;                      (end-of-line)
;;                      (cond
;;                       ;; Standard test function
;;                       ((re-search-backward
;;                         "^func[ \t]+\\(Test[[:alnum:]_]+\\)[ \t]*(t[ \t]*\\*testing\\.T)"
;;                         nil t)
;;                        (match-string 1))
;;                       ;; Testify suite method
;;                       ((re-search-backward
;;                         "^func[ \t]*(\\*?[[:alnum:]_]+)[ \t]+\\(Test[[:alnum:]_]+\\)[ \t]*("
;;                         nil t)
;;                        (match-string 2))
;;                       (t nil))))
;;              (_ (unless test (user-error "No Test* func found here")))
;;              ;; Search for nearest Run("name", ...) between point and end of defun
;;              (end (save-excursion (end-of-defun) (point)))
;;              (sub (save-excursion
;;                     (goto-char orig)
;;                     (when (re-search-forward "\\.Run[ \t]*(\"\\([^\"]+\\)\"" end t)
;;                       (match-string 1))))
;;              (regex (concat "^" test (when sub (concat "/" (regexp-quote sub))) "$")))
;;         ;; Return args for dlv test
;;         (message "Running test with regex: %s" regex)  ; Debug output
;;         (vector "-test.run" regex "-test.v"))))

;;   ;; --- Helper: get current Go module root ---
;;   (defun my/dape--go-module-root ()
;;     "Find the root directory of the current Go module."
;;     (or (locate-dominating-file default-directory "go.mod")
;;         (error "Not in a Go module (no go.mod found)")))

;;   ;; --- Config: run current package's tests, focused to test-at-point ------
;;   (add-to-list
;;    'dape-configs
;;    `(go-test@point
;;      modes (go-mode go-ts-mode)
;;      ;; Use dlv dap mode with auto port discovery
;;      command "dlv"
;;      command-args ("dap" "--listen" "127.0.0.1::autoport" "--headless" "--api-version=2")
;;      command-cwd ,(lambda () (my/dape--go-module-root))
;;      host "127.0.0.1"
;;      port :autoport
;;      ;; DAP configuration for Delve
;;      :type "debug"
;;      :request "launch"
;;      :mode "test"
;;      :cwd ,(lambda () (file-relative-name default-directory (my/dape--go-module-root)))
;;      :program "."
;;      ;; Build flags to disable optimizations for better debugging
;;      :buildFlags ["-gcflags=all=-N -l"]
;;      ;; Compute test args dynamically
;;      fn ,(lambda (cfg)
;;            (plist-put cfg :args (my/dape--go-test-run-args))
;;            cfg)))

;;   ;; --- Alternative config for debugging all tests in current package ------
;;   (add-to-list
;;    'dape-configs
;;    `(go-test@package
;;      modes (go-mode go-ts-mode)
;;      command "dlv"
;;      command-args ("dap" "--listen" "127.0.0.1::autoport" "--headless" "--api-version=2")
;;      command-cwd ,(lambda () (my/dape--go-module-root))
;;      host "127.0.0.1"
;;      port :autoport
;;      :type "debug"
;;      :request "launch"
;;      :mode "test"
;;      :cwd ,(lambda () (file-relative-name default-directory (my/dape--go-module-root)))
;;      :program "."
;;      :buildFlags ["-gcflags=all=-N -l"]
;;      :args ["-test.v"]))

;;   ;; --- Config for debugging a specific test file -------------------------
;;   (add-to-list
;;    'dape-configs
;;    `(go-test@file
;;      modes (go-mode go-ts-mode)
;;      command "dlv"
;;      command-args ("dap" "--listen" "127.0.0.1::autoport" "--headless" "--api-version=2")
;;      command-cwd ,(lambda () (my/dape--go-module-root))
;;      host "127.0.0.1"
;;      port :autoport
;;      :type "debug"
;;      :request "launch"
;;      :mode "test"
;;      :cwd ,(lambda () (file-relative-name default-directory (my/dape--go-module-root)))
;;      :program "."
;;      :buildFlags ["-gcflags=all=-N -l"]
;;      fn ,(lambda (cfg)
;;            (let* ((current-file (buffer-file-name))
;;                   (test-file (if (string-match "_test\\.go$" current-file)
;;                                current-file
;;                              (user-error "Current buffer is not a test file")))
;;                   (package-tests (shell-command-to-string
;;                                   (format "go test -list . %s 2>/dev/null | grep '^Test'"
;;                                           (file-name-directory test-file)))))
;;              (plist-put cfg :args (vector "-test.v"))
;;              cfg))))

;; ;; --- Keybindings (adjust to your preference) ---
;; (defun my/dape-go-test-at-point ()
;;   "Start debugging Go test at point."
;;   (interactive)
;;   (dape 'go-test@point))

;; (defun my/dape-go-test-package ()
;;   "Start debugging all Go tests in package."
;;   (interactive)
;;   (dape 'go-test@package))

;; (defun my/dape-go-test-file ()
;;   "Start debugging current Go test file."
;;   (interactive)
;;   (dape 'go-test@file))

;; (defun my/dape-go-test-at-point ()
;;   "Start debugging Go test at point."
;;   (interactive)
;;   (dape (alist-get 'go-test@point dape-configs)))

;; (map! :leader :desc "Debug Go test at point" "d t"
;;       #'my/dape-go-test-at-point)

;; ;; (map! :leader
;; ;;       (:prefix ("d" . "debug")
;; ;;        :desc "Debug Go test at point" "t" #'my/dape-go-test-at-point
;; ;;        :desc "Debug all tests in package" "T" #'my/dape-go-test-package
;; ;;        :desc "Debug current test file" "f" #'my/dape-go-test-file)))

;; ;; --- Additional helper commands ---
;; (defun my/dape-go-test-current-function ()
;;   "Debug the current Go test function."
;;   (interactive)
;;   (if (my/dape--go-test-run-args)
;;       (dape 'go-test@point)
;;     (user-error "Not in a Go test function")))

;; (defun my/dape-go-test-show-regex ()
;;   "Show the regex that would be used for the current test."
;;   (interactive)
;;   (condition-case err
;;       (let ((args (my/dape--go-test-run-args)))
;;         (message "Test regex: %s" (aref args 1)))
;;     (error (message "Error: %s" (error-message-string err)))))

;; ;; Optional: Add to go-mode hook for convenient access
;; (add-hook 'go-mode-hook
;;           (lambda ()
;;             (local-set-key (kbd "C-c d t") #'my/dape-go-test-current-function)
;;             (local-set-key (kbd "C-c d r") #'my/dape-go-test-show-regex))))


;; (after! dape
;;   ;; Keep your original helper function - it was working fine!
;;   (defun my/dape--go-test-run-args ()
;;     "Return a vector suitable for :args to run the test at point."
;;     (save-excursion
;;       (let* ((orig (point))
;;              (test (progn
;;                      (end-of-line)
;;                      (when (re-search-backward
;;                             "^func[ \t]+\\(Test[[:alnum:]_]+\\)[ \t]*(t[ \t]*\\*testing\\.T)"
;;                             nil t)
;;                        (match-string 1))))
;;              (_ (unless test (user-error "No Test* func found here")))
;;              (end (save-excursion (end-of-defun) (point)))
;;              (sub (save-excursion
;;                     (goto-char orig)
;;                     (when (re-search-forward "\\.Run[ \t]*(\"\\([^\"]+\\)\"" end t)
;;                       (match-string 1))))
;;              (regex (concat "^" test (when sub (concat "/" (regexp-quote sub))) "$")))
;;         (vector "-test.run" regex "-test.v"))))

;;   ;; Your original config was good - just small tweaks
;;   (add-to-list
;;    'dape-configs
;;    `(go-test@point
;;      modes (go-mode go-ts-mode)
;;      command "dlv"
;;      command-args ("dap" "--listen" "127.0.0.1::autoport")
;;      command-cwd dape-command-cwd     ; This was working - keep it!
;;      host "127.0.0.1"
;;      port :autoport
;;      :type "debug"
;;      :request "launch"
;;      :mode "test"
;;      :cwd dape-cwd                    ; This was working - keep it!
;;      :program "."
;;      :buildFlags ["-gcflags=all=-N -l"]  ; Just added this for better debugging
;;      fn ,(lambda (cfg)
;;            (plist-put cfg :args (my/dape--go-test-run-args))
;;            cfg)))

;;   ;; Fixed keybinding
;;   (defun my/dape-go-test-at-point ()
;;     "Start debugging Go test at point."
;;     (interactive)
;;     (dape (alist-get 'go-test@point dape-configs)))

;;   (map! :leader :desc "Debug Go test at point" "d t"
;;         #'my/dape-go-test-at-point))

(after! dape
  ;; Your helper function stays the same
  (defun my/dape--go-test-run-args ()
    "Return a vector suitable for :args to run the test at point."
    (save-excursion
      (let* ((orig (point))
             (test (progn
                     (end-of-line)
                     (when (re-search-backward
                            "^func[ \t]+\\(Test[[:alnum:]_]+\\)[ \t]*(t[ \t]*\\*testing\\.T)"
                            nil t)
                       (match-string 1))))
             (_ (unless test (user-error "No Test* func found here")))
             (end (save-excursion (end-of-defun) (point)))
             (sub (save-excursion
                    (goto-char orig)
                    (when (re-search-forward "\\.Run[ \t]*(\"\\([^\"]+\\)\"" end t)
                      (match-string 1))))
             (regex (concat "^" test (when sub (concat "/" (regexp-quote sub))) "$")))
        (vector "-test.run" regex "-test.v"))))

  ;; Fixed config - use actual directory paths instead of variables
  (add-to-list
   'dape-configs
   `(go-test@point
     modes (go-mode go-ts-mode)
     command "dlv"
     command-args ("dap" "--listen" "127.0.0.1::autoport")
     command-cwd ,(file-name-directory (buffer-file-name))  ; Current directory
     host "127.0.0.1"
     port :autoport
     :type "debug"
     :request "launch"
     :mode "test"
     :cwd "."                         ; Current directory for the test
     :program "."
     :buildFlags ["-gcflags=all=-N -l"]
     fn ,(lambda (cfg)
           (plist-put cfg :args (my/dape--go-test-run-args))
           cfg)))

  ;; Keybinding
  (defun my/dape-go-test-at-point ()
    "Start debugging Go test at point."
    (interactive)
    (dape (alist-get 'go-test@point dape-configs)))

  (map! :leader :desc "Debug Go test at point" "d t"
        #'my/dape-go-test-at-point))

;; (after! dap-mode
;;   (setq dap-python-debugger 'debugpy))
