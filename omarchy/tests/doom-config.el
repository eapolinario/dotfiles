;;; doom-config.el --- Shared Doom lifecycle tests -*- lexical-binding: t; -*-

;; Use fixtures/run-doom.sh to isolate HOME when running locally.

(require 'ert)
(require 'org)
(load (expand-file-name "fixtures/doom-test-helper.el" (file-name-directory load-file-name)) nil t)
(load (expand-file-name "fixtures/org-download.el" doom-test-directory) nil t)
(doom-test-register-after-blocks "+org.el" 'org-download)

(ert-deftest doom-org-download-separates-file-buffers ()
  (with-temp-buffer
    (setq buffer-file-name (expand-file-name "first.org" doom-test-directory))
    (org-mode)
    (let ((first-dir org-download-image-dir))
      (should (equal first-dir (expand-file-name "first-img" doom-test-directory)))
      (with-temp-buffer
        (setq buffer-file-name (expand-file-name "second.notes.org" doom-test-directory))
        (org-mode)
        (should (equal org-download-image-dir
                       (expand-file-name "second.notes-img" doom-test-directory))))
      (should (equal org-download-image-dir first-dir))
      (should-not (default-value 'org-download-image-dir)))))

(ert-deftest doom-org-download-ignores-unnamed-buffers ()
  (with-temp-buffer
    (org-mode)
    (should-not buffer-file-name)
    (should-not org-download-image-dir)
    (run-hooks 'after-save-hook)
    (should-not org-download-image-dir)))

(ert-deftest doom-org-download-handles-indirect-capture-buffers ()
  (with-temp-buffer
    (setq buffer-file-name (expand-file-name "capture.org" doom-test-directory))
    (org-mode)
    (let ((directory org-download-image-dir)
          (capture (clone-indirect-buffer " *org-download-capture*" nil)))
      (unwind-protect
          (with-current-buffer capture
            (should-not buffer-file-name)
            (my-org-download-set-image-dir)
            (should (equal org-download-image-dir directory)))
        (kill-buffer capture)))))

(ert-deftest doom-org-download-ignores-other-major-modes ()
  (with-temp-buffer
    (setq buffer-file-name (expand-file-name "notes.txt" doom-test-directory))
    (text-mode)
    (run-hooks 'after-save-hook)
    (should-not (local-variable-p 'org-download-image-dir))))

(ert-deftest doom-org-download-follows-first-save-and-save-as ()
  (doom-test-with-directory
    (with-temp-buffer
      (org-mode)
      (insert "* A note\n")
      (should-not org-download-image-dir)
      (write-file "first.org")
      (should (equal org-download-image-dir (expand-file-name "first-img")))
      (write-file "renamed.org")
      (should (equal org-download-image-dir (expand-file-name "renamed-img")))
      (should (file-exists-p "first.org"))
      (should (file-exists-p "renamed.org")))))

(ert-deftest doom-org-download-preserves-explicit-file-and-directory-locals ()
  (dolist (local-alist '(file-local-variables-alist dir-local-variables-alist))
    (dolist (value (list nil "./custom" (expand-file-name "first-img" doom-test-directory)))
      (with-temp-buffer
        (setq buffer-file-name (expand-file-name "first.org" doom-test-directory))
        (org-mode)
        (setq-local org-download-image-dir value)
        (set (make-local-variable local-alist) (list (cons 'org-download-image-dir value)))
        (setq buffer-file-name (expand-file-name "renamed.org" doom-test-directory))
        (run-hooks 'after-save-hook)
        (should (equal org-download-image-dir value))))))

(ert-deftest doom-org-download-preserves-file-locals-through-save-as ()
  (dolist (value '(nil "./attachments" "first-img"))
    (doom-test-with-directory
      (let ((enable-local-variables :all)
            (enable-local-eval nil))
        (with-temp-file "first.org"
          (insert (format "# -*- org-download-image-dir: %S; -*-\n* A note\n" value)))
        (let ((buffer (find-file-noselect "first.org")))
          (unwind-protect
              (with-current-buffer buffer
                (should (equal org-download-image-dir value))
                (write-file "renamed.org")
                (should (equal org-download-image-dir value)))
            (kill-buffer buffer)))))))

(ert-deftest doom-org-download-preserves-manual-buffer-customization ()
  (with-temp-buffer
    (setq buffer-file-name (expand-file-name "first.org" doom-test-directory))
    (org-mode)
    (setq-local org-download-image-dir "./handpicked")
    (setq buffer-file-name (expand-file-name "renamed.org" doom-test-directory))
    (run-hooks 'after-save-hook)
    (should (equal org-download-image-dir "./handpicked"))))

(ert-deftest doom-org-download-initializes-existing-org-buffers-on-package-load ()
  (doom-test-with-unloaded-package 'org-download
    (let ((org-mode-hook nil)
          (after-save-hook nil))
      (with-temp-buffer
        (setq buffer-file-name (expand-file-name "already-open.org" doom-test-directory))
        (org-mode)
        (should-not (local-variable-p 'org-download-image-dir))
        (doom-test-register-after-blocks "+org.el" 'org-download)
        (should-not (local-variable-p 'org-download-image-dir))
        (with-temp-buffer
          (load (expand-file-name "fixtures/org-download.el" doom-test-directory) nil t))
        (should (equal org-download-image-dir
                       (expand-file-name "already-open-img" doom-test-directory)))))))

(ert-deftest doom-org-download-preserves-existing-local-values-on-package-load ()
  (let ((org-mode-hook nil))
    (dolist (value '(nil "./existing"))
      (with-temp-buffer
        (setq buffer-file-name (expand-file-name "already-open.org" doom-test-directory))
        (org-mode)
        (setq-local org-download-image-dir value)
        (doom-test-register-after-blocks "+org.el" 'org-download)
        (should (equal org-download-image-dir value))))))

(defvar agent-shell-anthropic-claude-acp-command nil)
(defvar native-comp-enable-subr-trampolines)

(defun doom-test-no-live-call (&rest _)
  "Reject real secret, process, or network calls during lifecycle tests."
  (ert-fail "Unexpected live call in an isolated lifecycle test"))

(defun doom-test-agent-shell-command (executable upstream-command)
  "Return the configured adapter with EXECUTABLE and UPSTREAM-COMMAND mocked.
Load only the agent-shell configuration, without Doom or actual agent packages."
  (doom-test-with-unloaded-package 'agent-shell
    (let ((agent-shell-anthropic-claude-acp-command upstream-command)
          ;; Mocking primitives must not write native-comp caches into HOME.
          (native-comp-enable-subr-trampolines nil)
          (lookups 0))
      (cl-letf (((symbol-function 'executable-find)
                 (lambda (command &optional _remote)
                   (should (equal command "claude-code-acp"))
                   (setq lookups (1+ lookups))
                   executable))
                ((symbol-function 'agent-shell-anthropic-make-authentication)
                 (lambda (&rest args) args))
                ((symbol-function 'agent-shell-openai-make-authentication)
                 (lambda (&rest args) args))
                ((symbol-function 'auth-source-pick-first-password) #'doom-test-no-live-call)
                ((symbol-function 'make-network-process) #'doom-test-no-live-call)
                ((symbol-function 'make-process) #'doom-test-no-live-call))
        (should-not (featurep 'agent-shell))
        (should-not (featurep 'gptel))
        (doom-test-register-after-blocks "config.el" 'agent-shell)
        (should (equal lookups 0))
        (should (eq agent-shell-anthropic-claude-acp-command upstream-command))
        (load (expand-file-name "fixtures/agent-shell.el" doom-test-directory) nil t)
        (should (featurep 'agent-shell))
        (should-not (featurep 'gptel))
        (should (equal lookups 1))
        agent-shell-anthropic-claude-acp-command))))

(ert-deftest doom-agent-shell-prefers-available-arch-adapter-without-gptel ()
  (should (equal (doom-test-agent-shell-command
                 "/usr/bin/claude-code-acp" '("upstream-adapter"))
                 '("/usr/bin/claude-code-acp"))))

(ert-deftest doom-agent-shell-preserves-upstream-command-when-adapter-is-absent ()
  (let ((upstream-command '("upstream-adapter" "--keep-existing-arguments")))
    (should (eq (doom-test-agent-shell-command nil upstream-command) upstream-command))))

(ert-deftest doom-agent-shell-keeps-resolved-paths-with-spaces-as-one-argument ()
  (should (equal (doom-test-agent-shell-command
                 "/adapter tools/claude-code-acp" '("upstream-adapter"))
                 '("/adapter tools/claude-code-acp"))))
