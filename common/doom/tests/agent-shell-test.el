;;; agent-shell-test.el --- Isolated agent lifecycle tests -*- lexical-binding: t; -*-

(require 'ert)
(load (expand-file-name "test-helper.el" (file-name-directory load-file-name)) nil t)

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
