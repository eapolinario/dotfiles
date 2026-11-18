;;; doom-list-split.el --- Bounded list formatting tests -*- lexical-binding: t; -*-

;; Run via fixtures/run-doom.sh to isolate HOME without loading Doom or providers.

(require 'ert)

;; Evaluate only the helper under test, not the surrounding Doom configuration.
(with-temp-buffer
  (insert-file-contents
   (expand-file-name "../../common/doom/+bindings.el"
                     (file-name-directory load-file-name)))
  (goto-char (point-min))
  (re-search-forward "^(defun ea/split-comma-separated-list ()")
  (beginning-of-line)
  (eval (read (current-buffer)) t))

(ert-deftest doom-list-split-at-eof ()
  (with-temp-buffer
    (insert "(a,b)")
    (goto-char (point-min))
    (ea/split-comma-separated-list)
    (should (equal (buffer-string) "(\n  a,\n  b,\n)"))
    (should (= (point) (point-min)))))

(ert-deftest doom-list-split-preserves-trailing-text ()
  (with-temp-buffer
    (insert "(a,b)X")
    (goto-char (point-min))
    (ea/split-comma-separated-list)
    (should (equal (buffer-string) "(\n  a,\n  b,\n)X"))))

(ert-deftest doom-list-split-preserves-surrounding-content-and-indentation ()
  (with-temp-buffer
    (insert "before\n    call( a, b )X (c,d)\nafter\n")
    (goto-char (point-min))
    (forward-line 1)
    (ea/split-comma-separated-list)
    (should (equal (buffer-string)
                   "before\n    call(\n      a,\n      b,\n    )X (c,d)\nafter\n"))))

(ert-deftest doom-list-split-preserves-tab-indented-prefix ()
  (with-temp-buffer
    (setq-local tab-width 4)
    (insert "\tcall(a,b)\n")
    (goto-char (point-min))
    (ea/split-comma-separated-list)
    (should (equal (buffer-string) "\tcall(\n      a,\n      b,\n    )\n"))))

(ert-deftest doom-list-split-keeps-existing-comma-and-space-handling ()
  (with-temp-buffer
    (insert "( a, ,b, )")
    (goto-char (point-min))
    (ea/split-comma-separated-list)
    (should (equal (buffer-string) "(\n  a,\n  b,\n)"))))

(ert-deftest doom-list-split-missing-delimiters-do-not-edit ()
  (dolist (text '("plain text" "before (a,b"))
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (set-buffer-modified-p nil)
      (should-error (ea/split-comma-separated-list) :type 'search-failed)
      (should (equal (buffer-string) text))
      (should (= (point) (point-min)))
      (should-not (buffer-modified-p)))))

(ert-deftest doom-list-split-read-only-buffer-does-not-edit ()
  (with-temp-buffer
    (insert "(a,b)X")
    (goto-char (point-min))
    (set-buffer-modified-p nil)
    (setq buffer-read-only t)
    (should-error (ea/split-comma-separated-list) :type 'buffer-read-only)
    (should (equal (buffer-string) "(a,b)X"))
    (should (= (point) (point-min)))
    (should-not (buffer-modified-p))))

;;; doom-list-split.el ends here
