;;; doom-archive.el --- Org archive regressions -*- lexical-binding: t; -*-

;; Use fixtures/run-doom.sh to isolate HOME when running locally.

(require 'ert)
(require 'org)
(require 'org-archive)

;; Evaluate only the helper under test, without Doom bindings or other config.
(with-temp-buffer
  (insert-file-contents
   (expand-file-name "../../common/doom/+bindings.el" (file-name-directory load-file-name)))
  (goto-char (point-min))
  (re-search-forward "^(defun ea/org-archive-done-tasks\\_>")
  (beginning-of-line)
  (eval (read (current-buffer)) t))

(defconst doom-archive-test-todo-header "#+TODO: TODO | DONE CANCELLED\n"
  "TODO keywords used by disposable Org files.")

(defmacro doom-archive-test-with-files (contents &rest body)
  "Run BODY in a disposable Org file containing CONTENTS after its TODO header.
The archive destination is available to BODY as `archive-file'."
  (declare (indent 1) (debug (form body)))
  `(let* ((directory (make-temp-file "doom-archive-" t))
          (default-directory (file-name-as-directory directory))
          (source-file (expand-file-name "tasks.org" directory))
          (archive-file (expand-file-name "archive.org" directory))
          (org-archive-location (concat archive-file "::"))
          (org-archive-subtree-save-file-p t)
          (org-todo-keywords '((sequence "TODO" "|" "DONE" "CANCELLED")))
          (create-lockfiles nil)
          (make-backup-files nil)
          (auto-save-default nil))
     (unwind-protect
         (progn
           (with-temp-file source-file
             (insert doom-archive-test-todo-header ,contents))
           (with-current-buffer (find-file-noselect source-file)
             ,@body))
       (dolist (file (list source-file archive-file))
         (let ((buffer (find-buffer-visiting file)))
           (when buffer
             (with-current-buffer buffer (set-buffer-modified-p nil))
             (kill-buffer buffer))))
       (delete-directory directory t))))

(defun doom-archive-test-headings (file)
  "Return the headings from disposable FILE in order, including TODO states."
  (with-temp-buffer
    (insert-file-contents file)
    (org-mode)
    (org-map-entries (lambda () (org-get-heading t nil t t)))))

(defun doom-archive-test-file-contents (file)
  "Return the contents of disposable FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(ert-deftest doom-org-archive-done-tasks-archives-adjacent-completed-headings ()
  (dolist (separator '("\n" "\n\n"))
    (doom-archive-test-with-files
        (concat (mapconcat #'identity '("* DONE First" "* DONE Second" "* CANCELLED Third")
                           separator)
                separator "* TODO Keep\nKeep this body.\n")
      (ea/org-archive-done-tasks)
      (should (equal (buffer-string)
                     (concat doom-archive-test-todo-header "* TODO Keep\nKeep this body.\n")))
      (should (equal (doom-archive-test-headings archive-file)
                     '("DONE First" "DONE Second" "CANCELLED Third")))
      ;; A second invocation must not duplicate any archived subtree.
      (let ((source (buffer-string))
            (archive (doom-archive-test-file-contents archive-file)))
        (ea/org-archive-done-tasks)
        (should (equal (buffer-string) source))
        (should (equal (doom-archive-test-file-contents archive-file) archive))))))

(ert-deftest doom-org-archive-done-tasks-preserves-mixed-unfinished-headings-and-bodies ()
  (doom-archive-test-with-files
      (concat "* TODO Before\nBefore body.\n"
              "* DONE First\nFirst archived body.\n"
              "* TODO Between\nBetween body.\n"
              "* CANCELLED Second\nSecond archived body.\n"
              "* DONE Third\nThird archived body.\n"
              "* TODO After\nAfter body.\n"
              "* Notes\nDONE and CANCELLED in ordinary text stay here.\n")
    (ea/org-archive-done-tasks)
    (should (equal (buffer-string)
                   (concat doom-archive-test-todo-header
                           "* TODO Before\nBefore body.\n"
                           "* TODO Between\nBetween body.\n"
                           "* TODO After\nAfter body.\n"
                           "* Notes\nDONE and CANCELLED in ordinary text stay here.\n")))
    (should (equal (doom-archive-test-headings archive-file)
                   '("DONE First" "CANCELLED Second" "DONE Third")))
    (let ((archive (doom-archive-test-file-contents archive-file)))
      (dolist (body '("First archived body.\n" "Second archived body.\n" "Third archived body.\n"))
        (should (string-match-p (regexp-quote body) archive))))))

(ert-deftest doom-org-archive-done-tasks-handles-nested-and-final-subtrees ()
  (doom-archive-test-with-files
      (concat "* TODO Parent\nParent body.\n"
              "** DONE Nested first\nFirst archived body.\n"
              "** CANCELLED Nested second\nSecond archived body.\n"
              "** TODO Nested keep\nKeep body.\n"
              "* DONE Final\nFinal archived body.\n"
              "** DONE Child\nChild archived body.\n")
    (ea/org-archive-done-tasks)
    (should (equal (buffer-string)
                   (concat doom-archive-test-todo-header
                           "* TODO Parent\nParent body.\n"
                           "** TODO Nested keep\nKeep body.\n")))
    (should (equal (doom-archive-test-headings archive-file)
                   '("DONE Nested first" "CANCELLED Nested second" "DONE Final" "DONE Child")))
    (let ((archive (doom-archive-test-file-contents archive-file)))
      (should (string-match-p "^\\*\\* DONE Child$" archive))
      (dolist (body '("First archived body.\n" "Second archived body.\n"
                      "Final archived body.\n" "Child archived body.\n"))
        (should (string-match-p (regexp-quote body) archive))))))

(ert-deftest doom-org-archive-done-tasks-handles-changing-parent-statistics ()
  (doom-archive-test-with-files
      (concat "* TODO Parent [10/10]\nParent body.\n"
              (mapconcat (lambda (number) (format "** DONE Task %d\n" number))
                         (number-sequence 1 10) ""))
    (ea/org-archive-done-tasks)
    (should (equal (buffer-string)
                   (concat doom-archive-test-todo-header "* TODO Parent [0/0]\nParent body.\n")))
    (should (equal (doom-archive-test-headings archive-file)
                   (mapcar (lambda (number) (format "DONE Task %d" number))
                           (number-sequence 1 10))))))

(ert-deftest doom-org-archive-done-tasks-handles-only-completed-headings ()
  (doom-archive-test-with-files "* CANCELLED First\n* DONE Last"
    (ea/org-archive-done-tasks)
    (should (equal (buffer-string) doom-archive-test-todo-header))
    (should (equal (doom-archive-test-headings archive-file)
                   '("CANCELLED First" "DONE Last")))))

(ert-deftest doom-org-archive-done-tasks-leaves-files-without-matches-unchanged ()
  (doom-archive-test-with-files "* TODO Keep\nUnfinished body.\n* Notes\nDONE in plain text.\n"
    (let ((source (buffer-string)))
      (ea/org-archive-done-tasks)
      (should (equal (buffer-string) source))
      (should-not (file-exists-p archive-file)))))

;;; doom-archive.el ends here
