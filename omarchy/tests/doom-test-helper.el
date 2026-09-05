;;; doom-test-helper.el --- Isolated Doom test support -*- lexical-binding: t; -*-

(require 'cl-lib)

(defconst doom-test-directory (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing the isolated Doom configuration tests.")

(defconst doom-test-config-directory (expand-file-name "../../common/doom/" doom-test-directory)
  "Directory containing the shared Doom configuration under test.")

(defvar doom-test-directory-count 0
  "Counter for test-owned directories within the checkout.")

(defun doom-test-register-after-blocks (file package)
  "Register FILE's `after!' blocks for PACKAGE without loading Doom.
Use Emacs's real package-load lifecycle, leaving unrelated configuration,
including providers and secret lookups, unevaluated."
  (let ((found nil))
    (with-temp-buffer
      (insert-file-contents (expand-file-name file doom-test-config-directory))
      (emacs-lisp-mode)
      (check-parens)
      (goto-char (point-min))
      (condition-case nil
          (while t
            (let ((form (read (current-buffer))))
              (when (and (listp form)
                         (eq (car form) 'after!)
                         (eq (cadr form) package))
                (setq found t)
                (eval `(with-eval-after-load ',package ,@(cddr form)) t))))
        (end-of-file nil)))
    (unless found
      (error "No after! block for %s in %s" package file))))

(defmacro doom-test-with-unloaded-package (package &rest body)
  "Run BODY with PACKAGE absent and package-load callbacks isolated."
  (declare (indent 1) (debug (form body)))
  ;; `features' is not special in every Emacs version; bind it dynamically.
  `(cl-progv '(features) (list (remq ,package features))
     (let ((after-load-alist nil))
       ,@body)))

(defmacro doom-test-with-directory (&rest body)
  "Run BODY with all file writes confined to a test-owned checkout directory."
  (declare (indent 0) (debug t))
  `(let* ((default-directory doom-test-directory)
          (directory (format ".ert-files-%s-%s"
                             (emacs-pid) (cl-incf doom-test-directory-count)))
          (create-lockfiles nil)
          (make-backup-files nil)
          (auto-save-default nil))
     (make-directory directory)
     (unwind-protect
         (let ((default-directory (file-name-as-directory (expand-file-name directory))))
           ,@body)
       (delete-directory directory t))))

(provide 'doom-test-helper)
