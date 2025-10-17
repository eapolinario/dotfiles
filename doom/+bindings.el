;;; +bindings.el -*- lexical-binding: t; -*-

;;
;; Copyright (C) 2025 Eduardo Apolinario
;;
;; Author: Eduardo Apolinario <curupa@gmail.com>
;; Maintainer: Eduardo Apolinario <curupa@gmail.com>
;;; Commentary:
;; This is just a list of key bindings to be used in my doom emacs configuration.
;;
;;
;;; Code:

;; Since elisp doesn't have namespaces, we need to prefix our functions with a unique prefix, usually people use their
;; initials or a short name. In this case, I am using "ea" for Eduardo Apolinario.
(defun ea/split-comma-separated-list ()
  "Split comma-separated list into multiple lines."
  (interactive "*")
  (save-excursion
    (let* ((indent (+ (current-indentation) 2)) ; Adjust according to your requirement
           (list-start (progn (search-forward "(") (point)))
           (list-end (progn (search-forward ")") (1- (point))))
           (items (split-string (buffer-substring list-start list-end) "," t " ")))
      (delete-region list-start list-end)
      (goto-char list-start)
      ;; Insert each item on a new line with correct indentation and comma
      (dolist (item items)
        (insert (format "\n%s%s," (make-string indent ? ) item)))
      (newline)
      ;; Move closing parenthesis to  a new line with correct indentation
      (forward-char)
      (delete-char 1)
      (backward-char 2)
      (when (not (looking-at ","))
        (forward-char)
        (insert ","))
      (delete-char 1)
      (insert (format "\n%s)" (make-string (current-indentation) ? ))))))

;; Cribbed from jethro's emacs config
(defun ea/org-archive-done-tasks ()
  "Archive all done tasks."
  (interactive)
  (org-map-entries 'org-archive-subtree "/DONE|CANCELLED" 'file))

;; Navigate to next/previous sibling in org mode, hiding the previous one
(defun ea/org-forward-sibling-and-hide-previous ()
  "Hide current subtree, then move to next sibling."
  (interactive)
  (outline-hide-subtree)
  (outline-next-visible-heading 1)
  (outline-show-subtree))

(defun ea/org-backward-sibling-and-hide-previous ()
  "Hide current subtree, then move to previous sibling."
  (interactive)
  (outline-hide-subtree)
  (outline-previous-visible-heading 1)
  (outline-show-subtree)) 

(defun ea/insert-gh-commit-url (repo-url hash)
  "Insert https://github.com/OWNER/REPO/commit/HASH."
  (interactive "sGitHub repo URL: \nsCommit hash: ")
  (let* ((base (replace-regexp-in-string "\\.git\\'" "" repo-url))
         (base (replace-regexp-in-string "\\`git@github\\.com:" "https://github.com/" base)))
    (insert (format "%s/commit/%s" base hash))))

;; Define a function to generate a link to a commit in the original repository
;; Works for GitHub, GitLab, Bitbucket, and other platforms
(defun ea/git-commit-link (commit)
  "Build a web URL to COMMIT for the current repo's origin and copy it.
Supports GitHub, GitLab, Bitbucket, and generic servers.
When called interactively, prompt for COMMIT (defaults to HEAD)."
  (interactive
   (list (let ((s (read-string "Commit (SHA or ref, default HEAD): ")))
           (if (or (null s) (string= s "")) "HEAD" s))))
  (require 'subr-x) ;; for string-trim
  (let* ((remote (string-trim
                  (condition-case _
                      (car (process-lines "git" "config" "--get" "remote.origin.url"))
                    (error (user-error "Not in a Git repo or no remote.origin.url")))))
         host user repo)
    ;; SSH: git@HOST:USER/REPO(.git)
    (when (string-match
           "\\`[^@]+@\\([^:]+\\):\\([^/]+\\)/\\([^/]+?\\)\\(?:\\.git\\)?/?\\'"
           remote)
      (setq host (match-string 1 remote)
            user (match-string 2 remote)
            repo (match-string 3 remote)))
    ;; HTTPS: https://HOST/USER/REPO(.git)
    (when (and (not host)
               (string-match
                "\\`https?://\\([^/]+\\)/\\([^/]+\\)/\\([^/]+?\\)\\(?:\\.git\\)?/?\\'"
                remote))
      (setq host (match-string 1 remote)
            user (match-string 2 remote)
            repo (match-string 3 remote)))
    (unless (and host user repo)
      (user-error "Couldn't parse remote URL: %s" remote))
    ;; Resolve commit to full SHA if possible
    (let* ((resolved
            (string-trim
             (condition-case _
                 (car (process-lines "git" "rev-parse" "--verify" "--quiet" commit))
               (error commit))))
           (platform
            (cond
             ((string-match-p "github\\.com\\'" host) 'github)
             ((string-match-p "gitlab\\.com\\'" host) 'gitlab)
             ((string-match-p "bitbucket\\.org\\'" host) 'bitbucket)
             (t 'generic)))
           (url
            (pcase platform
              ('github    (format "https://%s/%s/%s/commit/%s" host user repo resolved))
              ('gitlab    (format "https://%s/%s/%s/-/commit/%s" host user repo resolved))
              ('bitbucket (format "https://%s/%s/%s/commits/%s" host user repo resolved))
              (_          (format "https://%s/%s/%s/commit/%s" host user repo resolved)))))
      (kill-new url)
      (message "Commit link copied: %s" url)
      url)))


;; Cribbed from https://karthinks.com/software/jumping-directories-in-eshell/
(defun eshell/z (&optional regexp)
  "Navigate to a previously visited directory in eshell, or to
any directory proferred by `consult-dir'."
  (let ((eshell-dirs (delete-dups
                      (mapcar 'abbreviate-file-name
                              (ring-elements eshell-last-dir-ring)))))
    (cond
     ((and (not regexp) (featurep 'consult-dir))
      (let* ((consult-dir--source-eshell `(:name "Eshell"
                                           :narrow ?e
                                           :category file
                                           :face consult-file
                                           :items ,eshell-dirs))
             (consult-dir-sources (cons consult-dir--source-eshell
                                        consult-dir-sources)))
        (eshell/cd (substring-no-properties
                    (consult-dir--pick "Switch directory: ")))))
     (t (eshell/cd (if regexp (eshell-find-previous-directory regexp)
                     (completing-read "cd: " eshell-dirs)))))))

(map!
 :leader
 (:map prog-mode-map
  :desc "Find all references" :n "r" #'lsp-find-references
  :desc "Split comma-separated list" :n "c S" #'ea/split-comma-separated-list)
 ;; Archive done tasks in org mode
 (:map org-mode-map
  :desc "Archive done tasks" :n "t A" #'ea/org-archive-done-tasks
  :desc "Forward sibling and hide previous" :n "] ]" #'ea/org-forward-sibling-and-hide-previous
  :desc "Backward sibling and hide previous" :n "[ [" #'ea/org-backward-sibling-and-hide-previous))

;;; +bindings.el ends here
