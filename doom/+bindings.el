;;; +bindings.el --- This is just a list of key bindings to be used in my doom emacs configuration -*- lexical-binding: t; -*-
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

(map!
 :leader
 (:map prog-mode-map
  :desc "Find all references" :n "r" #'lsp-find-references
  :desc "Split comma-separated list" :n "c S" #'ea/split-comma-separated-list
  )
 )

;;; +bindings.el ends here
