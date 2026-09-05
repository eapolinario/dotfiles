;;; org-download.el --- Minimal upstream variable contract -*- lexical-binding: t; -*-

(defvar org-download-image-dir nil
  "Image directory; buffer-local in upstream org-download.")
(make-variable-buffer-local 'org-download-image-dir)

(provide 'org-download)
