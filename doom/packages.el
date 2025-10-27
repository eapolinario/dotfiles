;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package:
;;
;;   1. Declare them here in a `package!' statement,
;;   2. Run 'doom sync' in the shell,
;;   3. Restart Emacs.
;;
;; Use 'C-h f package\!' to look up documentation for the `package!' macro.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;; (package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/radian-software/straight.el#the-recipe-format
;; (package! another-package
;;   :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;; (package! this-package
;;   :recipe (:host github :repo "username/repo"
;;            :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;; (package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;; (package! builtin-package :recipe (:nonrecursive t))
;; (package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see radian-software/straight.el#279)
;; (package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;; (package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;; (unpin! pinned-package)
;; ...or multiple packages
;; (unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;; (unpin! t)

;; Set up org-roam-ui as described in the manual: https://github.com/org-roam/org-roam-ui#doom
(unpin! org-roam)
(package! websocket)
(package! org-roam-ui
  :recipe (:host github :repo "org-roam/org-roam-ui" :files ("*.el" "out")))
;; This was needed to get org-roam to work on Linux
(package! simple-httpd)

;; Github copilot
(package! copilot
  :recipe (:host github :repo "zerolfx/copilot.el" :files ("*.el" "dist")))

;; git-link :heart:
(package! git-link)

;; better lists in org-mode
(package! org-autolist)

;; Enable the splendid https://github.com/rksm/clj-org-analyzer
(package! org-analyzer)

;; More org goodies
;; (package! org-pomodoro)
(package! org-autolist)
(package! org-reverse-datetree) ;; [[file:config.el::file+function my-org-links-file org-reverse-datetree-goto-date-in-file][Org template used to store links needs this]]
(package! org-download)

;; ast-grep
;; what wonders will that unlock?
(package! ast-grep)

;; org-web-tools
(package! org-web-tools)

;; yankpad
(package! yankpad)

;; alabaster theme as explained in https://tonsky.me/blog/syntax-highlighting/
(package! doom-alabaster-theme :recipe (:host github :repo "eapolinario/doom-alabaster-dark-theme"))

;; eca.dev
(package! eca :recipe (:host github :repo "editor-code-assistant/eca-emacs" :files ("*.el")))

;; Highlight hex color codes
(package! rainbow-mode)
