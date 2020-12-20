;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)


(package! eaf :recipe
  (:host github
   :repo "manateelazycat/emacs-application-framework"
   :files ("*.el" "*.py" "core" "app")
   :no-byte-compile t))
(package! dired-narrow)
(package! easy-kill)
(package! company-posframe)
(package! ace-pinyin)
(package! evil-find-char-pinyin
  :recipe (:host github :repo "cute-jumper/evil-find-char-pinyin"))
(package! ace-jump-mode)
(package! gif-screencast
  :recipe (:host gitlab :repo "ambrevar/emacs-gif-screencast"))
(package! org-download)
(package! modus-operandi-theme)
(package! outshine)
(package! git-link)
(package! vterm)
(package! vterm-toggle)
(package! cnfonts
  :recipe (:host github :repo "tumashu/cnfonts"))
(package! window-purpose)
;; TODO initial punctution , ; Thu 03 Dec 2020 03:42:48 PM CST
(package! pyim)
(package! org-roam-server
  :recipe (:host github :repo "org-roam/org-roam-server"))
(package! ace-window
  :recipe (:host github
           :repo "notmgsk/ace-window"
           :branch "feature/posframe"
           :files ("ace-window.el" "ace-window-posframe.el")))
(package! org-bullets)
(package! org-superstar)
;; TODO Add Notifications like sound/vib/blink ;; Thu 03 Dec 2020 03:31:15 PM CST
(package! org-pomodoro)
(package! telega
  :recipe (:host github :repo "zevlg/telega.el"))
(package! dash)
(package! f)
;; TODO Add the todo supports on magit todos ; Thu 03 Dec 2020 03:32:59 PM CST
(package! magit-todos
  :recipe (:host github :repo "alphapapa/magit-todos"))
;; DONE Thu 03 Dec 2020 03:30:41 PM CST
;; Added the todos
(package! hl-todo)
;; use poporg to edit commentaries in org-mode
(package! poporg)
;; baidu-translate with unicode-escape support
(package! baidu-translate)
(package! unicode-escape)
(package! easy-hugo)
(package! ox-hugo)
(package! nano-emacs
  :recipe (:host github :repo "rougier/nano-emacs"))
(when IS-LINUX
  (package! eaf :recipe (:host github
                            :repo "manateelazycat/emacs-application-framework"
                            :files ("*")
                            :no-byte-compile t))
  )
