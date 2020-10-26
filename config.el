;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Pei Yu"
      user-mail-address "yp9106@outlook.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.




;; emacs-application-framework
;; https://github.com/manateelazycat/emacs-application-framework
(use-package eaf
  :load-path "~/.emacs.d/pei-config/emacs-application-framework" ; Set to "/usr/share/emacs/site-lisp/eaf" if installed from AUR
  :custom
  (eaf-find-alternate-file-in-dired t)
  :config
  (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)
  (eaf-bind-key take_photo "p" eaf-camera-keybinding))

;; Chinese
;; search
(use-package! ace-pinyin
  :after evil
  :config
  (setq avy-all-windows t)
  (ace-pinyin-global-mode t))
;; evil-find-char-pinyin
(use-package! evil-find-char-pinyin
  :after evil
  :config
  (setq avy-all-windows t)
  (evil-find-char-pinyin-mode t))
;; ;; input method
(use-package! pyim
  :demand t
  :config
  ;; 激活 basedict 拼音词库，五笔用户请继续阅读 README
  (use-package pyim-basedict
    :config (pyim-basedict-enable))

  (setq default-input-method "pyim")

  ;; 我使用全拼
  (setq pyim-default-scheme 'quanpin)

  ;; 开启拼音搜索功能
  (pyim-isearch-mode 1)

  ;; 使用 popup-el 来绘制选词框, 如果用 emacs26, 建议设置
  ;; 为 'posframe, 速度很快并且菜单不会变形，不过需要用户
  ;; 手动安装 posframe 包。
  (setq pyim-page-tooltip 'posframe)

  ;; 选词框显示5个候选词
  (setq pyim-page-length 9)

  :bind
  (("M-j" . pyim-convert-string-at-point) ;与 pyim-probe-dynamic-english 配合
   ("C-;" . pyim-delete-word-from-personal-buffer)))

;; text editor
;; smartparens
(use-package! smartparens
  :init
  (map! :map smartparens-mode-map
        "C-M-f" #'sp-forward-sexp
        "C-M-b" #'sp-backward-sexp
        "C-M-u" #'sp-backward-up-sexp
        "C-M-d" #'sp-down-sexp
        "C-M-p" #'sp-backward-down-sexp
        "C-M-n" #'sp-up-sexp
        "C-M-s" #'sp-splice-sexp
        "C-)" #'sp-forward-slurp-sexp
        "C-}" #'sp-forward-barf-sexp
        "C-(" #'sp-backward-slurp-sexp
        "C-M-)" #'sp-backward-slurp-sexp
        "C-M-)" #'sp-backward-barf-sexp))
;; block
(map! :leader
       (:prefix-map ("H" . "hide code")
        :desc "hide block"                 "b" #'hs-hide-block
        :desc "hide level"                 "l" #'hs-hide-level
        :desc "hide all"                   "a" #'hs-hide-all)
       (:prefix-map ("S" . "show code")
        :desc "show block"                 "b" #'hs-show-block
        :desc "show all"                   "a" #'hs-show-all
        ))
;; comment
(global-set-key (kbd "C-c C-\\") (quote comment-line))

;; window
;; window swap - ace-window
(use-package! ace-window
  :config
  (setq aw-keys '(?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
  :init
  (map! :leader
        :prefix "w"
        :desc "ace-window-select" "a" #'ace-window))

;; org-mode
(setq org-directory "~/.org"
      org-ellipsis " ▼ "
      org-adapt-indentation nil)
(setq org-id-link-to-org-use-id t)

(after! org
  ;; set `SPC n c' as capture keymap
  ;; outline move method
  (map! :map org-mode-map
        "M-n" #'outline-next-visible-heading
        "M-p" #'outline-previous-visible-heading)
;; set agenda directory
(require 'find-lisp)
(setq pei/org-agenda-directory (file-truename "~/.org/"))
(setq org-agenda-files (find-lisp-find-files pei/org-agenda-directory "\.org$")))

;; set capture-templates
(after! org
        (setq org-capture-templates
                `(("i" "Inbox" entry (file "~/.org/inbox.org")
                ,(concat "* TODO %?\n"
                        "/Entered on/ %u")))))

;; tag system
(setq org-tag-alist '(("@errand" . ?e)
                      ("@office" . ?o)
                      ("@home" . ?h)
                      (:newline)
                      ("CANCELLED" . ?c)))

;; org-roam
(use-package! org-roam
  :commands (org-roam-insert org-roam-find-file org-roam-switch-to-buffer org-roam)
  :hook
  (after-init . org-roam-mode)
  :init
  (map! :leader
       (:prefix ("r" . "roam")
                :desc "Switch to buffer"              "b" #'org-roam-switch-to-buffer
                :desc "Org Roam Capture"              "c" #'org-roam-capture
                :desc "Find file"                     "f" #'org-roam-find-file
                :desc "Show graph"                    "g" #'org-roam-graph
                :desc "Insert"                        "i" #'org-roam-insert
                :desc "Insert (skipping org-capture)" "I" #'org-roam-insert-immediate
                :desc "Org Roam"                      "r" #'org-roam
                (:prefix ("d" . "by date")
                      :desc "Arbitrary date" "d" #'org-roam-dailies-date
                      :desc "Today"          "t" #'org-roam-dailies-today
                      :desc "Tomorrow"       "m" #'org-roam-dailies-tomorrow
                      :desc "Yesterday"      "y" #'org-roam-dailies-yesterday)))
  (setq org-roam-directory (file-truename "~/.org/braindump/org/")
        org-roam-db-gc-threshold most-positive-fixnum
        org-roam-graph-exclude-matcher "private"
        org-roam-tag-sources '(prop last-directory)
        org-id-link-to-org-use-id t)
  :config
  ;; org-roam-capture
  (setq org-roam-capture-templates
               ;; literally
        '(("l" "lit" plain (function org-roam--capture-get-point)
             "%?"
             :file-name "lit/${slug}"
             :head "#+title: ${title}\n"
             :unnarrowed t)
          ("c" "concept" plain (function org-roam--capture-get-point)
             "%?"
             :file-name "concepts/${slug}"
             :head "#+title: ${title}\n"
             :unnarrowed t)
          ("d" "default" plain (function org-roam--capture-get-point)
             "%?"
             :file-name "${slug}"
             :head "#+title: ${title}\n"
             :unnarrowed t)))
  ;; org-roam-capture-immediate
  (setq org-roam-capture-immediate-template
               ;; default
               '("d" "default" plain (function org-roam--capture-get-point)
                 "%?"
                 :file-name "${slug}"
                 :head "#+title: ${title}\n"
                 :unnarrowed t)))

(use-package! org-roam-protocol
  :after org-protocol)

(use-package! org-roam-server
  :config
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 9090
        org-roam-server-authenticate nil
        org-roam-server-export-inline-images t
        org-roam-server-serve-files nil
        org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
        org-roam-server-network-poll t
        org-roam-server-network-arrows nil
        org-roam-server-network-label-truncate t
        org-roam-server-network-label-truncate-length 60
        org-roam-server-network-label-wrap-length 20))

(unless (server-running-p)
  (org-roam-server-mode))


;; deft
(use-package! deft
  :after org
  :bind ("<f9>" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory "~/.org/"))

;; clock block view
(use-package! org-clock-convenience
  :bind (:map org-agenda-mode-map
              ("<S-up>" . org-clock-convenience-timestamp-up)
              ("<S-down>" . org-clock-convenience-timestamp-down)
              ("o" . org-clock-convenience-fill-gap)
              ("e" . org-clock-convenience-fill-gap-both)))
