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


(load! "~/.doom.d/+ui/font.el")


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
(setq org-directory "~/.org/"
      org-ellipsis " ▼ "
      org-adapt-indentation nil)

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

;; Hide-Show keybinds
(map! :leader
       (:prefix-map ("H" . "hide code")
        :desc "hide block"                 "b" #'hs-hide-block
        :desc "hide level"                 "l" #'hs-hide-level
        :desc "hide all"                   "a" #'hs-hide-all)
       (:prefix-map ("S" . "show code")
        :desc "show block"                 "b" #'hs-show-block
        :desc "show all"                   "a" #'hs-show-all
))

;; Get XeLaTeX in TeX mode
;; LSP for LaTeX-mode and TeX-mode
(use-package! lsp-latex
  :config
    (setq lsp-latex-texlab-executable "/usr/bin/")
    (with-eval-after-load "tex-mode"
      (add-hook 'tex-mode-hook 'lsp)
      (add-hook 'latex-mode-hook 'lsp))
  )
;;
;; Magic latex buffer
;; ace pinyin
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

(use-package! deadgrep
  :if (executable-find "rg")
  :init
  (map! "M-s" #'deadgrep))

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

(use-package! org-roam
  :commands (org-roam-insert org-roam-find-file org-roam-switch-to-buffer org-roam)
  :hook
  (after-init . org-roam-mode)
  :init
  (map! :leader
        :prefix "n"
        :desc "org-roam" "l" #'org-roam
        :desc "org-roam-insert" "i" #'org-roam-insert
        :desc "org-roam-switch-to-buffer" "b" #'org-roam-switch-to-buffer
        :desc "org-roam-find-file" "f" #'org-roam-find-file
        :desc "org-roam-show-graph" "g" #'org-roam-show-graph
        :desc "org-roam-insert" "i" #'org-roam-insert
        :desc "org-roam-capture" "c" #'org-roam-capture)
  (setq org-roam-directory (file-truename "~/.org/roam/")
        org-roam-db-gc-threshold most-positive-fixnum
        org-roam-graph-exclude-matcher "private"
        org-roam-tag-sources '(prop last-directory)
        org-id-link-to-org-use-id t)
  :config
  (setq org-roam-capture-templates
        '(("l" "lit" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "lit/${slug}"
           :head "#+hugo_slug: ${slug}
#+title: ${title}\n"
           :unnarrowed t)
          ("c" "concept" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "concepts/${slug}"
           :head "#+hugo_slug: ${slug}
#+title: ${title}\n"
           :unnarrowed t)
          ("p" "private" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "private/${slug}"
           :head "#+title: ${title}\n"
           :unnarrowed t)))
  (setq org-roam-capture-ref-templates
        '(("r" "ref" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "lit/${slug}"
           :head "#+roam_key: ${ref}
#+hugo_slug: ${slug}
#+roam_tags: website
#+title: ${title}

- source :: ${ref}"
           :unnarrowed t)))
  (set-company-backend! 'org-mode '(company-capf)))

(use-package! org-roam-protocol
  :after org-protocol)

(use-package org-roam-server
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

(use-package! org-roam-protocol

  :after org-protocol)

(setq org-capture-templates '(("t" "Todo [inbox]" entry (file+headline "~/.org/agenda/inbox.org" "Tasks")
              "* TODO %i%?")
              ("T" "Tickler" entry (file+headline "~/.org/agenda/tickler.org" "Tickler")
              "* %i%?\n %U")))


(setq org-refile-targets '(("~/.org/agenda/gtd.org" :maxlevel . 3)
                           ("~/.org/someday.org" :level . 1)
                           ("~/.org/tickler.org" :maxlevel . 2)))

(use-package deft
  :after org
  :bind ("<f9>" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory "~/.org/"))
