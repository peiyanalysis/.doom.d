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

;; Chinese
(cnfonts-enable)

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


;; input method
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
  ;; 为'posframe, 速度很快并且菜单不会变形，不过需要用户
  ;; 手动安装 posframe 包。
  (setq pyim-page-tooltip 'posframe)
  ;; 选词框显示 9 个候选词
  (setq pyim-page-length 9)
  ;; 半角标点
  (setq pyim-punctuation-translate-p '(no auto yes))
  :bind
  (("M-j" . pyim-convert-string-at-point) ;与 pyim-probe-dynamic-english 配合
   ("C-;" . pyim-delete-word-from-personal-buffer)
   ("C-c M-c C-w" . pyim-forward-word)
   ("C-c M-c C-b" . pyim-backward-word)))

;; hl-todo-mode
(use-package! hl-todo
  :init
  (setq hl-todo-keyword-faces
        '(("TODO"    . 'hl-todo-TODO)
          ("ADDCONT" . 'hl-todo-ADDCONT)
          ("REF"     . 'hl-todo-REF)
          ("MODCONT" . 'hl-todo-MODCONT)
          ("FIXME"   . 'hl-todo-FIXME)
          ("XXX"     . 'hl-todo-XXX)
          ("DONE"    . 'hl-todo-DONE)))
  (defface hl-todo-TODO    '((t :background "#00FF00"  :foreground "#FF0000" :inherit (hl-todo)))
    "Face for highlighting the HOLD keyword.")
  (defface hl-todo-ADDCONT '((t :background "#00FF00"  :foreground "#FF0000" :inherit (hl-todo)))
    "Face for highlighting the HOLD keyword.")
  (defface hl-todo-REF      '((t :background "#00FF00" :foreground "#ff0000" :inherit (hl-todo)))
    "Face for highlighting the HOLD keyword.")
  (defface hl-todo-FIXME   '((t :background "#0000FF"  :foreground "#FF0000" :inherit (hl-todo)))
    "Face for highlighting the HOLD keyword.")
  (defface hl-todo-MODCONT  '((t :background "#0000FF" :foreground "#FF0000" :inherit (hl-todo)))
    "Face for highlighting the HOLD keyword.")
  (defface hl-todo-XXX      '((t :background "#000000" :foreground "#FFFFFF" :inherit (hl-todo)))
    "Face for highlighting the HOLD keyword.")
  (defface hl-todo-DONE    '((t :background "#00FF00"  :foreground "#00FF00" :inherit (hl-todo)))
    "Face for highlighting the HOLD keyword.")
  (map! :leader
        :prefix "c"
        :desc "show comment tags" "g" #'hl-todo-mode))

(map! :leader
      :desc "ivy magit todo"             "g i" #'ivy-magit-todos)

;; basic org settings
(require 'find-lisp)
(setq org-directory "~/Dropbox/.org"
      org-ellipsis " ▼ "
      org-adapt-indentation nil)
(setq org-id-link-to-org-use-id t)

;; bullet settings
(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode))

;; org-outline quick movement
(after! org
  (map! :map org-mode-map
        "M-n" #'outline-next-visible-heading
        "M-p" #'outline-previous-visible-heading)
  (add-hook 'org-capture-mode-hook #'org-id-get-create))

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
  (setq org-roam-directory (file-truename "~/Dropbox/.org/roams/")
        org-roam-index-file "~/Dropbox/.org/roams/index.org"
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

(map! :leader
      :desc "save org buffers"           "f o" #'org-save-all-org-buffers)

;; deft
(use-package deft
  :after org
  :bind ("<f9>" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory "~/Dropbox/.org/"))

;; tex-live
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
;; (setq TeX-view-program-selection '((output-pdf "Okular")))
(setq TeX-view-program-selection
   (quote
    (((output-dvi has-no-display-manager)
      "dvi2tty")
     ((output-dvi style-pstricks)
      "dvips and gv")
     (output-dvi "xdvi")
;;     (output-pdf "Zathura")
     (output-pdf "Okular")
     (output-html "xdg-open"))))
(setq TeX-source-correlate-mode t)
(setq TeX-source-correlate-start-server t)
(setq TeX-PDF-mode t)

;; window
;; window swap - ace-window
(use-package! ace-window
  :config
  (setq aw-keys '(?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
  :init
  (map! :leader
        :prefix "w"
        :desc "ace-window-select" "a" #'ace-window))

;; telega
(setq telega-proxies
      (list
       '(:server "127.0.0.1" :port 1080 :enable :false
                 :type (:@type "proxyTypeSocks5"
                               :username "" :password ""))))

(use-package! poporg
  :bind (("C-c '" . poporg-dwim)))

;; Baidu translate
(use-package! baidu-translate
  :init
  (global-set-key (kbd "C-c m") 'baidu-translate-zh-mark)
  (global-set-key (kbd "C-c M") 'baidu-translate-zh-whole-buffer)
  ;;设置你的百度翻译 APPID
  (setq baidu-translate-appid "20200510000447604")
  ;;设置你的秘钥
  (setq baidu-translate-security "Z5Ga8KOYLjto3H3VN8Pi")
  (map! :leader
        :desc "EN->ZH marks"            "a z" #'baidu-translate-zh-mark
        :desc "EN->ZH buffer"           "a Z" #'baidu-translate-zh-whole-buffer
        :desc "ZH->EN marks"            "a e" #'baidu-translate-en-mark
        :desc "ZH->EN buffer"           "a E" #'baidu-translate-en-whole-buffer))

;; block
(map! :leader
      :prefix "c"
      (:prefix-map ("H" . "hide code")
       :desc "hide block"               "b" #'hs-hide-block
       :desc "hide level"               "l" #'hs-hide-level
       :desc "hide all"                 "a" #'hs-hide-all)
      (:prefix-map ("S" . "show code")
       :desc "show block"               "b" #'hs-show-block
       :desc "show level"               "l" #'hs-show-level
       :desc "show all"                 "a" #'hs-show-all))

;; feature-functions
(defun insert-time ()
  "Insert a timestamp according to locale's date and time format."
  (interactive)
  (insert (format-time-string "%c" (current-time))))
;; key-bindings
(map! :leader
      :desc "insert time"                "i t" #'insert-time)

;; comment
(global-set-key (kbd "C-c C-\\") (quote comment-line))

;; keybindings
(map! :leader
      :desc "Left workspace"                    "TAB ," #'+workspace/switch-left
      :desc "Right workspace"                   "TAB ." #'+workspace/switch-right
      :desc "Switch workspace"                  "TAB w" #'+workspace/switch-to)

;; map
(map! :leader
      (:prefix-map ("a" . "applications")
       :desc "eaf-browser-link"          "l" #'eaf-open-browser
       :desc "eaf-browser-bookmark"      "b" #'eaf-open-bookmark
       :desc "eaf-search-it"             "s" #'haf-open-bookmark))

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING" "BREAK"))))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("NEXT" :foreground "blue" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("HOLD" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold)
              ("MEETING" :foreground "forest green" :weight bold)
              ("PHONE" :foreground "forest green" :weight bold)
              ("BREAK" :foreground "forest green" :weight bold))))

(setq org-treat-S-cursor-todo-selection-as-state-change nil) ;

(setq org-capture-templates
      (quote (("t" "todo" entry (file "~/Dropbox/.org/inbox.org")
               "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t))))

(setq org-refile-targets ())

(setq org-agenda-files (quote ("~/Dropbox/.org/inbox.org"
                               "~/Dropbox/.org/todolist.org")))
(setq org-agenda-bin (quote ("~/Dropbox/.org/bin.org")))

(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9)
                                 (org-agenda-bin :maxlevel . 1))))
