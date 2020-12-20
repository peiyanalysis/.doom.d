;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Pei Yu"
      user-mail-address "yp9106@outlook.com")

(setq doom-theme 'nil)
(require 'nano-theme-dark)
(require 'nano-modeline)
(require 'nano-help)

(setq doom-font (font-spec :family "Source Code Pro" :size 16 :weight 'semi-light)
        doom-variable-pitch-font (font-spec :family "Libre Baskerville") ; inherits `doom-font''s :size
        doom-unicode-font (font-spec :family "Sarasa Mono SC")
        ;; doom-big-font (font-spec :family "Fira Mono" :size 19)
        )
(set-fontset-font t 'unicode "Symbola" nil 'prepend)

(setq display-line-numbers-type nil)

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
  ;; (use-package pyim-basedict
  ;;   :config (pyim-basedict-enable))
  ;; 设置zh－wiki词库和zh－moegirl词库
  (setq pyim-dicts
        '((:name "zh-tsinghua"          :file "/home/py06/.doom.d/pyim_dicts/zh-tsinghua.pyim")
          (:name "zh-wiki"              :file "/home/py06/.doom.d/pyim_dicts/zh-wiki.pyim")
          (:name "zh-math"              :file "/home/py06/.doom.d/pyim_dicts/zh-math.pyim")
          (:name "zh-moegirl"           :file "/home/py06/.doom.d/pyim_dicts/zh-moegirl.pyim")))
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
  (setq pyim-punctuation-dict nil)
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

(use-package parinfer
  :bind
  (("C-," . parinfer-toggle-mode))
  :init
  (progn
    (setq parinfer-extensions
          '(defaults       ; should be included.
            pretty-parens  ; different paren styles for different modes.
            evil           ; If you use Evil.
            lispy          ; If you use Lispy. With this extension, you should install Lispy and do not enable lispy-mode directly.
            paredit        ; Introduce some paredit commands.
            smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab.
            smart-yank))   ; Yank behavior depend on mode.
    (add-hook 'clojure-mode-hook #'parinfer-mode)
    (add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
    (add-hook 'common-lisp-mode-hook #'parinfer-mode)
    (add-hook 'scheme-mode-hook #'parinfer-mode)
    (add-hook 'lisp-mode-hook #'parinfer-mode)))

;; basic org settings
(require 'find-lisp)
(setq org-directory "~/Dropbox/.org"
      org-ellipsis " ▼ "
      org-adapt-indentation nil)
(setq org-id-link-to-org-use-id t)

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
        org-roam-index-file "/home/py06/Dropbox/.org/roams/index.org"
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
(setq TeX-view-program-selection '((output-pdf "Okular")))
;; (setq TeX-view-program-selection
;;    (quote
;;     (((output-dvi has-no-display-manager)
;;       "dvi2tty")
;;      ((output-dvi style-pstricks)
;;       "dvips and gv")
;;      (output-dvi "xdvi")
;;      (output-pdf "Okular")
;;      (output-html "xdg-open"))))
(setq TeX-source-correlate-mode t)
(setq TeX-source-correlate-start-server t)
(setq TeX-PDF-mode t)

(setq TeX-engine 'xetex)

(use-package! org-download
  :commands
  org-download-dnd
  org-download-yank
  org-download-screenshot
  org-download-dnd-base64
  :init
  (map! :map org-mode-map
        "s-Y" #'org-download-screenshot
        "s-y" #'org-download-yank)
  (pushnew! dnd-protocol-alist
            '("^\\(?:https?\\|ftp\\|file\\|nfs\\):" . +org-dragndrop-download-dnd-fn)
            '("^data:" . org-download-dnd-base64))
  (advice-add #'org-download-enable :override #'ignore)
  :config
  (defun +org/org-download-method (link)
    (let* ((filename
            (file-name-nondirectory
             (car (url-path-and-query
                   (url-generic-parse-url link)))))
           ;; Create folder name with current buffer name, and place in root dir
           (dirname (concat "./images/"
                            (replace-regexp-in-string " " "_"
                                                      (downcase (file-name-base buffer-file-name)))))
           (filename-with-timestamp (format "%s%s.%s"
                                            (file-name-sans-extension filename)
                                            (format-time-string org-download-timestamp)
                                            (file-name-extension filename))))
      (make-directory dirname t)
      (expand-file-name filename-with-timestamp dirname)))
  :config
  (setq org-download-screenshot-method
        (cond (IS-MAC "screencapture -i %s")
              (IS-LINUX
               (cond ((executable-find "maim")  "maim -u -s %s")
                     ((executable-find "scrot") "scrot -s %s")))))
  (setq org-download-method '+org/org-download-method))

(add-to-list 'load-path "/home/py06/.doom.d/packages")
(require 'mathpix)
(setq mathpix-app-id "yp9106_outlook_com_58f781_c2e02c"
      mathpix-app-key "b667a7350e26f378b208"
      mathpix-screenshot-method "scrot -s %s")

(use-package org-pdftools
  :hook (org-mode . org-pdftools-setup-link))

(use-package org-noter-pdftools
  :after org-noter
  :config
  (with-eval-after-load 'pdf-annot
    (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))

;; window
;; window swap - ace-window
(use-package! ace-window
  :config
  (setq aw-keys '(?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
  :init
  (map! :leader
        :prefix "w"
        :desc "ace-window-select" "a" #'ace-window))

(use-package! eaf
  :config
  ;; (setq eaf-enable-debug t) ; should only be used when eaf is wigging out
  (eaf-setq eaf-browser-dark-mode "false")
  (setq eaf-browser-default-search-engine "duckduckgo")
  (eaf-setq eaf-browse-blank-page-url "https://duckduckgo.com"))

;; telega
(setq telega-proxies
      (list
       '(:server "127.0.0.1" :port 1080 :enable t
                 :type (:@type "proxyTypeSocks5"
                               :username "" :password ""))))

(use-package! poporg
  :bind (("C-c '" . poporg-dwim)))

(use-package! easy-hugo
  :config
  (setq! easy-hugo-root "~/Blog/RandN/"
         easy-hugo-basedir "~/Blog/RandN/"
         easy-hugo-url "https://peiyanalysis.github.io"
         easy-hugo-previewtime "300"
         easy-hugo-default-ext ".md"
         easy-hugo-server-flags "-D"
         easy-hugo-postdir "content/post/")
  (map! :leader :desc "hugo blog" "B" #'easy-hugo)
  (map! :map easy-hugo-mode-map
      :nivm "n" 'easy-hugo-newpost
      :nivm "D" 'easy-hugo-article
      :nivm "p" 'easy-hugo-preview
      :nivm "P" 'easy-hugo-publish
      :nivm "o" 'easy-hugo-open
      :nivm "d" 'easy-hugo-delete
      :nivm "e" 'easy-hugo-open
      :nivm "c" 'easy-hugo-open-config
      :nivm "f" 'easy-hugo-open
      :nivm "N" 'easy-hugo-no-help
      :nivm "v" 'easy-hugo-view
      :nivm "r" 'easy-hugo-refresh
      :nivm "g" 'easy-hugo-refresh
      :nivm "s" 'easy-hugo-sort-time
      :nivm "S" 'easy-hugo-sort-char
      :nivm "G" 'easy-hugo-github-deploy
      :nivm "A" 'easy-hugo-amazon-s3-deploy
      :nivm "C" 'easy-hugo-google-cloud-storage-deploy
      :nivm "q" 'evil-delete-buffer
      :nivm "TAB" 'easy-hugo-open
      :nivm "RET" 'easy-hugo-preview))

(use-package ox-hugo
  :after ox)

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

(use-package! company-posframe
  :hook (company-mode . company-posframe-mode))

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

(use-package winner-mode
  :hook (after-init . winner-mode))

(use-package saveplace
  :hook (after-init . save-place-mode))

(use-package whitespace
  :hook ((prog-mode markdown-mode conf-mode latex-mode ) . whitespace-mode)
  :config
  (setq whitespace-style '(face trailing)))

(use-package so-long
  :config (global-so-long-mode 1))

(use-package autorevert
  :hook (after-init . global-auto-revert-mode))

;; feature-functions
(defun insert-time ()
  "Insert a timestamp according to locale's date and time format."
  (interactive)
  (insert (format-time-string "%c" (current-time))))
;; key-bindings
(map! :leader
      :desc "insert time"                "i t" #'insert-time)

(map! :leader :desc"doom/scratch"            "X" #'doom/open-scratch-buffer)

(after! org
  (map! :leader :desc "org-capture"           "x" #'org-capture))

;; comment
(global-set-key (kbd "C-c C-\\") (quote comment-line))

;; keybindings
(map! :leader
      :desc "Left workspace"                    "TAB ," #'+workspace/switch-left
      :desc "Right workspace"                   "TAB ." #'+workspace/switch-right
      :desc "Switch workspace"                  "TAB w" #'+workspace/switch-to)

(map! :leader
      :desc "Other frame"                       "o o" #'other-frame)

(map! :leader
      :desc "Other frame"                       "o o" #'other-frame)

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

(after! org
  (setq org-capture-templates nil)
  (add-to-list 'org-capture-templates
             '("t" "todo" entry (file "~/Dropbox/.org/inbox.org") "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)))

(setq org-agenda-files (quote ("~/Dropbox/.org/inbox.org"
                               "~/Dropbox/.org/todolist.org" )))
(setq org-agenda-bin  '("~/Dropbox/.org/bin.org"))
(setq org-agenda-future  '("~/Dropbox/.org/future.org"))

(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9)
                                 (org-agenda-bin :maxlevel . 1))))

(use-package! org-agenda
  :init
  ;; customize ort-agenda custom command
  (map! "<f1>" #'jethro/switch-to-agenda)
  ;; ?
  (setq org-agenda-block-separator nil
        org-agenda-start-with-log-mode t)
  ;; useful switch direct
  (defun jethro/switch-to-agenda ()
    (interactive)
    (org-agenda nil " "))
  :config
  ;; is project mode
  (defun jethro/is-project-p ()
  "Any task with a todo keyword subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task has-subtask))))
  ;; skip project
  (defun jethro/skip-projects ()
  "Skip trees that are projects"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((org-is-habit-p)
        next-headline)
       ((jethro/is-project-p)
        next-headline)
       (t
        nil)))))

(setq org-columns-default-format "%40ITEM(Task) %Effort(EE){:} %CLOCKSUM(Time Spent) %SCHEDULED(Scheduled) %DEADLINE(Deadline)")
(setq org-agenda-custom-commands
    `((" " "Agenda"
       ((agenda ""
               ((org-agenda-span 'week)
                (org-deadline-warning-days 365)))
       (todo "TODO"
             ((org-agenda-overriding-header "Inbox")
              (org-agenda-files '("~/Dropbox/.org/inbox.org"))))
       (todo "NEXT"
             ((org-agenda-overriding-header "In Progress")
              (org-agenda-files '("~/Dropbox/.org/todolist.org"))))
       ;; (todo "TODO"
       ;;       ((org-agenda-overriding-header "Active Projects")
       ;;        (org-agenda-skip-function #'jethro/skip-projects)
       ;;        (org-agenda-files '(,(expand-file-name "projects.org" jethro/org-agenda-directory)))))
       (todo "TODO"
             ((org-agenda-overriding-header "One-off Tasks")
              (org-agenda-files '("~/Dropbox/.org/todolist.org" "~/Dropbox/.org/inbox.org"))
              (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled)))))))))

(defun jethro/skip-projects ()
"Skip trees that are projects"
(save-restriction
  (widen)
  (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
    (cond
     ((org-is-habit-p)
      next-headline)
     ((jethro/is-project-p)
      next-headline)
     (t
      nil)))))
