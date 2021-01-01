;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(map! :leader
      :desc "Other frame"                       "o o" #'other-frame)

(map! :leader :desc"doom/scratch"            "X" #'doom/open-scratch-buffer)

(setq user-full-name "Pei Yu"
      user-mail-address "yp9106@outlook.com")

(setq doom-theme 'doom-manegarm)

(setq doom-theme 'doom-manegarm)

(setq doom-font (font-spec :family "Source Code Pro" :size 16 :weight 'semi-light)
        doom-variable-pitch-font (font-spec :family "Libre Baskerville") ; inherits `doom-font''s :size
        doom-unicode-font (font-spec :family "Sarasa Mono SC")
        ;; doom-big-font (font-spec :family "Fira Mono" :size 19)
        )
(set-fontset-font t 'unicode "Symbola" nil 'prepend)

(setq display-line-numbers-type nil)

(use-package doom-modeline
  :hook
  (window-setup . doom-modeline-mode)
  :config
  (use-package nyan-mode
    :hook (doom-modeline-mode . nyan-mode)
    :config
    (nyan-mode 1)
    (setq nyan-animate-nyancat t)
    (setq nyan-wavy-trail t)
    (setq mode-line-format
          (list
           '(:eval (list (nyan-create))))))
  (display-time-mode t)
  (setq doom-modeline-icon (display-graphic-p))
  (setq doom-modeline-height 40)
  (setq doom-modeline-bar-width 3)
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-major-mode-color-icon t)
  (setq doom-modeline-buffer-state-icon t)
  (setq doom-modeline-buffer-modification-icon t)
  (setq doom-modeline-modal-icon t)
  (setq doom-modeline-buffer-encoding nil))
(use-package posframe)

;; window
;; window swap - ace-window
(use-package! ace-window
  :config
  (setq aw-keys '(?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
  :init
  (map! :leader
        :prefix "w"
        :desc "ace-window-select" "a" #'ace-window))

(use-package winner-mode
  :hook (after-init . winner-mode))

(use-package saveplace
  :hook (after-init . save-place-mode))

;; keybindings
(map! :leader
      :desc "Left workspace"                    "TAB ," #'+workspace/switch-left
      :desc "Right workspace"                   "TAB ." #'+workspace/switch-right
      :desc "Switch workspace"                  "TAB w" #'+workspace/switch-to)

;; input method
(use-package! pyim
  :demand t
  :config
  ;; 激活 basedict 拼音词库，五笔用户请继续阅读 README
  ;; (use-package pyim-basedict
  ;;   :config (pyim-basedict-enable))
  ;; 设置 zh－wiki 词库和 zh－moegirl 词库
  (setq pyim-dicts
        '((:name "zh-tsinghua"          :file "/home/py06/.doom.d/pyim_dicts/zh-tsinghua.pyim")
          (:name "zh-wiki"              :file "/home/py06/.doom.d/pyim_dicts/zh-wiki.pyim")
          (:name "zh-math"              :file "/home/py06/.doom.d/pyim_dicts/zh-math.pyim")
          (:name "zh-moegirl"           :file "/home/py06/.doom.d/pyim_dicts/zh-moegirl.pyim")))
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
  (("C-c M-c C-w" . pyim-forward-word)
   ("C-c M-c C-b" . pyim-backward-word)))
(define-key pyim-mode-map "." 'pyim-page-next-page)
(define-key pyim-mode-map "," 'pyim-page-previous-page)
(define-key pyim-mode-map ";"
  (lambda ()
    (interactive)
    (pyim-page-select-word-by-number 2)))

(use-package rime
  :config
  (setq rime-user-data-dir "~/.local/share/fcitx5/rime/")
  (setq rime-posframe-properties
        (list :background-color "#333333"
              :foreground-color "#dcdccc"
              :internal-border-width 10))
  (setq rime-posframe-style 'vertical)
  (setq default-input-method "rime"
        rime-show-candidate 'posframe))
;;  http://ergoemacs.org/emacs/emacs_bind_number_pad_keys.html
  (map! "<kp-1>" "1"
        "<kp-2>" "2"
        "<kp-3>" "3"
        "<kp-4>" "4"
        "<kp-5>" "5"
        "<kp-6>" "6"
        "<kp-7>" "7"
        "<kp-8>" "8"
        "<kp-9>" "9"
        "<kp-0>" "0")

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

(setq TeX-engine 'xetex)

(use-package auto-activating-snippets
  :hook (latex-mode . latex-auto-activating-snippets-mode))

(use-package! latex-auto-activating-snippets)

(use-package! latex-auto-activating-snippets)

(use-package cdlatex
  :hook ((LaTeX-mode . turn-on-cdlatex)
         (org-mode . turn-on-org-cdlatex))
  :config
  (setq cdlatex-math-modify-alist
        '(( ?s  "\\mathscr" nil t nil nil )
          ( ?b  nil         nil t nil nil )
          ( ?/  "\\slashed" nil t nil nil ))))

(add-to-list 'load-path "/home/py06/.doom.d/packages")
(require 'mathpix)
(setq mathpix-app-id "yp9106_outlook_com_58f781_c2e02c"
      mathpix-app-key "b667a7350e26f378b208"
      mathpix-screenshot-method "scrot -s %s")

(use-package! gif-screencast
  :bind
  ("<f12>" . gif-screencast-start-or-stop))

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

(use-package! poporg
  :bind (("C-c '" . poporg-dwim)))

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

(use-package yasnippet
  :config
  (add-to-list 'yas-snippet-dirs "~/.doom.d/snippets"))

(use-package yasnippet-snippets)

(use-package ivy-yasnippet)

(setq hungry-delete-mode t)
(map! :leader
      (:prefix ("e" . "edit")
               :desc "hungry delete" "d" #'hungry-delete-forward))

(use-package! company-posframe
  :hook (company-mode . company-posframe-mode))

(use-package so-long
  :config (global-so-long-mode 1))

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

(use-package whitespace
  :hook ((prog-mode markdown-mode conf-mode latex-mode ) . whitespace-mode)
  :config
  (setq whitespace-style '(face trailing)))

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

(global-set-key (kbd "C-c C-\\") (quote comment-line))

(use-package! nov)

(use-package! nov)

(use-package org-pdftools
  :hook (org-mode . org-pdftools-setup-link))

(use-package org-noter-pdftools
  :after org-noter
  :config
  (with-eval-after-load 'pdf-annot
    (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))

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

(setq elfeed-use-curl nil)
(setq elfeed-protocol-ttrss-maxsize 200) ;; bigger than 200 is invalid
(setq elfeed-feeds
      '(("ttrss+https://pei@rss.archpei.ink"
         :password "fee8deb91c")))
(elfeed-protocol-enable)

(use-package elfeed
  :config
  (setq elfeed-use-curl t)
  (setq elfeed-curl-max-connections 10)
  (setq elfeed-db-directory "~/.doom.d/elfeed-db/")) ; customize this ofc

(defun elfeed-mark-all-as-read ()
  "Mark the whole buffer as read."
  (interactive)
  (mark-whole-buffer)
  (elfeed-search-untag-all-unread))

(defun bjm/elfeed-load-db-and-open ()
  "Wrapper to load the elfeed db from disk before opening"
  (interactive)
  (elfeed-db-load)
  (elfeed)
  (elfeed-search-update--force))

(defun bjm/elfeed-save-db-and-bury ()
  "Wrapper to save the elfeed db to disk before burying buffer"
  (interactive)
  (elfeed-db-save)
  (quit-window))

(use-package elfeed-goodies
  :config
  (elfeed-goodies/setup))

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
(map! :leader
      (:prefix "a"
       :desc "Telega" "t" #'telega))

(require 'telega)
(map! :after telega
      :map telega-root-mode-map
      :leader
      (:prefix ("l" . "Telega")
       :desc "Open chat with" "w" #'telega-chat-with
       :desc "View folders" "f" #'telega-view-folders
       :desc "Kill telega" "K" #'telega-kill
       :desc "Browse url" "u" #'telega-browse-url))

(require 'telega)
(map! :after telega
      :map telega-chat-mode-map
      :leader
      (:prefix "l"
       :desc "Attach" "a" #'telega-chatbuf-attach
       :desc "Cancel aux" "x" #'telega-chatbuf-cancel-aux))

(use-package ebib
  :config
  (setq ebib-file-search-dirs  '("~/Dropbox/bibliography/"))
  (setq ebib-preload-bib-files '("~/Dropbox/bibliography/references.bib" )))
  (setq ebib-file-associations '(("pdf" . "PDF tools") ("djvu" . "PDF tools")))
;; map the keys
(global-set-key (kbd "<f5>") 'ebib)

(use-package helm-bibtex
  :bind ("<f11>" . helm-bibtex)
  :commands (helm-bibtex)
  :init
  (add-hook 'bibtex-completion-edit-notes 'org-ref-open-bibtex-notes)
  (setq bibtex-completion-open-any 'org-ref-open-bibtex-pdf)
  :config
  (setq bibtex-completion-bibliography "~/Dropbox/bibliography/references.bib"
        bibtex-completion-library-path "~/Dropbox/bibliography/bibtex-pdfs"
        bibtex-completion-notes-path   "~/Dropbox/bibliography/helm-bibtex-notes/")
  ;(setq bibtex-completion-display-formats
  ;  '((t . "${=type=:7} ${year:4} ${=has-pdf=:1}${=has-note=:1} ${author:30} ${title:72} ")))
  (setq bibtex-completion-additional-search-fields '(keywords))
  (setq bibtex-completion-notes-template-one-file
	(format "\n** TODO ${=key=} - ${title}\n  :PROPERTIES:\n    :Author: ${author-or-editor}\n    :Journal: ${journal}\n  :END:\n\n"))
  (setq bibtex-completion-display-formats
	'((t . "${author:20} ${year:4} ${=has-pdf=:3} ${=has-note=:1} ${=type=:7} ${title:90}")))
  (setq bibtex-completion-pdf-field "file")
  (setq bibtex-completion-pdf-symbol "PDF")
  (setq bibtex-completion-notes-symbol "N")
 )

(use-package org-ref
  :after (org)
  :config
  ;;(setq reftex-default-bibliography '("~/OneDrive/2020.03.28_PunchingShearReferences/Literature.bib"))
  ;; see org-ref for use of these variables
  (setq bibtex-completion-pdf-field "file")
  (setq org-ref-bibliography-notes  "~/Dropbox/bibliography/notes.org"
      org-ref-default-bibliography  '("~/Dropbox/bibliography/references.bib")
      org-ref-pdf-directory         "~/Dropbox/bibliography/bibtex-pdfs/")
  ;;(setq bibtex-completion-bibliography "~/OneDrive/2020.03.28_PunchingShearReferences/Literature.bib"
  ;;    bibtex-completion-library-path "~/OneDrive/2020.03.28_PunchingShearReferences/PDFs"
  ;;    bibtex-completion-notes-path "~/OneDrive/2020.03.28_PunchingShearReferences/Literature-manuscript.org")
  (setq org-ref-show-broken-links nil)
  (setq bibtex-completion-pdf-open-function 'org-open-file)
  (setq org-ref-note-title-format
   "** TODO %k - %t
 :PROPERTIES:
  :CUSTOM_ID: %k
  :AUTHOR: %9a
  :JOURNAL: %j
  :DOI: %D
  :URL: %U
 :END:
")

  (setq bibtex-completion-display-formats
	'((t . "${author:20} ${year:4} ${=has-pdf=:3} ${=has-note=:1} ${=type=:7} ${title:90}")))
  (defun my/org-ref-notes-function (candidates)
    (let ((key (helm-marked-candidates)))
      (funcall org-ref-notes-function (car key))))

  (helm-delete-action-from-source "Edit notes" helm-source-bibtex)
;; Note that 7 is a magic number of the index where you want to insert the command. You may need to change yours.
  (helm-add-action-to-source "Edit notes" 'my/org-ref-notes-function helm-source-bibtex 7)
)

;; basic org settings
(require 'find-lisp)
(setq org-directory "~/Dropbox/.org/")

(setq org-id-link-to-org-use-id t)

(setq org-id-link-to-org-use-id t)

(setq org-ellipsis " ▼ ")

(setq org-ellipsis " ▼ ")

(setq org-adapt-indentation t)

(setq org-adapt-indentation t)

;; org-outline quick movement
(after! org
  (map! :map org-mode-map
        "M-n" #'outline-next-visible-heading
        "M-p" #'outline-previous-visible-heading))

(map! :leader
      :desc "save org buffers"           "f o" #'org-save-all-org-buffers)

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

(use-package org-noter-pdftools
  :after org-noter
  :config
  (with-eval-after-load 'pdf-annot
    (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))

(use-package org-pdftools
  :hook (org-mode . org-pdftools-setup-link))

;; org-roam
(use-package! org-roam
  :commands (org-roam-insert org-roam-find-file org-roam-switch-to-buffer org-roam)
  :hook
  (after-init . org-roam-mode)
  :init
  (map! :leader
       (:prefix ("r" . "roam")
                :desc "Switch to buffer"              "b" #'org-roam-switch-to-buffer
                (:prefix ("d" . "by date")
                      :desc "Arbitrary date" "d" #'org-roam-dailies-date
                      :desc "Today"          "t" #'org-roam-dailies-today
                      :desc "Tomorrow"       "m" #'org-roam-dailies-tomorrow
                      :desc "Yesterday"      "y" #'org-roam-dailies-yesterday)
                :desc "Find file"                     "f" #'org-roam-find-file
                :desc "Show graph"                    "g" #'org-roam-graph
                :desc "Insert new text"               "i" #'org-roam-insert
                :desc "Insert selected text"          "I" #'org-roam-insert-immediate
                :desc "Jump to index"                 "j" #'org-roam-jump-to-index
                :desc "Roam buffer"                   "r" #'org-roam
                :desc "Org Roam Capture"              "x" #'org-roam-capture))
  :config
  (setq org-roam-directory (file-truename "~/Dropbox/.org/roams/")
        org-roam-index-file (concat org-roam-directory "index.org")
        org-roam-dailies-directory "scratch/"
        org-roam-db-gc-threshold most-positive-fixnum
        org-roam-graph-exclude-matcher "private"
        org-roam-tag-sources '(prop last-directory)
        org-id-link-to-org-use-id t))

(setq org-roam-capture-templates
             ;; literally
      '(("d" "default" plain (function org-roam--capture-get-point)
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
               :unnarrowed t))

(setq org-roam-capture-ref-templates nil)
(add-to-list 'org-roam-capture-ref-templates
             '("r" "ref" plain (function org-roam-capture--get-point)
               ""
               :file-name "${slug}"
               :head "#+title: ${title}\n#+roam_key: ${ref}\n"
               :unnarrowed t))
(add-to-list 'org-roam-capture-ref-templates
             '("a" "Annotation" plain (function org-roam-capture--get-point)
               "%U \n${body}\n"
               :file-name "${slug}"
               :head "#+title: ${title}\n#+roam_key: ${ref}\n#+roam_alias:\n"
               :immediate-finish t
               :unnarrowed t))

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
;; kept server running
(unless (server-running-p)
  (org-roam-server-mode))

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
  (map! :leader :desc "org-capture"           "x" #'org-capture))
(after! org
  (add-hook 'org-capture-mode-hook #'org-id-get-create)
  (setq org-capture-templates
          `(("i" "Inbox" entry (file "~/Dropbox/.org/inbox.org")
             ,(concat "* TODO %?\n"
                      "/Entered on/ %u"))
            ;; metacognition (元认知) in current org-file
            ("m" "Metacognition")
            ;; meta question: 思考疑问？
            ("mq" "Questions" entry (function ,(lambda ()
                                                 (jethro/olp-current-buffer "Metacog" "Questions")))
             ,(concat "* TODO Q: %?\n"
                      "/Entered on/ %u"))
            ;; meta note: 记录思考
            ("mn" "Notes" entry (function ,(lambda ()
                                             (jethro/olp-current-buffer "Metacog" "Notes")))
             "* %?\n"))))

(defun jethro/olp-current-buffer (&rest outline-path)
  "Find the OUTLINE-PATH of the current buffer."
  (let ((m (jethro/find-or-create-olp (cons (buffer-file-name) outline-path))))
    (set-buffer (marker-buffer m))
    (org-capture-put-target-region-and-position)
    (widen)
    (goto-char m)
    (set-marker m nil)))

(defun jethro/find-or-create-olp (path &optional this-buffer)
  "Return a marker pointing to the entry at outline path OLP.
If anything goes wrong, throw an error, and if you need to do
something based on this error, you can catch it with
`condition-case'.
If THIS-BUFFER is set, the outline path does not contain a file,
only headings."
  (let* ((file (pop path))
         (level 1)
         (lmin 1)
         (lmax 1)
         (start (point-min))
         (end (point-max))
         found flevel)
    (unless (derived-mode-p 'org-mode)
      (error "Buffer %s needs to be in Org mode" buffer))
    (org-with-wide-buffer
     (goto-char start)
     (dolist (heading path)
       (let ((re (format org-complex-heading-regexp-format
                         (regexp-quote heading)))
             (cnt 0))
         (while (re-search-forward re end t)
           (setq level (- (match-end 1) (match-beginning 1)))
           (when (and (>= level lmin) (<= level lmax))
             (setq found (match-beginning 0) flevel level cnt (1+ cnt))))
         (when (> cnt 1)
           (error "Heading not unique on level %d: %s" lmax heading))
         (when (= cnt 0)
           ;; Create heading if it doesn't exist
           (goto-char end)
           (unless (bolp) (newline))
           (org-insert-heading nil nil t)
           (unless (= lmax 1) (org-do-demote))
           (insert heading)
           (setq end (point))
           (goto-char start)
           (while (re-search-forward re end t)
             (setq level (- (match-end 1) (match-beginning 1)))
             (when (and (>= level lmin) (<= level lmax))
               (setq found (match-beginning 0) flevel level cnt (1+ cnt))))))
       (goto-char found)
       (setq lmin (1+ flevel) lmax (+ lmin (if org-odd-levels-only 1 0)))
       (setq start found
             end (save-excursion (org-end-of-subtree t t))))
     (point-marker))))

(setq org-agenda-files (quote ("~/Dropbox/.org/inbox.org"
                               "~/Dropbox/.org/repeater.org"
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
