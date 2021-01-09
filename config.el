;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq doom-font (font-spec :family "Source Code Pro" :size 16 :weight 'semi-light)
        doom-variable-pitch-font (font-spec :family "Libre Baskerville") ; inherits `doom-font''s :size
        doom-unicode-font (font-spec :family "Sarasa Mono SC"))
(set-fontset-font t 'unicode "Symbola" nil 'prepend)

(setq display-line-numbers-type nil)

(use-package doom-modeline
  :hook
  (window-setup . doom-modeline-mode)
  :config
  ;; (use-package nyan-mode
  ;;   :hook (doom-modeline-mode . nyan-mode)
  ;;   :config
  ;;   (nyan-mode 1)
  ;;   (setq nyan-animate-nyancat t)
  ;;   (setq nyan-wavy-trail t)
  ;;   (setq mode-line-format
  ;;         (list
  ;;          '(:eval (list (nyan-create))))))
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

(map! :leader
      :desc "Other frame"                       "o o" #'other-frame)

(use-package saveplace
  :hook (after-init . save-place-mode))

;; keybindings
(map! :leader
      :desc "Left workspace"                    "TAB ," #'+workspace/switch-left
      :desc "Right workspace"                   "TAB ." #'+workspace/switch-right
      :desc "Switch workspace"                  "TAB w" #'+workspace/switch-to)

(use-package! ace-window
  :config
  (setq aw-keys '(?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
  :init
  (map! :leader
        :prefix "w"
        :desc "ace-window-select" "a" #'ace-window))

(use-package winner-mode
  :hook (after-init . winner-mode))
(map! :leader
      :prefix "w"
      :desc   "winner-undo"  "u"        #'winner-undo
      :desc   "winner-redo"  "C-r"      #'winner-redo)

(use-package! pyim
  :demand t
  :config
  (setq pyim-dicts
        '((:name "zh-tsinghua"          :file "/home/py06/.doom.d/pyim_dicts/zh-tsinghua.pyim")
          (:name "zh-wiki"              :file "/home/py06/.doom.d/pyim_dicts/zh-wiki.pyim")
          (:name "zh-math"              :file "/home/py06/.doom.d/pyim_dicts/zh-math.pyim")
          (:name "zh-moegirl"           :file "/home/py06/.doom.d/pyim_dicts/zh-moegirl.pyim")))
  :bind
  (("C-c M-c C-w" . pyim-forward-word)
   ("C-c M-c C-b" . pyim-backward-word)))

(use-package rime
  :config
  (setq rime-user-data-dir "~/.local/share/fcitx5/rime/")
  (setq rime-posframe-properties
        (list :background-color "#333333"
              :foreground-color "#dcdccc"
              :internal-border-width 10))
  (setq rime-posframe-style 'vertical)
  (setq default-input-method "rime"
        rime-show-candidate 'posframe)
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
  :bind
  (:map rime-active-mode-map
  ("<tab>" . 'rime-inline-ascii)
  :map rime-mode-map
  ("C-`" . 'rime-send-keybinding)    ;; <----
  ("M-j" . 'rime-force-enable)))

;; search
(use-package! ace-pinyin
  :after evil
  :config
  ;; 允许avy跨窗口搜索
  (setq avy-all-windows t)
  ;; 全局使用ace搜索
  (ace-pinyin-global-mode t))

;; evil-find-char-pinyin
(use-package! evil-find-char-pinyin
  :after evil
  :config
  ;;  允许avy跨窗口搜索
  (setq avy-all-windows t)
  ;; 全局使用ace搜索
  (evil-find-char-pinyin-mode t))

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(setq TeX-view-program-selection '((output-pdf "Okular")))
(setq TeX-source-correlate-mode t)
(setq TeX-source-correlate-start-server t)
(setq TeX-PDF-mode t)

(setq TeX-engine 'xetex)

(setq TeX-engine 'xetex)

(use-package! latex-auto-activating-snippets)

(use-package! latex-auto-activating-snippets)

(use-package auto-activating-snippets
  :hook (latex-mode . latex-auto-activating-snippets-mode))

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
(map! "" #'mathpix-screenshot)
(setq mathpix-app-id "yp9106_outlook_com_58f781_c2e02c"
      mathpix-app-key "b667a7350e26f378b208"
      mathpix-screenshot-method "scrot -s %s")

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
(setq org-directory "~/Dropbox/.org/")

(setq org-ellipsis " ▼ ")

(setq org-ellipsis " ▼ ")

(map! )

(map! )

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

(map! :leader :desc "doom/scratch"            "X" #'doom/open-scratch-buffer)

(setq user-full-name "Pei Yu"
      user-mail-address "yp9106@outlook.com")

(setq   py/org-inbox        (concat org-directory "inbox.org")
        py/org-todolist     (concat org-directory "todolist.org")
        py/org-bin          (concat org-directory "bin.org")
        py/org-repeater     (concat org-directory "repeater.org")
        py/org-archive      (concat org-directory "archive.org")
        py/org-maybe_future       (concat org-directory "maybe_future.org"))

(setq py/org-project-directory (file-truename (concat org-directory "projects/")))

(setq py/org-project-files
      (directory-files-recursively py/org-project-directory (rx ".org" eos)))

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "STUCKED(s@/!)" "|" "CANCELLED(c@/!)"))))
(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("NEXT" :foreground "blue" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("STUCKED" :foreground "grey" :weight bold)
              ("HOLD" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold))))

(setq org-agenda-files py/org-project-files)

(setq org-treat-S-cursor-todo-selection-as-state-change nil) ;

(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t))
              ("HOLD" ("WAITING") ("HOLD" . t))
              (done ("WAITING") ("HOLD"))
              ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
              ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
              ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

(after! org
  (map! :leader :desc "org-capture"           "x" #'org-capture))

(require 'org-protocol-capture-html)

(use-package doct
  :ensure t
  ;;recommended: defer until calling doct
  :commands (doct))

(setq org-capture-templates
      (doct '(
              ;;Standard inbox inbox
              ("Inbox"
               :keys "i"
               :file py/org-inbox
               :template ("* %{todo-state} %? \n")
               :todo-state "TODO"
               :create-id t)
              ;;org-protocol-capture-html
              ;; ("Web Content"
              ;;  :keys "w"
              ;;  :file ""
              ;;  :todo-state "TODO"
              ;;  :template ("* %a :website:\n\n%U %?\n\n%:initial"))
              ;;Metacognition
              ("Metacog"
               :keys "m"
               :prepend t
               :template ("* %{todo-state} %? \n")
               :children (;; MetaNotes
                          ("MetaNotes"
                           :keys "n"
                           :type entry
                           :todo-state "TODO"
                           :function (lambda () (jethro/olp-current-buffer "Metacog" "Notes")))
                          ("MetaQuestions"
                           :keys "q"
                           :type entry
                           :todo-state "TODO"
                           :function (lambda () (jethro/olp-current-buffer "Metacog" "Questions")))
                          ("MetaTodos"
                           :keys "t"
                           :type entry
                           :todo-state "TODO"
                           :function (lambda () (jethro/olp-current-buffer "Metacog" "Todos"))))))))

(add-hook 'org-capture-mode-hook #'org-id-get-create)

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

(defun jethro/olp-current-buffer (&rest outline-path)
  "Find the OUTLINE-PATH of the current buffer."
  (let ((m (jethro/find-or-create-olp (cons (buffer-file-name) outline-path))))
    (set-buffer (marker-buffer m))
    (org-capture-put-target-region-and-position)
    (widen)
    (goto-char m)
    (set-marker m nil)))

(map! :leader
      (:prefix-map ("z" . "tasks detailize")
                   :desc "1. file-kill task"                "1" #'org-cut-subtree
                   :desc "2. file-tags: work/position"      "2" #'org-set-tags-command
                   :desc "3. file-Schedule"                 "3" #'org-schedule
                   :desc "4. file-Deadline"                 "4" #'org-deadline
                   :desc "5. file-Priority"                 "5" #'org-priority
                   :desc "6. file-E. E."                    "6" #'org-set-effort
                   :desc "q. agenda-kill task"              "q" #'org-agenda-kill
                   :desc "w. agenda-tags: work/position"    "w" #'org-agenda-set-tags
                   :desc "e. agenda-Schedual"               "e" #'org-agenda-schedule
                   :desc "r. agenda-Deadline"               "r" #'org-agenda-deadline
                   :desc "t. agenda-Priority"               "t" #'org-agenda-priority
                   :desc "y. agenda-E. E."                  "y" #'org-agenda-set-effort))

(setq org-refile-allow-creating-parent-nodes 'confirm)

(setq org-refile-targets '((nil :maxlevel . 9)
                           (py/org-bin :maxlevel . 9)
                           (py/org-todolist :maxlevel . 9)
                           (py/org-project-files :maxlevel . 9)
                           (py/org-archive :maxlevel . 9)
                           (py/org-maybe_future :maxlevel . 9)
                           (org-roam-index-file :maxlevel . 9)))

(defun bh/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))
(setq org-refile-target-verify-function 'bh/verify-refile-target)

(setq org-agenda-files py/org-inbox) ;will be py/org-inbox

(setq org-columns-default-format "%40ITEM(Task) %Effort(EE){:} %CLOCKSUM(Time Spent) %SCHEDULED(Scheduled) %DEADLINE(Deadline)")

(setq org-agenda-custom-commands `(("z" "Agenda"
                                    ((agenda ""
                                             ((org-agenda-span 'week)
                                              (org-agenda-files '(,(expand-file-name py/org-inbox)))
                                              (org-deadline-warning-days 365)
                                              (org-agenda-use-time-grid t)
                                              (org-agenda-time-grid '((daily today)
                                                                      (0600 0800 1000 1200 1400 1600 1800 2000 2200)
                                                                      "......"
                                                                      "----------------"))))
                                     (todo "TODO"
                                           ((org-agenda-overriding-header "To Refile.")
                                            (org-agenda-files '(,(expand-file-name py/org-inbox)))))
                                     (todo "NEXT"
                                           ((org-agenda-overriding-header "In progress.")
                                            (org-agenda-files '(,(expand-file-name py/org-todolist)))))
                                     (todo "STUCKED"
                                           ((org-agenda-overriding-header "Stucked.")
                                            (org-agenda-files '(,(expand-file-name py/org-todolist)))))
                                     (todo "TODO"
                                           ((org-agenda-overriding-header "One-off Tasks.")
                                            (org-agenda-files '(,(expand-file-name py/org-todolist)))
                                            (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))))
                                     ))))

(map! "<C-f2>" #'py/switch-to-agenda)
(defun py/switch-to-agenda ()
  (interactive)
  (org-agenda nil "z"))
