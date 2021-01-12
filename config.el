;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq doom-theme 'doom-dracula)

(setq doom-theme 'doom-dracula)

(setq doom-font (font-spec :family "Sarasa Mono SC Nerd" :size 14)
      doom-big-font (font-spec :family "Sarasa Mono SC Nerd" :size 20)
      doom-variable-pitch-font (font-spec :family "Monaco" :size 18))
(push "Sarasa Mono SC Nerd" doom-unicode-extra-fonts)
;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:

(setq display-line-numbers-type nil)

(setq display-line-numbers-type nil)

(use-package beacon
  :ensure t
  :config
  (beacon-mode 1))

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

(require 'awesome-pair)
(dolist (hook (list
               'c-mode-common-hook
               'c-mode-hook
               'c++-mode-hook
               'java-mode-hook
               'haskell-mode-hook
               'latex-mode-hook
               'emacs-lisp-mode-hook
               'lisp-interaction-mode-hook
               'lisp-mode-hook
               'maxima-mode-hook
               'ielm-mode-hook
               'sh-mode-hook
               'makefile-gmake-mode-hook
               'php-mode-hook
               'python-mode-hook
               'js-mode-hook
               'go-mode-hook
               'qml-mode-hook
               'jade-mode-hook
               'css-mode-hook
               'ruby-mode-hook
               'coffee-mode-hook
               'rust-mode-hook
               'qmake-mode-hook
               'lua-mode-hook
               'swift-mode-hook
               'minibuffer-inactive-mode-hook
               ))
  (add-hook hook '(lambda () (awesome-pair-mode 1))))

(define-key awesome-pair-mode-map (kbd "(") 'awesome-pair-open-round)
(define-key awesome-pair-mode-map (kbd "[") 'awesome-pair-open-bracket)
(define-key awesome-pair-mode-map (kbd "{") 'awesome-pair-open-curly)
(define-key awesome-pair-mode-map (kbd ")") 'awesome-pair-close-round)
(define-key awesome-pair-mode-map (kbd "]") 'awesome-pair-close-bracket)
(define-key awesome-pair-mode-map (kbd "}") 'awesome-pair-close-curly)
(define-key awesome-pair-mode-map (kbd "%") 'awesome-pair-match-paren)
(define-key awesome-pair-mode-map (kbd "\"") 'awesome-pair-double-quote)
(define-key awesome-pair-mode-map (kbd "M-o") 'awesome-pair-backward-delete)
(define-key awesome-pair-mode-map (kbd "C-k") 'awesome-pair-kill)
(define-key awesome-pair-mode-map (kbd "M-\"") 'awesome-pair-wrap-double-quote)
(define-key awesome-pair-mode-map (kbd "M-[") 'awesome-pair-wrap-bracket)
(define-key awesome-pair-mode-map (kbd "M-{") 'awesome-pair-wrap-curly)
(define-key awesome-pair-mode-map (kbd "M-(") 'awesome-pair-wrap-round)
(define-key awesome-pair-mode-map (kbd "M-)") 'awesome-pair-unwrap)
(define-key awesome-pair-mode-map (kbd "M-p") 'awesome-pair-jump-right)
(define-key awesome-pair-mode-map (kbd "M-n") 'awesome-pair-jump-left)
(define-key awesome-pair-mode-map (kbd "M-:") 'awesome-pair-jump-out-pair-and-newline)

(use-package maple-iedit
  :ensure nil
  :commands (maple-iedit-match-all maple-iedit-match-next maple-iedit-match-previous)
  :config
  (setq maple-iedit-ignore-case t)

  (defhydra maple/iedit ()
    ("n" maple-iedit-match-next "next")
    ("t" maple-iedit-skip-and-match-next "skip and next")
    ("T" maple-iedit-skip-and-match-previous "skip and previous")
    ("p" maple-iedit-match-previous "prev"))
  :bind (:map evil-visual-state-map
              ("n" . maple/iedit/body)
              ("C-n" . maple-iedit-match-next)
              ("C-p" . maple-iedit-match-previous)
              ("C-t" . maple-iedit-skip-and-match-next)))

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

(use-package! hungry-delete
  :config
  (add-hook! 'after-init-hook #'global-hungry-delete-mode))

(use-package! company-posframe
  :hook (company-mode . company-posframe-mode))

(use-package so-long
  :config (global-so-long-mode 1))

(map! :leader
      (:prefix-map  ("e" . "edit")
       (:prefix-map ("h" . "hide code")
        :desc "hide block"               "b" #'hs-hide-block
        :desc "hide level"               "l" #'hs-hide-level
        :desc "hide all"                 "a" #'hs-hide-all)
       (:prefix-map ("s" . "show code")
        :desc "show block"               "b" #'hs-show-block
        :desc "show level"               "l" #'hs-show-level
        :desc "show all"                 "a" #'hs-show-all )))

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

(use-package! writeroom-mode
  :hook
  (w3m-mode . writeroom-mode)
  :config
  (advice-add 'text-scale-adjust :after
              #'visual-fill-column-adjust)
  ;;https://github.com/joostkremers/writeroom-mode#fullscreen-effect
  (setq writeroom-fullscreen-effect 'maximized))

(use-package grugru
  :config (grugru-default-setup))

(use-package! undo-fu
  :after-call doom-switch-buffer after-find-file
  :init
  (after! undo-tree
    (global-undo-tree-mode -1))
  :config
  ;; Store more undo history to prevent loss of data
  (setq undo-limit 400000
        undo-strong-limit 3000000
        undo-outer-limit 3000000)

  (define-minor-mode undo-fu-mode
    "Enables `undo-fu' for the current session."
    :keymap (let ((map (make-sparse-keymap)))
              (define-key map [remap undo] #'undo-fu-only-undo)
              (define-key map [remap redo] #'undo-fu-only-redo)
              (define-key map (kbd "C-_")     #'undo-fu-only-undo)
              (define-key map (kbd "M-_")     #'undo-fu-only-redo)
              (define-key map (kbd "C-M-_")   #'undo-fu-only-redo-all)
              (define-key map (kbd "C-x r u") #'undo-fu-session-save)
              (define-key map (kbd "C-x r U") #'undo-fu-session-recover)
              map)
    :init-value nil
    :global t)
  (undo-fu-mode +1))

(map! :leader
      (:prefix "e"
       :desc "undo-fu-only-undo"          "u"      #'undo-fu-only-undo
       :desc "undo-fu-only-redo"          "r"      #'undo-fu-only-redo
       :desc "undo-fu-only-redo-allow"    "a"      #'undo-fu-only-redo-all
       :desc "undo-fu-session-save"       "e"      #'undo-fu-session-save
       :desc "undo-fu-session-recover"    "d"      #'undo-fu-session-recover ))

(use-package! undo-fu-session
  :hook (undo-fu-mode . global-undo-fu-session-mode)
  :preface
  (setq undo-fu-session-directory (concat doom-cache-dir "undo-fu-session/")
        undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))

  ;; HACK We avoid `:config' here because `use-package's `:after' complicates
  ;;      the load order of a package's `:config' block and makes it impossible
  ;;      for the user to override its settings with merely `after!' (or
  ;;      `eval-after-load'). See jwiegley/use-package#829.
  (after! undo-fu-session
    ;; HACK Use the faster zstd to compress undo files instead of gzip
    (when (executable-find "zstd")
      (defadvice! doom--undo-fu-session-use-zstd-a (filename)
        :filter-return #'undo-fu-session--make-file-name
        (if undo-fu-session-compression
            (concat (file-name-sans-extension filename) ".zst")
          filename)))))

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

(use-package maple-explorer
  :commands (maple-explorer-file maple-explorer-buffer maple-explorer-imenu maple-explorer-recentf)
  :config
  (setq maple-explorer-file-display-alist '((side . left) (slot . -1))))

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

(map! :leader :desc "doom/scratch"            "X" #'doom/open-scratch-buffer)

(setq user-full-name "Pei Yu"
      user-mail-address "yp9106@outlook.com")

(custom-set-faces
 '(org-level-1 ((t (:inherit outline-1 :height 1.3))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.2))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.1)))))

(use-package! org-superstar
  :config
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))
  (setq org-superstar-headline-bullets-list '("☰" "☷" "☵" "☲"  "☳" "☴"  "☶"  "☱" )))

(setq org-ellipsis " ··· ")

(setq org-ellipsis " ··· ")

(setq org-hide-emphasis-markers t)

(setq org-hide-emphasis-markers t)

(use-package! valign
  :init
  (require 'valign)
  :hook
  ('org-mode . #'valign-mode))

(map! :leader
      (:prefix "m"
       (:prefix-map ("m" . "modify")
        :desc "item or text"             "i" #'org-toggle-item
        :desc "heading or text"          "h" #'org-toggle-heading )))

(setq org-directory "~/Dropbox/.org/")

(setq org-directory "~/Dropbox/.org/")

(setq py/things-dir     (concat org-directory   "things/") ;things stand for roams
      py/braindump-dir  (concat py/things-dir   "braindump/") ;second brain
      py/project-dir    (concat py/things-dir   "project/") ;projects for project files
      py/image-dir      (concat py/things-dir   "image/")   ;image stored
      py/thoughts-dir   (concat py/braindump-dir  "thoughts/") ;like roaming, but more glue
      py/arts-dir        (concat py/braindump-dir  "arts/")) ;novel, music, films, animate, comics, games, notes after reading

(setq   py/inbox                (concat org-directory   "inbox.org") ;idea records
        py/next                 (concat org-directory   "next.org")  ;one-off tasks as a todolist
        py/braindump-inbox      (concat py/braindump-dir "braindump_inbox.org")
        py/braindump-index      (concat py/braindump-dir "braindump_index.org")
        py/thoughts             (concat py/thoughts-dir "thoughts.org") ;some tempo ideas
        py/arts                 (concat py/arts-dir     "arts.org"))     ;tempo ideas of pastime

(after! org
  (map! :leader :desc "org-capture"           "x" #'org-capture))

(use-package doct
  :ensure t
  ;;recommended: defer until calling doct
  :commands (doct))

(setq org-capture-templates
      (doct '(
              ;;Standard inbox inbox
              ("Inbox"
               :keys "i"
               :file py/inbox
               :template ("* %{todo-state} %?"
                          ":PROPERTIES:"
                          ":INIT:       %U"
                          ":END:")
               :todo-state "🎬 TODO"
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
               :template ("* %{todo-state} %?"
                          ":PROPERTIES:"
                          ":INIT:       %U"
                          ":END:")
               :children (;; MetaNotes
                          ("MetaNotes"
                           :keys "n"
                           :type entry
                           :todo-state "🎬 TODO"
                           :function (lambda () (jethro/olp-current-buffer "Metacog" "Notes")))
                          ("MetaQuestions"
                           :keys "q"
                           :type entry
                           :todo-state "🎬 TODO"
                           :function (lambda () (jethro/olp-current-buffer "Metacog" "Questions")))
                          ("MetaTodos"
                           :keys "t"
                           :type entry
                           :todo-state "🎬 TODO"
                           :function (lambda () (jethro/olp-current-buffer "Metacog" "Todos"))))))))

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

(setq org-refile-targets nil)

(setq org-refile-targets nil)

(add-to-list 'org-refile-targets '(nil :maxlevel . 9))

(add-to-list 'org-refile-targets '(py/next :maxlevel . 1))

(add-to-list 'org-refile-targets '(py/arts :maxlevel . 1))

(add-to-list 'org-refile-targets '(py/braindump-inbox :maxlevel . 1))

(setq py/project-files
      (directory-files-recursively py/project-dir (rx ".org" eos)))

(add-to-list 'org-refile-targets '(py/project-files :maxlevel . 1))

(add-to-list 'org-refile-targets '(py/thoughts :maxlevel . 1))

(add-to-list 'org-refile-targets '(py/arts :maxlevel . 1))

(setq org-todo-keywords
        '((sequence
           "🎬 TODO(t)"  ; A task that needs doing & is ready to do
           "🗡 INPROCESS(s)"  ; A task that is in progress
           "📌 WAITING(w)"  ; Something is holding up this task; or it is paused
           "⏰ LEAVETO(l)"  ; entry delivered to others
           "⤴ REFILE?(r)"   ;might
           "|"
           "💡 NEXT(n)"
           "☯ DONE(d)"  ; Task successfully completed
           "CANCELED(c@)") ; Task was cancelled, aborted or is no longer applicable
           )) ; Task was completed

(setq org-todo-keyword-faces
      (quote (("🎬 TODO" :foreground "red" :weight bold)
              ("🗡 INPROCESS" :foreground "forest green" :weight bold)
              ("📌 WAITING" :foreground "orange" :weight bold)
              ("⏰ LEAVETO" :foreground "forest green" :weight bold)
              ("⤴ REFILE" :foreground "magenta" :weight bold)
              ("💡 NEXT" :foreground "blue" :weight bold)
              ("☯ DONE" :foreground "forest green" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold)
              )))

(setq org-tag-alist
      '(("@errand" . ?e)
        ("@office" . ?o)
        ("@home" . ?h)))

(map! :leader
      :prefix "n"
      (:prefix-map              ("i" . "id")
       :desc "id-create"        "C" #'org-id-get-create
       :desc "id-goto"          "g" #'org-id-goto
       :desc "id-copy"          "c" #'org-id-copy))

(map! :leader
      :desc "set initial property" "mdi" #'org-set-property-initial-time
      :desc "set initial property" "mcs" #'org-set-property-initial-time)

(defvar org-initial-current-time-format "[%Y-%m-%d %a %H:%M]"
  "Format of date to insert with `insert-current-date-time' func
See help of `format-time-string' for possible replacements")

(defun org-set-property-initial-time ()
  "Set the initial time property of entries in orgmode as\n
:INIT:    [YEAR-MONTH-DAY WEEKDAY HOUR:MIN]\n
when you realize it IS initalized."
  (interactive)
  (if (member "INIT" (org-entry-properties nil 'standard))
      ()
   (org-set-property "INIT" (format-time-string org-initial-current-time-format (current-time)))
    ))

(map! :leader
      (:prefix "m"
       (:prefix "c"
        :desc "set effort"    "e"     #'org-set-effort)))

(add-to-list 'org-global-properties
      '("Effort_ALL". "0:05 0:15 0:30 1:00 1:30 2:00 3:00 4:00"))

(use-package! notdeft
  :config
  (setq notdeft-extension "org")
  (setq notdeft-directories '("~/Dropbox/.org/"))
  (setq notdeft-xapian-program "/home/py06/.local/share/notdeft-xapian")
  :bind (:map notdeft-mode-map
         ("C-q" . notdeft-quit)
         ("C-r" . notdeft-refresh)))

(setenv "XAPIAN_CJK_NGRAM" "1")

(map! :leader
      (:prefix "n"
       :desc "notdeft" "n" #'notdeft ))

(use-package! org-roam
  :commands (org-roam-insert org-roam-find-file org-roam-switch-to-buffer org-roam)
  :hook
  (after-init . org-roam-mode))

(setq   org-roam-directory              py/braindump-dir
        org-roam-index-file             py/braindump-index
        org-roam-dailies-directory      "dailies/"
        org-roam-db-gc-threshold        most-positive-fixnum
        org-roam-graph-exclude-matcher  "private"
        org-roam-tag-sources            '(prop last-directory)
        org-id-link-to-org-use-id t)

(map! :leader
      (:prefix ("r" . "roam")
       :desc "Switch to buffer"              "b" #'org-roam-switch-to-buffer
       (:prefix ("d" . "by date")
        :desc "Arbitrary date" "d" #'org-roam-dailies-find-date
        :desc "Today"          "t" #'org-roam-dailies-find-today
        :desc "Tomorrow"       "m" #'org-roam-dailies-find-tomorrow
       :desc "Yesterday"       "y" #'org-roam-dailies-find-yesterday)
       :desc "Find file"                     "f" #'org-roam-find-file
       :desc "Show graph"                    "g" #'org-roam-graph
       :desc "Insert new text"               "i" #'org-roam-insert
       :desc "Insert selected text"          "I" #'org-roam-insert-immediate
       :desc "Jump to index"                 "j" #'org-roam-jump-to-index
       :desc "Roam buffer"                   "r" #'org-roam
       :desc "Org Roam Capture"              "x" #'org-roam-capture))

(use-package! org-roam-protocol
  :after org-protocol)

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

(use-package! org-super-agenda
:config
(add-hook! 'after-init-hook 'org-super-agenda-mode)
(setq
   org-agenda-skip-scheduled-if-done t
   org-agenda-skip-deadline-if-done t
   org-agenda-include-deadlines t
   org-agenda-include-diary nil
   org-agenda-block-separator nil
   org-agenda-compact-blocks t
   org-agenda-start-with-log-mode t)
(setq org-columns-default-format
      "%40ITEM(Task) %Effort(EE){:} %CLOCKSUM(Time Spent) %SCHEDULED(Scheduled) %DEADLINE(Deadline)")

)

(setq org-agenda-custom-commands
      `(("B" "BrainDump"
         ((todo "🎬 TODO|🗡 INPROCESS"
                ((org-agenda-overriding-header "To Refile")
                 (org-agenda-files '(,(expand-file-name py/braindump-inbox)))))
          (todo "🎬 TODO|🗡 INPROCESS"
                ((org-agenda-overriding-header "To Detail")
                 (org-agenda-files (directory-files-recursively py/braindump-dir (rx ".org" eos)))))
          (todo "🎬 TODO|🗡 INPROCESS"
                ((org-agenda-overriding-header "Arts, To Refile")
                 (org-agenda-files '(,(expand-file-name py/arts)))))
          (todo "🎬 TODO|🗡 INPROCESS"
                ((org-agenda-overriding-header "Arts, To Detail")
                 (org-agenda-files (directory-files-recursively py/arts-dir (rx ".org" eos)))))))
        ("A" "Pei's Agenda"
         ((agenda "" ((org-agenda-span 2)
                      (org-agenda-start-day "-1d")
                      (org-super-agenda-groups
                       '((:name "Today List"
                                :time-grid t
                                :date today
                                :todo "⚔ INPROCESS"
                                :scheduled today
                                :order 1)))))
          (alltodo "" ((org-agenda-overriding-header "")
                       (org-super-agenda-groups
                        '((:name "Next to do"
                                 :priority>= "B"
                                 :order 2)
                          (:name "Important"
                                 :todo "✰ Important"
                                 :order 6)
                          (:name "Due Today"
                                 :deadline today
                                 :order 3)
                          (:name "Due Soon"
                                 :deadline future
                                 :order 8)
                          (:name "Overdue"
                                 :deadline past
                                 :order 20)
                          (:name "Issues"
                                 :tag "Issue"
                                 :order 12)
                          (:name "Projects"
                                 :tag "Project"
                                 :order 14)
                          (:name "Emacs"
                                 :tag "Emacs"
                                 :order 13)
                          (:name "Research"
                                 :tag "Research"
                                 :order 15)
                          (:name "To read"
                                 :tag ("BOOK" "READ")
                                 :order 30)
                          (:name "Waiting"
                                 :todo "⚑ WAITING"
                                 :order 18)
                          (:name "trivial"
                                 :priority<= "C"
                                 :todo ("SOMEDAY")
                                 :order 90)))))
          ))))
