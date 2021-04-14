(setq rime-user-data-dir "~/.local/share/fcitx5/rime/")

(setq mathpix-dir (concat doom-private-dir "mathpix/"))
(setq mathpix-app-id (with-temp-buffer (insert-file-contents "secretes/mathpix-app-id") (buffer-string))
      mathpix-app-key (with-temp-buffer (insert-file-contents "secretes/mathpix-app-key") (buffer-string)))

(setq org-directory "~/Dropbox/Org/")

(setq py/things-dir     (concat org-directory   "things/") ;things stand for roams
      py/braindump-dir  (concat py/things-dir   "braindump/") ;second brain
      py/project-dir    (concat py/things-dir   "project/") ;projects for project files
      py/image-dir      (concat py/things-dir   "image/")   ;image stored
      py/thoughts-dir   (concat py/braindump-dir  "thoughts/") ;like roaming, but more glue
      py/arts-dir        (concat py/braindump-dir  "arts/")) ;novel, music, films, animate, comics, games, notes after reading

(setq   py/inbox                (concat org-directory   "inbox.org") ;idea records
        py/next                 (concat org-directory   "next.org")  ;one-off tasks as a todolist
        py/braindump-inbox      (concat py/braindump-dir "braindump_inbox.org")
        py/braindump-index      (concat py/braindump-dir "Index.org")
        py/thoughts             (concat py/thoughts-dir "thoughts.org") ;some tempo ideas
        py/arts                 (concat py/arts-dir     "arts.org"))     ;tempo ideas of pastime

(setq   py/bipolar (concat org-directory   "logs/bipolar.org"));idea records

(map! :leader :desc "doom/scratch"            "X" #'doom/open-scratch-buffer)

(setq doom-theme 'doom-dracula)

(setq doom-font (font-spec :family "Sarasa Mono SC Nerd" :size 14)
      doom-big-font (font-spec :family "Sarasa Mono SC Nerd" :size 20)
      doom-variable-pitch-font (font-spec :family "Monaco" :size 18))
(push "Sarasa Mono SC Nerd" doom-unicode-extra-fonts)
;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:

(setq display-line-numbers-type t)

(use-package doom-modeline
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
  :custom
  (doom-modeline-icon (display-graphic-p))
  (doom-modeline-height 40)
  (doom-modeline-bar-width 3)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-buffer-state-icon t)
  (doom-modeline-buffer-modification-icon t)
  (doom-modeline-modal-icon t)
  (doom-modeline-buffer-encoding nil))

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

(use-package ace-window
  :config
  (setq aw-keys '(?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
  :init
  (map! :leader
        :prefix "w"
        :desc "ace-window-select" "a" #'ace-window))

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
(use-package ace-pinyin
  :after evil
  :config
  ;; 允许avy跨窗口搜索
  (setq avy-all-windows t)
  ;; 全局使用ace搜索
  (ace-pinyin-global-mode t))

;; evil-find-char-pinyin
(use-package evil-find-char-pinyin
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

;(use-package latex-auto-activating-snippets)

;(use-package auto-activating-snippets
;  :hook (latex-mode . latex-auto-activating-snippets-mode))

(use-package cdlatex
  :hook ((LaTeX-mode . turn-on-cdlatex)
         (org-mode . turn-on-org-cdlatex))
  :config
  (setq cdlatex-math-modify-alist
        '(( ?s  "\\mathscr" nil t nil nil )
          ( ?b  nil         nil t nil nil )
          ( ?/  "\\slashed" nil t nil nil ))))

(add-to-list 'load-path mathpix-dir)
(require 'mathpix)
(map! "C-x m" #'mathpix-screenshot)

(setq      mathpix-screenshot-method "scrot -s %s")

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

(use-package! evil-visual-replace
  :init
  (evil-visual-replace-visual-bindings))

;; smartparens
(use-package smartparens
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

(use-package poporg
  :bind ((  "C-c '" . poporg-dwim)))

;; hl-todo-mode
(use-package hl-todo
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

(use-package hungry-delete
  :config
  (add-hook 'after-init-hook #'global-hungry-delete-mode))

(use-package company-posframe
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

(global-set-key (kbd "C-c C-\\") (quote comment-line))

(use-package writeroom-mode
  :hook
  (w3m-mode . writeroom-mode)
  :config
  (advice-add 'text-scale-adjust :after
              #'visual-fill-column-adjust)
  ;;https://github.com/joostkremers/writeroom-mode#fullscreen-effect
  (setq writeroom-fullscreen-effect 'maximized))

(use-package grugru
  :config (grugru-default-setup))

(use-package undo-fu
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

(use-package undo-fu-session
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

(use-package which-key
  :init
  (which-key-mode))

(custom-set-faces
 '(org-level-1 ((t (:inherit outline-1 :height 1.3))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.2))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.1)))))

(use-package org-superstar
  :config
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))
  (setq org-superstar-headline-bullets-list '("☰" "☷" "☵" "☲"  "☳" "☴"  "☶"  "☱" )))

(setq org-ellipsis " ··· ")

(setq org-hide-emphasis-markers t)

(use-package valign
  :init
  (require 'valign)
  :hook
  ('org-mode . #'valign-mode))

(map! :leader
      (:prefix "m"
       (:prefix-map ("m" . "modify")
        :desc "item or text"             "i" #'org-toggle-item
        :desc "heading or text"          "h" #'org-toggle-heading )))

(after! org
  (add-hook 'org-mode-hook (lambda () (evil-org-mode 1))))

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
            '("^\\(?:https?\\|ftp\\|file\\|nfs\\):" . org-download-dnd)
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
                                                      (downcase (file-name-base buffer-file-name))))))
      (make-directory dirname t)
      (expand-file-name filename dirname)))
  :config
  (setq org-download-screenshot-method
        (cond (IS-MAC "screencapture -i %s")
              (IS-LINUX
               (cond ((executable-find "maim")  "maim -u -s %s")
                     ((executable-find "scrot") "scrot -s %s")))))
  (setq org-download-method '+org/org-download-method))

(defun py/jump-to-inbox ()
  (interactive)
  (find-file py/inbox))

(defun py/jump-to-next()
  (interactive)
  (find-file py/next))

(map! :leader
      :desc "jump to inbox"             "n i" #'py/jump-to-inbox
      :desc "jump to next"              "n N" #'py/jump-to-next)

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
              ;;Metacognition
              ("Metacog"
               :keys "m"
               :prepend t
               :template ("* %{todo-state} %?"
                          ":PROPERTIES:"
                          ":INIT:       %U"
                          ":END:")
               :children (("MetaNotes"
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
                           :function (lambda () (jethro/olp-current-buffer "Metacog" "Todos")))))
              ("Bipolar"
               :keys "b"
               :prepend t
               :children (("服药情况"
                           :keys "1"
                           :type table-line
                           :file py/bipolar
                           :headline "服药情况"
                           :table-line-pos "I-1"
                           :template ("| %t | %^{碳酸锂正常服用?} | %^{拉莫三嗪正常服用?} | %^{盐酸普拉克索正常服用?} | %^{唑吡坦正常服用？} |"))
                          ("睡眠情况"
                           :keys "2"
                           :type table-line
                           :file py/bipolar
                           :headline "睡眠情况"
                           :table-line-pos "I-1"
                           :template ("| %t | %^{晚上睡了多久?} | %^{中午睡了多久?} |"))
                          ("急躁&焦虑情况"
                           :keys "3"
                           :type table-line
                           :file py/bipolar
                           :headline "急躁&焦虑情况"
                           :table-line-pos "I-1"
                           :template ("| %t | %^{备注} | %^{急躁等级 (1-10)} | %^{急躁内容} | %^{焦虑等级} | %^{焦虑内容} |"))
                          ("工作内容记录"
                           :keys "4"
                           :type table-line
                           :file py/bipolar
                           :headline "工作内容记录"
                           :table-line-pos "I-1"
                           :template ("| %t | %^{工作内容1} | %^{时长} | %^{工作内容2} | %^{时长} | %^{工作内容3} | %^{时长} | %^{工作内容4} | %^{时长} | "))
                          ("抑郁&躁狂记录"
                           :keys "5"
                           :type table-line
                           :file py/bipolar
                           :headline "抑郁&躁狂记录"
                           :table-line-pos "I-1"
                           :template ("| %t | %^{抑郁程度} | %^{躁狂程度} |"))
                          ("精神状态&幻觉&怪诞想法记录"
                           :keys "6"
                           :type table-line
                           :file py/bipolar
                           :headline "精神状态 幻觉 怪诞想法记录"
                           :table-line-pos "I-1"
                           :template ("| %t | %^{精神状态(随便说说)} | %^{幻觉?} | %^{怪诞想法} |"))
                          ("快乐事件记录"
                           :keys "7"
                           :type table-line
                           :file py/bipolar
                           :headline "快乐事件记录"
                           :table-line-pos "I-1"
                           :template ("| %t | %^{内容1} | %^{内容2} | %^{内容3} |")))))))

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
      (:prefix-map              ("I" . "id")
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

(use-package org-roam
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

(use-package org-roam-protocol
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

(use-package org-super-agenda
:config
(add-hook! 'after-init-hook 'org-super-agenda-mode)
(setq org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t
      org-agenda-include-deadlines t
      org-agenda-include-diary nil
      org-agenda-block-separator nil
      org-agenda-compact-blocks t
      org-agenda-start-with-log-mode t)
(setq org-columns-default-format
      "%40ITEM(Task) %Effort(EE){:} %CLOCKSUM(Time Spent) %SCHEDULED(Scheduled) %DEADLINE(Deadline)"))

(setq org-agenda-custom-commands
      `(("b" "BrainDump"
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
        ("p" "Pei's Agenda"
         ((alltodo "" ((org-agenda-overriding-header "To Refile")
                       (org-agenda-files '(,(expand-file-name py/inbox)))))
          (alltodo "" ((org-agenda-overriding-header "One-off-task" )
                       (org-agenda-files '(,(expand-file-name py/next )))))
          (agenda "" ((org-agenda-span 3)
                      (org-agenda-start-day "-1d")
                      (org-agenda-files (directory-files-recursively py/project-dir (rx ".org" eos)))
                      (org-super-agenda-groups
                       '((:name "Schedual"
                                :time-grid t
                                :date today
                                :scheduled today
                                :order 1)
                         (:name "Deadline"
                                :time-grid t
                                :date today
                                :deadline today
                                :order 2)))))
          ))))
