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

;; Font settings
;; :ID:       f09244df-4c0e-4c93-861a-c648265d284f
(load! "~/.doom.d/+ui/font.el")


;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "Sarasa Nerd" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "Sarasa Nerd Font" :size 15))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Dropbox/org/"
      org-ellipsis " ▼ "
      org-adapt-indentation nil)

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
;;
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
;;  Smart input_method switch
(use-package! sis                       ; :ID:       f09244df-4c0e-4c93-861a-c648265d284f
:config
  (sis-ism-lazyman-config nil nil 'fcitx5)
  ;; enable the /cursor color/ mode
  (sis-global-cursor-color-mode t)
  ;; enable the /respect/ mode
  (sis-global-respect-mode t)
  ;; enable the /follow context/ mode for all buffers
  (sis-global-follow-context-mode t)
  ;; enable the /inline english/ mode for all buffers
  (sis-global-inline-mode t)
  )
;;
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
(use-package! magic-latex-buffer
  :commands magic-latex-buffer
  :delight magic-latex-buffer
  :ghook ('LaTeX-mode-hook #'magic-latex-buffer)
  :init
  (progn
    (setq magic-latex-enable-block-highlight t ;; Prettify blocks that change their font size
           magic-latex-enable-suscript t        ;; Prettify sub and super script blocks
           magic-latex-enable-pretty-symbols t  ;; Convert latex variables into their UTF8 symbol
           magic-latex-enable-block-align nil   ;; Don't make \centering blocks appear centered in the LaTeX buffer
           magic-latex-enable-inline-image t))) ;; Display images inline in the LaTeX document
;;
;; ace pinyin
(use-package! ace-pinyin
  :after evil
  :config
  (setq avy-all-windows t)
  (ace-pinyin-global-mode t))
;;
;; evil-find-char-pinyin
(use-package! evil-find-char-pinyin
  :after evil
  :config
  (setq avy-all-windows t)
  (evil-find-char-pinyin-mode t))
;;
;; nov-mode
(use-package! nov
  :hook (nov-mode . variable-pitch-mode)
  :mode ("\\.\\(epub\\|mobi\\)\\'" . nov-mode))
;;
;; agenda
(require 'find-lisp)
(setq org-agenda-files '("~/Dropbox/org/gtd/inbox.org" "~/Dropbox/org/gtd/projects.org" "~/Dropbox/org/gtd/next.org" "~/Dropbox/org/gtd/test.org"))
(setq pei/org-gtd-directory "~/Dropbox/org/gtd/")
;;

;; ;; todo-keywords
;; (after! org
;;   (setq org-todo-keywords
;;         '((sequence "TODO(t)" "NEXT(n)" "WAIT(w)" "HOLD(h)" "|" "DONE(d)" "KILL(k)" "STUCK(s)")))
;;   (setq org-todo-keyword-faces '(("TODO" . "red") ("NEXT" . "yellow") ("HOLD" . "orange")
;;                                  ("DONE" . "green") ("KILL" . "grey") ("STUCK". "blue")))
;;   )
;; ;; Capture-Refile
;; (after! org 
;;   (setq org-capture-templates
;;         `(("i" "inbox" entry (file ,(concat pei/org-gtd-directory "inbox.org"))
;;            "* TODO %?")
;;           ;; ("e" "email" entry (file+headline ,(concat pei/org-agenda-directory "emails.org") "Emails")
;;           ;;      "* TODO [#A] Reply: %a :@home:@office:"
;;           ;;      :immediate-finish t)
;;           ("c" "org-protocol-capture" entry (file ,(concat pei/org-gtd-directory "inbox.org"))
;;                "* TODO [[%:link][%:description]]\n\n %i"
;;                :immediate-finish t))))
;; ;; 
;; ;; refile
;; (after! org
;;   (setq org-refile-targets '(("next.org" :level . 0)
;;                              ("someday.org":level . 2)
;;                              ("tickler.org":level . 0)
;;                              ("projects.org":level . 2))))
;; ;;
;; agenda ui, time grid
;; (after! org
;;  (setq org-agenda-time-grid (quote ((daily today require-timed)
;;                                     (300 600 900 1200 1500 1800 2100 2400)
;;                                    "......" "-----------------------------------------------------"))))
;; ;;
;; ;;agenda ui, column view
;;  (after! org
;;    (setq org-columns-default-format "%25ITEM %TODO %3PRIORITY %TAGS %Effort(EE){:} %CLOCKSUM(Time Spent) %SCHEDULED(Scheduled) %DEADLINE(Deadline)"))

  (let ((org-super-agenda-groups
       '((:auto-group t))))
  (org-agenda-list))
