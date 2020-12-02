;;; org-gtd.el -*- lexical-binding: t; -*-
;; org-mode files
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

;; Get-things-done
;; initial documents
;; (after! org
;;   (setq gtd-file-inbox "~/Dropbox/.org/inbox.org")
;;   (setq gtd-file-trash "~/Dropbox/.org/trash.org")
;;   (setq gtd-file-maybe/someday "~/Dropbox/.org/maybe_someday.org")
;;   (setq gtd-file-todolist "~/Dropbox/.org/todolist.org")
;;   (setq org-agenda-files '("~/Dropbox/.org/inbox.org" "~/Dropbox/.org/maybe_someday.org" "~/Dropbox/.org/todolist.org")))

;; capture
;; (after! org
;;   (add-to-list 'org-capture-templates `("i" "inbox" entry (file gtd-file-inbox) ,(concat "* TODO %?\n"))))


;; keyword
;; (after! org
;;         (setq org-todo-keywords
;;               '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
;;                 (sequence "HOLD(h@/!)" "STUCKED(s@/!)" "WAITING(w@/!)" "MAYBE/SOMEDAY(m@/!)")
;;                 (sequence "CANCELED(c@/!)")))
;;         (setq org-todo-keyword-faces
;;               '(("TODO"            . (:foreground "palevioletred" :weight bold))
;;                 ("NEXT"            . (:foreground "peru"          :weight bold))
;;                 ("DONE"            . (:foreground "goldenrod"     :weight bold))
;;                 ("HOLD"            . (:foreground "gray"           :weight bold))
;;                 ("WAITING"         . (:foreground "goldenrod"     :weight bold))
;;                 ("STUDCKED"        . (:foreground "red"           :weight bold))
;;                 ("MAYBE/SOMEDAY"   . (:foreground "cadetblue"     :weight bold))
;;                 ("DONE"            . (:foreground "limegreen"     :weight bold))
;;                 ("CANCELLED"       . (:foreground "darkgray"      :weight bold))))
;;      ;; ("TODO"      . (:foreground "palevioletred" :weight bold))
;;      ;; ("STARTED"   . (:foreground "peru"          :weight bold))
;;      ;; ("WAITING"   . (:foreground "goldenrod"     :weight bold))
;;      ;; ("NEXT"      . (:foreground "darksalmon"    :weight bold))
;;      ;; ("POSTPONED" . (:foreground "rosybrown"     :weight bold))
;;      ;; ("SOMEDAY"   . (:foreground "cadetblue"     :weight bold))
;;      ;; ("TOLEARN"   . (:foreground "lightcoral"    :weight bold))
;;      ;; ("TOREVIEW"  . (:foreground "sandybrown"    :weight bold))
;;      ;; ("DONE"      . (:foreground "limegreen"     :weight bold))
;;      ;; ("CANCELLED" . (:foreground "darkgray"      :weight bold))
;; )
