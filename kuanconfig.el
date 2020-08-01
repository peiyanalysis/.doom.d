;;; ~/.doom.d/kuanconfig.el -*- lexical-binding: t; -*-

;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-


(after! org
  require 'org-habit)
  (with-eval-after-load 'flycheck
    (flycheck-add-mode 'proselint 'org-mode))

  (map! :leader
        :prefix "n"
        "c" #'org-capture)
  (map! :map org-mode-map
        "M-n" #'outline-next-visible-heading
        "M-p" #'outline-previous-visible-heading)
  (setq org-src-window-setup 'current-window
        org-return-follows-link t
        org-babel-load-languages '((emacs-lisp . t)
                                   (python . t)
                                   (dot . t)
                                   (R . t))
        org-confirm-babel-evaluate nil
        org-use-speed-commands t
        org-catch-invisible-edits 'show
        org-preview-latex-image-directory "/tmp/ltximg/"
        org-structure-template-alist '(("a" . "export ascii")
                                       ("c" . "center")
                                       ("C" . "comment")
                                       ("e" . "example")
                                       ("E" . "export")
                                       ("h" . "export html")
                                       ("l" . "export latex")
                                       ("q" . "quote")
                                       ("s" . "src")
                                       ("v" . "verse")
                                       ("el" . "src emacs-lisp")
                                       ("d" . "definition")
                                       ("t" . "theorem")))

  (defun jethro/org-archive-done-tasks ()
    "Archive all done tasks."
    (interactive)
    (org-map-entries 'org-archive-subtree "/DONE" 'file))
  (require 'find-lisp)
  (setq jethro/org-agenda-directory "~/Dropbox/org/gtd/")
  (setq org-agenda-files
        (find-lisp-find-files jethro/org-agenda-directory "\.org$"))


(setq org-log-done 'time
      org-log-into-drawer t
      org-log-state-notes-insert-after-drawers nil)

(setq org-tag-alist '(("@errand" . ?e)
                      ("@office" . ?o)
                      ("@home" . ?h)
                      (:newline)
                      ("CANCELLED" . ?c)))

(setq org-fast-tag-selection-single-key nil)
(setq org-refile-use-outline-path 'file
      org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)
(setq org-refile-targets '(("next.org" :level . 0)
                           ("someday.org" :level . 0)
                           ("work.org" :level . 0)
                           ("reading.org" :level . 1)
                           ("projects.org" :maxlevel . 1)))

(defvar jethro/org-agenda-bulk-process-key ?f
  "Default key for bulk processing inbox items.")

(defun jethro/org-process-inbox ()
  "Called in org-agenda-mode, processes all inbox items."
  (interactive)
  (org-agenda-bulk-mark-regexp "inbox:")
  (jethro/bulk-process-entries))

(defvar jethro/org-current-effort "1:00"
  "Current effort for agenda items.")

(defun jethro/my-org-agenda-set-effort (effort)
  "Set the effort property for the current headline."
  (interactive
   (list (read-string (format "Effort [%s]: " jethro/org-current-effort) nil nil jethro/org-current-effort)))
  (setq jethro/org-current-effort effort)
  (org-agenda-check-no-diary)
  (let* ((hdmarker (or (org-get-at-bol 'org-hd-marker)
                       (org-agenda-error)))
         (buffer (marker-buffer hdmarker))
         (pos (marker-position hdmarker))
         (inhibit-read-only t)
         newhead)
    (org-with-remote-undo buffer
      (with-current-buffer buffer
        (widen)
        (goto-char pos)
        (org-show-context 'agenda)
        (funcall-interactively 'org-set-effort nil jethro/org-current-effort)
        (end-of-line 1)
        (setq newhead (org-get-heading)))
      (org-agenda-change-all-lines newhead hdmarker))))

(defun jethro/org-agenda-process-inbox-item ()
  "Process a single item in the org-agenda."
  (org-with-wide-buffer
   (org-agenda-set-tags)
   (org-agenda-priority)
   (call-interactively 'jethro/my-org-agenda-set-effort)
   (org-agenda-refile nil nil t)))

(defun jethro/bulk-process-entries ()
  (if (not (null org-agenda-bulk-marked-entries))
      (let ((entries (reverse org-agenda-bulk-marked-entries))
            (processed 0)
            (skipped 0))
        (dolist (e entries)
          (let ((pos (text-property-any (point-min) (point-max) 'org-hd-marker e)))
            (if (not pos)
                (progn (message "Skipping removed entry at %s" e)
                       (cl-incf skipped))
              (goto-char pos)
              (let (org-loop-over-headlines-in-active-region) (funcall 'jethro/org-agenda-process-inbox-item))
              ;; `post-command-hook' is not run yet.  We make sure any
              ;; pending log note is processed.
              (when (or (memq 'org-add-log-note (default-value 'post-command-hook))
                        (memq 'org-add-log-note post-command-hook))
                (org-add-log-note))
              (cl-incf processed))))
        (org-agenda-redo)
        (unless org-agenda-persistent-marks (org-agenda-bulk-unmark-all))
        (message "Acted on %d entries%s%s"
                 processed
                 (if (= skipped 0)
                     ""
                   (format ", skipped %d (disappeared before their turn)"
                           skipped))
                 (if (not org-agenda-persistent-marks) "" " (kept marked)")))))

(defun jethro/org-inbox-capture ()
  (interactive)
  "Capture a task in agenda mode."
  (org-capture nil "i"))

(setq org-agenda-bulk-custom-functions `((,jethro/org-agenda-bulk-process-key jethro/org-agenda-process-inbox-item)))

(map! :map org-agenda-mode-map
      "i" #'org-agenda-clock-in
      "r" #'jethro/org-process-inbox
      "R" #'org-agenda-refile
      "c" #'jethro/org-inbox-capture)

(defun jethro/set-todo-state-next ()
  "Visit each parent task and change NEXT states to TODO"
  (org-todo "NEXT"))

(add-hook 'org-clock-in-hook 'jethro/set-todo-state-next 'append)

(use-package! org-clock-convenience
  :bind (:map org-agenda-mode-map
              ("<S-up>" . org-clock-convenience-timestamp-up)
              ("<S-down>" . org-clock-convenience-timestamp-down)
              ("o" . org-clock-convenience-fill-gap)
              ("e" . org-clock-convenience-fill-gap-both)))

(use-package! org-agenda
  :init
  (map! "<f1>" #'jethro/switch-to-agenda)
  (setq org-agenda-block-separator nil
        org-agenda-start-with-log-mode t)
  (defun jethro/switch-to-agenda ()
    (interactive)
    (org-agenda nil " "))
  :config
  (setq org-columns-default-format "%40ITEM(Task) %Effort(EE){:} %CLOCKSUM(Time Spent) %SCHEDULED(Scheduled) %DEADLINE(Deadline)")
  (setq org-agenda-custom-commands `((" " "Agenda"
                                      ((agenda ""
                                               ((org-agenda-span 'week)
                                                (org-deadline-warning-days 365)))
                                       (todo "TODO"
                                             ((org-agenda-overriding-header "To Refile")
                                              (org-agenda-files '(,(concat jethro/org-agenda-directory "inbox.org")))))
                                       (todo "TODO"
                                             ((org-agenda-overriding-header "Emails")
                                              (org-agenda-files '(,(concat jethro/org-agenda-directory "emails.org")))))
                                       (todo "NEXT"
                                             ((org-agenda-overriding-header "In Progress")
                                              (org-agenda-files '(,(concat jethro/org-agenda-directory "someday.org")
                                                                  ,(concat jethro/org-agenda-directory "projects.org")
                                                                  ,(concat jethro/org-agenda-directory "next.org")))
                                              ))
                                       (todo "TODO"
                                             ((org-agenda-overriding-header "Projects")
                                              (org-agenda-files '(,(concat jethro/org-agenda-directory "projects.org")
                                                                  ,(concat jethro/org-agenda-directory "work.org")))))
                                       (todo "TODO"
                                             ((org-agenda-overriding-header "One-off Tasks")
                                              (org-agenda-files '(,(concat jethro/org-agenda-directory "next.org")))
                                              (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled)))))))))

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
  (setq org-roam-directory "/home/jethro/Dropbox/org/braindump/org/"
        org-roam-db-location "/home/jethro/org-roam.db"
        org-roam-db-gc-threshold most-positive-fixnum
        org-roam-graph-exclude-matcher "private"
        org-roam-tag-sources '(prop last-directory)
        org-id-link-to-org-use-id t)
  :config
  (setq org-roam-capture-templates
        '(("l" "lit" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "lit/${slug}"
           :head "#+setupfile:./hugo_setup.org
#+hugo_slug: ${slug}
#+title: ${title}\n"
           :unnarrowed t)
          ("c" "concept" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "concepts/${slug}"
           :head "#+setupfile:./hugo_setup.org
#+hugo_slug: ${slug}
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
           :head "#+setupfile:./hugo_setup.org
#+roam_key: ${ref}
#+hugo_slug: ${slug}
#+roam_tags: website
#+title: ${title}

- source :: ${ref}"
           :unnarrowed t))))

(use-package! org-roam-protocol
  :after org-protocol)

(after! company
  (map! "M-/" #'company-indent-or-complete-common))

(use-package! company-posframe
  :hook (company-mode . company-posframe-mode))

(use-package company-org-roam
  :when (featurep! :completion company)
  :after org-roam
  :config
  (set-company-backend! 'org-mode '(company-org-roam company-yasnippet company-dabbrev)))

(after! (org-roam)
  (winner-mode +1)
  (map! :map winner-mode-map
        "<M-right>" #'winner-redo
        "<M-left>" #'winner-undo))

(after! (org ox-hugo)
  (defun jethro/conditional-hugo-enable ()
    (save-excursion
      (if (cdr (assoc "SETUPFILE" (org-roam--extract-global-props '("SETUPFILE"))))
          (org-hugo-auto-export-mode +1)
        (org-hugo-auto-export-mode -1))))
  (add-hook 'org-mode-hook #'jethro/conditional-hugo-enable))

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

(after! org-journal
  (setq org-journal-date-prefix "#+TITLE: "
        org-journal-file-format "%Y-%m-%d.org"
        org-journal-dir "/home/jethro/Dropbox/org/braindump/org/private/"
        org-journal-carryover-items nil))

(use-package! citeproc-org
  :after org
  :config
  (citeproc-org-setup))

(use-package! mathpix.el
  :commands (mathpix-screenshot)
  :init
  (map! "C-x m" #'mathpix-screenshot)
  :config
  (setq mathpix-screenshot-method "maim -u -s %s"
        mathpix-app-id (password-store-get "mathpix/app-id")
        mathpix-app-key (password-store-get "mathpix/app-key")))

(use-package! anki-editor
  :commands (anki-editor-mode))

(use-package! gif-screencast
  :bind
  ("<f12>" . gif-screencast-start-or-stop))

(defun insert-date ()
  "Insert a timestamp according to locale's date and time format."
  (interactive)
  (insert (format-time-string "%c" (current-time))))

(use-package! outshine
  :commands (outshine-mode))

(after! ivy
  (map! :map ivy-minibuffer-map
        "S-SPC" nil)
  (add-to-list 'ivy-re-builders-alist '(counsel-projectile-find-file . ivy--regex-plus)))

(after! mixed-pitch
  (dolist (f (-filter (lambda (sym)
                        (s-prefix? "company-" (symbol-name sym)))
                      (face-list)))
    (pushnew! mixed-pitch-fixed-pitch-faces f)))

(use-package! org-roam-bibtex
  :after (org-roam)
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :config
  (setq org-roam-bibtex-preformat-keywords
   '("=key=" "title" "url" "file" "author-or-editor" "keywords"))
  (setq orb-templates
        `(("r" "ref" plain (function org-roam-capture--get-point)
           ""
           :file-name "lit/${slug}"
           :head ,(concat
                   "#+setupfile: ./hugo_setup.org\n"
                   "#+title: ${=key=}: ${title}\n"
                   "#+roam_key: ${ref}\n\n"
                   "* ${title}\n"
                   "  :PROPERTIES:\n"
                   "  :Custom_ID: ${=key=}\n"
                   "  :URL: ${url}\n"
                   "  :AUTHOR: ${author-or-editor}\n"
                   "  :NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n"
                   "  :NOTER_PAGE: \n"
                   "  :END:\n")
           :unnarrowed t))))

(use-package! bibtex-completion
  :config
  (setq bibtex-completion-notes-path "~/Dropbox/org/braindump/org/"
        bibtex-completion-bibliography "~/Dropbox/org/braindump/org/biblio.bib"
        bibtex-completion-pdf-field "file"
        bibtex-completion-notes-template-multiple-files
         (concat
          "#+title: ${title}\n"
          "#+roam_key: cite:${=key=}\n"
          "* TODO Notes\n"
          ":PROPERTIES:\n"
          ":Custom_ID: ${=key=}\n"
          ":NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n"
          ":AUTHOR: ${author-abbrev}\n"
          ":JOURNAL: ${journaltitle}\n"
          ":DATE: ${date}\n"
          ":YEAR: ${year}\n"
          ":DOI: ${doi}\n"
          ":URL: ${url}\n"
          ":END:\n\n"
          )))

(defun jethro/open-with (arg)
  "Open visited file in default external program.
When in dired mode, open file under the cursor.
With a prefix ARG always prompt for command to use."
  (interactive "P")
  (let* ((current-file-name
          (if (eq major-mode 'dired-mode)
              (dired-get-file-for-visit)
            buffer-file-name))
         (open (pcase system-type
                 (`darwin "open")
                 ((or `gnu `gnu/linux `gnu/kfreebsd) "xdg-open")))
         (program (if (or arg (not open))
                      (read-shell-command "Open current file with: ")
                    open)))
    (call-process program nil 0 nil current-file-name)))

(map! "C-c o o" 'jethro/open-with)


(defun jethro/ledger-cleanup-csv ()
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "\n" nil t)
      (beginning-of-line)
      (delete-horizontal-space)
      (end-of-line)
      (delete-horizontal-space)
      (when (string-equal (buffer-substring-no-properties (line-beginning-position) (line-end-position))
                          "")
        (delete-char -1)))))
