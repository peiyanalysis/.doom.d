(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("f7216d3573e1bd2a2b47a2331f368b45e7b5182ddbe396d02b964b1ea5c5dc27" default))
 '(elfeed-feeds
   '("http://rss.sciencedirect.com/publication/science/00221236" "http://epubs.siam.org/rss/SJMAAH.xml" "http://arxiv.org/rss/math.AP" "http://rss.sciencedirect.com/publication/science/00220396" "http://link.springer.com/search.rss?facet-content-type=Article&facet-journal-id=222&channel-name=Inventiones%20mathematicae" "http://www.worldscientific.com/action/showFeed?type=etoc&feed=rss&jc=m3as" "http://rss.sciencedirect.com/publication/science/02529602" "http://rss.sciencedirect.com/publication/science/02941449" "http://link.springer.com/search.rss?facet-content-type=Article&facet-journal-id=526&channel-name=Calculus+of+Variations+and+Partial+Differential+Equations" "http://www.tandfonline.com/action/showFeed?type=etoc&feed=rss&jc=lpde20" "https://projecteuclid.org/feeds/euclid.ade_article_rss.xml" "https://link.springer.com/search.rss?facet-content-type=Article&facet-journal-id=205&channel-name=Archive+for+Rational+Mechanics+and+Analysis" "https://www.ams.org/rss/jams.rss" "https://projecteuclid.org/feeds/euclid.apde_article_rss.xml" "https://www.aimsciences.org/rss/1531-3492_current.xml" "https://aimsciences.org/rss/1078-0947_current.xml"))
 '(org-roam-index-file "~/.org/index.org")
 '(safe-local-variable-values
   '((org-refile-targets)
     (org-refile-targets list
                         (concat org-agenda-directory "next.org"))
     (org-refile-targets . "~/.org/agenda/next.org")
     (org-refile-targets
      (concat org-agenda-directory "next.org")
      :level . 1)
     (org-refile-targets format "%s"
                         (concat org-agenda-directory "next.org"))
     (org-refile-targets . "haha")
     (org-refile-targets . "inbox.org")
     (org-refile-targets . "~/.org/agenda/inbox.org")
     (org-refile-targets concat org-agenda-directory "inbox.org")
     (org-refile-targets concat org-agenda-directory "next.org")
     (org-refile-targets
      (org-agenda-files :maxlevel . 4)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-background-face ((t nil)))
 '(aw-leading-char-face ((t (:foreground "magenta" :box (:line-width 2 :color "magenta" :style released-button) :height 4.0)))))
