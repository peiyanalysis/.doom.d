;;; ~/.doom.d/+ui/font.el -*- lexical-binding: t; -*-

(setq doom-font
      (font-spec :family "Sarasa Nerd Font" :size 18 :weight 'normal))

(after! doom-big-font-mode
  (setq doom-font
        (font-spec :family "Sarasa Nerd Font")))

;; https://blog.csdn.net/xh_acmagic/article/details/78939246
(defun +my/better-font()
  (interactive)
  ;; english font
  (if (display-graphic-p)
      (progn
        (set-face-attribute 'default nil :font (format   "%s:pixelsize=%d" "Sarasa Nerd Font" 18)) ;; 11 13 17 19 23
        ;; chinese font
        (dolist (charset '(kana han symbol cjk-misc bopomofo))
          (set-fontset-font (frame-parameter nil 'font)
                            charset
                            (font-spec :family "Sarasa Nerd Font")))) ;; 14 16 20 22 28
    ))

(defun +my|init-font(frame)
  (with-selected-frame frame
    (if (display-graphic-p)
        (+my/better-font))))

(if (and (fboundp 'daemonp) (daemonp))
    (add-hook 'after-make-frame-functions #'+my|init-font)
  (+my/better-font))
