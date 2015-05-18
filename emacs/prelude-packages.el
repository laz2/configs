
(require 'cl)

(setq package-enable-at-startup nil)
(package-initialize)

(setq package-archives
      '(("melpa" . "http://melpa.org/packages/")))

(defvar prelude-packages
  '(auctex paredit auto-complete cmake-mode color-theme desktop
           multi-term idomenu markdown-mode zenburn-theme
           js2-mode coffee-mode
           helm helm-projectile helm-ag projectile
           project-explorer ace-jump-mode ace-jump-buffer
           nginx-mode
           ggtags
           golden-ratio
           python-mode
           s)
  "A list of packages to ensure are installed at launch.")

(defun prelude-packages-installed-p ()
  (loop for p in prelude-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(defun prelude-packages-install ()
  (interactive)
  (unless (prelude-packages-installed-p)
    ;; check for new packages (package versions)
    (message "%s" "Emacs Prelude is now refreshing its package database...")
    (package-refresh-contents)
    (message "%s" " done.")
    ;; install the missing packages
    (dolist (p prelude-packages)
      (when (not (package-installed-p p))
        (package-install p)))))

(provide 'prelude-packages)
;;; prelude-packages.el ends here
