
(require 'cl)

(setq package-enable-at-startup nil)
(package-initialize)

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")))

(defvar prelude-packages
  '(dired+
    auctex
    paredit
    auto-complete
    color-theme
    desktop
    multi-term
    idomenu
    zenburn-theme
    helm
    helm-projectile
    helm-ag
    projectile
    project-explorer
    ace-jump-mode
    ace-jump-buffer
    ggtags
    golden-ratio
    yasnippet
    s
    markdown-mode
    js2-mode
    coffee-mode
    nginx-mode
    python-mode
    cmake-mode
    web-mode
    yaml-mode
    tern tern-auto-complete
    json-reformat json-mode json-snatcher
    flycheck
    py-autopep8)
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
