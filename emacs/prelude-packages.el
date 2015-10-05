
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
    color-theme zenburn-theme
    desktop
    multi-term
    idomenu
    ag
    helm helm-ag
    projectile helm-projectile
    avy
    ggtags
    golden-ratio
    yasnippet
    s
    markdown-mode
    js2-mode ac-js2 tern tern-auto-complete
    coffee-mode
    nginx-mode
    python-mode py-autopep8 jedi
    cmake-mode
    web-mode
    stylus-mode
    yaml-mode
    json-reformat json-mode json-snatcher
    erlang
    flycheck)
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
