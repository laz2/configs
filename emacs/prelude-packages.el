
(require 'cl)

(setq package-enable-at-startup nil)
(package-initialize)

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")))

(defvar prelude-packages
  '(
    dired+
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
    golden-ratio
    yasnippet
    s
    markdown-mode
    js2-mode ac-js2 js2-refactor tern tern-auto-complete
    coffee-mode
    nginx-mode
    python-mode py-autopep8 jedi
    cmake-mode
    web-mode
    stylus-mode
    yaml-mode
    json-snatcher
    ;;super-save
    haskell-mode
    crux
    seq
    use-package
    go-mode
    anzu
    flycheck flycheck-pos-tip flycheck-color-mode-line
    flycheck-checkbashisms
    dockerfile-mode
    nsis-mode
    )
  "A list of packages to ensure are installed at launch.")

(package-refresh-contents)
(dolist (p prelude-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(provide 'prelude-packages)
;;; prelude-packages.el ends here
