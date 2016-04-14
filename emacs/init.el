
(let ((default-directory "~/cfg/emacs/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

(require 'prelude-packages)

(require 'use-package)
(setq use-package-verbose t)

(defun my/load-rc (name)
  (let ((rc-name (concat "rc-" name)))
    (load (concat "~/emacs/" rc-name))))

(defun my/local-move-key (from to)
  (let ((to-function (local-key-binding to)))
    (local-set-key to (local-key-binding from))
    to-function))

(defun my/local-swap-keys (l r)
  (local-set-key r (my/local-move-key l r)))

(defun my/global-move-key (from to)
  (let ((to-function (global-key-binding to)))
    (global-set-key to (global-key-binding from))
    to-function))

(defun my/global-swap-keys (l r)
  (global-set-key r (my/global-move-key l r)))

(use-package diminish)

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  ;; rename after killing uniquified
  (setq uniquify-after-kill-buffer-p t)
  ;; don't muck with special buffers
  (setq uniquify-ignore-buffers-re "^\\*"))

(use-package volatile-highlights
  :config
  (volatile-highlights-mode t)
  :diminish volatile-highlights-mode)

;;; Backup settings
(setq backup-by-copying t               ; don't clobber symlinks
      backup-directory-alist
      '(("." . "~/.backups"))           ; don't litter my fs tree
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)                ; use versioned backups

(setq inhibit-startup-message   t)   ; Don't want any startup message
(setq auto-save-list-file-name  nil) ; Don't want any .saves files
(setq auto-save-default         nil) ; Don't want any auto saving

(blink-cursor-mode -1)
(setq-default cursor-type 'box)

(setq search-highlight           t) ; Highlight search object
(setq query-replace-highlight    t) ; Highlight query object
(setq mouse-sel-retain-highlight t) ; Keep mouse high-lightening

(global-font-lock-mode t)

(setq show-trailing-whitespace t)

(transient-mark-mode t)         ; make the current 'selection' visible
(delete-selection-mode t)       ; delete the selection area with a keypress
(line-number-mode t)
(column-number-mode t)

(scroll-bar-mode -1)
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(setq redisplay-dont-pause t)

(ac-config-default)

(use-package saveplace
  :init
  (setq-default save-place t)
  :config
  (setq save-place-file (expand-file-name "saveplace" user-emacs-directory)))

(my/load-rc "ido")

;;
;; ibuffer
;;
(require 'ibuffer)
(setq ibuffer-saved-filter-groups
      (quote (("default"
               ))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))

(global-set-key (kbd "C-x C-b") 'ibuffer)

(setq scroll-step 1)
(setq default-tab-width 4)
(global-hl-line-mode -1)
(windmove-default-keybindings 'meta)
(desktop-save-mode t)
(fset 'yes-or-no-p 'y-or-n-p)

;;
;; Setup codepage under Windows
;;
(set-language-environment 'UTF-8)
(setq default-input-method 'russian-computer)
(set-selection-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)

(my/load-rc "prog-common")

(global-set-key (kbd "M-]") 'next-error)
(global-set-key (kbd "M-[") 'previous-error)

(use-package compile
  :config
  (setq compilation-disable-input nil)
  (setq compilation-scroll-output 'first-error)
  (setq mode-compile-always-save-buffer-p t)
  (global-set-key [f9] 'compile))

(put 'narrow-to-region 'disabled nil)

(require 'helm)
(require 'helm-config)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z") 'helm-select-action) ; list actions using C-z

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

;; open helm buffer inside current window, not occupy whole other window
(setq helm-split-window-in-side-p t)
;; move to end or beginning of source when reaching top or bottom of source.
(setq helm-move-to-line-cycle-in-source t)
;; search for library in `require' and `declare-function' sexp.
(setq helm-ff-search-library-in-sexp t)
;; scroll 8 lines other window using M-<next>/M-<prior>
(setq helm-scroll-amount 8)
(setq helm-ff-file-name-history-use-recentf t)

(helm-mode 1)

(projectile-global-mode)
(helm-projectile-on)
(setq projectile-switch-project-action 'projectile-dired)
(setq projectile-completion-system 'helm)
(setq projectile-enable-caching t)
(setq projectile-mode-line '(:eval (format " P[%s]" (projectile-project-name))))

(global-set-key (kbd "C-S-n") 'helm-projectile-find-file)
(global-set-key (kbd "C-S-o") 'helm-imenu)
(global-set-key (kbd "C-S-g") 'helm-occur)
(global-set-key (kbd "C-S-f") 'projectile-ag)
(global-set-key (kbd "C-S-h") 'helm-projectile-ag)
(global-set-key (kbd "C-S-p") 'helm-projectile-switch-project)
(global-set-key (kbd "C-M-g") 'grunt-exec)

(use-package golden-ratio
  :ensure t
  :diminish golden-ratio-mode
  :config
  (golden-ratio-mode 1)
  (setq golden-ratio-exclude-modes '("ediff-mode"
                                     "eshell-mode"))
  (add-to-list 'golden-ratio-exclude-buffer-names "*Help*")
  (add-to-list 'golden-ratio-exclude-buffer-names "*Flycheck errors*")
  (add-to-list 'golden-ratio-exclude-buffer-names "*Occur*")
  (add-to-list 'golden-ratio-exclude-buffer-names "*compilation*")
  (add-to-list 'golden-ratio-exclude-buffer-names "*jedi:doc*")
  (add-to-list 'golden-ratio-exclude-buffer-names "*helm jedi:related-names*")
  (add-to-list 'golden-ratio-exclude-buffer-names "*helm projectile*")
  (add-to-list 'golden-ratio-exclude-buffer-names "*helm-ag*")
  (add-to-list 'golden-ratio-exclude-buffer-names "*helm grep*")
  (add-to-list 'golden-ratio-exclude-buffer-names "*helm imenu*")
  (add-to-list 'golden-ratio-exclude-buffer-names "*helm M-x*")
  (add-to-list 'golden-ratio-exclude-buffer-names "*grep*")
  (add-to-list 'golden-ratio-exclude-buffer-names "*helm kill ring*")

  (add-to-list 'golden-ratio-exclude-buffer-regexp "*ag search"))

(setq split-width-threshold nil)

(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

(defun my/scroll-other-window-up ()
  (interactive)
  (scroll-other-window -1))
(defun my/scroll-other-window-down ()
  (interactive)
  (scroll-other-window 1))
(global-set-key (kbd "M-P") 'my/scroll-other-window-up)
(global-set-key (kbd "M-N") 'my/scroll-other-window-down)

(global-set-key (kbd "C-;") 'comment-or-uncomment-region)

(my/load-rc "kstation")

(use-package nginx-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("/etc/nginx/sites-available/.*" . nginx-mode))
  (add-to-list 'auto-mode-alist '("nginx.conf" . nginx-mode))
  (add-to-list 'auto-mode-alist '("nginx\.conf$"  . nginx-mode))
  (add-to-list 'auto-mode-alist '("/etc/nginx/.*" . nginx-mode)))

(setq show-trailing-whitespace t)

(electric-pair-mode 1)

(global-set-key (kbd "C-c j") 'just-one-space)
(global-set-key (kbd "C-x O") (lambda ()
                                (interactive)
                                (other-window -1)))

(defun my/sh-mode-hook ()
  (setq sh-basic-offset 2
        sh-indentation 2))
(add-hook 'sh-mode-hook 'my/sh-mode-hook)

(use-package crux
  :ensure t
  :bind (("C-c o" . crux-open-with)
         ("M-o" . crux-smart-open-line)
         ("M-O" . crux-smart-open-line-above)
         ([remap kill-whole-line] . crux-kill-whole-line)
         ([(shift return)] . crux-smart-open-line)
         ("C-c d" . crux-delete-file-and-buffer)
         ("C-c r" . crux-rename-buffer-and-file)
         ("C-c I" . crux-find-user-init-file)
         ("C-a" . crux-move-beginning-of-line)
         ("C-^" . crux-top-join-line)))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (local-set-key (kbd "<f5>") 'eval-buffer)))

(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.json?\\'" . web-mode))
  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-enable-current-element-highlight nil)
  (setq web-mode-enable-current-column-highlight t)
  (web-mode-set-engine "angular"))

(global-set-key (kbd "C-x C-v") 'find-alternate-file)

(use-package dired
  :config
  ;; dired - reuse current buffer by pressing 'a'
  (put 'dired-find-alternate-file 'disabled nil)

  ;; always delete and copy recursively
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)

  ;; if there is a dired buffer displayed in the next window, use its
  ;; current subdir, instead of the current subdir of this dired buffer
  (setq dired-dwim-target t)

  (require 'dired-x)

  (toggle-diredp-find-file-reuse-dir 1)

  (defun my/dired-sort ()
    "Sort dired listings with directories first."
    (save-excursion
      (let (buffer-read-only)
        (forward-line 2) ;; beyond dir. header
        (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max)))
      (set-buffer-modified-p nil)))

  (defadvice dired-readin
    (after dired-after-updating-hook first () activate)
    "Sort dired listings with directories first before adding marks."
    (my/dired-sort)))

(use-package markdown-mode
  :ensure t
  :config
  (add-hook 'markdown-mode-hook 'whitespace-mode)
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))

(use-package yaml-mode
  :ensure t)

(use-package flycheck
  :ensure t
  :config
  ;; (flycheck-pos-tip-mode)
  (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (defadvice flycheck-list-errors (around my/flycheck-list-errors activate)
    (interactive)
    (let ((window (get-buffer-window "*Flycheck errors*")))
      (if window
          (delete-window window)
        (call-interactively (ad-get-orig-definition 'flycheck-list-errors))))))

(use-package flycheck-checkbashisms
  :ensure t
  :config
  (flycheck-checkbashisms-setup))

(use-package flyspell
  :config
  (setq ispell-program-name "aspell"
        ispell-extra-args '("--sug-mode=ultra"))
  (add-hook 'text-mode-hook #'flyspell-mode)
  (add-hook 'prog-mode #'flyspell-prog-mode))

(defun buffer-on-bottom-side (&rest res)
  (dolist (re res)
    (add-to-list 'display-buffer-alist
                 `(,re
                   (display-buffer-reuse-window
                    display-buffer-in-side-window)
                   (reusable-frames . visible)
                   (side            . bottom)
                   (window-height   . 0.3)))))

(buffer-on-bottom-side "^\\*Flycheck errors\\*$"
                       "^\\*Compilation\\*$"
                       "^\\*Occur\\*$"
                       "^\\*Help\\*$"
                       "^\\*jedi:doc\\*$"
                       "^\\*helm jedi:related-names\\*$"
                       "^\\*helm-ag\\*$"
                       "^\\*helm grep\\*$"
                       ;;"\\`\\*helm.*?\\*\\'"
                       "^\\*Warnings\\*$"
                       "^\\*grep\\*$"
                       "^\\*ag search"
                       "^\\*Compile-Log\\*$")

(defun my/quit-bottom-side-windows ()
  "Quit bottom side windows of the current frame."
  (interactive)
  (dolist (window (window-at-side-list nil 'bottom))
    (if (eq (window-parameter window 'window-side) 'bottom)
        (quit-window 'kill window))))

(global-set-key (kbd "s-q") 'my/quit-bottom-side-windows)
(global-set-key (kbd "s-k") 'kill-this-buffer)

(use-package avy
  :ensure t
  :bind ("s-." . avy-goto-word-or-subword-1)
  :config
  (add-to-list 'golden-ratio-extra-commands 'avy-goto-word-or-subword-1))

(use-package ace-window
  :ensure t
  :bind ("s-o" . ace-window)
  :config
  (add-to-list 'golden-ratio-extra-commands 'ace-window))

(defun my/new-empty-buffer ()
  "Open a new empty buffer."
  (interactive)
  (let ((buf (generate-new-buffer "untitled")))
    (switch-to-buffer buf)
    (funcall (and initial-major-mode))
    (setq buffer-offer-save t)))

(global-set-key (kbd "<f7>") 'my/new-empty-buffer)

(setq python-environment-directory "~/.virtualenvs")

(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)
(setq jedi:use-shortcuts t)

(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))

(color-theme-initialize)
(color-theme-sitaramv-nt)

(add-hook 'window-setup-hook
          '(lambda () (set-cursor-color "black")))

(use-package anzu
  :ensure t
  :diminish anzu-mode
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp))
  :config
  (global-anzu-mode +1))

(use-package js2-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (add-to-list 'auto-mode-alist '("\\.tern-project$" . js-mode))
  (add-to-list 'auto-mode-alist '("\\.tern-config$" . js-mode))

  (add-hook 'js2-mode-hook 'ac-js2-mode)
  (add-hook 'js2-mode-hook #'js2-refactor-mode)

  (setq js2-highlight-level 2)
  (setq js2-strict-inconsistent-return-warning nil)
  (setq js2-strict-trailing-comma-warning nil)

  (setq js-indent-level 4)
  (setq-default js2-basic-offset 4)

  (aset js2-kwd-tokens js2-THIS 'font-lock-keyword-face)
  (aset js2-kwd-tokens js2-SUPER 'font-lock-keyword-face)
  (aset js2-kwd-tokens js2-VOID 'font-lock-keyword-face)
  (aset js2-kwd-tokens js2-NULL 'font-lock-keyword-face)
  (aset js2-kwd-tokens js2-TRUE 'font-lock-keyword-face)
  (aset js2-kwd-tokens js2-FALSE 'font-lock-keyword-face)

  (eval-after-load 'tern
    '(progn
       (require 'tern-auto-complete)
       (tern-ac-setup)))

  (add-hook 'js2-mode-hook (lambda ()
                             (tern-mode t)
                             (js2-imenu-extras-mode t)
                             (local-set-key (kbd "M-.") 'tern-find-definition)))

  (defadvice js2-record-name-node
    (after my/js2-record-name-node first () activate)
    (let ((node (ad-get-arg 0))
          (leftpos (js2-node-abs-pos node)))
      (js2-set-face leftpos
                    (+ leftpos (js2-node-len node))
                    'font-lock-variable-name-face
                    'record)))

  (setq coffee-tab-width 2))

(use-package haskell-mode
  :ensure t
  :config
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (setq haskell-process-suggest-remove-import-lines t)
  (setq haskell-process-auto-import-loaded-modules t)
  (setq haskell-process-log t)
  (setq haskell-process-show-debug-tips nil)
  (setq haskell-stylish-on-save t)
  (setq haskell-interactive-popups-error nil)
  (buffer-on-bottom-side "^\\*haskell\\*$")
  (add-to-list 'golden-ratio-exclude-buffer-names "*haskell*"))

(defun my/c++-mode-hook ()
  (setq tab-width 4)
  (define-key c++-mode-map "\C-m" 'reindent-then-newline-and-indent)
  (define-key c++-mode-map "\C-ce" 'c-comment-edit)
  (setq c++-auto-hungry-initial-state 'none)
  (setq c++-delete-function 'backward-delete-char)
  (setq c++-tab-always-indent t)
  (setq c-indent-level 4)
  (setq c-basic-offset 4)
  (setq c-default-style "linux")
  (setq c-indentation-style "linux")
  (setq c-continued-statement-offset 4)
  (setq c++-empty-arglist-indent 4))

(defun my/c-mode-common-hook ()
  (define-key c-mode-base-map (kbd "M-o") 'eassist-switch-h-cpp)
  (define-key c-mode-base-map (kbd "M-m") 'eassist-list-methods))

(add-hook 'c-mode-common-hook 'my/c-mode-common-hook)
(add-hook 'c++-mode-hook 'my/c++-mode-hook)

(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line
         number input"
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (goto-line (read-number "Goto line: ")))
    (linum-mode -1)))

(global-set-key [remap goto-line] 'goto-line-with-feedback)

(use-package ess
  :ensure t
  :config
  )

(add-hook 'after-init-hook 'server-start t)

(use-package tex-site
  :defer
  :config
  (setq preview-image-type 'pnm)

  (defun TeX-insert-macro-verb ()
    (interactive)
    (TeX-insert-macro "verb"))

  (defun TeX-insert-macro-lstinline ()
    (interactive)
    (TeX-insert-macro "lstinline"))

  (defun my/TeX-mode-hook ()
    (local-set-key (kbd "C-c C-g C-v") 'TeX-insert-macro-verb)
    (local-set-key (kbd "C-c C-g C-l") 'TeX-insert-macro-lstinline))

  (add-hook 'TeX-mode-hook 'my/TeX-mode-hook))

(use-package reftex
  :diminish reftex-mode
  :config
  (add-hook 'latex-mode-hook 'turn-on-reftex)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)

  (setq reftex-extra-bindings t)
  (setq reftex-enable-partial-scans t)
  (setq reftex-save-parse-info t)
  (setq reftex-use-multiple-selection-buffers t)

  (defun my/reftex-mode-hook ()
    (local-set-key (kbd "C-c C-y h") 'reftex-change-label)
    (local-set-key (kbd "C-c C-y g") 'reftex-goto-label))

  (add-hook 'reftex-mode-hook 'my/reftex-mode-hook))

(use-package tramp
  :config
  (setq tramp-default-method "ssh"))

(setq custom-file "~/emacs/custom.el")
(load custom-file t)
