;;; Add my emacs directory to load-path
;;; and all its subdirs
(let ((default-directory "~/cfg/emacs/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

(require 'prelude-packages)

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

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;;(server-start)

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

(blink-cursor-mode t)                ; Enable cursor from blinking
(setq-default cursor-type 'bar)

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
(tool-bar-mode -1)

(setq redisplay-dont-pause t)

(ac-config-default)

;;
;; saveplace
;;
(require 'saveplace)
(setq save-place-file "~/.emacs.d/saveplace")
(setq-default save-place t)

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
(my/load-rc "rst")
(my/load-rc "js")
(my/load-rc "markdown")

;;
;; Compile
;;
(require 'compile)
(setq compilation-disable-input nil)
(setq compilation-scroll-output t)
(setq mode-compile-always-save-buffer-p t)

(global-set-key [f9] 'compile)

(put 'narrow-to-region 'disabled nil)

(my/load-rc "tex")

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
(global-set-key (kbd "C-<f8>") 'project-explorer-toggle)
(global-set-key (kbd "<f8>") 'project-explorer-open)
(global-set-key (kbd "C-M-g") 'grunt-exec)

(setq pe/directory-tree-function 'pe/get-directory-tree-external)
;;(setq pe/cache-enabled nil)
(setq pe/omit-enabled nil)
;; (setq pe/get-directory-tree-external-command
;;       "find . -maxdepth 1 \\( -type d -printf \"%p/\\n\" , -type f -print \\) ")

(golden-ratio-mode 1)
(setq golden-ratio-exclude-modes '("ediff-mode"
                                   "eshell-mode"
                                   "dired-mode"))
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

(add-to-list 'auto-mode-alist
             '("nginx\.conf$"  . nginx-mode)
             '("/etc/nginx/.*" . nginx-mode))

(setq show-trailing-whitespace t)

(electric-pair-mode 1)

(defun smart-open-line ()
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))
(global-set-key (kbd "M-o") 'smart-open-line)

(defun smart-open-line-above ()
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

(global-set-key (kbd "M-O") 'smart-open-line-above)

(global-set-key (kbd "C-c j") 'just-one-space)
(global-set-key (kbd "C-x O") (lambda ()
                                (interactive)
                                (other-window -1)))

(defun smart-kill-whole-line (&optional arg)
  (interactive "P")
  (kill-whole-line arg)
  (back-to-indentation))
(global-set-key [remap kill-whole-line] 'smart-kill-whole-line)

(defun find-user-init-file ()
  (interactive)
  (find-file-other-window user-init-file))
(global-set-key (kbd "C-c I") 'find-user-init-file)

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (local-set-key (kbd "<f5>") 'eval-buffer)))

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(setq web-mode-markup-indent-offset 2)
(setq web-mode-enable-current-element-highlight nil)
(setq web-mode-enable-current-column-highlight t)

(global-set-key (kbd "C-x C-v") 'find-alternate-file)

(my/load-rc "dired")

(defun my/close-help-buffer-window ()
  (interactive)
  (delete-window (get-buffer-window "*Help*"))
  (kill-buffer "*Help*"))
(global-set-key (kbd "C-h q") 'my/close-help-buffer-window)
(global-set-key (kbd "s-k") 'kill-this-buffer)

(defadvice flycheck-list-errors (around my/flycheck-list-errors activate)
  (interactive)
  (let ((window (get-buffer-window "*Flycheck errors*")))
    (if window
        (delete-window window)
      (call-interactively (ad-get-orig-definition 'flycheck-list-errors)))))

(add-hook 'python-mode-hook (lambda ()
                              (flycheck-mode t)))

(add-to-list 'display-buffer-alist
             `(,(rx bos "*Flycheck errors*" eos)
               (display-buffer-reuse-window
                display-buffer-in-side-window)
               (reusable-frames . visible)
               (side            . bottom)
               (window-height   . 0.4)))
(add-to-list 'display-buffer-alist
             `(,(rx bos "*Compilation*" eos)
               (display-buffer-reuse-window
                display-buffer-in-side-window)
               (reusable-frames . visible)
               (side            . bottom)
               (window-height   . 0.4)))
(add-to-list 'display-buffer-alist
             `(,(rx bos "*Occur*" eos)
               (display-buffer-reuse-window
                display-buffer-in-side-window)
               (reusable-frames . visible)
               (side            . bottom)
               (window-height   . 0.4)))
(add-to-list 'display-buffer-alist
             `(,(rx bos "*Help*" eos)
               (display-buffer-reuse-window
                display-buffer-in-side-window)
               (reusable-frames . visible)
               (side            . bottom)
               (window-height   . 0.4)))
(add-to-list 'display-buffer-alist
             `(,(rx bos "*jedi:doc*" eos)
               (display-buffer-reuse-window
                display-buffer-in-side-window)
               (reusable-frames . visible)
               (side            . bottom)
               (window-height   . 0.4)))
(add-to-list 'display-buffer-alist
             `(,(rx bos "*helm jedi:related-names*" eos)
               (display-buffer-reuse-window
                display-buffer-in-side-window)
               (reusable-frames . visible)
               (side            . bottom)
               (window-height   . 0.4)))
(add-to-list 'display-buffer-alist
             `(,(rx bos "*helm-ag*" eos)
               (display-buffer-reuse-window
                display-buffer-in-side-window)
               (reusable-frames . visible)
               (side            . bottom)
               (window-height   . 0.4)))
(add-to-list 'display-buffer-alist
             `(,(rx bos "*helm grep*" eos)
               (display-buffer-reuse-window
                display-buffer-in-side-window)
               (reusable-frames . visible)
               (side            . bottom)
               (window-height   . 0.4)))
(add-to-list 'display-buffer-alist
             `(,(rx bos "*grep*" eos)
               (display-buffer-reuse-window
                display-buffer-in-side-window)
               (reusable-frames . visible)
               (side            . bottom)
               (window-height   . 0.4)))

(defun my/quit-bottom-side-windows ()
  "Quit side windows of the current frame."
  (interactive)
  (dolist (window (window-at-side-list))
    (quit-window nil window)))

(global-set-key (kbd "C-c q") 'my/quit-bottom-side-windows)

(setq python-environment-directory "~/.virtualenvs")

(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)
(setq jedi:use-shortcuts t)

(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))

(setq custom-file "~/emacs/custom.el")
(load custom-file t)

;; (color-theme-initialize)
;; (color-theme-sitaramv-nt)
