;;; Add my emacs directory to load-path
;;; and all its subdirs
(let ((default-directory "~/emacs/"))
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

                                        ;(server-start)

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
                                        ;(set-default-font "-outline-DejaVu Sans Mono-normal-normal-normal-mono-15-*-*-*-c-*-windows-1258")
(set-default-font "-unknown-DejaVu Sans Mono-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1")
(tool-bar-mode -1)

(setq redisplay-dont-pause t)

;;
;; Auto-complete
;;
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/modules/auto-complete/dict")
(ac-config-default)
(add-to-list 'ac-sources ac-source-semantic)

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

(setq custom-file "~/emacs/custom.el")
(load custom-file t)
