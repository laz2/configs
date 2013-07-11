
;;; Add my emacs directory to load-path
;;; and all its subdirs
(let ((default-directory "~/emacs/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

(setq package-archives '(("ELPA" . "http://tromey.com/elpa/")
                          ("gnu" . "http://elpa.gnu.org/packages/")
                          ("marmalade" . "http://marmalade-repo.org/packages/")))

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

(set-default-font "-outline-DejaVu Sans Mono-normal-normal-normal-mono-15-*-*-*-c-*-windows-1258")
(tool-bar-mode -1)

(setq redisplay-dont-pause t)

;;
;; Auto-complete
;;
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/emacs/modules/auto-complete/dict")
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
               ("sc-core"
                (filename . "dev/ostis_trunk/kpm/sc-core/"))
               ("IRC" (mode . erc-mode))))))

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

;;
;; Color
;;
(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-sitaramv-nt)))

(my/load-rc "prog-common")
(my/load-rc "rst")

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

(setq custom-file "~/emacs/custom.el")
(load custom-file t)
