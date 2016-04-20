
(let ((default-directory "~/cfg/emacs/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

(setq package-enable-at-startup nil)
(package-initialize)

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")))

(dolist (p '(
             use-package
             diminish
             ))
  (let (refreshed)
    (when (not (package-installed-p p))
      (unless refreshed
        (package-refresh-contents)
        (setq refreshed t))
      (package-install p))))

(require 'use-package)
(setq use-package-verbose t)
(use-package diminish)

(defun my/load-rc (name)
  (let ((rc-name (concat "rc-" name)))
    (load (concat "~/emacs/" rc-name))))

(use-package s
  :ensure)

(use-package seq
  :ensure)

(setq-default indent-tabs-mode nil)

(global-subword-mode)
(global-font-lock-mode)
(add-hook 'prog-mode-hook (lambda ()
                            (font-lock-add-keywords nil '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t)))))

;; clean trailing whitespaces automatically
(setq my/trailing-whitespace-modes nil)
(defun my/trailing-whitespace-hook ()
  (when (member major-mode my/trailing-whitespace-modes)
    (delete-trailing-whitespace)))
(add-hook 'before-save-hook 'my/trailing-whitespace-hook)

;; untabify some modes
(setq my/untabify-modes nil)
(defun my/untabify-hook ()
  (when (member major-mode my/untabify-modes)
    (untabify (point-min) (point-max))))
(add-hook 'before-save-hook 'my/untabify-hook)

(defun my/indent-region-or-buffer ()
  (interactive)
  (delete-trailing-whitespace)
  (if (use-region-p)
      (indent-region (region-beginning) (region-end))
    (save-excursion
      (indent-region (point-min) (point-max)))))
(global-set-key "\C-\M-\\" 'my/indent-region-or-buffer)

(use-package uniquify
  :demand
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  ;; rename after killing uniquified
  (setq uniquify-after-kill-buffer-p t)
  ;; don't muck with special buffers
  (setq uniquify-ignore-buffers-re "^\\*"))

(use-package volatile-highlights
  :ensure
  :demand
  :diminish volatile-highlights-mode
  :config
  (volatile-highlights-mode t))

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

(use-package auto-complete
  :ensure
  :demand
  :diminish auto-complete-mode
  :config
  (ac-config-default))

(use-package saveplace
  :ensure
  :demand
  :init
  (setq-default save-place t)
  :config
  (setq save-place-file (expand-file-name "saveplace" user-emacs-directory)))

(global-set-key (kbd "<f5>")
                (lambda ()
                  (interactive)
                  (Switch-to-buffer "*scratch*")))

(global-set-key (kbd "<f6>")
                (lambda ()
                  (interactive)
                  (find-file "~/.emacs")))

(use-package ido
  :demand
  :config
  (ido-mode 'both)

  (setq
   ido-save-directory-list-file "~/.emacs.d/ido.last"

   ido-ignore-buffers ;; ignore these guys
   '("\\` "
     "^\*Mess"
     "^\*Back"
     ".*Completion"
     "^\*Ido"
     "^\*trace"
     "^\*compilation"
     "^\*GTAGS"
     "^session\.*"
     "^\\*"
     "^#"
     "^irc.")
   ido-work-directory-list '("~/" "~/Desktop" "~/src")
   ido-case-fold  t                 ; be case-insensitive

   ido-enable-last-directory-history t ; remember last used dirs
   ido-max-work-directory-list 30   ; should be enough
   ido-max-work-file-list      50   ; remember many
   ido-use-filename-at-point nil    ; don't use filename at point (annoying)
   ido-use-url-at-point nil         ; don't use url at point (annoying)

   ido-enable-flex-matching nil     ; don't try to be too smart
   ido-max-prospects 8              ; don't spam my minibuffer
   ido-confirm-unique-completion t) ; wait for RET, even with unique completion

  ;; when using ido, the confirmation is rather annoying...
  (setq confirm-nonexistent-file-or-buffer nil)

  ;; Display ido results vertically, rather than horizontally
  (setq ido-decorations
        (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))

  (defun ido-disable-line-trucation ()
    (set (make-local-variable 'truncate-lines) nil))
  (add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-trucation))

;;
;; ibuffer
;;
(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :config
  (setq ibuffer-saved-filter-groups
        (quote (("default"
                 ))))

  (add-hook 'ibuffer-mode-hook
            (lambda ()
              (ibuffer-switch-to-saved-filter-groups "default"))))

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

(global-set-key (kbd "M-]") 'next-error)
(global-set-key (kbd "M-[") 'previous-error)

(use-package compile
  :commands compile-mode
  :bind ([f9] . compile)
  :config
  (setq compilation-disable-input nil)
  (setq compilation-scroll-output 'first-error)
  (setq mode-compile-always-save-buffer-p t))

(put 'narrow-to-region 'disabled nil)

(use-package helm
  :ensure
  :diminish helm-mode
  :commands helm-mode
  :bind (("C-c h"   . helm-command-prefix)
         ("M-x"     . helm-M-x)
         ("M-y"     . helm-show-kill-ring)
         ("C-x b"   . helm-mini)
         ("C-x C-f" . helm-find-files)
         ("C-S-o" . helm-imenu)
         ("C-S-g" . helm-occur)
         :map helm-map
         ;; rebind tab to run persistent action
         ("<tab>" . helm-execute-persistent-action)
         ;; make TAB works in terminal
         ("C-i"   . helm-execute-persistent-action)
         ;; list actions using C-z
         ("C-z"   . helm-select-action))
  :config
  (require 'helm-config)
  ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
  ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
  ;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
  (global-unset-key (kbd "C-x c"))

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

  (setq helm-split-window-in-side-p t)

  (helm-mode 1))

(use-package ag
  :ensure)

(use-package helm-ag
  :ensure)

(use-package yasnippet
  :ensure)

(use-package dired+
  :ensure)

(use-package dired
  :commands dired-mode
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
  (require 'dired+)

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

(use-package projectile
  :ensure
  :demand
  :bind (
         ("C-S-f" . projectile-ag)
         )
  :config
  (projectile-global-mode)
  (setq projectile-enable-caching t)
  (setq projectile-mode-line '(:eval (format " P[%s]" (projectile-project-name)))))

(use-package helm-projectile
  :ensure
  :demand
  :bind (
         ("C-S-n" . helm-projectile-find-file)
         ("C-S-h" . helm-projectile-ag)
         ("C-S-p" . helm-projectile-switch-project)
         )
  :config
  (setq projectile-completion-system 'helm)
  (helm-projectile-on)
  (setq projectile-switch-project-action 'projectile-dired))

(use-package golden-ratio
  :ensure
  :demand
  :diminish golden-ratio-mode
  :config
  (golden-ratio-mode t)

  (add-to-list 'golden-ratio-exclude-modes "ediff-mode")
  (add-to-list 'golden-ratio-exclude-modes "eshell-mode")

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
  (add-to-list 'golden-ratio-exclude-buffer-names "*Compile-Log*")

  (add-to-list 'golden-ratio-exclude-buffer-regexp "*ag search"))

(setq split-width-threshold nil)

(use-package bm
  :ensure
  :bind ("C-M-;" . bm-toggle)
  :init
  (setq-default bm-highlight-style 'bm-highlight-only-fringe))

(use-package helm-bm
  :ensure
  :bind ("C-M-'" . helm-bm))

(use-package ruby-mode
  :commands ruby-mode
  :mode "Rakefile"
  :init
  (add-to-list 'my/untabify-modes 'ruby-mode)
  (add-to-list 'my/trailing-whitespace-modes 'ruby-mode))

(use-package python
  :init
  (add-to-list 'my/untabify-modes 'python-mode)
  (add-to-list 'my/trailing-whitespace-modes 'python-mode))

(use-package py-autopep8
  :ensure)

(setq python-environment-directory "~/.virtualenvs")

(use-package jedi
  :ensure
  :demand)

(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)
(setq jedi:use-shortcuts t)
(setq jedi:environment-root "ks")

(use-package sh-mode
  :init
  (add-to-list 'my/untabify-modes 'sh-mode)
  (add-to-list 'my/trailing-whitespace-modes 'sh-mode)
  :config
  (setq-default sh-basic-offset 2
                sh-indentation 2))

(defun my/scroll-other-window-up ()
  (interactive)
  (scroll-other-window -1))
(defun my/scroll-other-window-down ()
  (interactive)
  (scroll-other-window 1))
(global-set-key (kbd "M-P") 'my/scroll-other-window-up)
(global-set-key (kbd "M-N") 'my/scroll-other-window-down)

(global-set-key (kbd "C-;") 'comment-or-uncomment-region)

(electric-pair-mode 1)

(global-set-key (kbd "C-c j") 'just-one-space)

(setq-default show-paren-delay 0
              show-paren-style 'parenthesis)
(show-paren-mode t)

(use-package cmake-mode
  :ensure
  :commands cmake-mode)

(use-package crux
  :ensure
  :demand
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

(use-package emacs-lisp-mode
  :bind (("<f5>" . eval-buffer))
  :init
  (add-to-list 'my/untabify-modes 'emacs-lisp-mode)
  (add-to-list 'my/trailing-whitespace-modes 'emacs-lisp-mode))

(use-package json-snatcher
  :ensure)

(use-package json-reformat
  :ensure)

(use-package web-mode
  :ensure
  :commands web-mode
  :mode "\\.phtml\\'"
  :mode "\\.tpl\\.php\\'"
  :mode "\\.[agj]sp\\'"
  :mode "\\.as[cp]x\\'"
  :mode "\\.erb\\'"
  :mode "\\.mustache\\'"
  :mode "\\.djhtml\\'"
  :mode "\\.html?\\'"
  :mode "\\.json?\\'"
  :config
  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-enable-current-element-highlight nil)
  (setq web-mode-enable-current-column-highlight t)
  (web-mode-set-engine "angular"))

(global-set-key (kbd "C-x C-v") 'find-alternate-file)

(use-package nsis-mode
  :ensure
  :commands nsis-mode)

(use-package dockerfile-mode
  :ensure
  :commands dockerfile-mode)

(use-package markdown-mode
  :ensure
  :commands markdown-mode)

(use-package yaml-mode
  :ensure
  :commands yaml-mode)

(use-package nginx-mode
  :ensure
  :commands nginx-mode)

(use-package go-mode
  :ensure
  :commands go-mode)

(use-package flycheck
  :ensure
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (defadvice flycheck-list-errors (around my/flycheck-list-errors activate)
    (interactive)
    (let ((window (get-buffer-window "*Flycheck errors*")))
      (if window
          (delete-window window)
        (call-interactively (ad-get-orig-definition 'flycheck-list-errors))))))

(use-package flycheck-checkbashisms
  :ensure
  :config
  (flycheck-checkbashisms-setup))

(use-package flycheck-color-mode-line
  :ensure
  :config
  (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))

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
                       "^\\*Backtrace\\*$"
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
  :ensure
  :bind ("s-." . avy-goto-word-or-subword-1)
  :config
  (add-to-list 'golden-ratio-extra-commands 'avy-goto-word-or-subword-1))

(use-package ace-window
  :ensure
  :bind ("s-o" . ace-window)
  :init
  (add-to-list 'golden-ratio-extra-commands 'ace-window))

(defun my/new-empty-buffer ()
  "Open a new empty buffer."
  (interactive)
  (let ((buf (generate-new-buffer "untitled")))
    (switch-to-buffer buf)
    (funcall (and initial-major-mode))
    (setq buffer-offer-save t)))

(global-set-key (kbd "<f7>") 'my/new-empty-buffer)

(use-package color-theme
  :ensure
  :config
  (color-theme-initialize)
  (color-theme-sitaramv-nt)
  (add-hook 'window-setup-hook
            '(lambda () (set-cursor-color "black"))))

(use-package anzu
  :ensure
  :diminish anzu-mode
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp))
  :config
  (global-anzu-mode +1))

(use-package js2-mode
  :ensure
  :mode "\\.js\\'"
  :config
  (add-to-list 'my/untabify-modes 'js2-mode)
  (add-to-list 'my/trailing-whitespace-modes 'js2-mode)

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

  (add-hook 'js2-mode-hook (lambda ()
                             (js2-imenu-extras-mode t)))

  (defadvice js2-record-name-node
    (after my/js2-record-name-node first () activate)
    (let ((node (ad-get-arg 0))
          (leftpos (js2-node-abs-pos node)))
      (js2-set-face leftpos
                    (+ leftpos (js2-node-len node))
                    'font-lock-variable-name-face
                    'record))))

(use-package ac-js2
  :ensure
  :commands ac-js2-mode
  :init
  (add-hook 'js2-mode-hook 'ac-js2-mode))

(use-package js2-refactor
  :ensure
  :commands js2-refactor-mode
  :init
  (add-hook 'js2-mode-hook #'js2-refactor-mode))

(use-package tern
  :ensure
  :commands tern-mode
  :mode ("\\.tern-project$" . js2-mode)
  :mode ("\\.tern-config$" . js2-mode)
  :init
  (add-hook 'js2-mode-hook (lambda ()
                             (tern-mode t)
                             (local-set-key (kbd "M-.") 'tern-find-definition))))

(use-package tern-auto-complete
  :ensure
  :commands tern-mode
  :config
  (tern-ac-setup))

(use-package coffee-mode
  :ensure
  :commands coffee-mode
  :init
  (setq-default coffee-tab-width 2))

(use-package haskell-mode
  :ensure
  :commands haskell-mode
  :config
  (add-to-list 'my/untabify-modes 'haskell-mode)
  (add-to-list 'my/trailing-whitespace-modes 'haskell-mode)
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (setq haskell-process-suggest-remove-import-lines t)
  (setq haskell-process-auto-import-loaded-modules t)
  (setq haskell-process-log t)
  (setq haskell-process-show-debug-tips nil)
  (setq haskell-stylish-on-save t)
  (setq haskell-interactive-popups-error nil)
  (buffer-on-bottom-side "^\\*haskell\\*$")
  (add-to-list 'golden-ratio-exclude-buffer-names "*haskell*"))

(use-package c++-mode
  :commands c++-mode
  :init
  (add-to-list 'my/untabify-modes 'c++-mode)
  (add-to-list 'my/trailing-whitespace-modes 'c++-mode)
  (defun my/c++-mode-hook ()
    (setq tab-width 4)
    (setq c++-auto-hungry-initial-state 'none)
    (setq c++-delete-function 'backward-delete-char)
    (setq c++-tab-always-indent t)
    (setq c-indent-level 4)
    (setq c-basic-offset 4)
    (setq c-default-style "linux")
    (setq c-indentation-style "linux")
    (setq c-continued-statement-offset 4)
    (setq c++-empty-arglist-indent 4))
  (add-hook 'c++-mode-hook 'my/c++-mode-hook))

(use-package c-mode
  :commands c-mode
  :init
  (add-to-list 'my/untabify-modes 'c-mode)
  (add-to-list 'my/trailing-whitespace-modes 'c-mode))

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
  :ensure
  :disabled t)

(use-package edit-server
  :if window-system
  :init
  (add-hook 'after-init-hook 'server-start t))

(use-package tex-site
  :ensure auctex
  :disabled t
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
  :disabled t
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

(use-package stylus-mode
  :commands stylus-mode
  :config
  (add-to-list 'my/untabify-modes 'stylus-mode)
  (add-to-list 'my/trailing-whitespace-modes 'stylus-mode)
  (add-hook 'stylus-mode-hook (lambda ()
                                (setq sws-tab-width 4))))

(use-package whitespace-mode
  :config
  ;; (set-face-background 'show-paren-match-face
  ;; (face-attribute 'show-paren-match-face :foreground))
  ;; (set-face-foreground 'show-paren-match-face nil)
  (set-face-attribute 'show-paren-match-face nil
                      :weight 'bold :underline nil :overline nil :slant 'normal)

  (set-face-foreground 'show-paren-mismatch-face "red")
  (set-face-attribute 'show-paren-mismatch-face nil
                      :weight 'bold :underline nil :overline nil :slant 'normal)
  (setq show-trailing-whitespace t)
  (setq whitespace-display-mappings (assq-delete-all 'newline-mark
                                                     whitespace-display-mappings))
  (push (list 'space-mark ?\  [?.]) whitespace-display-mappings)
  (set-face-attribute 'whitespace-space nil :background nil :foreground "gray40")
  (set-face-attribute 'whitespace-indentation nil :background nil :foreground "gray40")
  (set-face-attribute 'whitespace-line nil :background nil :foreground nil)
  (setq whitespace-style (remove 'newline whitespace-style)))

(use-package miniedit
  :ensure
  :demand
  :init
  (miniedit-install)
  :config
  (setq miniedit-show-help-p nil))

(my/load-rc "kstation")

(setq custom-file "~/emacs/custom.el")
(load custom-file t)
