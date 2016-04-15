
(require 'cl)

;;
;; Parenthesis highlite
;;
(show-paren-mode t)
(setq show-paren-delay 0)           ; how long to wait?
(show-paren-mode t)                 ; turn paren-mode on
(setq show-paren-style 'parenthesis) ; alternatives are 'parenthesis' and 'mixed'

(setq-default indent-tabs-mode nil)

(global-subword-mode)

                                        ;(set-face-background 'show-paren-match-face
                                        ;                    (face-attribute 'show-paren-match-face :foreground))
                                        ;(set-face-foreground 'show-paren-match-face nil)
(set-face-attribute 'show-paren-match-face nil
                    :weight 'bold :underline nil :overline nil :slant 'normal)

(set-face-foreground 'show-paren-mismatch-face "red")
(set-face-attribute 'show-paren-mismatch-face nil
                    :weight 'bold :underline nil :overline nil :slant 'normal)

(defun my/common-hook ()
  (local-set-key "\C-c:" 'uncomment-region)
  (local-set-key "\C-c;" 'comment-region)
  (local-set-key "\C-c\C-c" 'comment-region)
  (font-lock-mode 1))

;; show FIXME/TODO/BUG keywords
(defun my/show-prog-keywords ()
  ;; highlight additional keywords
  (font-lock-add-keywords nil '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t))))

(defun my/common-prog-hook ()
  (subword-mode 1)
  (my/show-prog-keywords))

;; clean trailing whitespaces automatically
(setq my/trailing-whitespace-modes '(c++-mode
                                     c-mode
                                     haskell-mode
                                     emacs-lisp-mode
                                     lisp-mode
                                     scheme-mode
                                     erlang-mode
                                     python-mode
                                     js-mode js2-mode
                                     stylus-mode))
(defun my/trailing-whitespace-hook ()
  (when (member major-mode my/trailing-whitespace-modes)
    (delete-trailing-whitespace)))
(add-hook 'before-save-hook 'my/trailing-whitespace-hook)

;; untabify some modes
(setq my/untabify-modes '(haskell-mode
                          emacs-lisp-mode
                          lisp-mode
                          scheme-mode
                          erlang-mode
                          clojure-mode
                          python-mode
                          stylus-mode
                          js-mode js2-mode))
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
