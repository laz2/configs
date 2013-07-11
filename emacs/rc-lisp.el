
(add-hook 'emacs-lisp-mode-hook       (lambda () (paredit-mode +1)))
(add-hook 'lisp-mode-hook             (lambda () (paredit-mode +1)))
(add-hook 'lisp-interaction-mode-hook (lambda () (paredit-mode +1)))

(define-key lisp-mode-shared-map (kbd "M-m") 'eassist-list-methods)

;;
;; SLIME
;;
;; (require 'slime)
;; (setq slime-net-coding-system 'utf-8-unix)
;; (slime-setup '(slime-repl
;;                slime-c-p-c
;;                slime-editing-commands
;;                slime-fancy-inspector
;;                slime-fuzzy
;;                slime-presentations
;;                slime-scratch
;;                slime-references
;;                slime-package-fu
;;                slime-fontifying-fu
;;                slime-compiler-notes-tree))
;; (setq slime-enable-evaluate-in-emacs t)
;; (setq slime-protocol-version 'ignore)

;; (defun my/slime-common-hook ()
;;   )

;; (add-hook 'slime-mode-hook 'my/slime-common-hook)
;; (add-hook 'slime-repl-mode-hook 'my/slime-common-hook)

;; (setq slime-startup-animation nil)

;;
;; ac-slime
;;
(require 'ac-slime)
(add-to-list 'ac-modes 'slime-mode-hook)
(add-to-list 'ac-modes 'slime-repl-mode)

(defun ac-slime-my-hook ()
  (set-up-slime-ac t))

(add-hook 'slime-mode-hook 'ac-slime-my-hook)
(add-hook 'slime-repl-mode-hook 'ac-slime-my-hook)
