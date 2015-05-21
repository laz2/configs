
(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.tern-project$" . js-mode))
(add-to-list 'auto-mode-alist '("\\.tern-config$" . js-mode))

;(add-hook 'js2-mode-hook 'ac-js2-mode)

(setq js2-highlight-level 3)

(setq js-indent-level 2)
(setq-default js2-basic-offset 2)

(eval-after-load 'tern
   '(progn
      (require 'tern-auto-complete)
      (tern-ac-setup)))

(add-hook 'js2-mode-hook (lambda ()
                           (electric-indent-mode +1)
                           (tern-mode t)))
