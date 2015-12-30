

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.tern-project$" . js-mode))
(add-to-list 'auto-mode-alist '("\\.tern-config$" . js-mode))

(add-hook 'js2-mode-hook 'ac-js2-mode)

(setq js2-highlight-level 3)

(setq js-indent-level 2)
(setq-default js2-basic-offset 2)

(eval-after-load 'tern
   '(progn
      (require 'tern-auto-complete)
      (tern-ac-setup)))

(add-hook 'js2-mode-hook (lambda ()
                           (flycheck-mode t)
                           (tern-mode t)
                           (js2-imenu-extras-mode t)
                           (local-set-key (kbd "M-.") 'tern-find-definition)))

(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))
(add-hook 'json-mode-hook (lambda ()
                             (flycheck-mode t)
                             (local-set-key (kbd "C-M-\\") 'json-mode-beautify)))
(setq json-reformat:pretty-string? t)

(setq coffee-tab-width 2)
