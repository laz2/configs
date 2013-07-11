
;(require 'tex-site)
;(require 'reftex)

(setq preview-image-type 'pnm)

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'latex-mode-hook 'turn-on-reftex)

(defun TeX-insert-macro-verb ()
  (interactive)
  (TeX-insert-macro "verb"))

(defun TeX-insert-macro-lstinline ()
  (interactive)
  (TeX-insert-macro "lstinline"))

(defun my/TeX-mode-hook ()
  (local-set-key (kbd "C-c C-g C-v") 'TeX-insert-macro-verb)
  (local-set-key (kbd "C-c C-g C-l") 'TeX-insert-macro-lstinline))

(add-hook 'TeX-mode-hook 'my/TeX-mode-hook)

(setq reftex-extra-bindings t)
(setq reftex-enable-partial-scans t)
(setq reftex-save-parse-info t)
(setq reftex-use-multiple-selection-buffers t)

(defun my/reftex-mode-hook ()
  (local-set-key (kbd "C-c C-y h") 'reftex-change-label)
  (local-set-key (kbd "C-c C-y g") 'reftex-goto-label))

(add-hook 'reftex-mode-hook 'my/reftex-mode-hook)
