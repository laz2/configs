(defun my/sh-mode-hook ()
  (setq sh-basic-offset 2
        sh-indentation 2))

(add-hook 'sh-mode-hook 'my/sh-mode-hook)
