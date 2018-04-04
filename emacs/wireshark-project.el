

(defun wireshark-c-mode ()
  (interactive)
  (c-mode)
  (ggtags-mode)
  (helm-gtags-mode))

(defun wireshark-projectile-mode ()
  (interactive)
  (setq-local projectile-globally-ignored-file-suffixes
              (append
               (list ".o")
               projectile-globally-ignored-file-suffixes)))

(provide 'wireshark-project)
