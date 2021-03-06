
(c-add-style "pgsql"
             '("bsd"
               (fill-column . 79)
               (indent-tabs-mode . t)
               (c-basic-offset   . 4)
               (tab-width . 4)
               (c-offsets-alist .
                                ((case-label . +))) ; indent case labels by c-indent-level, too
               )
             nil)

(defun pgsql-c-mode ()
  (interactive)
  (c-mode)
  (c-set-style "pgsql"))

(defun pgsql-projectile-mode ()
  (interactive)
  (setq-local projectile-globally-ignored-file-suffixes
              (append
               (list ".o")
               projectile-globally-ignored-file-suffixes))
  (ggtags-mode)
  (helm-gtags-mode))

(provide 'postgresql-project)
