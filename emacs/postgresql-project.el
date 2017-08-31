
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
  (c-mode)
  (c-set-style "pgsql")
  (ggtags-mode)
  (helm-gtags-mode)
  (setq-local projectile-globally-ignored-file-suffixes
              (append
               (list ".o")
               projectile-globally-ignored-file-suffixes)))

(provide 'postgresql-project)
