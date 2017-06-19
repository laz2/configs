
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
  (c-set-style "pgsql"))

(provide 'postgresql-project)
