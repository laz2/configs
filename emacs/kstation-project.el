
(defun kstation/format-enter-cmd ()
  (format (concat "cd %s && "
                  "source ~/.virtualenvs/oldfashioned/bin/activate")
          (projectile-project-root)))

(defun kstation/django-test (arg file)
  (compile
   (format (concat
            (kstation/format-enter-cmd) " && "
            "cd backend/s7 && "
            "python -W ignore::DeprecationWarning:RemovedInDjango19Warning ./manage.py test %s--noinput %s")
           (if arg "" "--keepdb ")
           file)
   t))

(defun kstation/django-test-buffer-path ()
  (s-replace "/" "."
             (s-chop-suffix ".py"
                            (file-relative-name
                             (buffer-file-name)
                             (expand-file-name
                              (concat (projectile-project-root) "/backend/s7"))))))

(defun kstation/django-test-project (arg)
  (interactive "P")
  (kstation/django-test arg ""))

(defun kstation/django-test-file (arg)
  (interactive "P")
  (kstation/django-test arg (kstation/django-test-buffer-path)))

(defun kstation/django-test-test (arg)
  (interactive "P")
  (let* ((imenu (save-excursion
                  (python-imenu--build-tree)))
         (defun-point (save-mark-and-excursion
                        (beginning-of-defun)
                        (point)))
         (entries (-filter
                   (lambda (el)
                     (and (listp el)
                          (string-match "^test_.+ (\\(async \\)?def)$" (car el))
                          (= defun-point (cdr el))))
                   imenu))
         (entry (car entries)))
    (when entry
      (kstation/django-test arg
                            (format
                             "%s.%s.%s"
                             (kstation/django-test-buffer-path)
                             (first (split-string (car imenu)))
                             (first (split-string (car entry))))))))

(defun kstation/lint-generic (cmd)
  (compile (format "%s && %s" (kstation/format-enter-cmd) cmd)))

(defun kstation/lint-all ()
  (interactive)
  (kstation/lint-generic "fab lint"))

(defun kstation/lint-py ()
  (interactive)
  (kstation/lint-generic "fab lint_py"))

(defun kstation/lint-ui ()
  (interactive)
  (kstation/lint-generic "fab lint_ui"))

(defun kstation-python-mode ()
  (interactive)
  (python-mode)
  (setq-local flycheck-python-pycompile-executable "~/.virtualenvs/oldfashioned/bin/python3")
  (setq-local flycheck-python-flake8-executable "~/.virtualenvs/oldfashioned/bin/flake8")
  (local-set-key (kbd "C-c C-t p") 'kstation/django-test-project)
  (local-set-key (kbd "C-c C-t f") 'kstation/django-test-file)
  (local-set-key (kbd "C-c C-t t") 'kstation/django-test-test))

(provide 'kstation-project)
