
(defvar kstation/test-last-cmd)

(defun kstation/virtualenv-path ()
  (format "~/.virtualenvs/%s" (projectile-project-name)))

(defun kstation/format-enter-cmd ()
  (format (concat "cd %s && "
                  "source %s/bin/activate && "
                  "source lxc/os/env/development.sh")
          (projectile-project-root)
          (kstation/virtualenv-path)))

(defun kstation/run-test (cmd)
  (setq kstation/test-last-cmd cmd)
  (compile cmd t))

(defun kstation/django-test (arg file &optional cmd)
  (let ((cmd (or cmd
                 (format (concat
                          (kstation/format-enter-cmd)
                          " && "
                          "cd backend/s7 && "
                          "python -W ignore::DeprecationWarning:RemovedInDjango19Warning ./manage.py test %s--noinput %s")
                         (if arg "" "--keepdb ")
                         file))))
    (kstation/run-test cmd)))

(defun kstation/django-test-repeat-last ()
  (interactive)
  (if kstation/test-last-cmd
      (kstation/run-test kstation/test-last-cmd)
    (message "No previous test execution")))

(defun kstation/django-test-buffer-path ()
  (s-replace
   "/"
   "."
   (s-chop-suffix
    ".py"
    (file-relative-name
     (buffer-file-name)
     (expand-file-name
      (concat (projectile-project-root)
              "/backend/s7"))))))

(defun kstation/django-test-project (arg)
  (interactive "P")
  (kstation/django-test arg ""))

(defun kstation/django-test-file (arg)
  (interactive "P")
  (kstation/django-test arg (kstation/django-test-buffer-path)))

(defun kstation/django-test-class (arg)
  (interactive "P")
  (let* ((tree (save-excursion
                 (python-imenu--build-tree))))
    (when (string-match ".+ (class)" (first tree))
      (kstation/django-test arg
                            (format
                             "%s.%s"
                             (kstation/django-test-buffer-path)
                             (first (split-string (car tree))))))))

(defun kstation/django-test-test (arg)
  (interactive "P")
  (let* ((tree (save-excursion
                 (python-imenu--build-tree)))
         (defun-point (save-excursion
                        (beginning-of-defun)
                        (point)))
         (entries (-filter
                   (lambda (el)
                     (and (listp el)
                          (string-match "^test_.+ (\\(async \\)?def)$" (car el))
                          (= defun-point (cdr el))))
                   tree))
         (entry (car entries)))
    (when entry
      (kstation/django-test arg
                            (format
                             "%s.%s.%s"
                             (kstation/django-test-buffer-path)
                             (first (split-string (car tree)))
                             (first (split-string (car entry))))))))

(defun kstation/lint-generic (cmd)
  (compile (format "%s && %s"
                   (kstation/format-enter-cmd)
                   cmd)))

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
  (setq-local flycheck-python-pycompile-executable
              (format "%s/bin/python3"
                      (kstation/virtualenv-path)))
  (setq-local flycheck-python-flake8-executable
              (format "%s/bin/flake8"
                      (kstation/virtualenv-path)))
  (local-set-key (kbd "C-c C-t r") 'kstation/django-test-repeat-last)
  (local-set-key (kbd "C-c C-t C-r") 'kstation/django-test-repeat-last)
  (local-set-key (kbd "C-c C-t p") 'kstation/django-test-project)
  (local-set-key (kbd "C-c C-t C-p") 'kstation/django-test-project)
  (local-set-key (kbd "C-c C-t f") 'kstation/django-test-file)
  (local-set-key (kbd "C-c C-t C-f") 'kstation/django-test-file)
  (local-set-key (kbd "C-c C-t c") 'kstation/django-test-class)
  (local-set-key (kbd "C-c C-t C-c") 'kstation/django-test-class)
  (local-set-key (kbd "C-c C-t t") 'kstation/django-test-test)
  (local-set-key (kbd "C-c C-t C-t") 'kstation/django-test-test))

(provide 'kstation-project)
