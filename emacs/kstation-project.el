
(setq flycheck-python-pycompile-executable "~/.virtualenvs/oldfashioned/bin/python3")
(setq flycheck-python-flake8-executable "~/.virtualenvs/oldfashioned/bin/flake8")

(defun kstation/django-test (arg file)
  (compile
   (format (concat
            "cd %s && "
            "source ~/.virtualenvs/oldfashioned/bin/activate && "
            "source lxc/os/env/development.sh && "
            "cd backend/s7 && "
            "python -W ignore::DeprecationWarning:RemovedInDjango19Warning ./manage.py test %s--noinput %s")
           (projectile-project-root)
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

(defun kstation/django-test-app (arg)
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

(defun kstation/lint (arg)
  (interactive "P")
  (compile (format (concat
                    "cd %s && "
                    "source ~/.virtualenvs/oldfashioned/bin/activate && "
                    "fab lint")
                   (projectile-project-root))))

(defun kstation/ui-lint (arg)
  (interactive "P")
  (compile (format (concat
                    "cd %s && "
                    "source ~/.virtualenvs/oldfashioned/bin/activate && "
                    "cd ui/ && "
                    "gulp lint")
                   (projectile-project-root))))

(global-set-key (kbd "C-c C-t a") 'kstation/django-test-app)
(global-set-key (kbd "C-c C-t r") 'kstation/django-test-file)
(global-set-key (kbd "C-c C-t t") 'kstation/django-test-test)

(dir-locals-set-class-variables
 'kstation-project
 '((nil . ((fill-column . 80)))
   ("backend/s7"
    . ((fill-column . 120)
       (eval . (progn
                 (message "Hello from kstation!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
                 (local-set-key (kbd "C-c t a") 'kstation/django-compile-file)))))
   ("ui" . ((eval . (progn
                      (local-set-key (kbd "C-c t a") 'kstation/ui-compile-command)))))))

(dir-locals-set-directory-class "/home/user/dev/kstation" 'kstation-project)

(provide 'kstation-project)
