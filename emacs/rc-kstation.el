
(setq flycheck-python-pycompile-executable "~/.virtualenvs/ks/bin/python3")
(setq flycheck-python-flake8-executable "~/.virtualenvs/ks/bin/flake8")

(defun kstation/django-test-app (arg)
  (interactive "P")
  (compile (format (concat
                    "cd %s && "
                    "source ~/.virtualenvs/ks/bin/activate && "
                    "source lxc/os/env/development.sh && "
                    "cd backend/s7 && "
                    "python -W ignore::DeprecationWarning:RemovedInDjango19Warning ./manage.py test %s--noinput")
                   (projectile-project-root)
                   (if arg "" "--keepdb ")) t))

(defun kstation/django-test-file (arg)
  (interactive "P")
  (compile (format (concat
                    "cd %s && "
                    "source ~/.virtualenvs/ks/bin/activate && "
                    "source lxc/os/env/development.sh && "
                    "cd backend/s7 && "
                    "python -W ignore::DeprecationWarning:RemovedInDjango19Warning ./manage.py test --failfast %s--noinput %s")
                   (projectile-project-root)
                   (if arg "" "--keepdb ")
                   (s-replace "/" "."
                              (s-chop-suffix ".py"
                                             (file-relative-name
                                              (buffer-file-name)
                                              (expand-file-name
                                               (concat (projectile-project-root) "/backend/s7")))))) t))

(defun kstation/ui-compile-command ()
  (interactive)
  (compile (format (concat "cd %s && "
                           "cd ui && "
                           "grunt unit:dev --spec=%s --no-color")
                   (projectile-project-root)
                   (file-relative-name
                    (buffer-file-name)
                    (concat (projectile-project-root) "ui")) t)))

(defun kstation/lint (arg)
  (interactive "P")
  (compile (format "cd %s && fab lint"
                   (projectile-project-root))))

(global-set-key (kbd "C-c C-t a") 'kstation/django-test-app)
(global-set-key (kbd "C-c C-t r") 'kstation/django-test-file)

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
