(require 's)

(setq flycheck-python-pycompile-executable "~/.virtualenvs/ks/bin/python")
(setq flycheck-python-flake8-executable "~/.virtualenvs/ks/bin/flake8")

(defun kstation/django-test-app ()
  (interactive)
  (compile (format (concat "cd %s && "
                           "source ~/.virtualenvs/ks/bin/activate && "
                           "source backend/os/env/development.sh && "
                           "cd backend/s7 && "
                           "./manage.py test --failfast")
                   (projectile-project-root)) t))

(defun kstation/django-test-file ()
  (interactive)
  (compile (format (concat "cd %s && "
                           "source ~/.virtualenvs/ks/bin/activate && "
                           "source backend/os/env/development.sh && "
                           "cd backend/s7 && "
                           "./manage.py test --failfast %s")
                   (projectile-project-root)
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

(global-set-key (kbd "C-c C-t a") 'kstation/django-test-app)
(global-set-key (kbd "C-c C-t r") 'kstation/django-test-file)

(dir-locals-set-class-variables
 'kstation-project
 '(("backend/s7" . ((hello . "Hello")
                    (eval . (progn
                              (message "Hello from kstation!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
                              (local-set-key (kbd "C-c t a") 'kstation/django-compile-file)))))
   ("ui" . ((eval . (progn
                      (local-set-key (kbd "C-c t a") 'kstation/ui-compile-command)))))))

(dir-locals-set-directory-class
 (expand-file-name "~/dev/kstation") 'kstation-project)
