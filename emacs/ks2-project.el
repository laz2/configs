
(defvar ks2/test-last-cmd)

(defconst ks2/lxc-env-path
  "lxc/os/env/development.sh")

(defun ks2/format-enter-cmd ()
  (format (concat "cd %s && "
                  "source %s")
          (projectile-project-root)
          ks2/lxc-env-path))

(defun ks2/run-test (cmd)
  (setq ks2/test-last-cmd cmd)
  (compile cmd t))

(defun ks2/django-test (arg file &optional cmd)
  (let ((cmd (or cmd
                 (format (concat
                          (ks2/format-enter-cmd) " && "
                          "cd backend/s7 && "
                          "python -W ignore::DeprecationWarning:RemovedInDjango19Warning ./manage.py test %s--noinput %s")
                         (if arg "" "--keepdb ")
                         file))))
    (ks2/run-test cmd)))

(defun ks2/django-test-repeat-last ()
  (interactive)
  (if ks2/test-last-cmd
      (ks2/run-test ks2/test-last-cmd)
    (message "No previous test execution")))

(defun ks2/django-test-buffer-path ()
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

(defun ks2/django-test-project (arg)
  (interactive "P")
  (ks2/django-test arg ""))

(defun ks2/django-test-file (arg)
  (interactive "P")
  (ks2/django-test arg (ks2/django-test-buffer-path)))

(defun ks2/django-test-class (arg)
  (interactive "P")
  (let* ((tree (save-excursion
                 (python-imenu--build-tree))))
    (when (string-match ".+ (class)" (first tree))
      (ks2/django-test arg
                            (format
                             "%s.%s"
                             (ks2/django-test-buffer-path)
                             (first (split-string (car tree))))))))

(defun ks2/django-test-test (arg)
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
      (ks2/django-test arg
                            (format
                             "%s.%s.%s"
                             (ks2/django-test-buffer-path)
                             (first (split-string (car tree)))
                             (first (split-string (car entry))))))))

(defun ks2/lint-generic (cmd)
  (compile (format "%s && %s"
                   (ks2/format-enter-cmd)
                   cmd)))

(defun ks2/lint-all ()
  (interactive)
  (ks2/lint-generic "fab lint"))

(defun ks2/lint-py ()
  (interactive)
  (ks2/lint-generic "fab lint_py"))

(defun ks2/lint-ui ()
  (interactive)
  (ks2/lint-generic "fab lint_ui"))

(defun ks2-projectile-mode ()
  (interactive)
  (setq-local projectile-globally-ignored-file-suffixes
              (append
               (list ".min.js")
               projectile-globally-ignored-file-suffixes)))

(defun ks2-python-mode ()
  (interactive)
  (python-mode)
  (setq-local python-shell-interpreter "bash")
  (setq-local python-shell-interpreter-args
              (format (concat "-c \""
                              "cd %s && "
                              "source %s && "
                              "exec python -i backend/s7/manage.py shell_plus --plain"
                              "\"")
                      (projectile-project-root)
                      ks2/lxc-env-path))
  (setq-local python-shell-interpreter-interactive-arg "")
  (setq-local python-shell-prompt-detect-enabled nil)
  (setq-local python-shell-completion-native-enable t)
  (local-set-key (kbd "C-c C-t r") 'ks2/django-test-repeat-last)
  (local-set-key (kbd "C-c C-t C-r") 'ks2/django-test-repeat-last)
  (local-set-key (kbd "C-c C-t p") 'ks2/django-test-project)
  (local-set-key (kbd "C-c C-t C-p") 'ks2/django-test-project)
  (local-set-key (kbd "C-c C-t f") 'ks2/django-test-file)
  (local-set-key (kbd "C-c C-t C-f") 'ks2/django-test-file)
  (local-set-key (kbd "C-c C-t c") 'ks2/django-test-class)
  (local-set-key (kbd "C-c C-t C-c") 'ks2/django-test-class)
  (local-set-key (kbd "C-c C-t t") 'ks2/django-test-test)
  (local-set-key (kbd "C-c C-t C-t") 'ks2/django-test-test))

(provide 'ks2-project)
