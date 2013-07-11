
(require 'ido)
(ido-mode 'both)

(setq
 ido-save-directory-list-file "~/.emacs.d/ido.last"

 ido-ignore-buffers ;; ignore these guys
 '("\\` "
   "^\*Mess"
   "^\*Back"
   ".*Completion"
   "^\*Ido"
   "^\*trace"
   "^\*compilation"
   "^\*GTAGS"
   "^session\.*"
   "^\\*"
   "^#"
   "^irc.")
 ido-work-directory-list '("~/" "~/Desktop" "~/src")
 ido-case-fold  t                 ; be case-insensitive

 ido-enable-last-directory-history t ; remember last used dirs
 ido-max-work-directory-list 30   ; should be enough
 ido-max-work-file-list      50   ; remember many
 ido-use-filename-at-point nil    ; don't use filename at point (annoying)
 ido-use-url-at-point nil         ; don't use url at point (annoying)

 ido-enable-flex-matching nil     ; don't try to be too smart
 ido-max-prospects 8              ; don't spam my minibuffer
 ido-confirm-unique-completion t) ; wait for RET, even with unique completion

;; when using ido, the confirmation is rather annoying...
(setq confirm-nonexistent-file-or-buffer nil)

;; cycle through buffers with Ctrl-Tab (like Firefox)
(global-set-key (kbd "<C-tab>") 'bury-buffer)

(global-set-key (kbd "<f5>")
                (lambda()(interactive)(switch-to-buffer "*scratch*")))

(global-set-key (kbd "<f6>")
                (lambda()(interactive)(find-file "~/.emacs")))

(autoload 'idomenu "idomenu" nil t)

(global-set-key (kbd "C-S-o") 'idomenu)

;; Display ido results vertically, rather than horizontally
(setq ido-decorations
      (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))

(defun ido-disable-line-trucation ()
  (set (make-local-variable 'truncate-lines) nil))
(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-trucation)
