
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-PDF-mode t)
 '(TeX-auto-save t)
 '(TeX-auto-untabify t)
 '(TeX-parse-self t)
 '(ansi-color-names-vector
   ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "white"])
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(fci-rule-color "#383838")
 '(frame-brackground-mode (quote dark))
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#fdf6e3" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors
   (quote
    (("#eee8d5" . 0)
     ("#B4C342" . 20)
     ("#69CABF" . 30)
     ("#69B7F0" . 50)
     ("#DEB542" . 60)
     ("#F2804F" . 70)
     ("#F771AC" . 85)
     ("#eee8d5" . 100))))
 '(hl-bg-colors
   (quote
    ("#DEB542" "#F2804F" "#FF6E64" "#F771AC" "#9EA0E5" "#69B7F0" "#69CABF" "#B4C342")))
 '(hl-fg-colors
   (quote
    ("#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3")))
 '(magit-diff-use-overlays nil)
 '(package-selected-packages
   (quote
    (buffer-mode buffer-move powerline keyfreq jump-char exec-path-from-shell undo-tree super-save magit zenburn-theme yaml-mode web-mode volatile-highlights use-package toggle-quotes tern-auto-complete stylus-mode seq restart-emacs python-mode py-autopep8 popwin pip-requirements paredit pallet nsis-mode nginx-mode multi-term miniedit markdown-mode json-mode js2-refactor jedi idomenu highlight-symbol helm-projectile helm-flycheck helm-describe-modes helm-descbinds helm-bm helm-ag haskell-mode handoff golden-ratio go-mode ggtags free-keys flycheck-pos-tip flycheck-color-mode-line flycheck-checkbashisms ess erlang dockerfile-mode dired+ crux color-theme-modern color-theme coffee-mode cmake-mode auctex anzu ag ace-window ace-jump-buffer ac-js2)))
 '(pos-tip-background-color "#eee8d5")
 '(pos-tip-foreground-color "#586e75")
 '(reftex-plug-into-AUCTeX t)
 '(safe-local-variable-values (quote ((c-basic-offset 2) (c-noise-macro-names "UNINIT"))))
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3")
 '(weechat-color-list
   (quote
    (unspecified "#fdf6e3" "#eee8d5" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#657b83" "#839496")))
 '(whitespace-style
   (quote
    (face tabs spaces trailing space-before-tab newline indentation empty space-after-tab space-mark tab-mark newline-mark))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cursor ((t (:background "black"))))
 '(font-latex-string-face ((t (:foreground "indian red"))) t)
 '(font-latex-verbatim-face ((t (:inherit nil :foreground "SaddleBrown"))) t)
 '(font-lock-builtin-face ((t (:inherit default))))
 '(font-lock-constant-face ((t (:foreground "CadetBlue" :weight bold))))
 '(font-lock-function-name-face ((t (:foreground "dark red" :weight bold))))
 '(font-lock-keyword-face ((t (:foreground "blue"))))
 '(font-lock-string-face ((t (:foreground "dark green"))))
 '(font-lock-variable-name-face ((t (:foreground "navy" :weight bold))))
 '(helm-bookmark-file ((t (:inherit compilation-info))))
 '(helm-grep-lineno ((t (:inherit compilation-line-number))))
 '(helm-grep-match ((t (:inherit match))))
 '(helm-match ((t (:inherit match))))
 '(helm-moccur-buffer ((t (:inherit compilation-info))))
 '(helm-source-header ((t (:background "#abd7f0" :foreground "black" :weight bold :height 1.3))))
 '(js2-function-call ((t (:inherit default :foreground "dark red"))))
 '(js2-function-param ((t (:inherit font-lock-variable-name-face))))
 '(js2-object-property ((t (:inherit default :foreground "navy"))))
 '(show-paren-mismatch ((t (:foreground "red" :overline nil :underline nil :slant normal :weight bold))))
 '(web-mode-html-attr-engine-face ((t (:inherit web-mode-html-attr-name-face))))
 '(web-mode-html-attr-equal-face ((t (:inherit web-mode-tag-bracket-face))))
 '(web-mode-html-attr-name-face ((t (:foreground "blue" :weight bold))))
 '(web-mode-html-attr-value-face ((t (:foreground "forest green" :weight bold))))
 '(web-mode-html-tag-face ((t (:foreground "dark blue" :weight bold))))
 '(web-mode-json-key-face ((t (:inherit font-lock-keyword-face))))
 '(whitespace-space ((t (:foreground "lightgray"))))
 '(whitespace-tab ((t (:foreground "lightgray")))))
