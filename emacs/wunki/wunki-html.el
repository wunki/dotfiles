;; websites
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(setq web-mode-engines-alist '(("django" . "\\.html\\'")))

(add-hook 'web-mode-hook
          '(lambda()
             (setq web-mode-markup-indent-offset 2)
             (setq web-mode-css-indent-offset 2)
             (setq web-mode-code-indent-offset 2)
             (setq web-mode-style-padding 2)
             (setq web-mode-script-padding 2)
             (setq indent-tabs-mode nil)))

(provide 'wunki-html)
