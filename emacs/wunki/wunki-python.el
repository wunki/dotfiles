;;; package --- Setup Python development environment

(defun wunki-python-defaults ()
  "Default modes for Python programming"
  (subword-mode +1)
  (pyenv-mode)
  (flycheck-mode)
  (set (make-local-variable 'company-backends) '(company-anaconda))
  (anaconda-eldoc-mode)
  (anaconda-mode))

(custom-set-variables
  '(python-shell-interpreter "python2"))

(add-hook 'python-mode-hook 'wunki-python-defaults)

(provide 'wunki-python)
