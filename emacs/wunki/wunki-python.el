;;; package --- Setup Python development environment

(defun wunki-python-defaults ()
  "Default modes for Python programming"
  (subword-mode +1)
  (anaconda-mode)
  (eldoc-mode))

(add-hook 'python-mode-hook 'wunki-python-defaults)

(provide 'wunki-python)
