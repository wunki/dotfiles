;; configuration for Erlang
(setq load-path (cons  "/usr/lib/erlang/lib/tools-2.6.10/emacs" load-path))
(setq erlang-root-dir "/usr/lib/erlang")
(setq exec-path (cons "/usr/lib/erlang/bin" exec-path))

;; use EDTS when available
;; (when (file-directory-p "/home/wunki/src/edts")
;;   (add-to-list 'load-path "/home/wunki/src/edts")
;;   (require 'edts-start))

(require 'erlang-start)
