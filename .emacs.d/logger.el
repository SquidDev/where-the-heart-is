;;; logger -- Highlight log files
;;; Commentary:
;; Attempted highlighting of log files
;;; Code:

(setq logger-mode-actions (regexp-opt '("[error]" "[notice]" "[Error]" "[Notice]" "[Timings]" "[Debug]" "[Takedown]")))
(setq logger-mode-date "^\\[[A-Z][a-z][a-z] [A-Z][a-z][a-z] [0-9][0-9] [0-9][0-9]:[0-9][0-9]:[0-9][0-9] [0-9]\\{4\\}\\]")
(setq logger-mode-date-alt "^[0-9]\\{4\\}-[0-9][0-9]-[0-9][0-9] [0-9][0-9]:[0-9][0-9]:[0-9][0-9] UTC ([0-9]+)")
(setq logger-mode-ip (regexp-quote "[client 127.0.0.1]"))
(setq logger-mode-php-trace-item "PHP +[0-9]+\\. [A-Za-z0-9_{}():]+")
(setq logger-mode-php-trace-item-alt "^#[0-9]")
(setq logger-mode-php-trace-head (concat (regexp-opt '("PHP Stack trace" "PHP Fatal error" "PHP Notice" "PHP Parse error" "Notice" "Stack trace")) ":.*"))
(setq logger-mode-highlights `(
  (,logger-mode-actions . font-lock-variable-name-face)
  (,logger-mode-date . font-lock-constant-face)
  (,logger-mode-date-alt . font-lock-constant-face)
  (,logger-mode-ip . font-lock-comment-face)
  (,logger-mode-php-trace-item . font-lock-function-name-face)
  (,logger-mode-php-trace-item-alt . font-lock-function-name-face)
  (,logger-mode-php-trace-head . font-lock-warning-face)))
(define-derived-mode logger-mode fundamental-mode
  (setq font-lock-defaults '(logger-mode-highlights))
  (setq mode-name "Apache Log"))

(add-hook 'logger-mode-hook (lambda () (auto-revert-tail-mode t)))
(add-to-list 'auto-mode-alist '("error_log\\'" . logger-mode))
(add-to-list 'auto-mode-alist '("\\.log\\'" . logger-mode))
(provide 'logger)
;;; logger.el ends here
