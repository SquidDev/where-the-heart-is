(defun linum-custom-format (offset)
  "Custom formatting function for relative line numbers. Space for 2 numbers then a line"
  (format "%2d\u2502" (abs offset)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/backup"))))
 '(browse-url-browser-function (quote browse-url-text-emacs))
 '(browse-url-generic-program "lynx")
 '(company-auto-complete t)
 '(company-ghc-show-info t)
 '(company-idle-delay 0)
 '(custom-enabled-themes (quote (squid)))
 '(custom-file "~/.emacs.d/custom.el")
 '(custom-safe-themes
   (quote
    ("6b152e87b5d73d3c75b79554732ce75b4c29602863f17af65f3e7af663ca6c82" default)))
 '(diff-switches "-u")
 '(editorconfig-mode t)
 '(evil-toggle-key "")
 '(evil-want-fine-undo t)
 '(fill-column 120)
 '(flycheck-disabled-checkers (quote (haskell-stack-ghc)))
 '(flycheck-keymap-prefix "f")
 '(flycheck-syntax-check-failed-hook nil)
 '(global-company-mode t)
 '(global-flycheck-mode t)
 '(global-relative-line-numbers-mode t)
 '(global-whitespace-mode t)
 '(haskell-interactive-popup-errors nil)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-show-debug-tips nil)
 '(haskell-process-suggest-remove-import-lines t)
 '(helm-M-x-fuzzy-match t)
 '(helm-ff-file-name-history-use-recentf t)
 '(helm-ff-search-library-in-sexp t)
 '(helm-move-to-line-cycle-in-source t)
 '(helm-scroll-amount 8)
 '(helm-split-window-in-side-p t)
 '(hexl-bits 8)
 '(ibuffer-saved-filter-groups
   (quote
    (("Default"
      ("Terminals"
       (used-mode . term-mode))
      ("Programming"
       (saved . "programming"))
      ("Temporary"
       (or
        (name . "\\*.*\\*")
        (name . "\\*magit.*")))))))
 '(ibuffer-saved-filters
   (quote
    (("gnus"
      ((or
        (mode . message-mode)
        (mode . mail-mode)
        (mode . gnus-group-mode)
        (mode . gnus-summary-mode)
        (mode . gnus-article-mode))))
     ("programming"
      ((or
        (mode . c-mode)
        (mode . cperl-mode)
        (mode . emacs-lisp-mode)
        (mode . haskell-mode)
        (mode . idl-mode)
        (mode . inferior-emacs-lisp-mode)
        (mode . java-mode)
        (mode . javascript-mode)
        (mode . lisp-mode)
        (mode . lua-mode)
        (mode . perl-mode)
        (mode . php-mode)
        (mode . python-mode)
        (mode . web-mode)
        (mode . html-mode)
        (mode . yaml-mode)
        (name . "\\*scratch\\*")))))))
 '(ido-create-new-buffer (quote never))
 '(ido-enable-flex-matching t)
 '(ido-ignore-directories (quote ("\\`CVS/" "\\`\\.\\./" "\\`\\./")))
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(ispell-dictionary "en_GB")
 '(ispell-program-name "/usr/bin/aspell")
 '(linum-format (quote linum-custom-format))
 '(menu-bar-mode nil)
 '(multi-term-dedicated-select-after-open-p t)
 '(next-line-add-newlines nil)
 '(org-todo-keywords (quote ((sequence "TODO(t)" "FEEDBACK(f@)" "DONE(d)"))))
 '(projectile-global-mode t)
 '(relative-line-numbers-format (quote linum-custom-format))
 '(relative-line-numbers-max-count 0)
 '(require-final-newline t)
 '(ring-bell-function (quote ignore))
 '(save-place t nil (saveplace))
 '(save-place-file (concat user-emacs-directory ".saved-places"))
 '(savehist-mode t)
 '(scroll-bar-mode nil)
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(tramp-default-method "ssh")
 '(uniquify-buffer-name-style (quote post-forward-angle-brackets) nil (uniquify))
 '(whitespace-style
   (quote
    (face tabs trailing spaces indentation empty tab-mark space-mark))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
