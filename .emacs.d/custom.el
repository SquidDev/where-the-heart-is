(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-default nil)
 '(backup-directory-alist (quote (("." . "~/.emacs.d/backup"))))
 '(browse-url-browser-function (quote browse-url-text-emacs))
 '(browse-url-generic-program "lynx")
 '(column-number-mode t)
 '(company-auto-complete t)
 '(company-ghc-show-info t)
 '(company-idle-delay 0.3)
 '(company-tooltip-align-annotations t)
 '(current-language-environment "UTF-8")
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
 '(flycheck-checker-error-threshold 500)
 '(flycheck-clang-args (quote ("--std=c++17")))
 '(flycheck-disabled-checkers (quote (haskell-stack-ghc)))
 '(flycheck-keymap-prefix "f")
 '(flycheck-syntax-check-failed-hook nil)
 '(global-whitespace-mode t)
 '(haskell-interactive-popup-errors nil)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-show-debug-tips nil)
 '(haskell-process-suggest-remove-import-lines t)
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
        (name . "magit[^:]*:.*")
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
        (mode . html-mode)
        (mode . idl-mode)
        (mode . inferior-emacs-lisp-mode)
        (mode . java-mode)
        (mode . javascript-mode)
        (mode . js-mode)
        (mode . lisp-mode)
        (mode . lua-mode)
        (mode . makefile-mode)
        (mode . makefile-gmake-mode)
        (mode . markdown-mode)
        (mode . perl-mode)
        (mode . php-mode)
        (mode . python-mode)
        (mode . scss-mode)
        (mode . typescript-mode)
        (mode . web-mode)
        (mode . yaml-mode)
        (name . "\\*scratch\\*")))))))
 '(ido-create-new-buffer (quote never))
 '(ido-enable-flex-matching t)
 '(ido-ignore-directories (quote ("\\`CVS/" "\\`\\.\\./" "\\`\\./")))
 '(ido-use-faces nil)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(ispell-dictionary "en_GB")
 '(ispell-program-name "/usr/bin/aspell")
 '(menu-bar-mode nil)
 '(multi-term-dedicated-select-after-open-p t)
 '(next-line-add-newlines nil)
 '(org-todo-keywords (quote ((sequence "TODO(t)" "FEEDBACK(f@)" "DONE(d)"))))
 '(projectile-mode t)
 '(require-final-newline t)
 '(ring-bell-function (quote ignore))
 '(save-place-file (concat user-emacs-directory ".saved-places"))
 '(savehist-mode t)
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(tramp-default-method "ssh")
 '(uniquify-buffer-name-style (quote post-forward-angle-brackets) nil (uniquify))
 '(whitespace-style
   (quote
    (face tabs trailing spaces indentation empty tab-mark space-mark)))
 '(xterm-mouse-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flx-highlight-face ((t (:inherit ido-first-match)))))
