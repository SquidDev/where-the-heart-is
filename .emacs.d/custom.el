(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-default nil)
 '(backup-directory-alist (quote (("." . "~/.emacs.d/backup"))))
 '(browse-url-generic-program "lynx")
 '(company-auto-complete t)
 '(company-ghc-show-info t)
 '(company-idle-delay 0.3)
 '(company-tooltip-align-annotations t)
 '(current-language-environment "UTF-8")
 '(custom-enabled-themes (quote (zeromini)))
 '(custom-file "~/.emacs.d/custom.el")
 '(custom-safe-themes t)
 '(diff-switches "-u")
 '(editorconfig-mode t)
 '(evil-toggle-key "")
 '(evil-want-fine-undo t)
 '(explicit-shell-file-name "zsh")
 '(fill-column 120)
 '(flycheck-checker-error-threshold 500)
 '(flycheck-clang-args (quote ("--std=c++17")))
 '(flycheck-disabled-checkers (quote (haskell-stack-ghc)))
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
        (mode . makefile-gmake-mode)
        (mode . makefile-mode)
        (mode . markdown-mode)
        (mode . perl-mode)
        (mode . php-mode)
        (mode . python-mode)
        (mode . ruby-mode)
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
 '(ispell-program-name "hunspell")
 '(menu-bar-mode nil)
 '(multi-term-dedicated-select-after-open-p t)
 '(next-line-add-newlines nil)
 '(ns-command-modifier (quote meta))
 '(org-todo-keywords (quote ((sequence "TODO(t)" "FEEDBACK(f@)" "DONE(d)"))))
 '(projectile-rails-global-mode t)
 '(require-final-newline t)
 '(ring-bell-function (quote ignore))
 '(ruby-insert-encoding-magic-comment nil)
 '(save-place-file (concat user-emacs-directory ".saved-places"))
 '(savehist-mode t)
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(tramp-default-method "ssh" nil (tramp))
 '(uniquify-buffer-name-style (quote post-forward-angle-brackets) nil (uniquify))
 '(which-key-mode t)
 '(whitespace-style
   (quote
    (face tabs trailing spaces indentation empty tab-mark space-mark)))
 '(xterm-mouse-mode t))
