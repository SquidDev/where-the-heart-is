(eval-when-compile (require 'cl))

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(package-initialize)
(unless package-archive-contents (package-refresh-contents))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(setq backup-directory-alist '(("." . "~/.emacs.d/backup")))

(defvar package-required ())
(defun package-require (&rest packages)
  "Install PACKAGES if they are not already installed."
  (dolist (package packages)
    (add-to-list 'package-required package)
    (unless (package-installed-p package)
      (package-install package))))

; Version specific p
(if (version< emacs-version "24.4")
  (progn
    )
  (progn
    (package-require 'magit) ; Git integration (requires recent Git and Emacs versions though)
    ))

; Core packages
(package-require 'ido 'ido-completing-read+ 'smex 'flx-ido) ; Nicer M-x and co.
(package-require 'evil)     ; Better editing
(package-require 'flycheck) ; Linter
(package-require 'company) ; Autocomplete
(package-require 'undo-tree)
(package-require 'neotree)
(package-require 'editorconfig)
(package-require 'projectile) ;; Project management
(package-require 'multi-term) ; A terminal which works with zsh
(package-require 'nix-sandbox) ; Required for ghc/hlint to function correctly under nix.

; Various modes
(package-require 'lua-mode)
(package-require 'markdown-mode)
(package-require 'web-mode)
(package-require 'yaml-mode)
(package-require 'haskell-mode 'company-ghci)
(package-require 'irony 'flycheck-irony)

(package-require 'rust-mode 'flycheck-rust 'racer)

(defun package-required-p (package)
  "Check if the specified PACKAGE is loaded."
  (memq package package-required))

(defmacro exec-after-load (file &rest body)
  "Once FILE is loaded, execute BODY.
FILE is normally a feature name, but it can also be a file name,
in case that file does not provide any feature."
  (declare (indent 1) (debug t))
  `(eval-after-load ,file (lambda ()
    (if (package-required-p ,file) (progn ,@body)))))

(defmacro exec-if-load (package &rest body)
  "If PACKAGE is loaded, execute BODY."
  (declare (indent 1) (debug t))
  `(if (package-required-p ,package) (progn ,@body)))

; Prepare to setup the basic code
(progn
  (set-language-environment "UTF-8")
  (fset 'yes-or-no-p 'y-or-n-p)
  (setq frame-title-format
        (concat  "%b - emacs@" (system-name))))

(exec-if-load 'evil
  (require 'evil)
  (evil-mode t)
  (define-key evil-insert-state-map (kbd "C-x TAB") 'indent-relative)

  (evil-set-initial-state 'term-mode 'emacs)
  (evil-set-initial-state 'calculator-mode 'emacs)
  (evil-set-initial-state 'profiler-report-mode 'emacs)
  (evil-set-initial-state 'neotree-mode 'emacs)

  (evil-set-initial-state 'git-commit-mode 'normal))

(exec-if-load 'company (require 'company))

(exec-after-load 'flycheck
  (flycheck-add-mode 'html-tidy 'web-mode)
  (flycheck-add-mode 'php 'web-mode)
  (flycheck-add-mode 'css-csslint 'web-mode)

  (add-to-list 'display-buffer-alist
    `(,(rx bos "*Flycheck errors*" eos)
      (display-buffer-reuse-window
       display-buffer-in-side-window)
      (reusable-frames . visible)
      (side            . bottom)
      (window-height   . 0.4)))
  (setq flycheck-command-wrapper-function
    (lambda (args)
      (if (nix-current-sandbox)
        (apply 'nix-shell-command (nix-current-sandbox) args)
        args)))
  (setq flycheck-executable-find
    (lambda (cmd)
      (if (nix-current-sandbox)
        (nix-executable-find (nix-current-sandbox) cmd)
        cmd)))

  (defun quit-bottom-side-windows ()
    "Quit side windows of the current frame."
    (interactive)
    (dolist (window (window-at-side-list))
      (quit-window nil window)))
  (global-set-key (kbd "C-c q") #'quit-bottom-side-windows))

(exec-if-load 'neotree
  (add-hook 'neotree-mode-hook
    (lambda ()
      (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
      (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-enter)
      (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
      (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)))
  (global-set-key (kbd "C-\\") 'neotree-toggle))

(exec-if-load 'ido
  (require 'ido)
  (require 'ido-completing-read+)
  (require 'flx-ido)
  (ido-everywhere t)
  (ido-mode t)
  (ido-ubiquitous-mode 1)
  (flx-ido-mode)
  (global-set-key (kbd "M-x") 'smex))

(exec-if-load 'lua-mode
  (add-to-list 'auto-mode-alist '("\\.rockspec\\'" . lua-mode)))

(exec-if-load 'web-mode
  (require 'web-mode)
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mhtml\\'" . web-mode))
  (setq web-mode-engines-alist
        '(("mason"    . "\\.mhtml\\'")
          ("blade"  . "\\.blade\\."))))

(exec-if-load 'company-php
  (defun ac-php-keybindings ()
    (define-key evil-normal-state-local-map (kbd "gD") 'ac-php-find-symbol-at-point)
    (define-key evil-normal-state-local-map (kbd "gb") 'ac-php-location-stack-back)
    (define-key evil-normal-state-local-map (kbd "gh") 'ac-php-show-tip))
  (add-hook 'web-mode-hook 'ac-php-keybindings)
  (add-hook 'php-mode-hook 'ac-php-keybindings))

(exec-after-load 'haskell-mode
  (require 'haskell-mode)
  (require 'haskell-interactive-mode)
  (require 'haskell-process)
  (setq haskell-process-wrapper-function
    (lambda (args)
      (if (nix-current-sandbox)
        (apply 'nix-shell-command (nix-current-sandbox) args)
        args)))
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (add-to-list 'company-backends 'company-ghci))

(exec-after-load 'rust-mode
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode)

  (add-hook 'racer-mode-hook #'company-mode)
  (require 'rust-mode)
  (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
  (setq company-tooltip-align-annotations t))

(exec-if-load 'irony
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)

  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
  (eval-after-load 'flycheck
    '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup)))

(add-to-list 'auto-mode-alist '("\\.pddl\\'" . lisp-mode))

(progn
  (add-to-list 'auto-mode-alist '("\\.ml\\'" . tuareg-mode))
  (add-to-list 'auto-mode-alist '("\\.mli\\'" . tuareg-mode))

 (push "/home/squid/.opam/4.06.1/share/emacs/site-lisp" load-path)
 (setq merlin-command "/home/squid/.opam/4.06.1/bin/ocamlmerlin")
 (autoload 'merlin-mode "merlin" "Merlin mode" t)
 (autoload 'tuareg-mode "tuareg" "Tuareg mode" t)
 (add-hook 'tuareg-mode-hook 'merlin-mode)
 (add-hook 'caml-mode-hook 'merlin-mode)

 (eval-after-load 'company
   (eval-after-load 'merlin-mode
     (add-to-list 'company-backends 'merlin-company-backend))))

(unless (display-graphic-p)
  ; Show title in buffer
  (defvar last-buffer "")
  (defun xterm-title-update ()
    (interactive)
    (if (string= last-buffer (buffer-name)) nil
      (setq last-buffer (buffer-name))
      (send-string-to-terminal (concat "\033]2; " (if buffer-file-name (buffer-file-name) (buffer-name)) " - emacs\007")))))

; General
(defalias 'repl 'ielm)
(show-paren-mode 1)
(xterm-mouse-mode t)
(global-set-key (kbd "C-l") 'kill-whole-line)
(global-set-key (kbd "<select>") 'move-end-of-line)
(global-set-key (kbd "<mouse-4>") 'scroll-down-line)
(global-set-key (kbd "<mouse-5>") 'scroll-up-line)
(global-set-key (kbd "C-/") 'comment-or-uncomment-region)

(add-hook 'ibuffer-hook (lambda() (ibuffer-switch-to-saved-filter-groups "Default")))
(add-hook 'post-command-hook 'xterm-title-update)

(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(defadvice show-paren-function (after show-matching-paren-offscreen activate)
  "If the matching paren is offscreen, show the matching line in the echo area.
Has no effect if the character before point is not of the syntax class ')'."
  (interactive)
  (let* ((cb (char-before (point)))
         (matching-text (and cb
                             (char-equal (char-syntax cb) ?\) )
                             (blink-matching-open))))
    (when matching-text (message matching-text))))

(defun align-space (start end)
  "Repeat alignment using spaces as the delimiter between START and END."
  (interactive "r")
  (align-regexp start end "\\(\\s-*\\) " 1 0 t))

(provide '.emacs)
