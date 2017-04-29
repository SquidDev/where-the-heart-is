(eval-when-compile (require 'cl))

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(package-initialize)
(unless package-archive-contents (package-refresh-contents))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

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
    (package-require 'ido 'smex) ; Alternative to helm
    )
  (progn
    (package-require 'ido 'smex) ; Alternative to helm
    (package-require 'helm 'helm-projectile) ; Nicer interactive prompt
    (package-require 'magit) ; Git integration (requires recent Git and Emacs versions though)
    ))

; Core packages
(package-require 'evil)     ; Better editing
(package-require 'flycheck) ; Linter
(package-require 'company) ; Autocomplete
(package-require 'undo-tree)
(package-require 'neotree)
(package-require 'fill-column-indicator) ; Useful, but breaks company mode though
(package-require 'editorconfig)
(package-require 'multi-term) ; A terminal which works with zsh

; Various modes
(package-require 'lua-mode)
(package-require 'markdown-mode)
(package-require 'web-mode)
(package-require 'yaml-mode)
(package-require 'haskell-mode 'ghc 'company-ghc)

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
  (defun evil-default-emacs-state (mode)
    (delete mode evil-insert-state-modes)
    (add-to-list 'evil-emacs-state-modes mode))
  (mapc 'evil-default-emacs-state '(term-mode calculator-mode profiler-report-mode)))

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

(exec-if-load 'helm
  (require 'helm)
  (require 'helm-config)
  (require 'helm-projectile)

  (helm-mode 1)
  (helm-autoresize-mode 1)
  (helm-projectile-on)

  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))

  (global-set-key (kbd "M-x") 'helm-M-x)
  (exec-after-load 'term (define-key term-raw-map (kbd "C-c M-x") 'helm-M-x))

  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; rebind tab to run persistent action
  )

(exec-if-load 'ido
  (require 'ido)
  (ido-everywhere t)
  (ido-mode t)
  (global-set-key (kbd "M-x") 'smex))

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

(exec-if-load 'compnay-php
  (defun ac-php-keybindings ()
    (define-key evil-normal-state-local-map (kbd "gD") 'ac-php-find-symbol-at-point)
    (define-key evil-normal-state-local-map (kbd "gb") 'ac-php-location-stack-back)
    (define-key evil-normal-state-local-map (kbd "gh") 'ac-php-show-tip))
  (add-hook 'web-mode-hook 'ac-php-keybindings)
  (add-hook 'php-mode-hook 'ac-php-keybindings))

(exec-after-load 'haskell-mode
  (add-hook 'haskell-mode-hook (lambda () (ghc-init)))
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (add-to-list 'company-backends 'company-ghc))

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

(add-to-list 'auto-mode-alist '("\\blog\\'" . (lambda () (auto-revert-tail-mode t))))
(add-hook 'ibuffer-hook (lambda() (ibuffer-switch-to-saved-filter-groups "Default")))
(add-hook 'post-command-hook 'xterm-title-update)

(defadvice show-paren-function (after show-matching-paren-offscreen activate)
  "If the matching paren is offscreen, show the matching line in the echo area.
Has no effect if the character before point is not of the syntax class ')'."
  (interactive)
  (let* ((cb (char-before (point)))
         (matching-text (and cb
                             (char-equal (char-syntax cb) ?\) )
                             (blink-matching-open))))
    (when matching-text (message matching-text))))

(provide '.emacs)
