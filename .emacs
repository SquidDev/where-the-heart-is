(eval-when-compile (require 'cl))
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(package-initialize)
; (benchmark-init/activate)

(defvar package-list '(
  ; Linter
  flycheck
  ; Ctrl+P
  helm helm-projectile
  ; Autocomplete
  company
  ; Various editing modes, enable on demand
  ; lua-mode
  ; markdown-mode
  web-mode
  ; yaml-mode
  ; haskell-mode ghc company-ghc

  ; Not actually used ATM
  ; multiple-cursors
  ; File browser
  neotree
  ; Git Client
  magit
  ; Better undo
  undo-tree
  ; 'Better' keybindings
  evil
  ; A terminal which works with zsh
  multi-term
  ; Editor config
  editorconfig))

; fetch the list of packages available
(unless package-archive-contents (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

; Basic Things
(set-language-environment "UTF-8")
(fset 'yes-or-no-p 'y-or-n-p)
(setq frame-title-format
  (concat  "%b - emacs@" (system-name)))

(defun linum-custom-format (offset)
  "Custom formatting function for relative line numbers. Space for 2 numbers then a line"
  (format "%2d\u2502" (abs offset)))

; Evil doesn't seem to work with autoload
(require 'evil)
(evil-mode t)
(define-key evil-insert-state-map (kbd "C-x TAB") 'indent-relative)
(defun evil-default-emacs-state (mode)
  (delete mode evil-insert-state-modes)
  (add-to-list 'evil-emacs-state-modes mode))
(evil-default-emacs-state 'term-mode)
(evil-default-emacs-state 'calculator-mode)

; Flycheck config
(with-eval-after-load 'flycheck
  (flycheck-add-mode 'html-tidy 'web-mode)
  (flycheck-add-mode 'php 'web-mode)
  (flycheck-add-mode 'css-csslint 'web-mode))

(add-to-list 'display-buffer-alist
 `(,(rx bos "*Flycheck errors*" eos)
  (display-buffer-reuse-window
  display-buffer-in-side-window)
  (reusable-frames . visible)
  (side            . bottom)
  (window-height   . 0.4)))

; Close side windows
(defun quit-bottom-side-windows ()
  "Quit side windows of the current frame."
  (interactive)
  (dolist (window (window-at-side-list))
    (quit-window nil window)))

; General key bindings
(global-set-key (kbd "C-c q") #'quit-bottom-side-windows)
(global-set-key (kbd "C-\\") 'neotree-toggle)
(global-set-key (kbd "C-l") 'kill-whole-line)

(add-hook 'neotree-mode-hook
  (lambda ()
    (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
    (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-enter)
    (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
    (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)))

; Helm Mode
(require 'helm)
(require 'helm-config)
(require 'helm-projectile)

(helm-mode 1)
(helm-autoresize-mode 1)
(helm-projectile-on)

(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(global-set-key (kbd "M-x") 'helm-M-x)
(eval-after-load 'term '(progn
  (define-key term-raw-map (kbd "C-c M-x") 'helm-M-x)))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

; Web mode
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
       ("blade"  . "\\.blade\\.")))
(defun ac-php-keybindings ()
    (define-key evil-normal-state-local-map (kbd "gD") 'ac-php-find-symbol-at-point)
    (define-key evil-normal-state-local-map (kbd "gb") 'ac-php-location-stack-back)
    (define-key evil-normal-state-local-map (kbd "gh") 'ac-php-show-tip))
(add-hook 'web-mode-hook 'ac-php-keybindings)
(add-hook 'php-mode-hook 'ac-php-keybindings)

(global-set-key (kbd "<select>") 'move-end-of-line)

(xterm-mouse-mode t)
(global-set-key (kbd "<mouse-4>") 'scroll-down-line)
(global-set-key (kbd "<mouse-5>") 'scroll-up-line)
(global-set-key (kbd "C-/") 'comment-or-uncomment-region)

(add-to-list 'auto-mode-alist '("\\.gmk\\'" . makefile-mode))
(add-to-list 'auto-mode-alist '("\\blog\\'" . (lambda () (auto-revert-tail-mode t))))

; GHC mod
(autoload 'ghc-init "ghc" "GHC integration" t)
(autoload 'ghc-debug "ghc" "GHC debugging integration" t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))
;; (add-hook 'haskell-mode-hook 'interactive-haskell-mode)

; Company
(require 'company)
(add-to-list 'company-backends 'company-ghc)

; Company php
(autoload 'company-ac-php-backend "company-php" "Company php integration" t)
(add-to-list 'company-backends 'company-ac-php-backend) ; You have to manually patch it to allow web-mode and to run on PHP 5.3

; General
(show-paren-mode 1)
(defalias 'repl 'ielm)

; Show title in buffer
(defvar last-buffer "")
(defun xterm-title-update ()
  (interactive)
  (if (string= last-buffer (buffer-name)) nil
    (setq last-buffer (buffer-name))
    (send-string-to-terminal (concat "\033]2; " (if buffer-file-name (buffer-file-name) (buffer-name)) " - emacs\007"))))

(add-hook 'post-command-hook 'xterm-title-update)
(add-hook 'ibuffer-hook (lambda() (ibuffer-switch-to-saved-filter-groups "Default")))

(defvar ssh-term-history nil)

(eval-after-load 'savehist
  '(add-to-list 'savehist-additional-variables 'ssh-term-history))

(defun ssh-term (args)
  "Connect to a remote host by SSH."
  (interactive
   (list (read-from-minibuffer "ssh " nil nil nil 'ssh-term-history)))
  (let* ((switches (split-string-and-unquote args))
         (name (concat "ssh " args))
         (termbuf (apply 'make-term name "ssh" nil switches)))
    (set-buffer termbuf)
    (term-mode)
    (term-char-mode)
    (switch-to-buffer termbuf)))

(provide '.emacs)
;;; .emacs ends here
