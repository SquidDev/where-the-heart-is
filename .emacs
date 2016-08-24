;; (add-to-list 'load-path "/home/bonzo/benchmark-init-el")
;; (require 'benchmark-init-loaddefs)
;; (benchmark-init/activate)

(eval-when-compile (require 'cl))
(require 'package)

(setq frame-title-format
  (concat  "%b - emacs@" (system-name)))

; default to unified diffs
(setq diff-switches "-u")

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(package-initialize)

(defvar package-list '(
  ; Nice themes
  base16-theme
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

; Basic Things
(set-language-environment "UTF-8")
(fset 'yes-or-no-p 'y-or-n-p)

(defun relative-line-numbers-custom-format (offset)
  "Custom formatting function for relative line numbers. Space for 2 numbers then a line"
  (format "%2d\u2502" (abs offset)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/backup"))))
 '(company-ghc-show-info t)
 '(custom-enabled-themes (quote (base16-summerfruit-light)))
 '(custom-safe-themes
   (quote
    ("9e76732c9af8e423236ff8e37dd3b9bc37dacc256e42cc83810fb824eaa529b9" default)))
 '(editorconfig-mode t)
 '(evil-toggle-key "")
 '(evil-want-fine-undo t)
 '(flycheck-keymap-prefix "f")
 '(flycheck-syntax-check-failed-hook nil)
 '(global-relative-line-numbers-mode t)
 '(global-whitespace-mode t)
 '(helm-M-x-fuzzy-match t)
 '(helm-ff-file-name-history-use-recentf t)
 '(helm-ff-search-library-in-sexp t)
 '(helm-move-to-line-cycle-in-source t)
 '(helm-scroll-amount 8)
 '(helm-split-window-in-side-p t)
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
        (mode . perl-mode)
        (mode . php-mode)
        (mode . python-mode)
        (mode . web-mode)
        (name . "\\*scratch\\*")))))))
 '(inhibit-startup-screen t)
 '(multi-term-dedicated-select-after-open-p t)
 '(next-line-add-newlines nil)
 '(relative-line-numbers-format (quote relative-line-numbers-custom-format))
 '(relative-line-numbers-max-count 0)
 '(require-final-newline t)
 '(ring-bell-function (quote ignore) t)
 '(save-place-file (concat user-emacs-directory ".saved-places"))
 '(tab-width 4)
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
 '(font-lock-comment-delimiter-face ((t (:foreground "#707070"))))
 '(font-lock-comment-face ((t (:foreground "#909090"))))
 '(helm-ff-directory ((t (:foreground "#707070" :weight bold))))
 '(helm-selection ((t (:background "#B0B0B0" :underline nil))))
 '(highlight ((t (:background "#202020" :foreground "#B0B0B0"))))
 '(linum ((t (:inherit default :foreground "#505050" :background nil))))
 '(region ((t (:background "#B0B0B0"))))
 '(whitespace-hspace ((t (:foreground "#B0B0B0" :background nil))))
 '(whitespace-indentation ((t (:foreground "#B0B0B0" :background nil))))
 '(whitespace-space ((t (:foreground "#B0B0B0" :background nil))))
 '(whitespace-tab ((t (:foreground "#B0B0B0" :background nil)))))

; Whitespace highlighting
(autoload 'global-whitespace-mode "whitespace" "Toggle whitespace visualization." t)

; Save location
(setq-default save-place t)
(require 'saveplace)

; Unique buffers
(require 'uniquify)

(autoload 'editorconfig-mode "editorconfig" "Toggle EditorConfig feature." t)
(autoload 'relative-line-numbers-mode "relative-line-numbers" "Toggle Relative Line Numbers on or off." t)

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
(add-hook 'after-init-hook #'global-flycheck-mode)

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
; (helm-flx-mode 1)
; (helm-fuzzier-mode 1)
(helm-autoresize-mode 1)
(helm-projectile-on)

(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(global-set-key (kbd "M-x") 'helm-M-x)
(eval-after-load 'term '(progn
  (define-key term-raw-map (kbd "C-c M-x") 'helm-M-x)))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
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

(setq-default indent-tabs-mode nil)
(global-set-key (kbd "<select>") 'move-end-of-line)

(xterm-mouse-mode t)
(global-set-key (kbd "<mouse-4>") 'scroll-down-line)
(global-set-key (kbd "<mouse-5>") 'scroll-up-line)
(global-set-key (kbd "C-/") 'comment-or-uncomment-region)

(add-to-list 'auto-mode-alist '("\\.gmk\\'" . makefile-mode))

; GHC mod
(autoload 'ghc-init "ghc" "GHC integration" t)
(autoload 'ghc-debug "ghc" "GHC debugging integration" t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))
;; (add-hook 'haskell-mode-hook 'interactive-haskell-mode)

; Company
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(add-to-list 'company-backends 'company-ghc)

; Company php
(autoload 'company-ac-php-backend "company-php" "Company php integration" t)
(add-to-list 'company-backends 'company-ac-php-backend) ; You have to manually patch it to allow web-mode and to run on PHP 5.3
(projectile-global-mode)

; Magit
(autoload 'magit-status "magit" "Show the status of the current Git repository in a buffer.
With a prefix argument prompt for a repository to be shown.
With two prefix arguments prompt for an arbitrary directory.
If that directory isn't the root of an existing repository
then offer to initialize it as a new repository." t)
(global-set-key (kbd "C-c m") 'magit-status)

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
