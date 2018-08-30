;; Setup package.el and get everything else running.
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)
(unless package-archive-contents (package-refresh-contents))

;; Load our custom file as soon as possible. This ensures our theme other
;; appearance options are set up straight away.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file t)

(setq package-selected-packages '())

(defun package-require (&rest packages)
  "Install PACKAGES if they are not already installed."
  (dolist (package packages)
    (add-to-list 'package-selected-packages package)
    (unless (package-installed-p package)
      (package-install package))))

(defconst *enabled-modules*
  '(ido
    company
    evil
    flycheck
    git-gutter
    magit
    powerline
    projectile
    term

    ;; Various language modes
    elisp
    ;; haskell
    ;; javascript
    ;; lua
    ;; markdown
    ;; ocaml
    org
    ;; rainbow
    ;; ruby
    ;; rust
    ;; typescript
    ;; web
    ;; yaml

    nil))

(defmacro defmodule (module &rest body)
  "Define a module BODY which will only be evaluated when MODULE is in the *enabled-modules* set."
  `(when (memq ',module *enabled-modules*) ,@body))

(eval-when-compile (require 'cl))

(defun register-extensions (mode &rest exts)
  "Register a list of extensions EXTS to load with a particular MODE."
  (dolist (ext exts)
    (add-to-list 'auto-mode-alist `(,(concat "\\" ext "\\'") . ,mode))))

(package-require 'editorconfig 'which-key)

(defmodule ido
  (package-require 'ido-completing-read+ 'smex 'flx-ido)

  (ido-mode t)
  (ido-everywhere t)
  (ido-ubiquitous-mode 1)

  (flx-ido-mode)

  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands))

(defmodule company
  (package-require 'company)
  (add-hook 'after-init-hook 'global-company-mode)

  (global-set-key (kbd "TAB") 'company-indent-or-complete-common))

(defmodule evil
  (package-require 'evil 'undo-tree)

  (evil-mode t)

  (define-key evil-insert-state-map (kbd "C-x TAB") 'indent-relative)
  (define-key evil-normal-state-map (kbd "gc") 'whitespace-cleanup)
  (define-key evil-normal-state-map (kbd "g C-g") 'count-words)

  ;; I'm a bad person, but I like mouse keys
  (define-key evil-window-map [left]  'evil-window-left)
  (define-key evil-window-map [right] 'evil-window-right)
  (define-key evil-window-map [up]    'evil-window-up)
  (define-key evil-window-map [down]  'evil-window-down)

  ;; Change a couple of modes to use Emacs keybindings instead
  (dolist (mode '(term-mode
                  calculator-mode
                  profiler-report-mode
                  neotree-mode
                  image-mode))
    (evil-set-initial-state mode 'emacs))

  ;; Change a couple of modes to use Vim keybindings
  (dolist (mode '(git-commit-mode))
    (evil-set-initial-state mode 'normal)))

(defmodule flycheck
  (package-require 'flycheck 'nix-sandbox)

  (add-hook 'after-init-hook 'global-flycheck-mode)
  (add-to-list 'which-key-replacement-alist '((nil . "^flycheck-") . (nil . "f-")))

  ;; Ensure the window is displayed in a sensible place
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*Flycheck errors*" eos)
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (side            . bottom)
                 (reusable-frames . visible)
                 (window-height   . 0.33)))

  ;; Patch flycheck to work with nix
  (setq flycheck-command-wrapper-function
        (lambda (args)
          (if (nix-current-sandbox)
              (apply 'nix-shell-command (nix-current-sandbox) args)
            args)))
  (setq flycheck-executable-find
        (lambda (cmd)
              (nix-executable-find (nix-current-sandbox) cmd))))

(defmodule git-gutter
  (package-require 'git-gutter)
  (global-set-key (kbd "C-c m g") 'git-gutter-mode))

(defmodule magit
  (package-require 'magit)

  (with-eval-after-load 'git-commit
    (add-hook 'git-commit-mode-hook 'turn-on-orgstruct)
    (add-hook 'git-commit-mode-hook 'turn-on-orgstruct++)
    (add-hook 'git-commit-mode-hook (lambda ()
      (setq paragraph-start "\f\\|[ \t]*$\\|[ \t]*[-+*] "
            paragraph-separate "$"
            fill-column 72))))

  (with-eval-after-load 'evil
    ;; Switch to Emacs mode when entering blame (and revert when
    ;; leaving): means we can actually use all the key-bindings.
    (add-hook 'magit-blame-mode-hook (lambda ()
      (if magit-blame-mode
        (evil-emacs-state)
        (evil-exit-emacs-state))))))

(defmodule powerline
  (package-require 'spaceline)

  ;; Ideally we'd just depend on powerline, but this'll do for now.
  (require 'spaceline)
  (require 'spaceline-segments)
  (spaceline-compile 'squid
    '((evil-state
       :face highlight-face
       :priority 100)
      ((buffer-modified buffer-size buffer-id remote-host)
       :priority 98)
      (major-mode :priority 79)
      (process)
      ((flycheck-error flycheck-warning flycheck-info)
       :when active :priority 89)
      (projectile-root :when active :prority 70)
      (version-control :when active :priority 78))
    '((selection-info :priority 95)
      ((buffer-encoding-abbrev
        point-position
        line-column)
       :separator " | "
       :priority 96)
      (global :when active)
      (buffer-position :priority 99)))

  (setq-default mode-line-format '("%e" (:eval (spaceline-ml-squid)))))

(defmodule projectile
  ;; While ripgrep and multi-term aren't strictly needed, this is the only
  ;; module which really depends on them.
  (package-require 'projectile 'multi-term 'ripgrep)

  (add-hook 'after-init-hook (lambda ()
    (projectile-mode)
    ;; Yes, I'm a heathen. But this is more sane than C-c C-p.
    ;;
    ;; Technically projectile-keymap-prefix does this. It doesn't appear to work
    ;; for me though, so we'll go this route.
    (define-key projectile-mode-map (kbd "C-c C-p") nil)
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)))

  (add-to-list 'which-key-replacement-alist '((nil . "^projectile-") . (nil . "p-")))

  (with-eval-after-load 'projectile
    (defun projectile-run-multi-term ()
      "Invoke `multi-term' in the project's root."
      (interactive)
      (projectile-with-default-dir (projectile-project-root)
        (require 'multi-term)
        (let ((multi-term-buffer-name (concat multi-term-buffer-name "-" (projectile-project-name))))
          (multi-term))))

    (define-key projectile-command-map (kbd "x m") #'projectile-run-multi-term)))

(defmodule term
  (package-require 'eterm-256color)
  (add-hook 'term-mode-hook #'eterm-256color-mode)

  (add-hook 'term-mode-hook (lambda ()
    ;; Always send C-r to the term. I never use it in Emacs after all.
    (define-key term-raw-map (kbd "C-r") 'term-send-raw))))

;; Language specific modes

(defmodule elisp
  (with-eval-after-load 'elisp-mode
    ;; Add keybindings to execute code
    (define-key emacs-lisp-mode-map (kbd "C-c C-r") 'eval-region)
    (define-key emacs-lisp-mode-map (kbd "C-c C-l") 'eval-buffer)))

(defmodule haskell
  (package-require 'haskell-mode 'company-ghci 'nix-sandbox)

  (with-eval-after-load 'haskell-mode
    (require 'haskell-mode)
    (require 'haskell-interactive-mode)
    (require 'haskell-process))

  ;; Patch haskell-mode to work with nix
  (setq haskell-process-wrapper-function
    (lambda (args)
      (if (nix-current-sandbox)
        (apply 'nix-shell-command (nix-current-sandbox) args)
        args)))

  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)

  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-ghci)))

(defmodule javascript
  (package-require 'js2-mode 'js2-refactor 'rjsx-mode)

  (register-extensions 'rjsx-mode ".js" ".jsx")

  (add-hook 'js2-mode-hook #'js2-refactor-mode)

  (with-eval-after-load 'js2-mode
    (evil-make-overriding-map js2-mode-map 'normal)
    (evil-define-key 'normal js2-mode-map
      "gd" 'js2-jump-to-definition))

  (with-eval-after-load 'js2-refactor
    (js2r-add-keybindings-with-prefix "C-c j")))

(defmodule lua
  (package-require 'lua-mode)
  (register-extensions 'lua-mode ".rockspec"))

(defmodule markdown
  (package-require 'markdown-mode)
  (add-hook 'markdown-mode-hook 'flyspell-mode))

(defmodule ocaml
  (register-extensions 'tuareg-mode ".ml" ".mli")

 (push "~/.opam/4.06.1/share/emacs/site-lisp" load-path)
 (setq merlin-command "~/.opam/4.06.1/bin/ocamlmerlin")
 (autoload 'merlin-mode "merlin" "Merlin mode" t)
 (autoload 'tuareg-mode "tuareg" "Tuareg mode" t)
 (add-hook 'tuareg-mode-hook 'merlin-mode)
 (add-hook 'caml-mode-hook 'merlin-mode)

 (with-eval-after-load 'company
   (with-eval-after-load 'merlin-mode
     (add-to-list 'company-backends 'merlin-company-backend))))

(defmodule org
  (defun org-agenda-all ()
    (interactive)
    (org-agenda nil "n"))
  (global-set-key (kbd "C-c m a") 'org-agenda-all)

  (add-hook 'org-mode-hook 'flyspell-mode))

(defmodule rainbow
  (package-require 'rainbow-mode)
  (global-set-key (kbd "C-c m r") 'rainbow-mode))

(defmodule ruby
  (package-require 'projectile-rails 'rspec-mode 'robe)

  (add-hook 'ruby-mode-hook 'robe-mode)
  (add-hook 'ruby-mode-hook 'hs-minor-mode)
  (with-eval-after-load 'company (add-to-list 'company-backends 'company-robe))

  (add-to-list 'which-key-replacement-alist '((nil . "^projectile-rails-") . (nil . "r-")))
  (add-to-list 'which-key-replacement-alist '((nil . "^rspec-") . (nil . "r-")))

  (with-eval-after-load 'hideshow
    (add-to-list 'hs-special-modes-alist
                 `(ruby-mode
                   ,(rx (or "def" "class" "module" "do" "{" "[")) ; Block start
                   ,(rx (or "}" "]" "end"))                       ; Block end
                   ,(rx (or "#" "=begin"))                        ; Comment start
                   ruby-forward-sexp nil)))


  (with-eval-after-load 'robe
    (evil-make-overriding-map robe-mode-map 'normal)
    (evil-define-key 'normal robe-mode-map
      "gd" 'robe-jump
      "K"  'robe-doc)))

(defmodule rust
  (package-require 'rust-mode 'flycheck-rust 'racer)

  (add-hook 'flycheck-mode-hook 'flycheck-rust-setup)
  (add-hook 'rust-mode-hook 'racer-mode)
  (add-hook 'racer-mode-hook 'eldoc-mode))

(defmodule typescript
  (package-require 'tide)
  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1))

  ;; Formats the buffer before saving
  (add-hook 'before-save-hook 'tide-format-before-save)
  (add-hook 'typescript-mode-hook 'setup-tide-mode)

  (add-hook 'web-mode-hook (lambda ()
                             (when (string-equal "tsx" (file-name-extension buffer-file-name))
                               (setup-tide-mode))))

  (with-eval-after-load 'tide
    (evil-make-overriding-map tide-mode-map 'normal)
    (evil-define-key 'normal tide-mode-map
      "gd" 'tide-jump-to-definition)

    (define-key tide-mode-map (kbd "C-c t r") 'tide-rename-symbol)
    (define-key tide-mode-map (kbd "C-c t i") 'tide-organize-imports)))

(defmodule web
  (package-require 'web-mode)
  (register-extensions 'web-mode ".php" ".erb" ".tsx" ".html")

  (add-to-list 'which-key-replacement-alist '((nil . "^web-mode-") . (nil . "w-")))

  ;; Ensure flycheck runs on our files
  (with-eval-after-load 'flycheck
    (flycheck-add-mode 'html-tidy 'web-mode)
    (flycheck-add-mode 'php 'web-mode)
    (flycheck-add-mode 'css-csslint 'web-mode)
    (flycheck-add-mode 'typescript-tslint 'web-mode)))

(defmodule yaml
  (package-require 'yaml-mode))

(defadvice dired-find-file (around dired-subst-directory activate)
  "Replace current buffer if file is a directory."
  (interactive)
  (let* ((orig (current-buffer))
         (filename (dired-get-filename t t))
         (is-dir (file-directory-p filename)))
    ad-do-it
    (when (and is-dir (not (eq (current-buffer) orig)))
      (kill-buffer orig))))

(unless (display-graphic-p)
  ;; Show frame title in terminal window
  (defvar last-buffer "")
  (defun xterm-title-update ()
    (interactive)
    (if (string= last-buffer (buffer-name)) nil
      (setq last-buffer (buffer-name))
      (send-string-to-terminal (concat "\033]2; " (if buffer-file-name (buffer-file-name) (buffer-name)) " - emacs\007"))))
  (add-hook 'post-command-hook 'xterm-title-update))

(fset 'yes-or-no-p 'y-or-n-p)
(setq frame-title-format (concat  "%b - emacs@" (system-name)))

;; Patch some keybindings to work in the terminal
(global-set-key (kbd "<select>") 'move-end-of-line)
(global-set-key (kbd "<mouse-4>") 'scroll-down-line)
(global-set-key (kbd "<mouse-5>") 'scroll-up-line)

;; Silly keybindings for silly operating systems
(global-set-key (kbd "<home>") 'move-beginning-of-line)
(global-set-key (kbd "<end>") 'move-end-of-line)
(global-set-key (kbd "<next>") 'evil-scroll-page-down)
(global-set-key (kbd "<prior>") 'evil-scroll-page-up)

;; Switch to the default group when launching ibuffer
(add-hook 'ibuffer-hook (lambda() (ibuffer-switch-to-saved-filter-groups "Default")))

;; Flyspell on all code
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; Allow q to quit on view mode too
(with-eval-after-load 'view
  (evil-make-overriding-map view-mode-map 'normal)
  (evil-define-key 'normal view-mode-map
    "q" 'View-quit))

(global-set-key (kbd "C-c m l") 'display-line-numbers-mode)

(save-place-mode t)

(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

(defadvice show-paren-function (after show-matching-paren-offscreen activate)
  "If the matching paren is offscreen, show the matching line in the echo area.
   Has no effect if the character before point is not of the
   syntax class ')'."
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

(provide 'init)
