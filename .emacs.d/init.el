;; -*- lexical-binding: t -*-

;; Load our custom file as soon as possible. This ensures our theme other
;; appearance options are set up straight away.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file t)

;; Setup package.el and get everything else running.
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)
(unless package-archive-contents (package-refresh-contents))

(setq package-selected-packages '())

(defun package-require (&rest packages)
  "Install PACKAGES if they are not already installed."
  (dolist (package packages)
    (add-to-list 'package-selected-packages package)
    (unless (package-installed-p package)
      (package-install package))))

(defconst *enabled-modules*
  '(ido
    evil
    company
    flycheck

    ;; Various language modes
    ;; haskell
    ;; lua
    ;; markdown
    ;; ocaml
    ;; rust
    ;; typescript
    ;; web
    ;; yaml

    nil))

(defmacro defmodule (module &rest body)
  "Define a module which will only be evaluated when MODULE is in
   the *enabled-modules* set."
  `(when (memq ',module *enabled-modules*) ,@body))

(eval-when-compile (require 'cl))

(defun register-extensions (mode &rest exts)
  "Register a list of extensions to load with a particular mode."
  (dolist (ext exts)
    (message "Reverting `%s'..." ext)
    (add-to-list 'auto-mode-alist `(,(concat "\\" ext "\\'") . ,mode))))

;; Some core packages we'd like everywhere thank you very much
(package-require 'magit 'editorconfig 'projectile)

(defmodule ido
  (package-require 'ido-completing-read+ 'smex 'flx-ido)

  (ido-mode t)
  (ido-everywhere t)
  (ido-ubiquitous-mode 1)

  (flx-ido-mode)

  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands))

(defmodule evil
  (package-require 'evil 'undo-tree)

  (evil-mode t)
  (define-key evil-insert-state-map (kbd "C-x TAB") 'indent-relative)

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

(defmodule company
  (package-require 'company)
  (add-hook 'after-init-hook 'global-company-mode)

  (global-set-key (kbd "TAB") 'company-indent-or-complete-common))

(defmodule flycheck
  (package-require 'flycheck 'nix-sandbox)

  (add-hook 'after-init-hook 'global-flycheck-mode)

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
          (if (nix-current-sandbox)
              (nix-executable-find (nix-current-sandbox) cmd)
            cmd))))

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

(defmodule lua
  (package-require 'lua-mode)
  (register-extensions 'lua-mode ".rockspec"))

(defmodule markdown
  (package-require 'markdown-mode))

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
                               (setup-tide-mode)))))

(defmodule web
  (package-require 'web-mode)
  (register-extensions 'web-mode ".php" ".erb" ".tsx" ".html")

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

(add-hook 'ibuffer-hook (lambda() (ibuffer-switch-to-saved-filter-groups "Default")))

(add-hook 'prog-mode-hook 'flyspell-prog-mode)

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
