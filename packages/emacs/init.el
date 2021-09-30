;; -*- lexical-binding:true -*-

;; Useful hooks for startup failures.
(when nil (setq debug-on-error t) (setq debug-on-quit t))

;; Setup package.el and get everything else running.
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)
(unless package-archive-contents (package-refresh-contents))

;; Load our custom file as soon as possible. This ensures our theme other
;; appearance options are set up straight away.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file t)

(setq use-package-compute-statistics t)

(eval-when-compile
  (unless (package-installed-p 'use-package)
    (package-install 'use-package))

  (require 'use-package-ensure)
  (setq use-package-always-ensure t))

(use-package emacs
  :hook
  (;; Switch to the default group when launching ibuffer
   (ibuffer . (lambda() (ibuffer-switch-to-saved-filter-groups "Default"))))

  :bind
  (("C-c m l" . display-line-numbers-mode)
   :map emacs-lisp-mode-map
   ; Add keybindings to execute code
   ("C-c C-r" . eval-region)
   ("C-c C-l" . eval-buffer))

  :init
  (save-place-mode t)
  )

(use-package editorconfig
  :config
  (editorconfig-mode 1))

(use-package which-key
  :config
  (which-key-mode 1)
  (add-to-list 'which-key-replacement-alist '((nil . "^projectile-") . (nil . "p-")))
  (add-to-list 'which-key-replacement-alist '((nil . "^flycheck-") . (nil . "f-"))))

(use-package undo-tree
  :config
  (global-undo-tree-mode))

;; Setup ido and related works
(use-package ido
  :config
  (ido-mode t)
  (ido-everywhere t)
  :custom
  ;; '(ido-ignore-directories '("\\`CVS/" "\\`\\.\\./" "\\`\\./"))
  ;; (ido-create-new-buffer 'never "Don't create a file if we can't find it!")
  (ido-enable-flex-matching t)
  (ido-use-faces nil "We use flx-ido's custom faces instead."))

(use-package ido-completing-read+
  :after ido
  :config
  (ido-ubiquitous-mode 1))

(use-package smex
  :after ido
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)))

(use-package flx-ido
  :after ido
  :config
  (flx-ido-mode 1))

(use-package company
  :hook (after-init . global-company-mode)
  :custom
  (company-auto-commit t)
  (company-auto-complete t)
  (company-idle-delay 0.3)
  (company-tooltip-align-annotations t))

(use-package evil
  :demand
  :custom
  (evil-undo-system 'undo-tree)
  (evil-want-fine-undo t)
  :config
  (evil-mode 1)

  (dolist (mode '(term-mode
                  calculator-mode
                  diff-mode
                  exwm-mode
                  profiler-report-mode
                  neotree-mode
                  vterm-mode
                  image-mode))
    (evil-set-initial-state mode 'emacs))

  ;; Change a couple of modes to use Vim keybindings
  (dolist (mode '(git-commit-mode))
    (evil-set-initial-state mode 'normal))

  ;; Weird key bindings that don't fit anywhere else
  (evil-define-key 'normal 'view-mode "q" 'View-quit)

  :bind
  (("<next>" . evil-scroll-page-down)
   ("<prior>" . evil-scroll-page-up)

   :map evil-insert-state-map
   ("C-x <tab>" . indent-relative)

   :map evil-normal-state-map
   ("gc" . whitespace-cleanup)
   ("gr" . revert-buffer)
   ("g C-g" . count-words)

   :map evil-window-map
   ("<left>"  . evil-window-left)
   ("<right>" . evil-window-right)
   ("<up>"    . evil-window-up)
   ("<down>"  . evil-window-down)))

(use-package flycheck
  :hook (after-init . global-flycheck-mode)
  :config
  ;; Ensure the window is displayed in a sensible place
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*Flycheck errors*" eos)
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (side            . bottom)
                 (reusable-frames . visible)
                 (window-height   . 0.33))))

(use-package git-gutter
  :bind ("C-c m g" . git-gutter-mode))

(use-package orgalist
  :commands orgalist-mode
  :hook (git-commit-mode . orgalist-mode))

(use-package git-commit
  :hook
  ((git-commit-setup . git-commit-turn-on-flyspell)
   (git-commit-mode . (lambda ()
     (setq paragraph-start "\f\\|[ \t]*$\\|[ \t]*[-+*] "
           paragraph-separate "$"
           fill-column 72)))))

(use-package magit
  :custom
  (magit-revision-show-gravatars t)
  :hook
  ;; Switch to Emacs mode when entering blame (and revert when leaving): means
  ;; we can actually use all the key-bindings.
  (magit-blame-mode . (lambda ()
    (if magit-blame-mode
      (evil-emacs-state)
      (evil-exit-emacs-state)))))

(use-package spaceline
  :config
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

(use-package projectile
  :init (projectile-mode 1)
  :bind (:map projectile-mode-map
         ("C-c C-p" . nil)
         ("C-c p" . projectile-command-map)))

(use-package ripgrep
  :commands ripgrep-regexp)

(use-package flyspell
  :commands flyspell-mode
  :hook
  ((prog-mode . flyspell-prog-mode)
   (markdown-mode . flyspell-mode)
   (org-mode . flyspell-mode))
  :custom (flyspell-large-region 0))

(use-package whitespace
  :hook
  ;; Whitespace on all code and text
  ((prog-mode . whitespace-mode)
   (text-mode . whitespace-mode))
  :custom
  (whitespace-style '(face trailing tabs spaces empty indentation space-mark tab-mark)))

;; Language specific modes

(use-package lua-mode
  :mode "\\.lua\\'" "\\.rockspec\\'")

(use-package markdown-mode
  :mode "\\.md\\'")

(use-package reason-mode
  :mode "\\.re\\'" "\\.rei\\'")

(use-package org
  :demand
  :mode ("\\.org\\'" . org-mode)
  :config
  (require 'org-protocol)
  :custom
  (org-directory "~/Documents/org")
 '(org-agenda-files '("~/Documents/org/todo.org"))
  (org-agenda-window-setup 'current-window)
  (org-tags-column -120)
  (org-capture-templates
   '(;; Defines a bookmark
     ("b" "Bookmark" entry
      (file+headline "bookmarks.org" "Bookmarks")
      (file "templates/bookmark.org")
      :jump-to-captured t
      :empty-lines 1))))

(use-package rainbow-mode
  :bind ("C-c m r" . rainbow-mode))

(use-package yaml-mode
  :mode "\\.yml\\'" "\\.yaml\\'")

;; And any remaining config
(let ((local-config (expand-file-name "local.el" user-emacs-directory)))
  (when (file-exists-p local-config) (load local-config)))

(defadvice dired-find-file (around dired-subst-directory activate)
  "Replace current buffer if file is a directory."
  (interactive)
  (let* ((orig (current-buffer))
         (filename (dired-get-filename t t))
         (is-dir (file-directory-p filename)))
    ad-do-it
    (when (and is-dir (not (eq (current-buffer) orig)))
      (kill-buffer orig))))

;; Show frame title in terminal window
(defvar sq/last-buffer "")
(defun sq/xterm-title-update ()
  "Update the title of the current terminal window."
  (unless (or (string= sq/last-buffer (buffer-name)) (display-graphic-p))
    (setq sq/last-buffer (buffer-name))
    (send-string-to-terminal (concat "\033]2; " (if buffer-file-name (buffer-file-name) (buffer-name)) " - emacs\007"))))
(add-hook 'post-command-hook 'sq/xterm-title-update)

(fset 'yes-or-no-p 'y-or-n-p)
(setq frame-title-format (concat  "%b - emacs@" (system-name)))

(add-to-list 'default-frame-alist '(fullscreen . maximized)) ;; Maximize windows by default
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t)) ;; And disable double buffering.

(defadvice show-paren-function (after show-matching-paren-offscreen activate)
  "If the matching paren is offscreen, show the matching line in the echo area.
   Has no effect if the character before point is not of the
   syntax class ')'."
  (interactive)
  (let* ((cb (char-before (point))))
    (when (and cb (char-equal (char-syntax cb) ?\)))
      (blink-matching-open))))

(defun sq/align-space (start end)
  "Repeat alignment using spaces as the delimiter between START and END."
  (interactive "r")
  (align-regexp start end "\\(\\s-*\\) " 1 0 t))

(defun sq/org-id ()
  "Returns the ID property if set or generates and returns a new one if not set.
   The generated ID is stripped off potential progress indicator
   cookies and sanitized to get a slug. Furthermore, it is
   prepended with an ISO date-stamp if none was found before."
  (interactive)
  (when (not (org-entry-get nil "CUSTOM_ID"))
    (let* ((new-id ;; retrieve heading string
            (thread-last (org-heading-components)
             (nth 4) ;; Actual string
             (replace-regexp-in-string "[[][0-9%/]+[]] " "") ;; Progress indicators
             (replace-regexp-in-string "[^a-zA-Z0-9-]+" "-"))))
      (when (not (string-match "[12][0-9][0-9][0-9]-[01][0-9]-[0123][0-9]-.+" new-id))
        ;; only if no ISO date-stamp is found at the beginning of the new id:
      (setq new-id (concat (format-time-string "%Y-%m-%d-") new-id)))
      (org-set-property "CUSTOM_ID" new-id)))
  (kill-new (org-entry-get nil "CUSTOM_ID")))

(provide 'init)
