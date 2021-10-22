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
  :custom
  ; Overwrite backup to something sensible.
  (backup-by-copying t "Prefer copying over renaming, avoids clobbering symlinks.")
  (backup-directory-alist '((".*" . "~/.local/share/emacs")) "Don't pollute the local directory.")
  (auto-save-file-name-transform '((".*" "~/.local/share/emacs" t)) "Don't pollute the local directory.")
  (delete-old-versions t "Don't prompt when deleting old backups.")
  (version-control t "Used versioned backups.")

  (gc-cons-threshold 20000000 "Bump from the default 800000. Recommended on modern systems."))

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

(use-package counsel
  :demand t
  :config
  (ivy-mode 1)
  ; Use fuzzy matching and flx by default. Ideally this'd be in :custom, but I couldn't get that working.
  (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-count-format "(%d/%d) ")
  (ivy-magic-tilde nil "Keep ido's behaviour, not quite used to ivy's")
  (ivy-extra-directories nil "Hide ../ and ./")
  (ivy-on-del-error-function #'ignore "Don't close minibuffer when backspacing too much")
  :bind
  (:map ivy-minibuffer-map
   ; Make RET descend into directories rather than opening them. Can use C-m to call ivy-done normally.
   ("RET" . ivy-alt-done)))

(use-package amx
  :after counsel
  :config
  (amx-mode 1))

(use-package flx)

(use-package company
  :hook (after-init . global-company-mode)
  :custom
  (company-auto-commit t)
  (company-auto-complete t)
  (company-idle-delay 0.3)
  (company-tooltip-align-annotations t))

(use-package evil
  :demand t
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
  :mode "\\.md\\'"
  :custom
  (markdown-asymmetric-header t)
  (markdown-command "cmark"))

(use-package reason-mode
  :mode "\\.re\\'" "\\.rei\\'")

(use-package org
  :demand t
  :mode ("\\.org\\'" . org-mode)
  :config
  (require 'org-protocol)
  (evil-define-key 'normal org-mode-map (kbd "<tab>") #'org-cycle)
  ; org-html checkboxes should be disabled by default.
  (with-eval-after-load 'ox-html
    (setf (cdr (assoc 'html org-html-checkbox-types))
      '((on . "<input type=\"checkbox\" checked=\"checked\" disabled=\"disabled\" />")
         (off . "<input type=\"checkbox\" disabled=\"disabled\" />")
         (trans . "<input type=\"checkbox\" disabled\"disabled\" />"))))
  :custom
  (org-directory "~/Documents/org")
  (org-agenda-files '("~/Documents/org/todo.org"))
  (org-agenda-window-setup 'current-window)
  (org-tags-column -120)
  ; HTML export
  (org-html-doctype "html5")
  (org-html-postamble nil)
  (org-html-style
    "<style> body { font-family: -apple-system, BlinkMacSystemFont, \"Segoe UI\", Roboto, Oxygen, Ubuntu, Cantarell, \"Fira Sans\", \"Droid Sans\", \"Helvetica Neue\", Arial, sans-serif, \"Apple Color Emoji\", \"Segoe UI Emoji\", \"Segoe UI Symbol\"; background: #fff; color: #000; }</style>")
  (org-html-checkbox-type 'html "Export checkboxes as HTML - see above."))

(defun sq/org-roam--insert-timestamp ()
  (unless (org-entry-get nil "CREATED")
   (org-entry-put nil "CREATED" (format-time-string "[%Y-%m-%d %a %H:%M]"))))

(use-package org-roam
  :demand t
  :after (org)
  :hook
  (org-roam-capture-new-node . sq/org-roam--insert-timestamp)
  :bind
  (("C-c o f" . org-roam-node-find)
   ("C-c o i" . org-roam-node-insert)
   ("C-c o r" . org-roam-refile)
   ("C-c o t" . org-roam-buffer-toggle))
  :init
  (setq org-roam-v2-ack t)
  :config
  (unless (file-directory-p org-roam-directory) (make-directory org-roam-directory))
  (org-roam-db-autosync-mode)
  (require 'org-roam-protocol)
  ; Allow finding roam ids everywhere, not just in the roam directory.
  (add-hook 'org-open-at-point-functions #'org-roam-open-id-at-point)
  :custom
  (org-roam-directory "~/Documents/org/roam")
  (org-roam-capture-templates
   '(("d" "default" plain "%?"
     :target (file+head "%<%Y%m%d>-${slug}.org" "#+title: ${title}\n#+filetags: %^G")
     :unnarrowed t)))
  (org-roam-capture-ref-templates
   '(("r" "ref" plain "${body}%?"
     :target (file+head "%<%Y%m%d>-${slug}.org" "#+title: ${title}\n#+filetags: %^G")
     :unnarrowed t))))

(use-package org-download
  :commands (org-download-enable org-download-yank org-download-clipboard)
  :hook
  (dired-mode . org-download-enable)
  (org-mode . org-download-enable)
  :custom
  (org-download-image-dir "~/Documents/org/img")
  (org-download-heading-lvl nil)
  (org-download-timestamp "%Y%m%d-%H%M%S-"))

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
(add-to-list 'default-frame-alist '(undecorated . t)) ;; No title bar.
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t)) ;; And disable double buffering. No, not 100% sure why we need this.

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

(provide 'init)
