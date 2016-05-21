(eval-when-compile (require 'cl))
(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(package-initialize)

(defvar package-list)
(setq package-list '(
  ; Nice themes
  base16-theme
  ; Linter
  flycheck
  ; Ctrl+P
  helm
  ; Various editing modes
  lua-mode markdown-mode
  ; Not actually used ATM
  multiple-cursors
  ; File browser
  neotree
  ; Git Client
  magit
  ; Better undo
  undo-tree
  ; 'Better' keybindings
  evil
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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (base16-summerfruit-light)))
 '(custom-safe-themes
   (quote
    ("9e76732c9af8e423236ff8e37dd3b9bc37dacc256e42cc83810fb824eaa529b9" default)))
 '(flycheck-keymap-prefix "f")
 '(flycheck-syntax-check-failed-hook nil)
 '(global-whitespace-mode t)
 '(helm-M-x-fuzzy-match t)
 '(helm-ff-file-name-history-use-recentf t)
 '(helm-ff-search-library-in-sexp t)
 '(helm-move-to-line-cycle-in-source t)
 '(helm-scroll-amount 8)
 '(helm-split-window-in-side-p t)
 '(inhibit-startup-screen t)
 '(next-line-add-newlines nil)
 '(require-final-newline t)
 '(ring-bell-function (quote ignore) t)
 '(save-place-file (concat user-emacs-directory ".saved-places"))
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
 '(region ((t (:background "#B0B0B0"))))
 '(whitespace-hspace ((t (:foreground "#B0B0B0" :background nil))))
 '(whitespace-indentation ((t (:foreground "#B0B0B0" :background nil))))
 '(whitespace-space ((t (:foreground "#B0B0B0" :background nil))))
 '(whitespace-tab ((t (:foreground "#B0B0B0" :background nil)))))

; Whitespace highlighting
(require 'whitespace)

; Save location
(setq-default save-place t)
(require 'saveplace)

; Unique buffers
(require 'uniquify)

; Editor config
(require 'editorconfig)
(editorconfig-mode 1)

; Evil
(require 'evil)
(evil-mode 1)

; Ctrl+Arrows
;; (defun ignore-error-wrapper (fn)
;;   "Funtion return new function that ignore errors.
;;   The function wraps a function with `ignore-errors' macro."
;;   (lexical-let ((fn fn))
;;     (lambda ()
;;       (interactive)
;;       (ignore-errors
;;          (funcall fn)))))

;; (global-set-key (kbd "C-<left>") (ignore-error-wrapper 'windmove-left))
;; (global-set-key (kbd "C-<right>") (ignore-error-wrapper 'windmove-right))
;; (global-set-key (kbd "C-<up>") (ignore-error-wrapper 'windmove-up))
;; (global-set-key (kbd "C-<down>") (ignore-error-wrapper 'windmove-down))

; Flycheck config
(add-hook 'after-init-hook #'global-flycheck-mode)

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

; Helm Mode
(require 'helm)
(require 'helm-config)

(helm-mode 1)
(helm-autoresize-mode 1)

(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(global-set-key (kbd "M-x") 'helm-M-x)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

; General
(show-paren-mode 1)
(defalias 'repl 'ielm)
