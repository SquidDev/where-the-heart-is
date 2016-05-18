(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(setq my-packages
  '(base16 el-get flycheck helm lua-mode markdown-mode multiple-cursors neotree))
(el-get 'sync)

; Whitespace highlighting
(require 'whitespace)

(set-language-environment "UTF-8")
(fset 'yes-or-no-p 'y-or-n-p)

; Save location
(setq-default save-place t)
(setq save-place-file (concat user-emacs-directory ".saved-places"))
(require 'saveplace)

; Unique buffers
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(require 'uniquify)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (base16-summerfruit-light)))
 '(custom-safe-themes (quote ("9e76732c9af8e423236ff8e37dd3b9bc37dacc256e42cc83810fb824eaa529b9" default)))
 '(flycheck-keymap-prefix "f")
 '(flycheck-syntax-check-failed-hook nil)
 '(global-whitespace-mode t)
 '(helm-M-x-fuzzy-match t)
 '(inhibit-startup-screen t)
 '(next-line-add-newlines nil)
 '(require-final-newline t)
 '(ring-bell-function (quote ignore) t)
 '(whitespace-style (quote (face tabs trailing spaces indentation empty tab-mark space-mark))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-ff-directory ((t (:foreground "#707070" :weight bold))))
 '(helm-selection ((t (:background "#B0B0B0" :underline nil))))
 '(highlight ((t (:background "#202020" :foreground "#B0B0B0"))))
 '(region ((t (:background "#B0B0B0"))))
 '(sp-pair-overlay-face ((t (:inherit highlight :underline t))))
 '(whitespace-hspace ((t (:foreground "#B0B0B0" :background nil))))
 '(whitespace-indentation ((t (:foreground "#B0B0B0" :background nil))))
 '(whitespace-space ((t (:foreground "#B0B0B0" :background nil))))
 '(whitespace-tab ((t (:foreground "#B0B0B0" :background nil)))))

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

(global-set-key (kbd "C-c q") #'quit-bottom-side-windows)
(global-set-key (kbd "C-\\") 'neotree-toggle)

; Helm Mode
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(global-set-key (kbd "M-x") 'helm-M-x)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

(setq helm-split-window-in-side-p      t ; open helm buffer inside current window, not occupy whole other window
 helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
 helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
 helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
 helm-ff-file-name-history-use-recentf t)
(helm-mode 1)
(helm-autoresize-mode 1)

; General
(show-paren-mode 1)
