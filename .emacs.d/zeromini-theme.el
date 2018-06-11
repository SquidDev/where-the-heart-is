;;; zeromini-theme.el --- A dark, medium contrast theme for Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2015-2018  Nicolas Petton

;; Author: Nicolas Petton <nicolas@petton.fr>
;; Keywords: themes
;; Package-Version: 20180528.26
;; URL: https://github.com/NicolasPetton/zerodark-theme
;; Version: 4.3
;; Package: zeromini-theme

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is NOT part of GNU Emacs

;;; Commentary:
;;
;; A dark theme inspired from One Dark and Niflheim.

;;; Code:
(deftheme zeromini
  "A dark medium contrast theme")

(defun true-color-p ()
  "Return non-nil on displays that support 256 colors."
  (or
   (display-graphic-p)
   (= (tty-display-color-cells) 16777216)))

(let ((default (if (true-color-p) "#abb2bf" "#afafaf"))
      (light (if (true-color-p) "#ccd4e3" "#d7d7d7"))
      (background (if (true-color-p) "#282c34" "#333333"))
      (background-dark (if (true-color-p) "#24282f" "#222222"))
      (background-darker (if (true-color-p) "#22252c" "#222222"))
      (mode-line-inactive (if "#1c2129" "#222222"))
      (mode-line-active (if (true-color-p) "#6f337e" "#875f87"))
      (background-lighter (if (true-color-p) "#3a3f4b" "#5f5f5f"))
      (background-red (if (true-color-p) "#4c3840" "#5f5f5f"))
      (bright-background-red (if (true-color-p) "#744a5b" "#744a5b"))
      (background-purple (if (true-color-p) "#48384c" "#5f5f5f"))
      (background-blue (if (true-color-p) "#38394c" "#444444"))
      (bright-background-blue (if (true-color-p) "#4e5079" "#4e5079"))
      (background-green (if (true-color-p) "#3d4a41" "#5f5f5f"))
      (bright-background-green (if (true-color-p) "#3f6d54" "#3f6d54"))
      (background-orange (if (true-color-p) "#4a473d" "#5f5f5f"))
      (hl-line (if (true-color-p) "#2c323b" "#333333"))
      (grey (if (true-color-p) "#cccccc" "#cccccc"))
      (grey-dark (if (true-color-p) "#666666" "#666666"))
      (highlight (if (true-color-p) "#5a5a5a" "#5f5f5f"))
      (comment (if (true-color-p) "#787080" "#707070"))
      (orange (if (true-color-p) "#da8548" "#d7875f"))
      (orange-light (if (true-color-p) "#ddbd78" "#d7af87"))
      (red (if (true-color-p) "#ff6c6b" "#ff5f5f"))
      (purple (if (true-color-p) "#c678dd" "#d787d7"))
      (purple-dark (if (true-color-p) "#64446d" "#5f5f5f"))
      (blue (if (true-color-p) "#61afef" "#5fafff"))
      (blue-dark (if (true-color-p) "#1f5582" "#005f87"))
      (green (if (true-color-p) "#98be65" "#87af5f"))
      (green-light (if (true-color-p) "#9eac8c" "#afaf87"))
      (peach "PeachPuff3")
      (diff-added-background (if (true-color-p) "#284437" "#284437"))
      (diff-added-refined-background (if (true-color-p) "#1e8967" "#1e8967"))
      (diff-removed-background (if (true-color-p) "#583333" "#580000"))
      (diff-removed-refined-background (if (true-color-p) "#b33c49" "#b33c49"))
      (diff-current-background (if (true-color-p) "#29457b" "#29457b"))
      (diff-current-refined-background (if (true-color-p) "#4174ae" "#4174ae")))
  (custom-theme-set-faces
   'zeromini
   `(default ((t (:background ,background :foreground ,default))))
   `(cursor ((t (:background ,default))))

   ;; Highlighting faces
   `(fringe ((t (:foreground ,comment))))
   `(border ((t (:foreground ,background-lighter))))
   `(vertical-border ((t (:foreground ,background-lighter))))
   `(highlight ((t (:background ,highlight :foreground ,default :underline nil))))
   `(region ((t (:background ,highlight))))
   `(secondary-selection ((t (:background ,highlight :foreground ,default))))
   `(isearch ((t (:background ,orange-light :foreground ,highlight))))
   `(lazy-highlight ((t (:background ,grey-dark :foreground ,orange-light))))
   `(hl-line ((t (:background ,hl-line :underline unspecified :inherit nil))))
   `(shadow ((t (:foreground ,comment))))

   `(match ((t (:background ,background-green))))

   ;; Font lock faces
   `(font-lock-builtin-face ((t (:foreground ,blue :weight bold))))
   `(font-lock-comment-face ((t (:foreground ,comment :slant italic))))
   `(font-lock-constant-face ((t (:foreground ,orange :weight bold))))
   `(font-lock-function-name-face ((t (:foreground ,blue))))
   `(font-lock-keyword-face ((t (:foreground ,purple :weight bold))))
   `(font-lock-string-face ((t (:foreground ,green))))
   `(font-lock-doc-face ((t (:foreground ,green-light))))
   `(font-lock-type-face ((t (:foreground ,blue))))
   `(font-lock-variable-name-face ((t (:foreground ,blue))))
   `(font-lock-warning-face ((t (:foreground ,red :weight bold :background ,background-red))))

   ;; Mode line faces
   `(mode-line ((t (:background ,background-blue :height 0.9 :foreground ,blue))))
   `(mode-line-inactive ((t (:background ,background-darker :height 0.9 :foreground ,default))))
   `(header-line ((t (:inherit mode-line-inactive))))

   ;; error & success
   `(error ((t (:foreground ,red :weight bold))))
   `(warning ((t (:foreground ,orange :weight bold))))
   `(success ((t (:foreground ,green :weight bold))))

   ;; powerline
   `(powerline-active1 ((t (:height 0.9 :foreground ,blue :background ,background-darker))))
   `(powerline-active2 ((t (:height 0.9 :foreground ,blue :background ,background-lighter))))

   ;; mml
   `(message-mml-face ((t (:foreground ,comment))))

   ;; Org-clock mode line
   `(org-mode-line-clock ((t (:background unspecified (:inherit mode-line)))))

   ;; Escape and prompt faces
   `(minibuffer-prompt ((t (:foreground ,blue :weight bold))))
   `(escape-glyph ((t (:foreground ,blue :weight bold))))

   ;; linum
   `(linum ((t (:foreground ,comment :background ,background))))
   ;; from hlinum
   `(linum-highlight-face ((t (:foreground ,blue ,background ,background-blue))))

   ;; native line numbers (emacs 26)
   `(line-number ((t (:foreground ,comment :background ,background-darker))))
   `(line-number-current-line ((t (:foreground ,blue :background ,background-darker))))

   ;; eshell
   `(eshell-prompt ((t (:foreground ,blue :background ,background :weight bold))))
   `(eshell-ls-directory ((t (:foreground ,purple :background ,background :weight bold))))
   `(eshell-ls-symlink ((t (:foreground ,orange :background ,background :weight normal))))
   `(eshell-ls-executable ((t (:foreground ,green :background ,background :weight bold))))

   ;; whitespace
   `(whitespace-space ((t (:background unspecified :foreground ,highlight
                                            :inverse-video unspecified))))
   `(whitespace-hspace ((t (:background unspecified :foreground ,highlight
                                             :inverse-video unspecified))))
   `(whitespace-tab ((t (:background unspecified :foreground ,highlight
                                          :inverse-video unspecified))))
   `(whitespace-newline ((t (:background unspecified :foreground ,highlight
                                              :inverse-video unspecified))))
   `(whitespace-trailing ((t (:background ,red :foreground ,background :weight bold
                                               :inverse-video nil))))
   `(whitespace-line ((t (:background unspecified :foreground ,red
                                           :inverse-video unspecified))))
   `(whitespace-space-before-tab ((t (:inherit whitespace-space))))
   `(whitespace-space-after-tab ((t (:inherit whitespace-space))))
   `(whitespace-indentation ((t (:background unspecified :foreground ,highlight
                                                  :inverse-video unspecified))))
   `(whitespace-empty ((t (:background ,orange :foreground ,highlight
                                            :inverse-video unspecified))))

   ;; link faces
   `(link ((t (:foreground ,blue :underline t))))
   `(link-visited ((t (:foreground ,blue :underline t))))

   ;; widget faces
   `(widget-field ((t (:background ,highlight :box (:line-width 1 :color ,comment)))))
   `(widget-button ((t (:inherit link))))

   ;; custom
   `(custom-button ((t (:background ,background-lighter :box (:line-width 2 :style released-button)))))
   `(custom-button-mouse ((t (:background ,highlight :box (:line-width 2 :style released-button)))))
   `(custom-button-pressed ((t (:background ,highlight :box (:line-width 2 :style pressed-button)))))
   `(custom-group-tag ((t (:foreground ,purple :weight bold :height 1.4))))
   `(custom-variable-tag ((t (:foreground ,purple :weight bold))))
   `(custom-state ((t (:foreground ,green))))

   ;; compilation
   `(compilation-info ((t (:foreground ,purple :weight bold))))
   `(compilation-warning ((t (:foreground ,orange :weight bold))))
   `(compilation-error ((t (:foreground ,red :weight bold))))
   `(compilation-line-number ((t (:foreground ,green :weight bold))))
   `(compilation-mode-line-exit ((t (:foreground ,green :weight bold :inverse-video nil))))
   `(compilation-mode-line-run ((t (:foreground ,orange :weight bold))))
   `(compilation-mode-line-fail ((t (:foreground ,red :weight bold))))

   ;; dired
   `(dired-header ((t (:foreground ,blue :background ,background-blue :weight bold))))
   `(dired-directory ((t (:foreground ,purple :weight bold))))

   ;; diff
   `(diff-removed ((t (:background ,background-red :foreground ,red))))
   `(diff-added ((t (:background ,background-green :foreground ,green))))
   `(diff-hunk-header ((t (:background ,background-blue :weight bold :foreground ,blue))))
   `(diff-file-header ((t (:weight bold))))
   `(diff-header ((t (:background ,background :foreground ,blue))))
   `(diff-context ((t (:foreground ,default))))
   `(diff-refine-added ((t (:foreground ,green :background ,bright-background-green))))
   `(diff-refine-removed ((t (:background ,bright-background-red :foreground ,red))))

   ;; ediff
   `(ediff-fine-diff-B ((t (:inherit diff-refine-added))))
   `(ediff-current-diff-B ((t (:inherit diff-added))))
   `(ediff-fine-diff-A ((t (:inherit diff-refine-removed))))
   `(ediff-current-diff-A ((t (:inherit diff-removed))))
   `(ediff-fine-diff-C ((t (:foreground ,blue :background ,bright-background-blue))))
   `(ediff-current-diff-C ((t (:background ,background-blue :foreground ,blue))))

   ;; magit
   `(magit-diff-context-highlight ((t (:background ,background-darker))))
   `(magit-diff-file-heading ((t (:weight bold :foreground ,blue))))
   `(magit-diff-file-heading-highlight ((t (:weight bold :foreground ,blue :background ,background-blue))))
   `(magit-diff-removed-highlight ((t (:inherit diff-removed))))
   `(magit-diff-removed ((t (:inherit diff-removed))))
   `(magit-diff-added-highlight ((t (:inherit diff-added))))
   `(magit-diff-added ((t (:inherit diff-added))))
   `(magit-diff-lines-heading ((t (:background ,blue-dark :foreground "white"))))
   `(magit-diff-hunk-heading ((t (:background ,background-lighter))))
   `(magit-diff-hunk-heading-highlight ((t (:background ,blue-dark))))
   `(magit-diff-hunk-heading ((t (:background ,background-lighter))))

   `(magit-process-ok ((t (:foreground ,green :weight bold))))

   `(magit-section-highlight ((t (:background ,background-darker))))
   `(magit-section-heading ((t (:foreground ,grey :weight bold))))
   `(magit-branch-current ((t (:foreground ,blue :background ,background-darker :box 1))))
   `(magit-branch-local ((t (:foreground ,purple :background ,background-darker :box 1))))
   `(magit-branch-remote ((t (:foreground ,green :background ,background-darker :box 1))))

   `(magit-reflog-reset ((t (:background ,background-red :foreground ,red :weight bold))))
   `(magit-reflog-amend ((t (:background ,background-blue :foreground ,blue :weight bold))))
   `(magit-reflog-rebase ((t (:background ,background-blue :foreground ,blue :weight bold))))
   `(magit-reflog-commit ((t (:background ,background-green :foreground ,green :weight bold))))
   `(magit-reflog-checkout ((t (:background ,background-orange :foreground ,orange :weight bold))))
   `(magit-reflog-cherry-pick ((t (:background ,background-purple :foreground ,purple :weight bold))))

   `(magit-bisect-bad ((t (:background ,background-red :foreground ,red :box 1))))
   `(magit-bisect-good ((t (:background ,background-blue :foreground ,blue :box 1))))

   `(magit-signature-bad ((t (:foreground ,red))))
   `(magit-signature-good ((t (:foreground ,blue))))

   `(magit-blame-heading ((t (:foreground ,green :background ,background-green :box 1))))

   `(git-commit-summary ((t (:weight bold))))

   `(magit-tag ((t (:foreground ,purple :weight bold :box 1 :background "#202020"))))
   `(magit-sequence-part ((t (:foreground ,orange :weight bold))))
   `(magit-sequence-head ((t (:foreground ,green :weight bold))))

   ;; Message faces
   `(message-header-name ((t (:foreground ,blue :weight bold))))
   `(message-header-cc ((t (:foreground ,purple))))
   `(message-header-other ((t (:foreground ,purple))))
   `(message-header-subject ((t (:foreground ,green))))
   `(message-header-to ((t (:foreground ,purple))))
   `(message-cited-text ((t (:foreground ,comment))))
   `(message-separator ((t (:foreground ,red :weight bold))))

   ;; ido faces
   `(ido-first-match ((t (:foreground ,purple :weight bold))))
   `(ido-only-match ((t (:foreground ,purple :weight bold))))
   `(ido-subdir ((t (:foreground ,blue))))
   `(flx-highlight-face ((t (:inherit ido-first-match))))

   ;; company
   `(company-preview ((t (:background ,background-darker :foreground ,default))))
   `(company-preview-common ((t (:background ,background-darker :foreground ,purple))))
   `(company-preview-search ((t (:background ,blue :foreground ,default))))
   `(company-tooltip ((t (:background ,background-darker :foreground ,default))))
   `(company-scrollbar-bg ((t (:background ,background-darker))))
   `(company-scrollbar-fg ((t (:background ,background-blue))))
   `(company-tooltip-common ((t (:foreground ,purple :weight bold :background ,background-darker))))
   `(company-tooltip-annotation ((t (:foreground ,blue :weight bold :background ,background-blue))))
   `(company-tooltip-common-selection ((t (:foreground ,purple :background ,background-lighter :weight bold))))
   `(company-tooltip-selection ((t (:foreground ,default :background ,background-lighter))))
   `(company-tooltip-mouse ((t (:foreground ,default :background ,background-lighter))))

   ;; web-mode
   `(web-mode-html-tag-face ((t (:foreground ,purple :weight bold))))
   `(web-mode-symbol-face ((t (:foreground ,red :weight bold))))

   ;; js2-mode
   `(js2-function-param ((t (:foreground ,blue))))
   `(js2-error ((t (:foreground ,red))))
   `(js2-jsdoc-html-tag-name ((t (:foreground ,peach))))
   `(js2-jsdoc-html-tag-delimiter ((t (:foreground ,peach))))

   ;; flycheck
   `(flycheck-fringe-error ((t (:foreground ,red :weight bold))))
   `(flycheck-fringe-warning ((t (:foreground ,orange :weight bold))))
   `(flycheck-fringe-info ((t (:foreground ,green :weight bold))))
   `(flycheck-warning ((t (:underline (:color ,red :style wave)))))
   `(flycheck-error ((t (:underline (:color ,red :style wave)))))
   `(flycheck-info ((t (:underline (:color ,green :style wave)))))

   ;; FIC
   `(font-lock-fic-face ((t (:foreground ,background :background ,red :weight bold))))

   ;; org-mode todo
   `(org-hide ((t (:foreground ,background))))
   `(org-todo ((t (:foreground ,red :background ,background-red :weight bold))))
   `(org-done ((t (:foreground ,green :background ,background-green :weight bold))))
   `(org-date ((t (:background ,background-lighter))))
   `(org-scheduled-previously ((t (:foreground ,red))))
   `(org-scheduled ((t (:foreground ,default))))
   `(org-upcoming-deadline ((t (:foreground ,orange))))
   `(org-headline-done ((t (:foreground ,comment))))
   `(outline-1 ((t (:foreground ,blue :weight bold))))
   `(outline-2 ((t (:foreground ,purple :weight bold))))
   `(outline-3 ((t (:foreground ,peach :weight bold))))
   `(outline-4 ((t (:foreground ,green-light :weight bold))))
   `(outline-5 ((t (:foreground ,blue :weight bold))))
   `(outline-6 ((t (:foreground ,purple :weight bold))))
   `(outline-7 ((t (:foreground ,peach :weight bold))))
   `(outline-8 ((t (:foreground ,green-light :weight bold))))
   `(org-column-title ((t (:foreground unspecified :background unspecified))))
   `(org-agenda-date ((t (:foreground ,purple :weight bold))))
   `(org-agenda-date-today ((t (:foreground ,blue :weight bold :background ,background-blue :box 1))))
   `(org-agenda-structure ((t (:foreground ,blue :weight bold))))
   `(org-scheduled-today ((t (:foreground ,default :weight bold))))
   `(org-agenda-done ((t (:foreground ,comment))))
   `(org-time-grid ((t (:foreground ,comment))))

   ;; org columns
   `(org-column ((t (:background ,background-darker))))
   `(org-column-title ((t (:background ,background-blue :foreground ,blue :weight bold))))

   ;; org blocks
   `(org-block-begin-line ((t (:background ,background-green :foreground ,green-light :height 0.9))))
   `(org-block-end-line ((t (:background ,background-green :foreground ,green-light :height 0.9))))

   ;; org-drill
   `(org-drill-hidden-cloze-face ((t (:background ,red :foreground ,background))))
   `(org-drill-visible-cloze-face ((t (:background ,blue :foreground ,background-blue))))
   `(org-drill-visible-cloze-hint-face ((t (:background ,green :foreground ,background-green))))

   ;; Gnus faces -- from wombat, feel free to improve :)
   `(gnus-group-news-1 ((t (:weight bold :foreground "#95e454"))))
   `(gnus-group-news-1-low ((t (:foreground "#95e454"))))
   `(gnus-group-news-2 ((t (:weight bold :foreground "#cae682"))))
   `(gnus-group-news-2-low ((t (:foreground "#cae682"))))
   `(gnus-group-news-3 ((t (:weight bold :foreground "#ccaa8f"))))
   `(gnus-group-news-3-low ((t (:foreground "#ccaa8f"))))
   `(gnus-group-news-4 ((t (:weight bold :foreground "#99968b"))))
   `(gnus-group-news-4-low ((t (:foreground "#99968b"))))
   `(gnus-group-news-5 ((t (:weight bold :foreground "#cae682"))))
   `(gnus-group-news-5-low ((t (:foreground "#cae682"))))
   `(gnus-group-news-low ((t (:foreground "#99968b"))))
   `(gnus-group-mail-1 ((t (:weight bold :foreground "#95e454"))))
   `(gnus-group-mail-1-low ((t (:foreground "#95e454"))))
   `(gnus-group-mail-2 ((t (:weight bold :foreground "#cae682"))))
   `(gnus-group-mail-2-low ((t (:foreground "#cae682"))))
   `(gnus-group-mail-3 ((t (:weight bold :foreground "#ccaa8f"))))
   `(gnus-group-mail-3-low ((t (:foreground "#ccaa8f"))))
   `(gnus-group-mail-low ((t (:foreground "#99968b"))))
   `(gnus-header-content ((t (:foreground "#8ac6f2"))))
   `(gnus-header-from ((t (:weight bold :foreground "#95e454"))))
   `(gnus-header-subject ((t (:foreground "#cae682"))))
   `(gnus-header-name ((t (:foreground "#8ac6f2"))))
   `(gnus-header-newsgroups ((t (:foreground "#cae682"))))

   ;; which-function
   `(which-func ((t (:foreground ,purple))))

   `(ediff-even-diff-A ((t (:background ,highlight :foreground unspecified))))
   `(ediff-even-diff-B ((t (:background ,highlight :foreground unspecified))))
   `(ediff-even-diff-C ((t (:background ,highlight :foreground unspecified))))
   `(ediff-odd-diff-A ((t (:background ,highlight :foreground unspecified))))
   `(ediff-odd-diff-B ((t (:background ,highlight :foreground unspecified))))
   `(ediff-odd-diff-C ((t (:background ,highlight :foreground unspecified))))

   ;; ivy
   `(ivy-current-match ((t (:background ,background-purple :weight bold :foreground ,purple))))
   `(ivy-minibuffer-match-face-1 ((t (:foreground ,orange))))
   `(ivy-minibuffer-match-face-2 ((t (:foreground ,green))))
   `(ivy-minibuffer-match-face-3 ((t (:foreground ,green))))
   `(ivy-minibuffer-match-face-4 ((t (:foreground ,green))))
   `(ivy-match-required-face ((t (:foreground ,red :background ,background-red :weight bold))))
   `(ivy-modified-buffer ((t (:foreground ,red))))
   `(ivy-remote ((t (:foreground ,blue))))
   `(ivy-highlight-face ((t (:foreground ,blue :weight bold))))

   ;; visible mark
   `(visible-mark-face1 ((t (:foreground ,orange-light :inverse-video t))))
   `(visible-mark-face2 ((t (:foreground ,peach :inverse-video t))))

   ;; show-paren
   `(show-paren-match ((t (:background ,blue :weight bold))))

   ;; clojure
   `(clojure-keyword-face ((t (:inherit font-lock-builtin-face))))

   ;; ledger
   `(ledger-font-report-clickable-face ((t (:foreground ,blue))))
   `(ledger-font-posting-amount-face ((t (:foreground ,purple))))
   `(ledger-font-posting-date-face ((t (:foreground ,blue :background ,background-blue :box 1))))
   `(ledger-font-payee-uncleared-face ((t (:foreground ,default :weight bold))))
   `(ledger-font-payee-cleared-face ((t (:foreground ,green :weight bold))))
   `(ledger-font-posting-account-face ((t (:foreground ,default))))
   `(ledger-font-posting-account-pending-face ((t (:foreground ,red))))
   `(ledger-font-xact-highlight-face ((t (:background ,background-darker))))
   `(ledger-font-other-face ((t (:inherit ,font-lock-comment-face))))
   `(ledger-font-periodic-xact-face ((t (:foreground ,orange))))

   `(diff-hl-change ((t (:foreground ,bright-background-blue :background ,bright-background-blue))))
   `(diff-hl-delete ((t (:foreground ,bright-background-red :background ,bright-background-red))))
   `(diff-hl-insert ((t (:foreground ,bright-background-green :background ,bright-background-green))))

   `(git-gutter:added ((t (:foreground ,green :background ,background-green))))
   `(git-gutter:deleted ((t (:foreground ,red :background ,background-red))))
   `(git-gutter:modified ((t (:foreground ,purple :background ,background-purple))))
   `(git-gutter:separator ((t (:background ,background-orange))))
   `(git-gutter:unchanged ((t (:background ,background-orange))))

   `(term-color-black ((t (:foreground ,default :background ,background-darker))))
   `(term-color-red ((t (:foreground ,red :background ,background-red))))
   `(term-color-green ((t (:foreground ,green :background ,background-green))))
   `(term-color-yellow ((t (:foreground ,orange :background ,background-orange))))
   `(term-color-blue ((t (:foreground ,blue :background ,background-blue))))
   `(term-color-magenta ((t (:foreground ,purple :background ,background-purple))))
   `(term-color-cyan ((t (:foreground ,blue-dark))))
   `(term-color-white ((t (:foreground ,grey))))
   `(term ((t (:foreground ,default :background ,background))))
   `(term-default-fg-color ((t (:inherit term-color-white))))
   `(term-default-bg-color ((t (:inherit term-color-black))))

   `(sh-heredoc ((t (:foreground ,orange :weight bold))))

   `(avy-lead-face ((t :foreground ,red :background ,background-red)))
   `(avy-lead-face-0 ((t :foreground ,purple :background ,background-purple)))
   `(avy-lead-face-1 ((t :foreground ,blue :background ,background-blue)))
   `(avy-lead-face-2 ((t :foreground ,green :background ,background-green)))

   `(erc-nick-default-face ((t :foreground ,blue :background ,background-blue :weight bold)))
   `(erc-current-nick-face ((t :foreground ,red :weight bold :background ,background-red)))
   `(erc-my-nick-face ((t :foreground ,red :weight bold :background ,background-red)))
   `(erc-notice-face ((t :foreground ,comment)))
   `(erc-input-face ((t :foreground ,default :weight bold)))
   `(erc-prompt-face ((t :foreground ,purple :background ,background-purple :weight bold :box 1)))
   `(erc-timestamp-face ((t :foreground ,purple :weight bold)))

   ;; slack
   `(slack-message-output-header ((t :foreground ,blue :background ,background-blue :weight bold)))

   `(hydra-face-red ((t :foreground ,red :weight bold)))
   `(hydra-face-blue ((t :foreground ,blue :weight bold)))

   ;; elfeed
   `(elfeed-search-date-face ((t (:foreground ,blue))))
   `(elfeed-search-feed-face ((t (:foreground ,blue))))
   `(elfeed-search-tag-face ((t (:foreground ,green))))
   `(elfeed-search-title-face ((t (:foreground ,purple))))

   ;; wgrep
   `(wgrep-face ((t (:foreground ,orange))))
   `(wgrep-reject-face ((t (:foreground ,red :weight bold :background ,background-red))))
   `(wgrep-done-face ((t (:foreground ,blue :weight bold))))

   ;; AucTeX
   `(font-latex-math-face ((t :foreground ,green-light)))
   `(font-latex-sectioning-5-face ((t :foreground ,blue)))
   `(font-latex-string-face ((t :inherit font-lock-string-face)))
   `(font-latex-bold-face ((t :foreground ,green :weight bold)))
   `(font-latex-italic-face ((t :foreground ,green :slant italic)))
   `(font-latex-warning-face ((t :inherit warning)))

   ;; Anzu
   `(anzu-replace-highlight ((t :foreground ,red :background ,background-red :strike-through t)))
   `(anzu-replace-to ((t :foreground ,green :background ,background-green)))
   `(anzu-match-1 ((t :foreground ,red :background ,background-red :box t)))
   `(anzu-match-2 ((t :foreground ,red :background ,background-red :box t)))
   `(anzu-match-3 ((t :foreground ,red :background ,background-red :box t)))
   `(anzu-mode-line ((t :inherit mode-line :weight bold)))

   ;; ace-window
   `(aw-leading-char-face ((t :foreground ,red :weight bold)))
   `(aw-background-face ((t :foreground ,comment)))

   ;; paren-face.el
   `(parenthesis ((t (:foreground ,comment))))

   ;; rainbow-delimiters
   `(rainbow-delimiters-mismatched-face ((t :foreground ,red :weight bold :background ,background-red)))
   `(rainbow-delimiters-unmatched-face ((t :foreground ,red :weight bold :background ,background-red)))

   ;; makefile
   `(makefile-space ((t (:background ,background-blue))))

   ;; epa
   `(epa-validity-high ((t (:foreground ,green))))
   `(epa-validity-low ((t (:foreground ,default))))
   `(epa-validity-disabled ((t (:foreground ,red :weight bold :background ,background-red))))
   `(epa-field-name ((t (:foreground ,purple :weight bold))))
   `(epa-field-body ((t (:foreground ,orange))))

   ;; markup-face
   `(markup-title-0-face ((t (:foreground ,blue :weight bold :underline t))))
   `(markup-title-1-face ((t (:foreground ,purple :weight bold :underline t))))
   `(markup-title-2-face ((t (:foreground ,peach :weight bold :underline t))))
   `(markup-title-3-face ((t (:foreground ,green-light :weight bold :underline t))))
   `(markup-title-4-face ((t (:foreground ,blue :weight bold :underline t))))
   `(markup-title-5-face ((t (:foreground ,purple :weight bold :underline t))))
   `(markup-error-face ((t (:foreground ,red :background ,background-red :weight bold))))
   `(markup-gen-face ((t (:foreground ,blue))))
   `(markup-typewriter-face ((t (:inherit shadow))))
   `(markup-meta-face ((t (:foreground ,comment))))
   `(markup-meta-hide-face ((t (:foreground ,comment))))
   `(markup-verbatim-face ((t (:inherit shadow :background ,background-lighter))))
   `(markup-reference-face ((t (:inherit link))))
   `(markup-complex-replacement-face ((t (:background ,background-green))))
   `(markup-secondary-text-face ((t (:foreground ,comment))))
   )

  (custom-theme-set-variables
   'zeromini
   `(ansi-color-names-vector [,background
                              ,red
                              ,green
                              ,orange
                              ,blue
                              ,purple
                              ,blue-dark
                              ,default])))

(provide-theme 'zeromini)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; zeromini-theme.el ends here
