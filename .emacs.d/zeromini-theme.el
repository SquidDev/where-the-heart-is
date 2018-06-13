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

(let ((class '((class color) (min-colors 89)))
      (default (if (true-color-p) "#abb2bf" "#afafaf"))
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
   `(default ((,class (:background ,background :foreground ,default))))
   `(cursor ((,class (:background ,default))))

   ;; Highlighting faces
   `(fringe ((,class (:background ,background-dark :foreground ,comment))))
   `(border ((,class (:foreground ,background-lighter))))
   `(vertical-border ((,class (:foreground ,background-lighter))))
   `(highlight ((,class (:background ,highlight :foreground ,default :underline nil))))
   `(region ((,class (:background ,highlight))))
   `(secondary-selection ((,class (:background ,highlight :foreground ,default))))
   `(isearch ((,class (:background ,orange-light :foreground ,highlight))))
   `(lazy-highlight ((,class (:background ,grey-dark :foreground ,orange-light))))
   `(hl-line ((,class (:background ,hl-line :underline unspecified :inherit nil))))
   `(shadow ((,class (:foreground ,comment))))

   `(match ((,class (:background ,background-green))))

   ;; Font lock faces
   `(font-lock-builtin-face ((,class (:foreground ,blue :weight bold))))
   `(font-lock-comment-face ((,class (:foreground ,comment :slant italic))))
   `(font-lock-constant-face ((,class (:foreground ,orange :weight bold))))
   `(font-lock-function-name-face ((,class (:foreground ,blue))))
   `(font-lock-keyword-face ((,class (:foreground ,purple :weight bold))))
   `(font-lock-string-face ((,class (:foreground ,green))))
   `(font-lock-doc-face ((,class (:foreground ,green-light))))
   `(font-lock-type-face ((,class (:foreground ,blue))))
   `(font-lock-variable-name-face ((,class (:foreground ,blue))))
   `(font-lock-warning-face ((,class (:foreground ,red :weight bold :background ,background-red))))

   ;; Mode line faces
   `(mode-line ((,class (:background ,background-blue :height 0.9 :foreground ,blue))))
   `(mode-line-inactive ((,class (:background ,background-darker :height 0.9 :foreground ,default))))
   `(header-line ((,class (:inherit mode-line-inactive))))

   ;; error & success
   `(error ((,class (:foreground ,red :weight bold))))
   `(warning ((,class (:foreground ,orange :weight bold))))
   `(success ((,class (:foreground ,green :weight bold))))

   ;; powerline
   `(powerline-active1 ((,class (:height 0.9 :foreground ,blue :background ,background-darker))))
   `(powerline-active2 ((,class (:height 0.9 :foreground ,blue :background ,background-lighter))))

   ;; mml
   `(message-mml-face ((,class (:foreground ,comment))))

   ;; Org-clock mode line
   `(org-mode-line-clock ((,class (:background unspecified (:inherit mode-line)))))

   ;; Escape and prompt faces
   `(minibuffer-prompt ((,class (:foreground ,blue :weight bold))))
   `(escape-glyph ((,class (:foreground ,blue :weight bold))))

   ;; linum
   `(linum ((,class (:foreground ,comment :background ,background))))
   ;; from hlinum
   `(linum-highlight-face ((,class (:foreground ,blue ,background ,background-blue))))

   ;; native line numbers (emacs 26)
   `(line-number ((,class (:foreground ,comment :background ,background-darker))))
   `(line-number-current-line ((,class (:foreground ,blue :background ,background-darker))))

   ;; eshell
   `(eshell-prompt ((,class (:foreground ,blue :background ,background :weight bold))))
   `(eshell-ls-directory ((,class (:foreground ,purple :background ,background :weight bold))))
   `(eshell-ls-symlink ((,class (:foreground ,orange :background ,background :weight normal))))
   `(eshell-ls-executable ((,class (:foreground ,green :background ,background :weight bold))))

   ;; whitespace
   `(whitespace-space ((,class (:background unspecified :foreground ,highlight
                                            :inverse-video unspecified))))
   `(whitespace-hspace ((,class (:background unspecified :foreground ,highlight
                                             :inverse-video unspecified))))
   `(whitespace-tab ((,class (:background unspecified :foreground ,highlight
                                          :inverse-video unspecified))))
   `(whitespace-newline ((,class (:background unspecified :foreground ,highlight
                                              :inverse-video unspecified))))
   `(whitespace-trailing ((,class (:background ,red :foreground ,background :weight bold
                                               :inverse-video nil))))
   `(whitespace-line ((,class (:background unspecified :foreground ,red
                                           :inverse-video unspecified))))
   `(whitespace-space-before-tab ((,class (:inherit whitespace-space))))
   `(whitespace-space-after-tab ((,class (:inherit whitespace-space))))
   `(whitespace-indentation ((,class (:background unspecified :foreground ,highlight
                                                  :inverse-video unspecified))))
   `(whitespace-empty ((,class (:background ,orange :foreground ,highlight
                                            :inverse-video unspecified))))

   ;; link faces
   `(link ((,class (:foreground ,blue :underline t))))
   `(link-visited ((,class (:foreground ,blue :underline t))))

   ;; widget faces
   `(widget-field ((,class (:background ,highlight :box (:line-width 1 :color ,comment)))))
   `(widget-button ((,class (:inherit link))))

   ;; custom
   `(custom-button ((,class (:background ,background-lighter :box (:line-width 2 :style released-button)))))
   `(custom-button-mouse ((,class (:background ,highlight :box (:line-width 2 :style released-button)))))
   `(custom-button-pressed ((,class (:background ,highlight :box (:line-width 2 :style pressed-button)))))
   `(custom-group-tag ((,class (:foreground ,purple :weight bold :height 1.4))))
   `(custom-variable-tag ((,class (:foreground ,purple :weight bold))))
   `(custom-state ((,class (:foreground ,green))))

   ;; compilation
   `(compilation-info ((,class (:foreground ,purple :weight bold))))
   `(compilation-warning ((,class (:foreground ,orange :weight bold))))
   `(compilation-error ((,class (:foreground ,red :weight bold))))
   `(compilation-line-number ((,class (:foreground ,green :weight bold))))
   `(compilation-mode-line-exit ((,class (:foreground ,green :weight bold :inverse-video nil))))
   `(compilation-mode-line-run ((,class (:foreground ,orange :weight bold))))
   `(compilation-mode-line-fail ((,class (:foreground ,red :weight bold))))

   ;; dired
   `(dired-header ((,class (:foreground ,blue :background ,background-blue :weight bold))))
   `(dired-directory ((,class (:foreground ,purple :weight bold))))

   ;; diff
   `(diff-removed ((,class (:background ,background-red :foreground ,red))))
   `(diff-added ((,class (:background ,background-green :foreground ,green))))
   `(diff-hunk-header ((,class (:background ,background-blue :weight bold :foreground ,blue))))
   `(diff-file-header ((,class (:weight bold))))
   `(diff-header ((,class (:background ,background :foreground ,blue))))
   `(diff-context ((,class (:foreground ,default))))
   `(diff-refine-added ((,class (:foreground ,green :background ,bright-background-green))))
   `(diff-refine-removed ((,class (:background ,bright-background-red :foreground ,red))))

   ;; ediff
   `(ediff-fine-diff-B ((,class (:inherit diff-refine-added))))
   `(ediff-current-diff-B ((,class (:inherit diff-added))))
   `(ediff-fine-diff-A ((,class (:inherit diff-refine-removed))))
   `(ediff-current-diff-A ((,class (:inherit diff-removed))))
   `(ediff-fine-diff-C ((,class (:foreground ,blue :background ,bright-background-blue))))
   `(ediff-current-diff-C ((,class (:background ,background-blue :foreground ,blue))))

   ;; magit
   `(magit-diff-context-highlight ((,class (:background ,background-darker))))
   `(magit-diff-file-heading ((,class (:weight bold :foreground ,blue))))
   `(magit-diff-file-heading-highlight ((,class (:weight bold :foreground ,blue :background ,background-blue))))
   `(magit-diff-removed-highlight ((,class (:inherit diff-removed))))
   `(magit-diff-removed ((,class (:inherit diff-removed))))
   `(magit-diff-added-highlight ((,class (:inherit diff-added))))
   `(magit-diff-added ((,class (:inherit diff-added))))
   `(magit-diff-lines-heading ((,class (:background ,blue-dark :foreground "white"))))
   `(magit-diff-hunk-heading ((,class (:background ,background-lighter))))
   `(magit-diff-hunk-heading-highlight ((,class (:background ,blue-dark))))
   `(magit-diff-hunk-heading ((,class (:background ,background-lighter))))

   `(magit-process-ok ((,class (:foreground ,green :weight bold))))

   `(magit-section-highlight ((,class (:background ,background-darker))))
   `(magit-section-heading ((,class (:foreground ,grey :weight bold))))
   `(magit-branch-current ((,class (:foreground ,blue :background ,background-darker :box 1))))
   `(magit-branch-local ((,class (:foreground ,purple :background ,background-darker :box 1))))
   `(magit-branch-remote ((,class (:foreground ,green :background ,background-darker :box 1))))

   `(magit-reflog-reset ((,class (:background ,background-red :foreground ,red :weight bold))))
   `(magit-reflog-amend ((,class (:background ,background-blue :foreground ,blue :weight bold))))
   `(magit-reflog-rebase ((,class (:background ,background-blue :foreground ,blue :weight bold))))
   `(magit-reflog-commit ((,class (:background ,background-green :foreground ,green :weight bold))))
   `(magit-reflog-checkout ((,class (:background ,background-orange :foreground ,orange :weight bold))))
   `(magit-reflog-cherry-pick ((,class (:background ,background-purple :foreground ,purple :weight bold))))

   `(magit-bisect-bad ((,class (:background ,background-red :foreground ,red :box 1))))
   `(magit-bisect-good ((,class (:background ,background-blue :foreground ,blue :box 1))))

   `(magit-signature-bad ((,class (:foreground ,red))))
   `(magit-signature-good ((,class (:foreground ,blue))))

   `(magit-blame-heading ((,class (:foreground ,green :background ,background-green :box 1))))

   `(git-commit-summary ((,class (:weight bold))))

   `(magit-tag ((,class (:foreground ,purple :weight bold :box 1 :background "#202020"))))
   `(magit-sequence-part ((,class (:foreground ,orange :weight bold))))
   `(magit-sequence-head ((,class (:foreground ,green :weight bold))))

   ;; Message faces
   `(message-header-name ((,class (:foreground ,blue :weight bold))))
   `(message-header-cc ((,class (:foreground ,purple))))
   `(message-header-other ((,class (:foreground ,purple))))
   `(message-header-subject ((,class (:foreground ,green))))
   `(message-header-to ((,class (:foreground ,purple))))
   `(message-cited-text ((,class (:foreground ,comment))))
   `(message-separator ((,class (:foreground ,red :weight bold))))

   ;; ido faces
   `(ido-first-match ((,class (:foreground ,purple :weight bold))))
   `(ido-only-match ((,class (:foreground ,purple :weight bold))))
   `(ido-subdir ((,class (:foreground ,blue))))
   '(flx-highlight-face ((t (:inherit ido-first-match))))

   ;; company
   `(company-preview ((,class (:background ,background-darker :foreground ,default))))
   `(company-preview-common ((,class (:background ,background-darker :foreground ,purple))))
   `(company-preview-search ((,class (:background ,blue :foreground ,default))))
   `(company-tooltip ((,class (:background ,background-darker :foreground ,default))))
   `(company-scrollbar-bg ((,class (:background ,background-darker))))
   `(company-scrollbar-fg ((,class (:background ,background-blue))))
   `(company-tooltip-common ((,class (:foreground ,purple :weight bold :background ,background-darker))))
   `(company-tooltip-annotation ((,class (:foreground ,blue :weight bold :background ,background-blue))))
   `(company-tooltip-common-selection ((,class (:foreground ,purple :background ,background-lighter :weight bold))))
   `(company-tooltip-selection ((,class (:foreground ,default :background ,background-lighter))))
   `(company-tooltip-mouse ((,class (:foreground ,default :background ,background-lighter))))

   ;; web-mode
   `(web-mode-html-tag-face ((,class (:foreground ,purple :weight bold))))
   `(web-mode-symbol-face ((,class (:foreground ,red :weight bold))))

   ;; js2-mode
   `(js2-function-param ((,class (:foreground ,blue))))
   `(js2-error ((,class (:foreground ,red))))
   `(js2-jsdoc-html-tag-name ((,class (:foreground ,peach))))
   `(js2-jsdoc-html-tag-delimiter ((,class (:foreground ,peach))))

   ;; flycheck
   `(flycheck-fringe-error ((,class (:foreground ,red :background ,background-red :weight bold :inverse-video t))))
   `(flycheck-fringe-warning ((,class (:background ,background-orange :foreground ,orange :weight bold :inverse-video t))))
   `(flycheck-fringe-info ((,class (:background ,background-blue :foreground ,blue :weight bold :inverse-video t))))
   `(flycheck-warning ((,class (:underline (:color ,red :style wave)))))
   `(flycheck-error ((,class (:underline (:color ,red :style wave)))))

   ;; FIC
   `(font-lock-fic-face ((,class (:foreground ,background :background ,red :weight bold))))

   ;; org-mode todo
   `(org-hide ((,class (:foreground ,background))))
   `(org-todo ((,class (:foreground ,red :background ,background-red :weight bold))))
   `(org-done ((,class (:foreground ,blue :background ,background-blue :weight bold))))
   `(org-date ((,class (:background ,background-lighter))))
   `(org-scheduled-previously ((,class (:foreground ,red))))
   `(org-scheduled ((,class (:foreground ,default))))
   `(org-upcoming-deadline ((,class (:foreground ,orange))))
   `(org-headline-done ((,class (:foreground ,comment))))
   `(outline-1 ((,class (:foreground ,blue :weight bold))))
   `(outline-2 ((,class (:foreground ,purple :weight bold))))
   `(outline-3 ((,class (:foreground ,peach :weight bold))))
   `(outline-4 ((,class (:foreground ,green-light :weight bold))))
   `(outline-5 ((,class (:foreground ,blue :weight bold))))
   `(outline-6 ((,class (:foreground ,purple :weight bold))))
   `(outline-7 ((,class (:foreground ,peach :weight bold))))
   `(outline-8 ((,class (:foreground ,green-light :weight bold))))
   `(org-column-title ((,class (:foreground unspecified :background unspecified))))
   `(org-agenda-date ((,class (:foreground ,purple :weight bold))))
   `(org-agenda-date-today ((,class (:foreground ,blue :weight bold :background ,background-blue :box 1))))
   `(org-agenda-structure ((,class (:foreground ,blue :weight bold))))
   `(org-scheduled-today ((,class (:foreground ,default :weight bold))))
   `(org-agenda-done ((,class (:foreground ,comment))))
   `(org-time-grid ((,class (:foreground ,comment))))

   ;; org columns
   `(org-column ((,class (:background ,background-darker))))
   `(org-column-title ((,class (:background ,background-blue :foreground ,blue :weight bold))))

   ;; org blocks
   `(org-block-begin-line ((,class (:background ,background-green :foreground ,green-light :height 0.9))))
   `(org-block-end-line ((,class (:background ,background-green :foreground ,green-light :height 0.9))))

   ;; org-drill
   `(org-drill-hidden-cloze-face ((,class (:background ,red :foreground ,background))))
   `(org-drill-visible-cloze-face ((,class (:background ,blue :foreground ,background-blue))))
   `(org-drill-visible-cloze-hint-face ((,class (:background ,green :foreground ,background-green))))

   ;; Gnus faces -- from wombat, feel free to improve :)
   `(gnus-group-news-1 ((,class (:weight bold :foreground "#95e454"))))
   `(gnus-group-news-1-low ((,class (:foreground "#95e454"))))
   `(gnus-group-news-2 ((,class (:weight bold :foreground "#cae682"))))
   `(gnus-group-news-2-low ((,class (:foreground "#cae682"))))
   `(gnus-group-news-3 ((,class (:weight bold :foreground "#ccaa8f"))))
   `(gnus-group-news-3-low ((,class (:foreground "#ccaa8f"))))
   `(gnus-group-news-4 ((,class (:weight bold :foreground "#99968b"))))
   `(gnus-group-news-4-low ((,class (:foreground "#99968b"))))
   `(gnus-group-news-5 ((,class (:weight bold :foreground "#cae682"))))
   `(gnus-group-news-5-low ((,class (:foreground "#cae682"))))
   `(gnus-group-news-low ((,class (:foreground "#99968b"))))
   `(gnus-group-mail-1 ((,class (:weight bold :foreground "#95e454"))))
   `(gnus-group-mail-1-low ((,class (:foreground "#95e454"))))
   `(gnus-group-mail-2 ((,class (:weight bold :foreground "#cae682"))))
   `(gnus-group-mail-2-low ((,class (:foreground "#cae682"))))
   `(gnus-group-mail-3 ((,class (:weight bold :foreground "#ccaa8f"))))
   `(gnus-group-mail-3-low ((,class (:foreground "#ccaa8f"))))
   `(gnus-group-mail-low ((,class (:foreground "#99968b"))))
   `(gnus-header-content ((,class (:foreground "#8ac6f2"))))
   `(gnus-header-from ((,class (:weight bold :foreground "#95e454"))))
   `(gnus-header-subject ((,class (:foreground "#cae682"))))
   `(gnus-header-name ((,class (:foreground "#8ac6f2"))))
   `(gnus-header-newsgroups ((,class (:foreground "#cae682"))))

   ;; which-function
   `(which-func ((,class (:foreground ,purple))))

   `(ediff-even-diff-A ((,class (:background ,highlight :foreground unspecified))))
   `(ediff-even-diff-B ((,class (:background ,highlight :foreground unspecified))))
   `(ediff-even-diff-C ((,class (:background ,highlight :foreground unspecified))))
   `(ediff-odd-diff-A ((,class (:background ,highlight :foreground unspecified))))
   `(ediff-odd-diff-B ((,class (:background ,highlight :foreground unspecified))))
   `(ediff-odd-diff-C ((,class (:background ,highlight :foreground unspecified))))

   ;; ivy
   `(ivy-current-match ((,class (:background ,background-purple :weight bold :foreground ,purple))))
   `(ivy-minibuffer-match-face-1 ((,class (:foreground ,orange))))
   `(ivy-minibuffer-match-face-2 ((,class (:foreground ,green))))
   `(ivy-minibuffer-match-face-3 ((,class (:foreground ,green))))
   `(ivy-minibuffer-match-face-4 ((,class (:foreground ,green))))
   `(ivy-match-required-face ((,class (:foreground ,red :background ,background-red :weight bold))))
   `(ivy-modified-buffer ((,class (:foreground ,red))))
   `(ivy-remote ((,class (:foreground ,blue))))
   `(ivy-highlight-face ((,class (:foreground ,blue :weight bold))))

   ;; visible mark
   `(visible-mark-face1 ((,class (:foreground ,orange-light :inverse-video t))))
   `(visible-mark-face2 ((,class (:foreground ,peach :inverse-video t))))

   ;; show-paren
   `(show-paren-match ((,class (:background ,blue :weight bold))))

   ;; clojure
   `(clojure-keyword-face ((,class (:inherit font-lock-builtin-face))))

   ;; ledger
   `(ledger-font-report-clickable-face ((,class (:foreground ,blue))))
   `(ledger-font-posting-amount-face ((,class (:foreground ,purple))))
   `(ledger-font-posting-date-face ((,class (:foreground ,blue :background ,background-blue :box 1))))
   `(ledger-font-payee-uncleared-face ((,class (:foreground ,default :weight bold))))
   `(ledger-font-payee-cleared-face ((,class (:foreground ,green :weight bold))))
   `(ledger-font-posting-account-face ((,class (:foreground ,default))))
   `(ledger-font-posting-account-pending-face ((,class (:foreground ,red))))
   `(ledger-font-xact-highlight-face ((,class (:background ,background-darker))))
   `(ledger-font-other-face ((,class (:inherit ,font-lock-comment-face))))
   `(ledger-font-periodic-xact-face ((,class (:foreground ,orange))))

   `(diff-hl-change ((,class (:foreground ,bright-background-blue :background ,bright-background-blue))))
   `(diff-hl-delete ((,class (:foreground ,bright-background-red :background ,bright-background-red))))
   `(diff-hl-insert ((,class (:foreground ,bright-background-green :background ,bright-background-green))))

   `(git-gutter:added ((,class (:foreground ,green :background ,background-green))))
   `(git-gutter:deleted ((,class (:foreground ,red :background ,background-red))))
   `(git-gutter:modified ((,class (:foreground ,purple :background ,background-purple))))
   `(git-gutter:separator ((,class (:background ,background-orange))))
   `(git-gutter:unchanged ((,class (:background ,background-orange))))

   `(term-color-black ((,class (:foreground ,default :background ,background-darker))))
   `(term-color-red ((,class (:foreground ,red :background ,background-red))))
   `(term-color-green ((,class (:foreground ,green :background ,background-green))))
   `(term-color-yellow ((,class (:foreground ,orange :background ,background-orange))))
   `(term-color-blue ((,class (:foreground ,blue :background ,background-blue))))
   `(term-color-magenta ((,class (:foreground ,purple :background ,background-purple))))
   `(term-color-cyan ((,class (:foreground ,blue-dark))))
   `(term-color-white ((,class (:foreground ,grey))))
   `(term ((,class (:foreground ,default :background ,background))))
   `(term-default-fg-color ((,class (:inherit term-color-white))))
   `(term-default-bg-color ((,class (:inherit term-color-black))))

   `(sh-heredoc ((,class (:foreground ,orange :weight bold))))

   `(avy-lead-face ((,class :foreground ,red :background ,background-red)))
   `(avy-lead-face-0 ((,class :foreground ,purple :background ,background-purple)))
   `(avy-lead-face-1 ((,class :foreground ,blue :background ,background-blue)))
   `(avy-lead-face-2 ((,class :foreground ,green :background ,background-green)))

   `(erc-nick-default-face ((,class :foreground ,blue :background ,background-blue :weight bold)))
   `(erc-current-nick-face ((,class :foreground ,red :weight bold :background ,background-red)))
   `(erc-my-nick-face ((,class :foreground ,red :weight bold :background ,background-red)))
   `(erc-notice-face ((,class :foreground ,comment)))
   `(erc-input-face ((,class :foreground ,default :weight bold)))
   `(erc-prompt-face ((,class :foreground ,purple :background ,background-purple :weight bold :box 1)))
   `(erc-timestamp-face ((,class :foreground ,purple :weight bold)))

   ;; slack
   `(slack-message-output-header ((,class :foreground ,blue :background ,background-blue :weight bold)))

   `(hydra-face-red ((,class :foreground ,red :weight bold)))
   `(hydra-face-blue ((,class :foreground ,blue :weight bold)))

   ;; elfeed
   `(elfeed-search-date-face ((,class (:foreground ,blue))))
   `(elfeed-search-feed-face ((,class (:foreground ,blue))))
   `(elfeed-search-tag-face ((,class (:foreground ,green))))
   `(elfeed-search-title-face ((,class (:foreground ,purple))))

   ;; wgrep
   `(wgrep-face ((,class (:foreground ,orange))))
   `(wgrep-reject-face ((,class (:foreground ,red :weight bold :background ,background-red))))
   `(wgrep-done-face ((,class (:foreground ,blue :weight bold))))

   ;; AucTeX
   `(font-latex-math-face ((,class :foreground ,green-light)))
   `(font-latex-sectioning-5-face ((,class :foreground ,blue)))
   `(font-latex-string-face ((,class :inherit font-lock-string-face)))
   `(font-latex-bold-face ((,class :foreground ,green :weight bold)))
   `(font-latex-italic-face ((,class :foreground ,green :slant italic)))
   `(font-latex-warning-face ((,class :inherit warning)))

   ;; Anzu
   `(anzu-replace-highlight ((,class :foreground ,red :background ,background-red :strike-through t)))
   `(anzu-replace-to ((,class :foreground ,green :background ,background-green)))
   `(anzu-match-1 ((,class :foreground ,red :background ,background-red :box t)))
   `(anzu-match-2 ((,class :foreground ,red :background ,background-red :box t)))
   `(anzu-match-3 ((,class :foreground ,red :background ,background-red :box t)))
   `(anzu-mode-line ((,class :inherit mode-line :weight bold)))

   ;; ace-window
   `(aw-leading-char-face ((,class :foreground ,red :weight bold)))
   `(aw-background-face ((,class :foreground ,comment)))

   ;; paren-face.el
   `(parenthesis ((,class (:foreground ,comment))))

   ;; rainbow-delimiters
   `(rainbow-delimiters-mismatched-face ((,class :foreground ,red :weight bold :background ,background-red)))
   `(rainbow-delimiters-unmatched-face ((,class :foreground ,red :weight bold :background ,background-red)))

   ;; makefile
   `(makefile-space ((,class (:background ,background-blue))))

   ;; epa
   `(epa-validity-high ((,class (:foreground ,green))))
   `(epa-validity-low ((,class (:foreground ,default))))
   `(epa-validity-disabled ((,class (:foreground ,red :weight bold :background ,background-red))))
   `(epa-field-name ((,class (:foreground ,purple :weight bold))))
   `(epa-field-body ((,class (:foreground ,orange))))

   ;; markup-face
   `(markup-title-0-face ((,class (:foreground ,blue :weight bold :underline t))))
   `(markup-title-1-face ((,class (:foreground ,purple :weight bold :underline t))))
   `(markup-title-2-face ((,class (:foreground ,peach :weight bold :underline t))))
   `(markup-title-3-face ((,class (:foreground ,green-light :weight bold :underline t))))
   `(markup-title-4-face ((,class (:foreground ,blue :weight bold :underline t))))
   `(markup-title-5-face ((,class (:foreground ,purple :weight bold :underline t))))
   `(markup-error-face ((,class (:foreground ,red :background ,background-red :weight bold))))
   `(markup-gen-face ((,class (:foreground ,blue))))
   `(markup-typewriter-face ((,class (:inherit shadow))))
   `(markup-meta-face ((,class (:foreground ,comment))))
   `(markup-meta-hide-face ((,class (:foreground ,comment))))
   `(markup-verbatim-face ((,class (:inherit shadow :background ,background-lighter))))
   `(markup-reference-face ((,class (:inherit link))))
   `(markup-complex-replacement-face ((,class (:background ,background-green))))
   `(markup-secondary-text-face ((,class (:foreground ,comment))))
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
