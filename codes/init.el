(require 'package)

;; Add melpa package source when using package list
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
;; keep the installed packages in .emacs.d
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))

;; update the package metadata if the local cache is missing
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(setq user-full-name "datouguix"
      user-mail-address "datouguix@gmail.com")

;; Always load newest byte code
(setq load-prefer-newer t)

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

(defconst jack-savefile-dir (expand-file-name "savefile" user-emacs-directory))

;; create the savefile dir if it doesn't exist
(unless (file-exists-p jack-savefile-dir)
  (make-directory jack-savefile-dir))


;; the toolbar is just a waste of valuable screen estate
;; in a tty tool-bar-mode does not properly auto-load, and is
;; already disabled anyway
(tool-bar-mode -1)

;; the blinking cursor is nothing, but an annoyance
(blink-cursor-mode -1)

;; disable the annoying bell ring
(setq ring-bell-function 'ignore)

;; disable startup screen
(setq inhibit-startup-screen t)

;; auto fill page width
;; (add-hook 'text-mode-hook 'auto-fill-mode)

;; auto maximize window
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))


;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; hide scroll bar
(toggle-scroll-bar -1)


;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; show line number
;; (global-linum-mode t)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; Emacs modes typically provide a standard means to change the
;; indentation width -- eg. c-basic-offset: use that to adjust your
;; personal indentation width, while maintaining the style (and
;; meaning) of any files you load.
(setq-default indent-tabs-mode nil)   ;; don't use tabs to indent
(setq-default tab-width 4)            ;; but maintain correct appearance

;; Newline at end of file
(setq require-final-newline t)

;; Wrap lines at 80 characters
(setq-default fill-column 80)

;; delete the selection with a keypress
(delete-selection-mode t)

;; store all backup and autosave files in the /tmp/ dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

;; auto refresh dired when file changes
(add-hook 'dired-mode-hook 'auto-revert-mode)

;; 1. utf-8; 2. gbk
(prefer-coding-system 'gbk)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)


(global-set-key (kbd "C-x ;") 'comment-line)

;; switch between .c and .h file quickly
;; (global-set-key (kbd "C-c o") 'ff-find-other-file)

;; smart tab behavior - indent or complete
;; (setq tab-always-indent 'complete)

;; hide show minor mode
;; (add-hook 'prog-mode-hook
;;           'hs-minor-mode)

;; (add-hook 'prog-mode-hook
;;           'highlight-symbol-nav-mode)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-verbose t)


;;; packages for move and search
(use-package paren
  :config
  (show-paren-mode +1))

(use-package elec-pair
  :config
  (electric-pair-mode +1))

(use-package hideshow
  :ensure t
  :disabled
  :commands hs-minor-mode)

(use-package highlight-symbol
  :ensure t
  :disabled
  :commands highlight-symbol-nav-mode)

;; highlight the current line
(use-package hl-line
  :config
  (global-hl-line-mode +1))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  ;; rename after killing uniquified
  (setq uniquify-after-kill-buffer-p t)
  ;; don't muck with special buffers
  (setq uniquify-ignore-buffers-re "^\\*"))

;; saveplace remembers your location in a file when saving files
(use-package saveplace
  :config
  (setq save-place-file (expand-file-name "saveplace" jack-savefile-dir))
  ;; activate it for all buffers
  (setq-default save-place t))

(use-package savehist
  :config
  (setq savehist-additional-variables
        ;; search entries
        '(search-ring regexp-search-ring)
        ;; save every minute
        savehist-autosave-interval 60
        ;; keep the home clean
        savehist-file (expand-file-name "savehist" jack-savefile-dir))
  (savehist-mode +1))

(use-package recentf
  :config
  (setq recentf-save-file (expand-file-name "recentf" jack-savefile-dir)
        recentf-max-saved-items 500
        recentf-max-menu-items 15
        ;; disable recentf-cleanup on Emacs start, because it can cause
        ;; problems with remote files
        recentf-auto-cleanup 'never)
  (recentf-mode +1))

(use-package windmove
  :config
  ;; use shift + arrow keys to switch between visible buffers
  (windmove-default-keybindings))

;; winner mode [C-c Left], [C-c Right]
(use-package winner
  :config
  (winner-mode t))

(use-package dired
  :config
  ;; dired - reuse current buffer by pressing 'a'
  (put 'dired-find-alternate-file 'disabled nil)
  ;; always delete and copy recursively
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)
  ;; if there is a dired buffer displayed in the next window, use its
  ;; current subdir, instead of the current subdir of this dired buffer
  (setq dired-dwim-target t)
  ;; enable some really cool extensions like C-x C-j(dired-jump)
  (require 'dired-x))

(use-package helm
  :ensure t
  :bind (("M-x" . helm-M-x)
         ("C-h r" . helm-occur)
         ("C-h y" . helm-show-kill-ring)
         ("C-h m" . helm-all-mark-rings)
         ("C-h e" . helm-eshell-history)
         ("C-x f" . helm-find-files)
         ("C-x b" . helm-mini)
         ("C-h i" . helm-imenu))
  :config
  (helm-mode 1))

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1))

(use-package swiper
  :ensure t
  :bind (("C-s" . swiper)))

(use-package counsel
  :ensure t
  :config
  ;;  (global-set-key (kbd "M-x") 'counsel-M-x)
  ;;  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> k") 'describe-bindings)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
  ;;  (global-set-key (kbd "C-c g") 'counsel-git)
  ;;  (global-set-key (kbd "C-c j") 'counsel-git-grep)
  (global-set-key (kbd "C-c a") 'counsel-ag)
  (global-set-key (kbd "C-c l") 'counsel-locate)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))

(use-package crux
  :ensure t
  :bind (
         ;; ("C-c o" . crux-open-with)
         ;; ("M-o" . crux-smart-open-line)
         ;; ("C-c n" . crux-cleanup-buffer-or-region)
         ;; ("C-c f" . crux-recentf-find-file)
         ("C-M-n" . crux-indent-defun)
         ;; ("C-c u" . crux-view-url)
         ;; ("C-c e" . crux-eval-and-replace)
         ;; ("C-c w" . crux-swap-windows)
         ("C-c d" . crux-duplicate-current-line-or-region)
         ("C-c M-d" . crux-duplicate-and-comment-current-line-or-region)
         ("C-c D" . crux-delete-file-and-buffer)
         ("C-c n" . crux-rename-buffer-and-file)
         ;; ("C-c t" . crux-visit-term-buffer)
         ("C-c k" . crux-kill-other-buffers)
         ("C-c TAB" . crux-indent-rigidly-and-copy-to-clipboard)
         ("C-c I" . crux-find-user-init-file)
         ("C-c o" . crux-switch-to-previous-buffer)
         ("C-c t" . crux-transpose-windows)
         ;; ("C-c S" . crux-find-shell-init-file)
         ;; ("s-r" . crux-recentf-find-file)
         ;; ("s-j" . crux-top-join-line)
         ;; ("C-^" . crux-top-join-line)
         ;; ("s-k" . crux-kill-whole-line)
         ;; ("C-<backspace>" . crux-kill-line-backwards)
         ;; ("s-o" . crux-smart-open-line-above)
         ;; ([remap move-beginning-of-line] . crux-move-beginning-of-line)
         ([(shift return)] . crux-smart-open-line)
         ([(control shift return)] . crux-smart-open-line-above)
         ;; ([remap kill-whole-line] . crux-kill-whole-line)
         ;; ("C-c s" . crux-ispell-word-then-abbrev)
         ))

(use-package transpose-frame
  :ensure t
  :disabled
  :bind ("C-x t" . transpose-frame))

(use-package avy
  :ensure t
  :bind (;;("C-," . avy-goto-word-or-subword-1)
         ("C-," . avy-goto-char)
         )
  :config
  (setq avy-background t))


(use-package projectile
  :ensure t
  :after helm
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (setq projectile-completion-system 'helm)
  (projectile-mode +1))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package anzu
  :ensure t
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp))
  :config
  (global-anzu-mode))

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package move-text
  :ensure t
  :bind
  (([(meta up)] . move-text-up)
   ([(meta down)] . move-text-down)))

(use-package whitespace
  :ensure t
  :disabled
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook #'whitespace-mode))
  (add-hook 'before-save-hook #'whitespace-cleanup)
  :config
  (setq whitespace-line-column 80) ;; limit line length
  (setq whitespace-style '(face tabs empty trailing lines-tail)))

(use-package super-save
  :ensure t
  :config
  (super-save-mode +1))

(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode +1))

(use-package which-key
  :ensure t
  :config
  (which-key-mode +1))

(use-package undo-tree
  :ensure t
  :config
  ;; autosave the undo-tree history
  (setq undo-tree-history-directory-alist
        `((".*" . ,temporary-file-directory)))
  (setq undo-tree-auto-save-history t))

(use-package smart-mode-line
  :custom
  (sml/no-confirm-load-theme t)
  (sml/theme 'respectful)
  (sml/name-width 32)
  (sml/shorten-modes nil)
  (sml/replacer-regexp-list nil)
  :config (sml/setup))

(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))
(add-hook 'prog-mode-hook 'remove-dos-eol)













































;;; packages for programming language
(use-package yasnippet
  :ensure t
  :commands yas-minor-mode
  :bind ("C-c s" . yas-insert-snippet)
  :hook (prog-mode . yas-minor-mode))

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0.5)
  (setq company-show-numbers t)
  (setq company-minimum-prefix-length 1)
  ;; (setq company-tooltip-limit 10)
  ;; (setq company-tooltip-align-annotations t)
  ;; invert the navigation direction if the the completion popup-isearch-match
  ;; is displayed on top (happens near the bottom of windows)
  ;;  (setq company-tooltip-flip-when-above t)
  (global-company-mode))

(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode))

(use-package lsp-mode
  :ensure t
  :hook (go-mode javascript-mode php-mode html-mode)
  :bind (("M-?" . lsp-find-references)
         ("<backtab>" . lsp-format-buffer))
  )

(use-package lsp-ui
  :ensure t
  :config
  (add-hook 'lsp-mode-hook #'lsp-ui-mode))

(use-package company-lsp
  :ensure t
  :config
  (setq company-lsp-enable-recompletion t)
  (add-to-list 'company-backends 'company-lsp))

;; -------------------- BEGINE C -------------------------------
(use-package cquery
  :ensure t
  :config
  (add-hook 'c-mode-common-hook #'lsp)
  (setq cquery-executable "/usr/local/bin/bin/cquery")
  (setq cquery-extra-init-params '(:completion (:detailedLabel t))))
;; -------------------- END C -------------------------------


;; -------------------- BEGINE GO -------------------------------
;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'c-mode-common-hook #'lsp)
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
;; -------------------- END GO -------------------------------


;; -------------------- BEGIN CONF -------------------------------
(use-package conf-mode
  :mode ("\\.cnf\\'" "\\.ini\\'" ))
;; -------------------- END CONF -------------------------------


;; -------------------- BEGIN MARKDOWN -------------------------------
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  (setq markdown-command "multimarkdown"))
;; -------------------- BEGIN MARKDOWN -------------------------------


;; -------------------- BEGIN WEB -------------------------------
(use-package web-mode
  :ensure t
  :disabled
  :mode ("\\.html?\\'" "\\.css\\'" "\\.php\\'" "\\.js\\'" )
  :config
  (setq web-mode-enable-auto-indentation nil))
;; -------------------- BEGIN WEB -------------------------------


(use-package elpy
  :ensure t
  :disabled
  :config
  (add-hook 'python-mode 'elpy-mode)
  (setq elpy-rpc-python-command "python3")
  (setq python-shell-interpreter "python3")
  (setq python-shell-interpreter-args "-i"))

(use-package slime
  :ensure t
  :disabled
  :config
  (load (expand-file-name "~/quicklisp/slime-helper.el"))
  (setq inferior-lisp-program "sbcl"))

(global-set-key (kbd "C-c e") 'eshell)


;; config changes made through the customize UI will be stored here
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(when (file-exists-p custom-file)
  (load custom-file))

;;; init.el ends here
