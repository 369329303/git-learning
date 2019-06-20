(setq backup-directory-alist '((".*" . "~/.emacs.d/backup"))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; how many of the newest versions to keep
      kept-old-versions 5    ; and how many of the old

      auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t))
      )
;; autosave files #file#
;; backup files file~

(add-hook 'text-mode-hook 'auto-fill-mode)
(show-paren-mode t)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(display-time-mode t)
;; auto close bracket insertion
(electric-pair-mode t)
(setq inhibit-startup-screen t)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(ido-mode t)
; Display ido results vertically, rather than horizontally
(setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
(defun ido-disable-line-truncation () (set (make-local-variable 'truncate-lines) nil))
(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation)
(defun ido-define-keys () ;; C-n/p is more intuitive in vertical layout
(define-key ido-completion-map (kbd "C-n") 'ido-next-match)
(define-key ido-completion-map (kbd "C-p") 'ido-prev-match))
(add-hook 'ido-setup-hook 'ido-define-keys)
;; display any item that contains the chars you typed
(setq ido-enable-flex-matching t)
(setq ido-max-window-height 0.8)



(require 'package)
;; Add melpa package source when using package list
;; (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
			 ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
(package-initialize)

(elpy-enable)
(setq elpy-rpc-python-command "python3")
(setq python-shell-interpreter "python3"
      python-shell-interpreter-args "-i")


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Hack" :foundry "unknown" :slant normal :weight normal :height 163 :width normal)))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(display-time-mode t)
 '(package-selected-packages
   (quote
    (clang-format projectile flycheck web-mode-edit-element use-package solarized-theme elpygen elpy color-theme)))
 '(show-paren-mode t)
 '(tool-bar-mode nil))


(load-theme 'solarized-dark)
(projectile-mode t)
(define-key projectile-mode-map (kbd "M-p") 'projectile-command-map) 

(add-hook
 'c-mode-hook
 (lambda ()
   (local-set-key (kbd "<tab>") 'clang-format-buffer)))
