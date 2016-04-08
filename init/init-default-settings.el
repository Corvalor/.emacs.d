;;; inti-default-settings.el -- default settings
;;; Commentary:
;; Some default settings I want in every session, buffer and/or mode

;;; Code:
(require 'init-default-functions)

;; Push the custom variables somewhere else
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Disable the creation of lock files, would prefer to put them somewhere else, but disabling it seems
;; to be the only option right now
(setq create-lockfiles nil)

;; Put the temporary files (auto-save and backup) into a different folder to keep the source tree clean
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(require 'package)
(package-initialize)

;; Add additional package sources
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

(require 'cc-mode)
(setq-default c-basic-offset 4 c-default-style "k&r")
(setq-default tab-width 4 indent-tabs-mode nil)
(define-key c-mode-base-map (kbd "RET") 'newline-and-indent)
;; Disable Automatic Line break, only leave it enable in selected modes
(setq-default truncate-lines 1)
(setq nxml-child-indent 4)
(add-hook 'gdb-inferior-io-mode-hook 'visual-line-mode)
(add-hook 'compilation-mode-hook (lambda () (setq truncate-lines nil)))
(add-hook 'org-mode-hook (lambda () (setq truncate-lines nil)))
;; Seems to be called after compilation-mode-hook, since I don't want automatic
;; new lines, I redisable it
(add-hook 'ag-mode-hook (lambda () (setq truncate-lines 1)))

(setq lisp-body-indent 4)

;;Disable the varous things in the window-system, that we don't want
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(add-to-list 'custom-theme-load-path (df_emacs.d "etc/themes"))

;; Enable Line Numbers
(global-linum-mode t)

;; Enable Line Highlight
(global-hl-line-mode t)

(eval-after-load "cc-mode"
  '(define-key c-mode-base-map (kbd "RET") 'c-context-line-break))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(set-frame-font "Consolas-9:antialias=subpixel")

(server-start)

(provide 'init-default-settings)
;;; init-default-settings.el ends here
