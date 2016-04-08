;; init-i3-packages.el -- initializaion of the packages

;;; Commentary:
;; The packages I am using plus initialization

;;; Code:
(add-to-list 'load-path (expand-file-name "i3-emacs/" user-emacs-directory))
(require 'i3)
(require 'i3-integration)
(i3-one-window-per-frame-mode-on)

(set 'pop-up-frames 'graphic-only)

(use-package frames-only-mode)
(require 'frames-only-mode)

(use-package frame-cmds
    :ensure t)

(use-package oneonone
    :ensure t
    :init
    (setq 1on1-minibuffer-frame-foreground "#F8F8F2")
    (setq 1on1-minibuffer-frame-background "#1B1D1E")
    (setq 1on1-default-frame-foreground "#F8F8F2")
    (setq 1on1-default-frame-background "#1B1D1E")
    (setq 1on1-special-frame-foreground "#F8F8F2")
    (setq 1on1-special-frame-background "#1B1D1E")
    (setq 1on1-help-frame-background "#1B1D1E")
    (setq 1on1-inactive-minibuffer-frame-background "#1B1D1E")
    (setq 1on1-active-minibuffer-frame-background "#1B1D1E")
    (setq 1on1-active-mode-line-background "#1B1D1E")
    (setq 1on1-default-frame-cursor-type 'box)
    (setq 1on1-default-frame-cursor-color "#F8F8F2")
    (setq 1on1-help-frame-mouse+cursor-color "#F8F8F2")
    (setq 1on1-completions-frame-mouse+cursor-color "#F8F8F2")
    (setq 1on1-minibuffer-frame-font "-microsoft-Consolas-normal-normal-normal-*-11-*-*-*-m-0-iso10646-1")
    (setq 1on1-default-frame-font "-microsoft-Consolas-normal-normal-normal-*-11-*-*-*-m-0-iso10646-1")
    (setq 1on1-special-frame-font "-microsoft-Consolas-normal-normal-normal-*-11-*-*-*-m-0-iso10646-1")
    (1on1-emacs)
    (tool-bar-mode -1)
    (menu-bar-mode -1)
    (scroll-bar-mode -1)
)

(defun my-split-vertically()
    ""
    (interactive)
    (shell-command "python ~/.i3/split.py vertical")
    (split-frame-vertically 1)
    )

(defun my-split-horizontally()
    ""
    (interactive)
    (shell-command "python ~/.i3/split.py horizontal")
    (split-frame-horizontally 1)
    )

(global-set-key (kbd "s-v") 'my-split-vertically)
(global-set-key (kbd "s-h") 'my-split-horizontally)

(setq initial-frame-alist (append '((minibuffer . nil)) initial-frame-alist))
(setq default-frame-alist (append '((minibuffer . nil)) default-frame-alist))
(setq minibuffer-frame-alist (append '((top . 1)(left . 1)(height . 1)(width . 127)) minibuffer-frame-alist))
(setq minibuffer-auto-raise t)
(setq minibuffer-exit-hook '(lambda () (lower-frame)))

(shell-command "i3-msg 'workspace 1; append_layout ~/.i3/emacs_layout2'")

(defun helm-clean-up-minibuffer() ())

(use-package fullframe
    :ensure t)
(require 'fullframe)

(setq magit-display-buffer-function
      (lambda (buffer)
        (if magit-display-buffer-noselect
            ;; the code that called `magit-display-buffer-function'
            ;; expects the original window to stay alive, we can't go
            ;; fullscreen
            (magit-display-buffer-traditional buffer)
          (delete-other-windows)
          ;; make sure the window isn't dedicated, otherwise
          ;; `set-window-buffer' throws an error
          (set-window-dedicated-p nil nil)
          (set-window-buffer nil buffer)
          ;; return buffer's window
          (get-buffer-window buffer))))
(fullframe magit-status magit-mode-quit-window)

;;(add-hook 'magit-mode-hook (lambda () (switch-to-buffer "magit.*" nil t)))

(provide 'init-i3-packages)
;;; init-i3-packages.el ends here
