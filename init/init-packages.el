;; init-packages.el -- initializaion of the packages

;;; Commentary:
;; The packages I am using plus initialization

;;; Code:
;; Ensure, that the use-package package is installed and install it if not

(if (not (package-installed-p 'use-package))
	(progn
	  (package-refresh-contents)
	  (package-install 'use-package)))
	  
(eval-when-compile
  (require 'use-package))
  
(use-package rich-minority
	:ensure t
	:init
	(setq rm-blacklist (quote (
		" Abbrev"
		" company"
		" Undo-Tree"
		" Helm"
		" $"
	)))
	(rich-minority-mode)
)

(use-package ag
	:ensure t
	:config
)

(use-package helm
	:ensure t
	:config
	(helm-mode 1)
	(global-set-key (kbd "C-x b") 'helm-buffers-list)
)

(use-package helm-ag
	:ensure t
	:config
	(custom-set-variables
	 '(helm-ag-base-command "ag --nocolor --nogroup --ignore-case")
	 '(helm-ag-insert-at-point 'symbol)
	)
)

(use-package org
	:ensure t
	:config
	(setq org-directory "~/.org")
	(setq org-default-notes-file (concat org-directory "/notes.org"))
	(setq org-todo-keywords 
		'((sequence "TODO" "|" "DONE"))
	)
	(setq org-todo-keyword-faces
		'(("TODO" . "yellow")
		("DONE" . "green")
		("BLOCKED" . "orange")
		("INVALIDATED" . "orange"))
	)
	(defun my/return-capture-template ()
		(concat "* TODO [" user-login-name "] %?"))
	(setq org-capture-templates
      '(("t" "Todo" entry (file+headline org-default-notes-file "Tasks")
		 (function my/return-capture-template )
		 :empty-lines-after 0) ))
	(setq initial-buffer-choice
		(let ((name (car (last command-line-args))))
			(if (string= name "emacs")
				org-default-notes-file
				name
			)
		)
	)
	(add-hook 'org-agenda-mode-hook '(lambda() (universal-argument) (org-feed-update-all)))
	(add-hook 'org-agenda-mode-hook 'hl-line-mode)
	(setq org-agenda-show-outline-path nil)
)

(use-package evil-org
	:ensure t
	:config
)

(use-package evil-leader
	:ensure t
	:config
	(global-evil-leader-mode)
	(evil-leader/set-leader "<SPC>")
	(setq evil-leader/in-all-states 1)
	(evil-leader/set-key
		"j" 'compile
		"k" '(lambda() (interactive) (cdb (concat "cdb " debug-command)))
		"s" 'projectile-ag
		"t"	'helm-gtags-dwim
		"r"	'helm-gtags-pop-stack
		"o"	'projectile-find-other-file
		"m"	'magit-status
		"c"	'(lambda() (interactive) (org-capture nil "t"))
		";" '(lambda() (interactive) (find-file org-default-notes-file) (universal-argument))
		"v" 'org-insert-todo-subheading-respect-content
		"f" 'org-insert-todo-heading-respect-content
	)
)

(use-package evil-easymotion
	:ensure t
	:config
	(define-prefix-command 'easymotion-prefix)
	(evilem-default-keybindings "C-u")
	(evil-leader/set-key
		  "SPC w" 	(evilem-create 'evil-forward-word-begin)
		  "SPC W" 	(evilem-create 'evil-forward-WORD-begin)
		  "SPC e" 	(evilem-create 'evil-forward-word-end)
		  "SPC E" 	(evilem-create 'evil-forward-WORD-end)
		  "SPC b" 	(evilem-create 'evil-backward-word-begin)
		  "SPC B" 	(evilem-create 'evil-backward-WORD-begin)
		  "SPC ge" 	(evilem-create 'evil-backward-word-end)
		  "SPC gE" 	(evilem-create 'evil-backward-WORD-end)
		  "SPC j" 	(evilem-create 'next-line)
		  "SPC k" 	(evilem-create 'previous-line)
		  "SPC g j" (evilem-create 'next-line)
		  "SPC g k" (evilem-create 'previous-line)

		  "SPC t" (evilem-create 'evil-repeat-find-char
						 (lambda ()
						   (save-excursion
							 (let ((evil-cross-lines t))
							   (call-interactively 'evil-find-char-to))))
						 nil
						 ((evil-cross-lines t)))

		  "SPC T" (evilem-create 'evil-repeat-find-char
						 (lambda ()
						   (save-excursion
							 (let ((evil-cross-lines t))
							   (call-interactively 'evil-find-char-to-backward))))
						 nil
						 ((evil-cross-lines t)))

		  "SPC f" (evilem-create 'evil-repeat-find-char
						 (lambda ()
						   (save-excursion
							 (let ((evil-cross-lines t))
							   (call-interactively 'evil-find-char))))
						 nil
						 ((evil-cross-lines t)))

		  "SPC F" (evilem-create 'evil-repeat-find-char
						 (lambda ()
						   (save-excursion
							 (let ((evil-cross-lines t))
							   (call-interactively 'evil-find-char-backward))))
						 nil
						 ((evil-cross-lines t)))

		  "SPC [[" (evilem-create 'evil-backward-section-begin
						 (lambda ()
						   (setq evil-this-type 'line)))

		  "SPC []" (evilem-create 'evil-backward-section-end
						 (lambda ()
						   (setq evil-this-type 'line)))

		  "SPC ]]" (evilem-create 'evil-forward-section-begin
						 (lambda ()
						   (setq evil-this-type 'line)))

		  "SPC ][" (evilem-create 'evil-forward-section-end
						 (lambda ()
						   (setq evil-this-type 'line)))

		  "SPC  (" (evilem-create 'evil-forward-sentence)
		  "SPC  )" (evilem-create 'evil-backward-sentence)

		  "SPC  n" (evilem-create 'evil-search-next)
		  "SPC  N" (evilem-create 'evil-search-previous)
		  "SPC  *" (evilem-create 'evil-search-word-forward)
		  "SPC  #" (evilem-create 'evil-search-word-backward)

		  "SPC  -" (evilem-create 'evil-previous-line-first-non-blank)
		  "SPC  +" (evilem-create 'evil-next-line-first-non-blank)
	)
)

(use-package evil
	:ensure t
	:config
	(require 'init-evil)
	(evil-mode 1)
	(define-key evil-normal-state-map (kbd "C-p") 'projectile-find-file)
	;; Prevent d, D, c, C and p from yanking the target into the register
	(evil-define-key 'normal (current-global-map) (kbd "d") 'evil-delete-custom)
	(evil-define-key 'visual (current-global-map) (kbd "d") 'evil-delete-custom)
	(evil-define-key 'normal (current-global-map) (kbd "D") 'evil-delete-line-custom)
	(evil-define-key 'visual (current-global-map) (kbd "D") 'evil-delete-line-custom)
	(evil-define-key 'normal (current-global-map) (kbd "c") 'evil-change-custom)
	(evil-define-key 'visual (current-global-map) (kbd "c") 'evil-change-custom)
	(evil-define-key 'normal (current-global-map) (kbd "C") 'evil-change-line-custom)
	(evil-define-key 'visual (current-global-map) (kbd "C") 'evil-change-line-custom)
	;(evil-define-key 'normal (current-global-map) (kbd "p") 'evil-paste-after-custom)
	(evil-define-key 'visual (current-global-map) (kbd "p") 'evil-paste-after-custom)
	(evil-define-key 'normal (current-global-map) (kbd "m") 'evil-delete)
	(evil-define-key 'visual (current-global-map) (kbd "m") 'evil-delete)
	(evil-define-key 'normal (current-global-map) (kbd "M") 'evil-delete-line)
	(evil-define-key 'visual (current-global-map) (kbd "M") 'evil-delete-line)
)

(use-package molokai-theme
	:ensure t
	:config
	(load-theme 'molokai t)
	(load-theme 'molokai-overrides t)
)

(use-package irony
	:ensure t
	:config
	(add-hook 'c++-mode-hook 'irony-mode)
	(add-hook 'c-mode-hook 'irony-mode)
	(add-hook 'objc-mode-hook 'irony-mode)
	(setq w32-pipe-read-delay 0)
	(setq irony-additional-clang-options (quote ("-std=c++11" "-stdlib=libc++")))
	(defun my-irony-mode-hook()
		(define-key irony-mode-map [remap completion-at-point] 'irony-completion-at-point-async)
		(define-key irony-mode-map [remap complete-symbol] 'irony-completion-at-point-async)
	)
	(add-hook 'irony-mode-hook 'my-irony-mode-hook)
	(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
)

(use-package company
	:ensure t
	:config
	(add-hook 'after-init-hook 'global-company-mode)
	(setq company-idle-delay 0.2)
	(setq company-minimum-prefix-length 1)
	(global-set-key "\t" 'company-complete-common)
)

(use-package rtags
	:ensure t
	:config
)

(use-package company-irony
	:ensure t
	:config
	(eval-after-load 'company '(add-to-list 'company-backends 'company-irony))
	(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
)

(use-package company-c-headers
	:ensure t
	:config
	(eval-after-load 'company '(add-to-list 'company-backends 'company-c-headers))
)

(use-package company-racer
	:ensure t
	:config
)

(use-package racer
	:ensure t
	:config
	(setq racer-cmd "~/.cargo/bin/racer")
	(setq racer-rust-src-path "~/.rust/src")
)

(use-package flycheck
	:ensure t
	:config
)

(use-package rust-mode
	:ensure t
	:config
	(add-hook 'rust-mode-hook
		'(lambda()
			(defun brackets-auto-indent () (interactive)
				(insert "}")
				(company-indent-or-complete-common)
			)
			(racer-activate)
			(racer-turn-on-eldoc)
			(set (make-local-variable 'company-backends) '(company-racer))
			(local-set-key (kbd "M-.") #'company-go)
			(local-set-key (kbd "TAB") #'company-indent-or-complete-common)
			(local-set-key (kbd "}") #'brackets-auto-indent)
		)
	)
)

(use-package projectile
	:ensure t
	:config
	(setq projectile-indexing-method 'alien)
	(projectile-global-mode)
	(defun buffer-whole-string (buffer)
		(with-current-buffer buffer
			(save-restriction
				(widen)
				(buffer-substring-no-properties (point-min) (point-max)))))
	(defun projectile-files-with-string (string directory) 
		(let* (
			(ag-ignore-list (-union ag-ignore-list
								(append
								 (projectile-ignored-files-rel) (projectile-ignored-directories-rel)
								 grep-find-ignored-files grep-find-ignored-directories)))
			 (arguments (append (ag/format-ignore ag-ignore-list) '("-l" "--nocolor" "--noheading") ))
				(cmd (format "ag %s -- %s" (mapconcat 'identity arguments " ") string)))
			(projectile-files-from-cmd cmd directory)
		)
	)
)

(use-package helm-projectile
	:ensure t
	:config
	(setq projectile-completion-system 'helm)
	(helm-projectile-on)
)

(use-package nyan-mode
	:ensure t
	:config
)

(use-package yasnippet
	:ensure t
	:config
	(setq yas-snippet-dirs (append yas-snippet-dirs
					   '("~/.emacs.d/snippets")))
	(yas-global-mode 1)
	(define-key yas-minor-mode-map (kbd "<tab>") nil)
	(define-key yas-minor-mode-map (kbd "TAB") nil)
	;;(define-key yas-minor-mode-map (kbd "<the new key>") 'yas-expand)
)

(use-package helm-gtags
	:ensure t
	:init
	(setq
		helm-gtags-ignore-case t
		helm-gtags-auto-update t
		helm-gtags-use-input-at-cursor t
		helm-gtags-pulse-at-cursor t
		helm-gtags-prefix-key "\C-cg"
		helm-gtags-suggested-key-mapping t
	)

	:config
	;; Enable helm-gtags-mode
	(add-hook 'dired-mode-hook 'helm-gtags-mode)
	(add-hook 'eshell-mode-hook 'helm-gtags-mode)
	(add-hook 'c-mode-hook 'helm-gtags-mode)
	(add-hook 'c++-mode-hook 'helm-gtags-mode)
	(add-hook 'asm-mode-hook 'helm-gtags-mode)

	(define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
	(define-key helm-gtags-mode-map (kbd "C-j") 'helm-gtags-select)
	(define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
	(define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
	(define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
	(define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)
)

(use-package evil-magit
	:ensure t
	:config
	(evil-define-key evil-magit-state magit-mode-map "c" 'magit-commit-popup)
)

(use-package wgrep
	:ensure t
	:config
)

(use-package wgrep-ag
	:ensure t
	:config
	(autoload 'wgrep-ag-setup "wgrep-ag")
	(add-hook 'ag-mode-hook 'wgrep-ag-setup)
)

(use-package multiple-cursors
	:ensure t
	:config
)

(use-package magit
	:ensure t
	:config
	(add-to-list 'magit-log-arguments "--date-order")
)

(load "cdb-gud")
(load "cdb-mi")

(global-set-key [f5]    'gud-cont)
(global-set-key [f7]    'gud-tbreak)
(global-set-key [f8]    'gud-step)
(global-set-key [f9]    'gud-break)
(global-set-key [f10]   'gud-next)
(global-set-key [f11]   'gud-finish)

(provide 'init-packages)
;;; init-packages.el ends here
