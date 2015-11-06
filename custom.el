;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ag-arguments
   (quote
	("--line-number" "--smart-case" "--nogroup" "--column" "--stats" "--")))
 '(ag-ignore-list
   (quote
	("Dependencies" "_build" "_output" "FlashData" "Dokumentation" "Encryption" "Scripts")))
 '(custom-safe-themes
   (quote
	("c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "26614652a4b3515b4bbbb9828d71e206cc249b67c9142c06239ed3418eff95e2" default)))
 '(global-linum-mode t)
 '(helm-ag-base-command "ag --nocolor --nogroup --ignore-case")
 '(helm-ag-command-option "--all-test")
 '(helm-ag-ignore-patterns (quote ("Dependencies" "_build" "_output" "Scripts")))
 '(helm-ag-insert-at-point (quote symbol))
 '(safe-local-variable-values
   (quote
	((eval progn
		   (setq compile-command
				 (concat "msbuild /nologo /v:m /property:GenerateFullPaths=true "
						 (projectile-project-root)
						 "/_build/Maximus.sln /p:Configuration=PCDebug /p:Platform=Win32 /m /t:Engine"))
		   (add-to-list
			(quote ag-ignore-list)
			"Dependencies")
		   (add-to-list
			(quote ag-ignore-list)
			"_output")
		   (add-to-list
			(quote ag-ignore-list)
			"_build")
		   (add-to-list
			(quote ag-ignore-list)
			"Scripts"))
	 (eval progn
		   (setq compile-command
				 (concat "msbuild /nologo /v:m /property:GenerateFullPaths=true "
						 (projectile-project-root)
						 "/_build/Maximus.sln /p:Configuration=PCDebug /p:Platform=Win32 /m"))
		   (add-to-list
			(quote ag-ignore-list)
			"Dependencies")
		   (add-to-list
			(quote ag-ignore-list)
			"_output")
		   (add-to-list
			(quote ag-ignore-list)
			"_build")
		   (add-to-list
			(quote ag-ignore-list)
			"Scripts"))
	 (eval progn
		   (setq compile-command
				 (concat "msbuild /nologo /v:m /property:GenerateFullPaths=true "
						 (projectile-project-root)
						 "/_build/Maximus.sln /p:Configuration=PCDebug /p:Platform=Win32 /m")))
	 (eval progn
		   (setq compile-command
				 (concat "msbuild /nologo /v:m /property:GenerateFullPaths=true "
						 (projectile-project-root)
						 "/_build/Maximus.sln /p:Configuration=PCDebug /p:Platform=Win32 /m (windows/?)")))
	 (eval progn
		   (setq compile-command
				 (projectile-project-root)))
	 (eval progn
		   (setq compile-command "test"))
	 (eval progn
		   (setq compile-command
				 ("test"))))))
 '(sml/mode-width
   (if
		   (eq powerline-default-separator
			   (quote arrow))
		   (quote right)
	   (quote full)))
 '(sml/pos-id-separator
   (quote
	(""
	 (:propertize " " face powerline-active1)
	 (:eval
	  (propertize " "
				  (quote display)
				  (funcall
				   (intern
					(format "powerline-%s-%s" powerline-default-separator
							(car powerline-default-separator-dir)))
				   (quote powerline-active1)
				   (quote powerline-active2))))
	 (:propertize " " face powerline-active2))))
 '(sml/pos-minor-modes-separator
   (quote
	(""
	 (:propertize " " face powerline-active1)
	 (:eval
	  (propertize " "
				  (quote display)
				  (funcall
				   (intern
					(format "powerline-%s-%s" powerline-default-separator
							(cdr powerline-default-separator-dir)))
				   (quote powerline-active1)
				   nil)))
	 (:propertize " " face sml/global))))
 '(sml/pre-id-separator
   (quote
	(""
	 (:propertize " " face sml/global)
	 (:eval
	  (propertize " "
				  (quote display)
				  (funcall
				   (intern
					(format "powerline-%s-%s" powerline-default-separator
							(car powerline-default-separator-dir)))
				   nil
				   (quote powerline-active1))))
	 (:propertize " " face powerline-active1))))
 '(sml/pre-minor-modes-separator
   (quote
	(""
	 (:propertize " " face powerline-active2)
	 (:eval
	  (propertize " "
				  (quote display)
				  (funcall
				   (intern
					(format "powerline-%s-%s" powerline-default-separator
							(cdr powerline-default-separator-dir)))
				   (quote powerline-active2)
				   (quote powerline-active1))))
	 (:propertize " " face powerline-active1))))
 '(sml/pre-modes-separator (propertize " " (quote face) (quote sml/modes))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#1B1D1E" :foreground "#F8F8F2" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 80 :width normal :foundry "outline" :family "consolas")))))
