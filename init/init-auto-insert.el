(defun include-guard-start()
    (let ((file-name-upper-case
	(replace-regexp-in-string "\\." "_" (upcase (file-name-nondirectory (buffer-file-name))))))
	(concat
	    "#ifndef " file-name-upper-case "\n"
	    "#define " file-name-upper-case "\n")))

(defun include-guard-end()
    (let ((file-name-upper-case
	(replace-regexp-in-string "\\." "_" (upcase (file-name-nondirectory (buffer-file-name))))))
	    (concat
		"#endif // " file-name-upper-case )))

(defun find-namespaces()
    (when (boundp 'path-to-namespace)
	(setq real-path "")
	(setq real-namespace "")
	(dolist (pair path-to-namespace)
	    (let ((path (eval (car pair)))
		  (namespace (eval (cdr pair)))
		  (curr-path (file-name-directory (buffer-file-name))))
		(if (string-prefix-p path curr-path)
			(when (< (length real-path) (length path))
				(setq real-path path)
				(setq real-namespace namespace)
				(setq rel-path (string-remove-prefix path curr-path))))))
	(cons real-namespace (delete "lib" (delete "" (split-string rel-path "/"))))))

(defun namespace-start()
    (setq namespaces "")
    (dolist (value (find-namespaces))
	(setq namespaces (concat namespaces "namespace " value " {\n")))
    namespaces )

(defun namespace-end()
    (setq namespaces "")
    (dolist (value (find-namespaces))
	(setq namespaces (concat namespaces "}\n")))
    namespaces )

(defun header-include()
    (setq file-name-raw (car (s-split "\\." (file-name-nondirectory (buffer-file-name)))))
    (concat "#include \"" file-name-raw ".h\"\n") )


(setq auto-insert-alist nil)

(eval-after-load 'autoinsert
  '(define-auto-insert
	'("\\.\\([Hh]\\|hh\\|hpp\\)\\'" . "C/C++ header default")
		'(	nil
			(include-guard-start)
			(namespace-start)
			> _ \n
			(namespace-end)
			(include-guard-end)
		)
		)
  )

(eval-after-load 'autoinsert
  '(define-auto-insert
	'("\\.\\(cc\\|cpp\\)\\'" . "C/C++ source default")
		'(	nil
			(header-include)
			\n
			(namespace-start)
			> _ \n
			(namespace-end)
		) 
	)
)

(defun do-auto-insert-if-new()
    "Executed auto-insert if current buffer is associated with a file and that
     file does not exist yet"
    (message "test")
    (if buffer-file-name
	    (if (not (file-exists-p buffer-file-name))
		    (auto-insert))))

(add-hook 'c++-mode-hook 'do-auto-insert-if-new)
(setq auto-insert-query nil)

(provide 'init-auto-insert)
