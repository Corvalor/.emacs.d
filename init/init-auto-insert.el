(eval-after-load 'autoinsert
  '(define-auto-insert
	'("\\.\\([Hh]\\|hh\\|hpp\\)\\'" . "C/C++ headerguard")
		'(	nil
			"#ifndef " (replace-regexp-in-string "\\." "_" (upcase (file-name-nondirectory (buffer-file-name)))) "\n"
			"#define " (replace-regexp-in-string "\\." "_" (upcase (file-name-nondirectory (buffer-file-name)))) "\n"
			> _ \n
			"#endif // " (replace-regexp-in-string "\\." "_" (upcase (file-name-nondirectory (buffer-file-name)))) 
		) 
	)
)
	   
	   
(provide 'init-auto-insert)