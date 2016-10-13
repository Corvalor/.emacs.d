;; init-default-functions.el
;; Some custom internal functions to make configuration easier

;; expands the given path to point to a file or directory inside the .emacs.d folder
(defun df_emacs.d (path) (expand-file-name path user-emacs-directory))

(defun rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))


(defun org-insert-todo-subheading-respect-content (arg)
  "Insert a new subheading with TODO keyword or checkbox and demote it.
Works for outline headings and for plain lists alike."
  (interactive "P")
  (org-insert-todo-heading arg '(4))
  (cond
   ((org-at-heading-p) (org-do-demote))
   ((org-at-item-p) (org-indent-item))))

(defun delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (progn
          (delete-file filename)
          (message "Deleted file %s" filename)
          (kill-buffer))))))
(defun xml-format ()
  (interactive)
  (save-excursion
    (shell-command-on-region (point-min) (point-max) "export XMLLINT_INDET=$'\t' && xmllint --format -" (buffer-name) t)
  )
)

;; Changes so it does not ignore my ag-ignore-list
(defun my-projectile-ag (search-term &optional arg)
  "Run an ag search with SEARCH-TERM in the project.

With an optional prefix argument ARG SEARCH-TERM is interpreted as a
regular expression."
  (interactive
   (list (read-from-minibuffer
          (projectile-prepend-project-name (format "Ag %ssearch for: " (if current-prefix-arg "regexp " "")))
          (projectile-symbol-or-selection-at-point))
         current-prefix-arg))
  (if (require 'ag nil 'noerror)
      (let ((ag-command (if arg 'ag-regexp 'ag))
            (ag-ignore-list (-union ag-ignore-list
                                      (append
                                       (projectile-ignored-files-rel) (projectile-ignored-directories-rel)
                                       (projectile--globally-ignored-file-suffixes-glob)
                                       grep-find-ignored-files grep-find-ignored-directories)))
            ;; reset the prefix arg, otherwise it will affect the ag-command
            (current-prefix-arg nil))
        (funcall ag-command search-term (projectile-project-root)))
    (error "Package 'ag' is not available")))

(defun virtualize(beg end)
        (interactive "*r")
        (save-restriction
            (narrow-to-region beg end)
            (save-excursion
                (goto-char (point-min))
                (while (search-forward-regexp "\\(^\\s-*\\)\\([^ ]+? \\)\\(\\w+?\\)(\\(\\(.*?\n?\\)*?\\))" nil t)
                    (replace-match "\\1virtual \\2\\3(\\4) = 0" nil nil )))))

(defun kill-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

;;;
;;; Recursively list files in a given directory
;;;
;;; Author:    daniel m german dmg at uvic dot ca
;;; Copyright: daniel m german
;;; License:   Same as Emacs
;;;
(defun directory-files-recursive (directory match maxdepth ignore ignore-total)
  "List files in DIRECTORY and in its sub-directories. 
   Return files that match the regular expression MATCH but ignore     
   files and directories that match IGNORE (IGNORE is tested before MATCH. Recurse only 
   to depth MAXDEPTH. If zero or negative, then do not recurse.
   If directory containt ignore-total then ignore the entire directory."
  (let* ((files-list '())
         (current-directory-list
          (directory-files directory t))
	  (ignore-found nil))
    ;; while we are in the current directory
     (while current-directory-list
	(let ((f (car current-directory-list)))
         (cond 
          ((and
           ignore ;; make sure it is not nil
           (string-match ignore f))
           ; ignore
            nil
           )
          ((and
           ignore-total ;; make sure it is not nil
           (string-match ignore-total f))
           ; ignore
	   (if (file-name-directory (file-relative-name f))
		(setq ignore-found t))
            nil
           )
          ((and
            (file-regular-p f)
            (file-readable-p f)
            (string-match match f))
          (setq files-list (cons (file-relative-name f) files-list))
           )
          ((and
           (file-directory-p f)
           (file-readable-p f)
           (not (string-equal ".." (substring f -2)))
           (not (string-equal "." (substring f -1)))
           (> maxdepth 0))     
           ;; recurse only if necessary
	    (setq files-list (append files-list (directory-files-recursive f match (- maxdepth -1) ignore ignore-total)))
           )
          (t)
          )
         )
       (setq current-directory-list (cdr current-directory-list))
       )
       (if ignore-found
	(setq files-list '()))
       files-list
     )
    )

(defun qt-update ()
    (interactive)
    (save-excursion
	(let ((source-files (directory-files-recursive default-directory "\\.cpp$" 100 nil "\\.pro")))
	    (goto-char 0)
	    (if (re-search-forward "SOURCES\\s-*\\+=\\(\\s-*.*?\\s-*\\\\\n\\)*\\s-*.*" nil t)
		    (replace-match (concat "SOURCES += " (mapconcat 'identity source-files " \\\\\n           ")) t)
		))
	(let ((header-files (directory-files-recursive default-directory "\\.h$" 100 nil "\\.pro")))
	    (goto-char 0)
	    (if (re-search-forward "HEADERS\\s-*\\+=\\(\\s-*.*?\\s-*\\\\\n\\)*\\s-*.*" nil t)
		    (replace-match (concat "HEADERS += " (mapconcat 'identity header-files " \\\\\n           ")) t)
		))
	(let ((form-files (directory-files-recursive default-directory "\\.ui$" 100 nil "\\.pro")))
	    (goto-char 0)
	    (if (re-search-forward "FORMS\\s-*\\+=\\(\\s-*.*?\\s-*\\\\\n\\)*\\s-*.*" nil t)
		    (replace-match (concat "FORMS += " (mapconcat 'identity form-files " \\\\\n           ")) t)
		))
	))
  (defun uniquify-all-lines-region (start end)
    "Find duplicate lines in region START to END keeping first occurrence."
    (interactive "*r")
    (save-excursion
      (let ((lines) (end (copy-marker end)))
        (goto-char start)
        (while (and (< (point) (marker-position end))
                    (not (eobp)))
          (let ((line (buffer-substring-no-properties
                       (line-beginning-position) (line-end-position))))
            (if (member line lines)
                (delete-region (point) (progn (forward-line 1) (point)))
              (push line lines)
              (forward-line 1)))))))

(defun re-backward(regex &optional endmatch)
  (condition-case nil
    (let ((start_pos (point))
	    (result (re-search-backward regex)))
	(if endmatch
	    (setq result (re-forward endmatch)))
	(goto-char start_pos)
	result)
  (error (point-min))))

(defun re-forward(regex &optional endmatch)
  (condition-case nil
  (let ((start_pos (point))
	(result (re-search-forward regex)))
	(if endmatch
	    (setq result (re-backward endmatch)))
	(goto-char start_pos)
	result)
  (error (point-max))))

(defun current-class-name()
  (let ((class-string (buffer-substring (re-backward "class .*")
					(re-backward "class .*" "$"))))
    (string-match "class \\([a-zA-Z_]+\\)" class-string)
    (match-string 1 class-string)))

(defun pre-method-name(function)
    (string-match "\\(.* \\)[~a-zA-Z_]+(.*" function)
    (match-string 1 function))

(defun method-name(function)
    (string-match ".* \\([~a-zA-Z_]+\\)(.*" function)
    (match-string 1 function))

(defun post-method-name(function)
    (string-match ".* [~a-zA-Z_]+\\(([ a-zA-Z:,\n\r\t_<>]+)\\);" function)
    (match-string 1 function))

(defun remove-whitespace(str)
  (if (string-match "^[ \t]*" str)
      (replace-match "" nil nil str)
      str))

(defun remove-virtual(str)
  (if (string-match "virtual " str)
      (replace-match "" nil nil str)
      str))

(defun remove-override(str)
  (if (string-match "override " str)
      (replace-match "" nil nil str)
      str))

(defun clean-up-function-definition(str)
  (remove-virtual
   (remove-override
    (remove-whitespace str))))

(defun function-definition(function)
  (clean-up-function-definition
   (concat (pre-method-name function)
	   (current-class-name)
	   "::"
	   (method-name function)
	   (post-method-name function)
	   " {}")))

(defun implement-declaration()
  (interactive)
  (let ((end (re-forward ";"))
	(start (max (re-backward ";")
		    (re-backward "{")
		    (re-backward "}")
		    (re-backward "//.*" "$"))))
    (let ((matched (buffer-substring start
				     end )))
      (if (string-match "^[\n\r]*" matched)
	    (setq matched (replace-match "" nil nil matched)))
      (let ((starting-buffer buffer-file-name)
	    (function-definition-to-insert (function-definition matched)))
	(projectile-find-other-file)
	(when (not (equal starting-buffer buffer-file-name))
	  (beginning-of-buffer)
	  (let (( num-namespaces (count-matches "namespace .* {")))
	    (end-of-buffer)
	    (when (not (re-search-backward "}" nil t (+ num-namespaces 1)))
	      (re-search-backward "}" nil nil num-namespaces)
	      (re-search-backward "{"))
	    (end-of-line)
	    (newline-and-indent)
	    (newline-and-indent)
	    (insert function-definition-to-insert)
	    (backward-char)
	    (newline-and-indent)
	    (forward-line -1)
	    (end-of-line)
	    (newline-and-indent)
	    (evil-insert 0)))))))

(provide 'init-default-functions)
