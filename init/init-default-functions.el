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
		    (replace-match (concat "SOURCES += " (mapconcat 'identity source-files " \\\\\n           ")))
		))
	(let ((header-files (directory-files-recursive default-directory "\\.h$" 100 nil "\\.pro")))
	    (goto-char 0)
	    (if (re-search-forward "HEADERS\\s-*\\+=\\(\\s-*.*?\\s-*\\\\\n\\)*\\s-*.*" nil t)
		    (replace-match (concat "HEADERS += " (mapconcat 'identity header-files " \\\\\n           ")))
		))
	(let ((form-files (directory-files-recursive default-directory "\\.ui$" 100 nil "\\.pro")))
	    (goto-char 0)
	    (if (re-search-forward "FORMS\\s-*\\+=\\(\\s-*.*?\\s-*\\\\\n\\)*\\s-*.*" nil t)
		    (replace-match (concat "FORMS += " (mapconcat 'identity form-files " \\\\\n           ")))
		))
	))
(provide 'init-default-functions)
