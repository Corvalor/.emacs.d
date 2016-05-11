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
    (shell-command-on-region (mark) (point) "export XMLLINT_INDET=$'\t' && xmllint --format -" (buffer-name) t)
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

(provide 'init-default-functions)
