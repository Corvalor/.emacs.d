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

(provide 'init-default-functions)