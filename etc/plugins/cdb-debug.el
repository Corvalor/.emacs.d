;;; cdb-debug.el --- Attempt at creating a debug UI based on gud-cdb

;;; Commentary:
;; This code will act as a graphical user interface to cdb via gud.

;;; Code:

(require 'gud)

(defvar cdb-buffer-rules '())

(defvar-local cdb-buffer-type nil
  "One of the symbols bound in `cdb-buffer-rules'.")

(defvar cdb-thread-number nil
  "Main current thread.

Invalidation triggers use this variable to query cdb for
information on the specified thread by wrapping cdb/MI commands
in `cdb-current-context-command'.

This variable may be updated implicitly by cdb via `cdb-stopped'
or explicitly by `cdb-select-thread'.

Only `cdb-setq-thread-number' should be used to change this
value.")

(defvar cdb-buf-publisher '()
  "Used to invalidate cdb buffers by emitting a signal in `cdb-update'.
Must be a list of pairs with cars being buffers and cdr's being
valid signal handlers.")

(defvar cdb-source-window nil)

(defvar cdb-main-file nil "Source file from which program execution begins.")

(defun cdb-buffer-type (buffer)
  "Get value of `cdb-buffer-type' for BUFFER."
  (with-current-buffer buffer
    cdb-buffer-type))

(defcustom cdb-show-threads-by-default nil
  "Show threads list buffer instead of breakpoints list by default."
  :type 'boolean
  :group 'cdb-buffers
  :version "23.2")

(defun cdb-get-buffer (buffer-type &optional thread)
  "Get a specific cdb buffer.

In that buffer, `cdb-buffer-type' must be equal to BUFFER-TYPE
and `cdb-thread-number' (if provided) must be equal to THREAD."
  (catch 'found
    (dolist (buffer (buffer-list) nil)
      (with-current-buffer buffer
        (when (and (eq cdb-buffer-type buffer-type)
                   (or (not thread)
                       (equal cdb-thread-number thread)))
          (throw 'found buffer))))))

(defun cdb-rules-name-maker (rules-entry)
  (cadr rules-entry))
(defun cdb-rules-buffer-mode (rules-entry)
  (nth 2 rules-entry))
(defun cdb-rules-update-trigger (rules-entry)
  (nth 3 rules-entry))

(defmacro cdb-add-subscriber (publisher subscriber)
  "Register new PUBLISHER's SUBSCRIBER.

SUBSCRIBER must be a pair, where cdr is a function of one
argument (see `cdb-emit-signal')."
  `(add-to-list ',publisher ,subscriber t))

(defun cdb-bind-function-to-buffer (expr buffer)
  "Return a function which will evaluate EXPR in BUFFER."
  `(lambda (&rest args)
     (with-current-buffer ,buffer
       (apply ',expr args))))

(defun cdb-get-buffer-create (buffer-type &optional thread)
  "Create a new cdb buffer of the type specified by BUFFER-TYPE.
The buffer-type should be one of the cars in `cdb-buffer-rules'.

If THREAD is non-nil, it is assigned to `cdb-thread-number'
buffer-local variable of the new buffer.

Buffer mode and name are selected according to buffer type.

If buffer has trigger associated with it in `cdb-buffer-rules',
this trigger is subscribed to `cdb-buf-publisher' and called with
'update argument."
  (or (cdb-get-buffer buffer-type thread)
      (let ((rules (assoc buffer-type cdb-buffer-rules))
            (new (generate-new-buffer "limbo")))
	(with-current-buffer new
	  (let ((mode (cdb-rules-buffer-mode rules))
                (trigger (cdb-rules-update-trigger rules)))
	    (when mode (funcall mode))
	    (setq cdb-buffer-type buffer-type)
            (when thread
              (set (make-local-variable 'cdb-thread-number) thread))
	    (set (make-local-variable 'gud-minor-mode)
		 (buffer-local-value 'gud-minor-mode gud-comint-buffer))
	    (set (make-local-variable 'tool-bar-map) gud-tool-bar-map)
            (rename-buffer (funcall (cdb-rules-name-maker rules)))
	    (when trigger
              (cdb-add-subscriber cdb-buf-publisher
                                  (cons (current-buffer)
                                        (cdb-bind-function-to-buffer
                                         trigger (current-buffer))))
              (funcall trigger 'start))
            (current-buffer))))))

(defun cdb-debug()
	"Calls the method 'cdb-debug-setup-windows'."
	(interactive)
	(cdb-debug-setup-windows))

(defun cdb-debug-setup-windows ()
  "Layout the window pattern for debug menu."
  (cdb-get-buffer-create 'cdb-locals-buffer)
 ;; (cdb-get-buffer-create 'cdb-stack-buffer)
  ;;(cdb-get-buffer-create 'cdb-breakpoints-buffer)
  (set-window-dedicated-p (selected-window) nil)
  (switch-to-buffer gud-comint-buffer)
  (delete-other-windows)
  (let ((win0 (selected-window))
        (win1 (split-window nil ( / ( * (window-height) 3) 4)))
        (win2 (split-window nil ( / (window-height) 3)))
        (win3 (split-window-right)))
    (cdb-set-window-buffer (cdb-locals-buffer-name) nil win3)
    (select-window win2)
    (set-window-buffer
     win2
     (if gud-last-last-frame
         (gud-find-file (car gud-last-last-frame))
       (if cdb-main-file
           (gud-find-file cdb-main-file)
         ;; Put buffer list in window if we
         ;; can't find a source file.
         (list-buffers-noselect))))
    (setq cdb-source-window (selected-window))
    (let ((win4 (split-window-right)))
      (cdb-set-window-buffer
       (cdb-get-buffer-create 'cdb-inferior-io) nil win4))
    (select-window win1)
    (cdb-set-window-buffer (cdb-stack-buffer-name))
    (let ((win5 (split-window-right)))
      (cdb-set-window-buffer (if cdb-show-threads-by-default
                                 (cdb-threads-buffer-name)
                               (cdb-breakpoints-buffer-name))
                             nil win5))
    (select-window win0)))

(defun cdb-set-window-buffer (name &optional ignore-dedicated window)
  "Set buffer of selected window to NAME and dedicate window.

When IGNORE-DEDICATED is non-nil, buffer is set even if selected
window is dedicated."
  (unless window (setq window (selected-window)))
  (when ignore-dedicated
    (set-window-dedicated-p window nil))
  (set-window-buffer window (get-buffer name))
  (set-window-dedicated-p window t))

(defun cdb-locals-buffer-name ()
  (cdb-current-context-buffer-name
   (concat "locals of " (cdb-get-target-string))))

(defun cdb-stack-buffer-name ()
  (cdb-current-context-buffer-name
   (concat "stack frames of " (cdb-get-target-string))))

(defun cdb-threads-buffer-name ()
  (concat "*threads of " (cdb-get-target-string) "*"))

(defun cdb-breakpoints-buffer-name ()
  (concat "*breakpoints of " (cdb-get-target-string) "*"))

(defun cdb-current-context-buffer-name (name)
  "Add thread information and asterisks to string NAME.

If `cdb-thread-number' is nil, just wrap NAME in asterisks."
  (concat "*" name
          (if (local-variable-p 'cdb-thread-number)
              (format " (bound to thread %s)" cdb-thread-number)
            "")
          "*"))

(defun cdb-get-target-string ()
  (with-current-buffer gud-comint-buffer
    gud-target-name))

(provide 'cdb-debug)
;;; cdb-debug.el ends here
