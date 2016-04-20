;;; cdb-mi.el --- User Interface for running cdb  -*- lexical-binding: t -*-

;; Copyright (C) 2007-2015 Free Software Foundation, Inc.

;; Author: Nick Roberts <nickrob@gnu.org>
;; Maintainer: emacs-devel@gnu.org
;; Keywords: unix, tools

;; This file is part of GNU Emacs.

;; Homepage: http://www.emacswiki.org/emacs/cdb-MI

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Credits:

;; This file was written by Nick Roberts following the general design
;; used in cdb-ui.el for Emacs 22.1 - 23.1.  It was further developed
;; by Dmitry Dzhus <dima@sphinx.net.ru> as part of the Google Summer
;; of Code 2009 Project "Emacs cdb/MI migration".

;;; Commentary:

;; This mode acts as a graphical user interface to cdb.  You can interact with
;; cdb through the GUD buffer in the usual way, but there are also further
;; buffers which control the execution and describe the state of your program.
;; It separates the input/output of your program from that of cdb and displays
;; expressions and their current values in their own buffers.  It also uses
;; features of Emacs 21 such as the fringe/display margin for breakpoints, and
;; the toolbar (see the cdb Graphical Interface section in the Emacs info
;; manual).

;; M-x cdb will start the debugger.

;; This file uses cdb/MI as the primary interface to cdb.  It runs cdb with
;; cdb/MI (-interp=mi) and access CLI using "-interpreter-exec console
;; cli-command".  This code replaces cdb-ui.el and uses MI tokens instead
;; of queues.  Eventually MI should be asynchronous.

;; Windows Platforms:

;; If you are using Emacs and cdb on Windows you will need to flush the buffer
;; explicitly in your program if you want timely display of I/O in Emacs.
;; Alternatively you can make the output stream unbuffered, for example, by
;; using a macro:

;;           #ifdef UNBUFFERED
;;	     setvbuf (stdout, (char *) NULL, _IONBF, 0);
;;	     #endif

;; and compiling with -DUNBUFFERED while debugging.

;; If you are using Cygwin cdb and find that the source is not being displayed
;; in Emacs when you step through it, possible solutions are to:

;;   1) Use Cygwin X Windows and Cygwin Emacs.
;;        (Since 22.1 Emacs builds under Cygwin.)
;;   2) Use MinGW cdb instead.
;;   3) Use cygwin-mount.el

;;; Mac OSX:

;; cdb in Emacs on Mac OSX works best with FSF cdb as Apple have made
;; some changes to the version that they include as part of Mac OSX.
;; This requires cdb version 7.0 or later (estimated release date Aug 2009)
;; as earlier versions do not compile on Mac OSX.

;;; Known Bugs:

;; 1) Stack buffer doesn't parse MI output if you stop in a routine without
;;    line information, e.g., a routine in libc (just a TODO item).

;; TODO:
;; 2) Watch windows to work with threads.
;; 3) Use treebuffer.el instead of the speedbar for watch-expressions?
;; 4) Mark breakpoint locations on scroll-bar of source buffer?

;;; Code:

(require 'gud)
(require 'json)
(require 'bindat)
(require 'cl-lib)

(declare-function speedbar-change-initial-expansion-list
                  "speedbar" (new-default))
(declare-function speedbar-timer-fn "speedbar" ())
(declare-function speedbar-line-text "speedbar" (&optional p))
(declare-function speedbar-change-expand-button-char "speedbar" (char))
(declare-function speedbar-delete-subblock "speedbar" (indent))
(declare-function speedbar-center-buffer-smartly "speedbar" ())

(defvar tool-bar-map)
(defvar speedbar-initial-expansion-list-name)
(defvar speedbar-frame)

(defvar	cdb-memory-address "main")
(defvar	cdb-memory-last-address nil
  "Last successfully accessed memory address.")
(defvar	cdb-memory-next-page nil
  "Address of next memory page for program memory buffer.")
(defvar	cdb-memory-prev-page nil
  "Address of previous memory page for program memory buffer.")

(defvar cdb-thread-number nil
  "Main current thread.

Invalidation triggers use this variable to query cdb for
information on the specified thread by wrapping cdb/MI commands
in `cdb-current-context-command'.

This variable may be updated implicitly by cdb via `cdb-stopped'
or explicitly by `cdb-select-thread'.

Only `cdb-setq-thread-number' should be used to change this
value.")

(defvar cdb-frame-number nil
  "Selected frame level for main current thread.

Updated according to the following rules:

When a thread is selected or current thread stops, set to \"0\".

When current thread goes running (and possibly exits eventually),
set to nil.

May be manually changed by user with `cdb-select-frame'.")

(defvar cdb-frame-address nil "Identity of frame for watch expression.")

;; Used to show overlay arrow in source buffer. All set in
;; cdb-get-main-selected-frame. Disassembly buffer should not use
;; these but rely on buffer-local thread information instead.
(defvar cdb-selected-frame nil
  "Name of selected function for main current thread.")
(defvar cdb-selected-file nil
  "Name of selected file for main current thread.")
(defvar cdb-selected-line nil
  "Number of selected line for main current thread.")

(defvar cdb-threads-list nil
  "Associative list of threads provided by \"-thread-info\" MI command.

Keys are thread numbers (in strings) and values are structures as
returned from -thread-info by `cdb-json-partial-output'. Updated in
`cdb-thread-list-handler-custom'.")

(defvar cdb-running-threads-count nil
  "Number of currently running threads.

If nil, no information is available.

Updated in `cdb-thread-list-handler-custom'.")

(defvar cdb-stopped-threads-count nil
  "Number of currently stopped threads.

See also `cdb-running-threads-count'.")

(defvar cdb-breakpoints-list nil
  "Associative list of breakpoints provided by \"-break-list\" MI command.

Keys are breakpoint numbers (in string) and values are structures
as returned from \"-break-list\" by `cdb-json-partial-output'
\(\"body\" field is used). Updated in
`cdb-breakpoints-list-handler-custom'.")

(defvar cdb-current-language nil)
(defvar cdb-var-list nil
  "List of variables in watch window.
Each element has the form
  (VARNUM EXPRESSION NUMCHILD TYPE VALUE STATUS HAS_MORE FP)
where STATUS is nil (`unchanged'), `changed' or `out-of-scope', FP the frame
address for root variables.")
(defvar cdb-main-file nil "Source file from which program execution begins.")

;; Overlay arrow markers
(defvar cdb-stack-position nil)
(defvar cdb-thread-position nil)
(defvar cdb-disassembly-position nil)

(defvar cdb-location-alist nil
  "Alist of breakpoint numbers and full filenames.
Only used for files that Emacs can't find.")
(defvar cdb-active-process nil
  "GUD tooltips display variable values when t, and macro definitions otherwise.")
(defvar cdb-error "Non-nil when cdb is reporting an error.")
(defvar cdb-macro-info nil
  "Non-nil if cdb knows that the inferior includes preprocessor macro info.")
(defvar cdb-register-names nil "List of register names.")
(defvar cdb-changed-registers nil
  "List of changed register numbers (strings).")
(defvar cdb-buffer-fringe-width nil)
(defvar cdb-last-command nil)
(defvar cdb-prompt-name nil)
(defvar cdb-token-number 0)
(defvar cdb-handler-list '()
  "List of cdb-handler keeping track of all pending cdb commands.")
(defvar cdb-source-file-list nil
  "List of source files for the current executable.")
(defvar cdb-first-done-or-error t)
(defvar cdb-source-window nil)
(defvar cdb-inferior-status nil)
(defvar cdb-continuation nil)
(defvar cdb-supports-non-stop nil)
(defvar cdb-filter-output nil
  "Message to be shown in GUD console.

This variable is updated in `cdb-done-or-error' and returned by
`gud-cdbmi-marker-filter'.")

(defvar cdb-non-stop nil
  "Indicates whether current cdb session is using non-stop mode.

It is initialized to `cdb-non-stop-setting' at the beginning of
every cdb session.")

(defvar-local cdb-buffer-type nil
  "One of the symbols bound in `cdb-buffer-rules'.")

(defvar cdb-output-sink 'nil
  "The disposition of the output of the current cdb command.
Possible values are these symbols:

    `user' -- cdb output should be copied to the GUD buffer
              for the user to see.

    `emacs' -- output should be collected in the partial-output-buffer
	       for subsequent processing by a command.  This is the
	       disposition of output generated by commands that
	       cdb mode sends to cdb on its own behalf.")

(defcustom cdb-discard-unordered-replies t
  "Non-nil means discard any out-of-order cdb replies.
This protects against lost cdb replies, assuming that cdb always
replies in the same order as Emacs sends commands.  When receiving a
reply with a given token-number, assume any pending messages with a
lower token-number are out-of-order."
  :type 'boolean
  :group 'gud
  :version "24.4")

(cl-defstruct cdb-handler
  "Data required to handle the reply of a command sent to cdb."
  ;; Prefix of the command sent to cdb.  The cdb reply for this command
  ;; will be prefixed with this same TOKEN-NUMBER
  (token-number nil :read-only t)
  ;; Callback to invoke when the reply is received from cdb
  (function nil :read-only t)
  ;; PENDING-TRIGGER is used to prevent congestion: Emacs won't send
  ;; two requests with the same PENDING-TRIGGER until a reply is received
  ;; for the first one."
  (pending-trigger nil))

(defun cdb-add-handler (token-number handler-function &optional pending-trigger)
  "Insert a new cdb command handler in `cdb-handler-list'.
Handlers are used to keep track of the commands sent to cdb
and to handle the replies received.
Upon reception of a reply prefixed with TOKEN-NUMBER,
invoke the callback HANDLER-FUNCTION.
If PENDING-TRIGGER is specified, no new cdb commands will be
sent with this same PENDING-TRIGGER until a reply is received
for this handler."

  (push (make-cdb-handler :token-number token-number
                          :function handler-function
                          :pending-trigger pending-trigger)
        cdb-handler-list))

(defun cdb-delete-handler (token-number)
  "Remove the handler TOKEN-NUMBER from `cdb-handler-list'.
Additionally, if `cdb-discard-unordered-replies' is non-nil,
discard all handlers having a token number less than TOKEN-NUMBER."
  (if cdb-discard-unordered-replies

      (setq cdb-handler-list
            (cl-delete-if
             (lambda (handler)
               "Discard any HANDLER with a token number `<=' than TOKEN-NUMBER."
               (when (< (cdb-handler-token-number handler) token-number)
                 (message "WARNING! Discarding cdb handler with token #%d\n"
			  (cdb-handler-token-number handler)))
               (<= (cdb-handler-token-number handler) token-number))
             cdb-handler-list))

    (setq cdb-handler-list
          (cl-delete-if
           (lambda (handler)
             "Discard any HANDLER with a token number `eq' to TOKEN-NUMBER."
             (eq (cdb-handler-token-number handler) token-number))
           cdb-handler-list))))

(defun cdb-get-handler-function (token-number)
  "Return the function callback registered with the handler TOKEN-NUMBER."
  (cdb-handler-function
   (cl-find-if (lambda (handler) (eq (cdb-handler-token-number handler)
                                      token-number))
                cdb-handler-list)))


(defun cdb-pending-handler-p (pending-trigger)
  "Return non-nil if a command handler is pending with trigger PENDING-TRIGGER."
  (cl-find-if (lambda (handler) (eq (cdb-handler-pending-trigger handler)
                                     pending-trigger))
               cdb-handler-list))


(defun cdb-handle-reply (token-number)
  "Handle the cdb reply TOKEN-NUMBER.
This invokes the handler registered with this token number
in `cdb-handler-list' and clears all pending handlers invalidated
by the reception of this reply."
  (let ((handler-function (cdb-get-handler-function token-number)))
    (when handler-function
      (funcall handler-function)
      (cdb-delete-handler token-number))))

(defun cdb-remove-all-pending-triggers ()
  "Remove all pending triggers from cdb-handler-list.
The handlers are left in cdb-handler-list so that replies received
from cdb could still be handled.  However, removing the pending triggers
allows Emacs to send new commands even if replies of previous commands
were not yet received."
  (dolist (handler cdb-handler-list)
    (setf (cdb-handler-pending-trigger handler) nil)))

(defmacro cdb-wait-for-pending (&rest body)
  "Wait for all pending cdb commands to finish and evaluate BODY.

This function checks every 0.5 seconds if there are any pending
triggers in `cdb-handler-list'."
  `(run-with-timer
    0.5 nil
    '(lambda ()
       (if (not (cl-find-if (lambda (handler)
                               (cdb-handler-pending-trigger handler))
                             cdb-handler-list))
	   (progn ,@body)
	 (cdb-wait-for-pending ,@body)))))

;; Publish-subscribe

(defmacro cdb-add-subscriber (publisher subscriber)
  "Register new PUBLISHER's SUBSCRIBER.

SUBSCRIBER must be a pair, where cdr is a function of one
argument (see `cdb-emit-signal')."
  `(add-to-list ',publisher ,subscriber t))

(defmacro cdb-delete-subscriber (publisher subscriber)
  "Unregister SUBSCRIBER from PUBLISHER."
  `(setq ,publisher (delete ,subscriber
                            ,publisher)))

(defun cdb-get-subscribers (publisher)
  publisher)

(defun cdb-emit-signal (publisher &optional signal)
  "Call cdr for each subscriber of PUBLISHER with SIGNAL as argument."
  (dolist (subscriber (cdb-get-subscribers publisher))
    (funcall (cdr subscriber) signal)))

(defvar cdb-buf-publisher '()
  "Used to invalidate cdb buffers by emitting a signal in `cdb-update'.
Must be a list of pairs with cars being buffers and cdr's being
valid signal handlers.")

(defgroup cdb nil
  "cdb graphical interface"
  :group 'tools
  :link '(info-link "(emacs)cdb Graphical Interface")
  :version "23.2")

(defgroup cdb-non-stop nil
  "cdb non-stop debugging settings"
  :group 'cdb
  :version "23.2")

(defgroup cdb-buffers nil
  "cdb buffers"
  :group 'cdb
  :version "23.2")

(defcustom cdb-debug-log-max 128
  "Maximum size of `cdb-debug-log'.  If nil, size is unlimited."
  :group 'cdb
  :type '(choice (integer :tag "Number of elements")
          (const   :tag "Unlimited" nil))
  :version "22.1")

(defcustom cdb-non-stop-setting t
  "When in non-stop mode, stopped threads can be examined while
other threads continue to execute.

cdb session needs to be restarted for this setting to take effect."
  :type 'boolean
  :group 'cdb-non-stop
  :version "23.2")

;; TODO Some commands can't be called with --all (give a notice about
;; it in setting doc)
(defcustom cdb-gud-control-all-threads t
  "When non-nil, GUD execution commands affect all threads when
in non-stop mode.  Otherwise, only current thread is affected."
  :type 'boolean
  :group 'cdb-non-stop
  :version "23.2")

(defcustom cdb-switch-reasons t
  "List of stop reasons for which Emacs should switch thread.
When t, switch to stopped thread no matter what the reason was.
When nil, never switch to stopped thread automatically.

This setting is used in non-stop mode only.  In all-stop mode,
Emacs always switches to the thread which caused the stop."
  ;; exited, exited-normally and exited-signaled are not
  ;; thread-specific stop reasons and therefore are not included in
  ;; this list
  :type '(choice
          (const :tag "All reasons" t)
          (set :tag "Selection of reasons..."
               (const :tag "A breakpoint was reached." "breakpoint-hit")
               (const :tag "A watchpoint was triggered." "watchpoint-trigger")
               (const :tag "A read watchpoint was triggered."
                      "read-watchpoint-trigger")
               (const :tag "An access watchpoint was triggered."
                      "access-watchpoint-trigger")
               (const :tag "Function finished execution." "function-finished")
               (const :tag "Location reached." "location-reached")
               (const :tag "Watchpoint has gone out of scope"
                      "watchpoint-scope")
               (const :tag "End of stepping range reached."
                      "end-stepping-range")
               (const :tag "Signal received (like interruption)."
                      "signal-received"))
          (const :tag "None" nil))
  :group 'cdb-non-stop
  :version "23.2"
  :link '(info-link "(cdb)cdb/MI Async Records"))

(defcustom cdb-stopped-functions nil
  "List of functions called whenever cdb stops.

Each function takes one argument, a parsed MI response, which
contains fields of corresponding MI *stopped async record:

    ((stopped-threads . \"all\")
     (thread-id . \"1\")
     (frame (line . \"38\")
            (fullname . \"/home/sphinx/projects/gsoc/server.c\")
            (file . \"server.c\")
            (args ((value . \"0x804b038\")
                   (name . \"arg\")))
            (func . \"hello\")
            (addr . \"0x0804869e\"))
     (reason . \"end-stepping-range\"))

Note that \"reason\" is only present in non-stop debugging mode.

`bindat-get-field' may be used to access the fields of response.

Each function is called after the new current thread was selected
and cdb buffers were updated in `cdb-stopped'."
  :type '(repeat function)
  :group 'cdb
  :version "23.2"
  :link '(info-link "(cdb)cdb/MI Async Records"))

(defcustom cdb-switch-when-another-stopped t
  "When nil, don't switch to stopped thread if some other
stopped thread is already selected."
  :type 'boolean
  :group 'cdb-non-stop
  :version "23.2")

(defcustom cdb-stack-buffer-locations t
  "Show file information or library names in stack buffers."
  :type 'boolean
  :group 'cdb-buffers
  :version "23.2")

(defcustom cdb-stack-buffer-addresses nil
  "Show frame addresses in stack buffers."
  :type 'boolean
  :group 'cdb-buffers
  :version "23.2")

(defcustom cdb-thread-buffer-verbose-names t
  "Show long thread names in threads buffer."
  :type 'boolean
  :group 'cdb-buffers
  :version "23.2")

(defcustom cdb-thread-buffer-arguments t
  "Show function arguments in threads buffer."
  :type 'boolean
  :group 'cdb-buffers
  :version "23.2")

(defcustom cdb-thread-buffer-locations t
  "Show file information or library names in threads buffer."
  :type 'boolean
  :group 'cdb-buffers
  :version "23.2")

(defcustom cdb-thread-buffer-addresses nil
  "Show addresses for thread frames in threads buffer."
  :type 'boolean
  :group 'cdb-buffers
  :version "23.2")

(defcustom cdb-show-threads-by-default nil
  "Show threads list buffer instead of breakpoints list by default."
  :type 'boolean
  :group 'cdb-buffers
  :version "23.2")

(defvar cdb-debug-log nil
  "List of commands sent to and replies received from cdb.
Most recent commands are listed first.  This list stores only the last
`cdb-debug-log-max' values.  This variable is used to debug cdb-MI.")

;;;###autoload
(define-minor-mode cdb-enable-debug
  "Toggle logging of transaction between Emacs and cdb.
The log is stored in `cdb-debug-log' as an alist with elements
whose cons is send, send-item or recv and whose cdr is the string
being transferred.  This list may grow up to a size of
`cdb-debug-log-max' after which the oldest element (at the end of
the list) is deleted every time a new one is added (at the front)."
  :global t
  :group 'cdb
  :version "22.1")

(defcustom cdb-cpp-define-alist-program "gcc -E -dM -"
  "Shell command for generating a list of defined macros in a source file.
This list is used to display the #define directive associated
with an identifier as a tooltip.  It works in a debug session with
cdb, when `gud-tooltip-mode' is t.

Set `cdb-cpp-define-alist-flags' for any include paths or
predefined macros."
  :type 'string
  :group 'cdb
  :version "22.1")

(defcustom cdb-cpp-define-alist-flags ""
  "Preprocessor flags for `cdb-cpp-define-alist-program'."
  :type 'string
  :group 'cdb
  :version "22.1")

(defcustom cdb-create-source-file-list t
  "Non-nil means create a list of files from which the executable was built.
Set this to nil if the GUD buffer displays \"initializing...\" in the mode
line for a long time when starting, possibly because your executable was
built from a large number of files.  This allows quicker initialization
but means that these files are not automatically enabled for debugging,
e.g., you won't be able to click in the fringe to set a breakpoint until
execution has already stopped there."
  :type 'boolean
  :group 'cdb
  :version "23.1")

(defcustom cdb-show-main nil
  "Non-nil means display source file containing the main routine at startup.
Also display the main routine in the disassembly buffer if present."
  :type 'boolean
  :group 'cdb
  :version "22.1")

(defvar cdbmi-debug-mode t
  "When non-nil, print the messages sent/received from cdb/MI in *Messages*.")

(defun cdb-force-mode-line-update (status)
  (let ((buffer gud-comint-buffer))
    (if (and buffer (buffer-name buffer))
	(with-current-buffer buffer
	  (setq mode-line-process
		(format ":%s [%s]"
			(process-status (get-buffer-process buffer)) status))
	  ;; Force mode line redisplay soon.
	  (force-mode-line-update)))))

;; These two are used for menu and toolbar
(defun cdb-control-all-threads ()
  "Switch to non-stop/A mode."
  (interactive)
  (setq cdb-gud-control-all-threads t)
  ;; Actually forcing the tool-bar to update.
  (force-mode-line-update)
  (message "Now in non-stop/A mode."))

(defun cdb-control-current-thread ()
  "Switch to non-stop/T mode."
  (interactive)
  (setq cdb-gud-control-all-threads nil)
  ;; Actually forcing the tool-bar to update.
  (force-mode-line-update)
  (message "Now in non-stop/T mode."))

(defun cdb-find-watch-expression ()
  (let* ((var (nth (- (line-number-at-pos (point)) 2) cdb-var-list))
	 (varnum (car var)) expr)
    (string-match "\\(var[0-9]+\\)\\.\\(.*\\)" varnum)
    (let ((var1 (assoc (match-string 1 varnum) cdb-var-list)) var2 varnumlet
	  (component-list (split-string (match-string 2 varnum) "\\." t)))
      (setq expr (nth 1 var1))
      (setq varnumlet (car var1))
      (dolist (component component-list)
	(setq var2 (assoc varnumlet cdb-var-list))
	(setq expr (concat expr
			   (if (string-match ".*\\[[0-9]+\\]$" (nth 3 var2))
			       (concat "[" component "]")
			     (concat "." component))))
	(setq varnumlet (concat varnumlet "." component)))
      expr)))

;; noall is used for commands which don't take --all, but only
;; --thread.
(defun cdb-gud-context-command (command &optional noall)
  "When `cdb-non-stop' is t, add --thread option to COMMAND if
`cdb-gud-control-all-threads' is nil and --all option otherwise.
If NOALL is t, always add --thread option no matter what
`cdb-gud-control-all-threads' value is.

When `cdb-non-stop' is nil, return COMMAND unchanged."
  (if cdb-non-stop
      (if (and cdb-gud-control-all-threads
               (not noall)
	       cdb-supports-non-stop)
          (concat command " --all ")
        (cdb-current-context-command command))
    command))

(defmacro cdb-gud-context-call (cmd1 &optional cmd2 noall noarg)
  "`gud-call' wrapper which adds --thread/--all options between
CMD1 and CMD2.  NOALL is the same as in `cdb-gud-context-command'.

NOARG must be t when this macro is used outside `gud-def'"
  `(gud-call
    (concat (cdb-gud-context-command ,cmd1 ,noall) " " ,cmd2)
    ,(when (not noarg) 'arg)))

(defun cdb--check-interpreter (filter proc string)
  (unless (zerop (length string))
    (remove-function (process-filter proc) #'cdb--check-interpreter)
    (unless (memq (aref string 0) '(?^ ?~ ?@ ?& ?* ?=))
      ;; Apparently we're not running with -i=mi.
      (let ((msg "Error: you did not specify -i=mi on cdb's command line!"))
        (message msg)
        (setq string (concat (propertize msg 'font-lock-face 'error)
                             "\n" string)))
      ;; Use the old gud-gbd filter, not because it works, but because it
      ;; will properly display cdb's answers rather than hanging waiting for
      ;; answers that aren't coming.
      (set (make-local-variable 'gud-marker-filter) #'gud-cdb-marker-filter))
    (funcall filter proc string)))

(defvar cdb-control-level 0)

;;;###autoload
(defun cdb_ (command-line)
  "Run cdb on program FILE in buffer *gud-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger.

COMMAND-LINE is the shell command for starting the cdb session.
It should be a string consisting of the name of the cdb
executable followed by command line options.  The command line
options should include \"-i=mi\" to use cdb's MI text interface.
Note that the old \"--annotate\" option is no longer supported.

If option `cdb-many-windows' is nil (the default value) then cdb just
pops up the GUD buffer unless `cdb-show-main' is t.  In this case
it starts with two windows: one displaying the GUD buffer and the
other with the source file with the main routine of the inferior.

If option `cdb-many-windows' is t, regardless of the value of
`cdb-show-main', the layout below will appear.  Keybindings are
shown in some of the buffers.

Watch expressions appear in the speedbar/slowbar.

The following commands help control operation :

`cdb-many-windows'    - Toggle the number of windows cdb uses.
`cdb-restore-windows' - To restore the window layout.

See Info node `(emacs)cdb Graphical Interface' for a more
detailed description of this mode.


+----------------------------------------------------------------------+
|                               cdb Toolbar                            |
+-----------------------------------+----------------------------------+
| GUD buffer (I/O of cdb)           | Locals buffer                    |
|                                   |                                  |
|                                   |                                  |
|                                   |                                  |
+-----------------------------------+----------------------------------+
| Source buffer                     | I/O buffer (of debugged program) |
|                                   | (comint-mode)                    |
|                                   |                                  |
|                                   |                                  |
|                                   |                                  |
|                                   |                                  |
|                                   |                                  |
|                                   |                                  |
+-----------------------------------+----------------------------------+
| Stack buffer                      | Breakpoints buffer               |
| RET      cdb-select-frame         | SPC    cdb-toggle-breakpoint     |
|                                   | RET    cdb-goto-breakpoint       |
|                                   | D      cdb-delete-breakpoint     |
+-----------------------------------+----------------------------------+"
  ;;
  (interactive (list (gud-query-cmdline 'cdb)))

  (when (and gud-comint-buffer
             (buffer-name gud-comint-buffer)
             (get-buffer-process gud-comint-buffer)
             (with-current-buffer gud-comint-buffer (eq gud-minor-mode 'cdba)))
    (cdb-restore-windows)
    (error
     "Multiple debugging requires restarting in text command mode"))
  ;;
  (gud-common-init command-line nil 'gud-cdbmi-marker-filter)

  ;; Setup a temporary process filter to warn when cdb was not started
  ;; with -i=mi.
  (let ((proc (get-buffer-process gud-comint-buffer)))
    (add-function :around (process-filter proc) #'cdb--check-interpreter))

  (set (make-local-variable 'gud-minor-mode) 'cdbmi)
  (set (make-local-variable 'cdb-control-level) 0)
  (setq comint-input-sender 'cdb-send)
  (when (ring-empty-p comint-input-ring) ; cf shell-mode
    (let ((hfile (expand-file-name (or (getenv "cdbHISTFILE")
				       (if (eq system-type 'ms-dos)
					   "_cdb_history"
					 ".cdb_history"))))
	  ;; cdb defaults to 256, but we'll default to comint-input-ring-size.
	  (hsize (getenv "HISTSIZE")))
      (dolist (file (append '("~/.cdbinit")
			    (unless (string-equal (expand-file-name ".")
                                                  (expand-file-name "~"))
			      '(".cdbinit"))))
	(if (file-readable-p (setq file (expand-file-name file)))
	    (with-temp-buffer
	      (insert-file-contents file)
	      ;; TODO? check for "set history save\\(  *on\\)?" and do
	      ;; not use history otherwise?
	      (while (re-search-forward
		      "^ *set history \\(filename\\|size\\)  *\\(.*\\)" nil t)
		(cond ((string-equal (match-string 1) "filename")
		       (setq hfile (expand-file-name
				    (match-string 2)
				    (file-name-directory file))))
		      ((string-equal (match-string 1) "size")
		       (setq hsize (match-string 2))))))))
      (and (stringp hsize)
	   (integerp (setq hsize (string-to-number hsize)))
	   (> hsize 0)
	   (set (make-local-variable 'comint-input-ring-size) hsize))
      (if (stringp hfile)
	  (set (make-local-variable 'comint-input-ring-file-name) hfile))
      (comint-read-input-ring t)))
  (gud-def gud-tbreak "tbreak %f:%l" "\C-t"
	   "Set temporary breakpoint at current line.")
  (gud-def gud-jump
	   (progn (gud-call "tbreak %f:%l") (gud-call "jump %f:%l"))
	   "\C-j" "Set execution address to current line.")

  (gud-def gud-up     "up %p"     "<" "Up N stack frames (numeric arg).")
  (gud-def gud-down   "down %p"   ">" "Down N stack frames (numeric arg).")
  (gud-def gud-print  "print %e"  "\C-p" "Evaluate C expression at point.")
  (gud-def gud-pstar  "print* %e" nil
	   "Evaluate C dereferenced pointer expression at point.")

  (gud-def gud-step   (cdb-gud-context-call "-exec-step" "%p" t)
           "\C-s"
	   "Step one source line with display.")
  (gud-def gud-stepi  (cdb-gud-context-call "-exec-step-instruction" "%p" t)
           "\C-i"
	   "Step one instruction with display.")
  (gud-def gud-next   (cdb-gud-context-call "-exec-next" "%p" t)
           "\C-n"
	   "Step one line (skip functions).")
  (gud-def gud-nexti  (cdb-gud-context-call "-exec-next-instruction" "%p" t)
           nil
	   "Step one instruction (skip functions).")
  (gud-def gud-cont   (cdb-gud-context-call "-exec-continue")
           "\C-r"
	   "Continue with display.")
  (gud-def gud-finish (cdb-gud-context-call "-exec-finish" nil t)
           "\C-f"
	   "Finish executing current function.")
  (gud-def gud-run    "-exec-run"
           nil
           "Run the program.")

  (gud-def gud-break (if (not (string-match "Disassembly" mode-name))
			 (gud-call "break %f:%l" arg)
		       (save-excursion
			 (beginning-of-line)
			 (forward-char 2)
			 (gud-call "break *%a" arg)))
	   "\C-b" "Set breakpoint at current line or address.")

  (gud-def gud-remove (if (not (string-match "Disassembly" mode-name))
			  (gud-call "clear %f:%l" arg)
			(save-excursion
			  (beginning-of-line)
			  (forward-char 2)
			  (gud-call "clear *%a" arg)))
	   "\C-d" "Remove breakpoint at current line or address.")

  ;; -exec-until doesn't support --all yet
  (gud-def gud-until  (if (not (string-match "Disassembly" mode-name))
			  (gud-call "-exec-until %f:%l" arg)
			(save-excursion
			  (beginning-of-line)
			  (forward-char 2)
			  (gud-call "-exec-until *%a" arg)))
	   "\C-u" "Continue to current line or address.")
  ;; TODO Why arg here?
  (gud-def
   gud-go (gud-call (if cdb-active-process
                        (cdb-gud-context-command "-exec-continue")
                      "-exec-run") arg)
   nil "Start or continue execution.")

  ;; For debugging Emacs only.
  (gud-def gud-pp
	   (gud-call
	    (concat
	     "pp " (if (eq (buffer-local-value
			    'major-mode (window-buffer)) 'speedbar-mode)
		       (cdb-find-watch-expression) "%e")) arg)
	   nil   "Print the Emacs s-expression.")

  (define-key gud-minor-mode-map [left-margin mouse-1]
    'cdb-mouse-set-clear-breakpoint)
  (define-key gud-minor-mode-map [left-fringe mouse-1]
    'cdb-mouse-set-clear-breakpoint)
  (define-key gud-minor-mode-map [left-margin C-mouse-1]
    'cdb-mouse-toggle-breakpoint-margin)
  (define-key gud-minor-mode-map [left-fringe C-mouse-1]
    'cdb-mouse-toggle-breakpoint-fringe)

  (define-key gud-minor-mode-map [left-margin drag-mouse-1]
    'cdb-mouse-until)
  (define-key gud-minor-mode-map [left-fringe drag-mouse-1]
    'cdb-mouse-until)
  (define-key gud-minor-mode-map [left-margin mouse-3]
    'cdb-mouse-until)
  (define-key gud-minor-mode-map [left-fringe mouse-3]
    'cdb-mouse-until)

  (define-key gud-minor-mode-map [left-margin C-drag-mouse-1]
    'cdb-mouse-jump)
  (define-key gud-minor-mode-map [left-fringe C-drag-mouse-1]
    'cdb-mouse-jump)
  (define-key gud-minor-mode-map [left-fringe C-mouse-3]
    'cdb-mouse-jump)
  (define-key gud-minor-mode-map [left-margin C-mouse-3]
    'cdb-mouse-jump)

  (set (make-local-variable 'gud-cdb-completion-function)
       'gud-cdbmi-completions)

  (add-hook 'completion-at-point-functions #'gud-cdb-completion-at-point
            nil 'local)
  (local-set-key "\C-i" 'completion-at-point)

  (local-set-key [remap comint-delchar-or-maybe-eof] 'cdb-delchar-or-quit)

  (setq cdb-first-prompt t)
  (setq gud-running nil)

  (cdb-update)

  (run-hooks 'cdb-mode-hook))

(defun cdb-init-1 ()
  ;; (Re-)initialize.
  (setq cdb-selected-frame nil
	cdb-frame-number nil
        cdb-thread-number nil
	cdb-var-list nil
	cdb-output-sink 'user
	cdb-location-alist nil
	cdb-source-file-list nil
	cdb-last-command nil
	cdb-token-number 0
	cdb-handler-list '()
	cdb-prompt-name nil
	cdb-first-done-or-error t
	cdb-buffer-fringe-width (car (window-fringes))
	cdb-debug-log nil
	cdb-source-window nil
	cdb-inferior-status nil
	cdb-continuation nil
        cdb-buf-publisher '()
        cdb-threads-list '()
        cdb-breakpoints-list '()
        cdb-register-names '()
        cdb-non-stop cdb-non-stop-setting)
  ;;
  (cdbmi-bnf-init)
  ;;
  (setq cdb-buffer-type 'cdbmi)
  ;;
  (cdb-force-mode-line-update
   (propertize "initializing..." 'face font-lock-variable-name-face))

  (cdb-get-buffer-create 'cdb-inferior-io)
  (cdb-clear-inferior-io)
  (cdb-inferior-io--init-proc (get-process "cdb-inferior"))

  (when (eq system-type 'windows-nt)
    ;; Don't create a separate console window for the debuggee.
    ;;TOCHECK (cdb-input "-cdb-set new-console off" 'ignore)
    ;; Force cdb to behave as if its input and output stream were
    ;; connected to a TTY device (since on Windows we use pipes for
    ;; communicating with cdb).
    ;; TOCHECK(cdb-input "-cdb-set interactive-mode on" 'ignore)
  )
  ;; TOCHECK(cdb-input "-cdb-set height 0" 'ignore)

  ;; TOCHECK(when cdb-non-stop
  ;; TOCHECK  (cdb-input "-cdb-set non-stop 1" 'cdb-non-stop-handler))

  ;; TOCHECK(cdb-input "-enable-pretty-printing" 'ignore)

  ;; Find source file and compilation directory here.
  (if cdb-create-source-file-list
      ;; Needs cdb 6.2 onwards.
      (cdb-input "-file-list-exec-source-files" 'cdb-get-source-file-list))
  ;; Needs cdb 6.0 onwards.
  (cdb-input "-file-list-exec-source-file" 'cdb-get-source-file)
  (cdb-input "-cdb-show prompt" 'cdb-get-prompt))

(defun cdb-non-stop-handler ()
  (goto-char (point-min))
  (if (re-search-forward "No symbol" nil t)
      (progn
	(message
         "This version of cdb doesn't support non-stop mode.  Turning it off.")
	(setq cdb-non-stop nil)
	(setq cdb-supports-non-stop nil))
    (setq cdb-supports-non-stop t)
    (cdb-input "-cdb-set target-async 1" 'ignore)
    (cdb-input "-list-target-features" 'cdb-check-target-async)))

(defun cdb-check-target-async ()
  (goto-char (point-min))
  (unless (re-search-forward "async" nil t)
    (message
     "Target doesn't support non-stop mode.  Turning it off.")
    (setq cdb-non-stop nil)
    (cdb-input "-cdb-set non-stop 0" 'ignore)))

(defun cdb-delchar-or-quit (arg)
  "Delete ARG characters or send a quit command to cdb.
Send a quit only if point is at the end of the buffer, there is
no input, and cdb is waiting for input."
  (interactive "p")
  (unless (and (eq (current-buffer) gud-comint-buffer)
	       (eq gud-minor-mode 'cdbmi))
    (error "Not in a cdb-MI buffer"))
  (let ((proc (get-buffer-process gud-comint-buffer)))
    (if (and (eobp)
             (process-live-p proc)
	     (not gud-running)
	     (= (point) (marker-position (process-mark proc))))
	;; Sending an EOF does not work with cdb-MI; submit an
	;; explicit quit command.
	(progn
	  (insert "quit")
	  (comint-send-input t t))
      (delete-char arg))))

(defvar cdb-define-alist nil "Alist of #define directives for GUD tooltips.")

(defun cdb-create-define-alist ()
  "Create an alist of #define directives for GUD tooltips."
  (let* ((file (buffer-file-name))
	 (output
	  (with-output-to-string
	    (with-current-buffer standard-output
 	      (and file
		   (file-exists-p file)
 		   ;; call-process doesn't work with remote file names.
		   (not (file-remote-p default-directory))
 		   (call-process shell-file-name file
				 (list t nil) nil "-c"
				 (concat cdb-cpp-define-alist-program " "
					 cdb-cpp-define-alist-flags))))))
         (define-list (split-string output "\n" t))
         (name))
    (setq cdb-define-alist nil)
    (dolist (define define-list)
      (setq name (nth 1 (split-string define "[( ]")))
      (push (cons name define) cdb-define-alist))))

(declare-function tooltip-show "tooltip" (text &optional use-echo-area))

(defconst cdb--string-regexp "\"\\(?:[^\\\"]\\|\\\\.\\)*\"")

(defun cdb-tooltip-print (expr)
  (with-current-buffer (cdb-get-buffer 'cdb-partial-output-buffer)
    (goto-char (point-min))
    (cond
     ((re-search-forward (concat ".*value=\\(" cdb--string-regexp
                                 "\\)")
                         nil t)
      (tooltip-show
       (concat expr " = " (read (match-string 1)))
       (or gud-tooltip-echo-area
	   (not (display-graphic-p)))))
     ((re-search-forward  "msg=\\(\".+\"\\)$" nil t)
      (tooltip-show (read (match-string 1))
       (or gud-tooltip-echo-area
	   (not (display-graphic-p))))))))

;; If expr is a macro for a function don't print because of possible dangerous
;; side-effects. Also printing a function within a tooltip generates an
;; unexpected starting annotation (phase error).
(defun cdb-tooltip-print-1 (expr)
  (with-current-buffer (cdb-get-buffer 'cdb-partial-output-buffer)
    (goto-char (point-min))
    (if (search-forward "expands to: " nil t)
	(unless (looking-at "\\S-+.*(.*).*")
	  (cdb-input (concat "-data-evaluate-expression \"" expr "\"")
		     `(lambda () (cdb-tooltip-print ,expr)))))))

(defun cdb-init-buffer ()
  (set (make-local-variable 'gud-minor-mode) 'cdbmi)
  (set (make-local-variable 'tool-bar-map) gud-tool-bar-map)
  (when gud-tooltip-mode
    (make-local-variable 'cdb-define-alist)
    (cdb-create-define-alist)
    (add-hook 'after-save-hook 'cdb-create-define-alist nil t)))

(defmacro cdb--if-arrow (arrow-position start-posn end-posn &rest body)
  (declare (indent 3))
  (let ((buffer (make-symbol "buffer")))
    `(if ,arrow-position
         (let ((,buffer (marker-buffer ,arrow-position)))
           (if (equal ,buffer (window-buffer (posn-window ,end-posn)))
               (with-current-buffer ,buffer
                 (when (or (equal ,start-posn ,end-posn)
                           (equal (posn-point ,start-posn)
                                  (marker-position ,arrow-position)))
                   ,@body)))))))

(defun cdb-mouse-until (event)
  "Continue running until a source line past the current line.
The destination source line can be selected either by clicking
with mouse-3 on the fringe/margin or dragging the arrow
with mouse-1 (default bindings)."
  (interactive "e")
  (let ((start (event-start event))
	(end (event-end event)))
    (cdb--if-arrow gud-overlay-arrow-position start end
      (let ((line (line-number-at-pos (posn-point end))))
        (gud-call (concat "until " (number-to-string line)))))
    (cdb--if-arrow cdb-disassembly-position start end
      (save-excursion
        (goto-char (point-min))
        (forward-line (1- (line-number-at-pos (posn-point end))))
        (forward-char 2)
        (gud-call (concat "until *%a"))))))

(defun cdb-mouse-jump (event)
  "Set execution address/line.
The destination source line can be selected either by clicking with C-mouse-3
on the fringe/margin or dragging the arrow with C-mouse-1 (default bindings).
Unlike `cdb-mouse-until' the destination address can be before the current
line, and no execution takes place."
  (interactive "e")
  (let ((start (event-start event))
	(end (event-end event)))
    (cdb--if-arrow gud-overlay-arrow-position start end
      (let ((line (line-number-at-pos (posn-point end))))
        (gud-call (concat "tbreak " (number-to-string line)))
        (gud-call (concat "jump " (number-to-string line)))))
    (cdb--if-arrow cdb-disassembly-position start end
      (save-excursion
        (goto-char (point-min))
        (forward-line (1- (line-number-at-pos (posn-point end))))
        (forward-char 2)
        (gud-call (concat "tbreak *%a"))
        (gud-call (concat "jump *%a"))))))

(defcustom cdb-show-changed-values t
  "If non-nil change the face of out of scope variables and changed values.
Out of scope variables are suppressed with `shadow' face.
Changed values are highlighted with the face `font-lock-warning-face'."
  :type 'boolean
  :group 'cdb
  :version "22.1")

(defcustom cdb-max-children 40
  "Maximum number of children before expansion requires confirmation."
  :type 'integer
  :group 'cdb
  :version "22.1")

(defcustom cdb-delete-out-of-scope t
  "If non-nil delete watch expressions automatically when they go out of scope."
  :type 'boolean
  :group 'cdb
  :version "22.2")

(define-minor-mode cdb-speedbar-auto-raise
  "Minor mode to automatically raise the speedbar for watch expressions.
With prefix argument ARG, automatically raise speedbar if ARG is
positive, otherwise don't automatically raise it."
  :global t
  :group 'cdb
  :version "22.1")

(defcustom cdb-use-colon-colon-notation nil
  "If non-nil use FUN::VAR format to display variables in the speedbar."
  :type 'boolean
  :group 'cdb
  :version "22.1")

(define-key gud-minor-mode-map "\C-c\C-w" 'gud-watch)
(define-key global-map (vconcat gud-key-prefix "\C-w") 'gud-watch)

(declare-function tooltip-identifier-from-point "tooltip" (point))

(defun gud-watch (&optional arg event)
  "Watch expression at point.
With arg, enter name of variable to be watched in the minibuffer."
  (interactive (list current-prefix-arg last-input-event))
  (let ((minor-mode (buffer-local-value 'gud-minor-mode gud-comint-buffer)))
    (if (eq minor-mode 'cdbmi)
	(progn
	  (if event (posn-set-point (event-end event)))
	  (require 'tooltip)
	  (save-selected-window
	    (let ((expr
		   (if arg
		       (completing-read "Name of variable: "
					'gud-cdb-complete-command)
		     (if (and transient-mark-mode mark-active)
			 (buffer-substring (region-beginning) (region-end))
		       (concat (if (derived-mode-p 'cdb-registers-mode) "$")
			       (tooltip-identifier-from-point (point)))))))
	      (set-text-properties 0 (length expr) nil expr)
	      (cdb-input (concat "-var-create - * "  expr "")
			 `(lambda () (cdb-var-create-handler ,expr))))))
      (message "gud-watch is a no-op in this mode."))))

(defun cdb-var-create-handler (expr)
  (let* ((result (cdb-json-partial-output)))
    (if (not (bindat-get-field result 'msg))
        (let ((var
	       (list (bindat-get-field result 'name)
		     (if (and (string-equal cdb-current-language "c")
			      cdb-use-colon-colon-notation cdb-selected-frame)
			 (setq expr (concat cdb-selected-frame "::" expr))
		       expr)
		     (bindat-get-field result 'numchild)
		     (bindat-get-field result 'type)
		     (bindat-get-field result 'value)
		     nil
		     (bindat-get-field result 'has_more)
                     cdb-frame-address)))
	  (push var cdb-var-list)
	  (speedbar 1)
	  (unless (string-equal
		   speedbar-initial-expansion-list-name "GUD")
	    (speedbar-change-initial-expansion-list "GUD")))
      (message-box "No symbol \"%s\" in current context." expr))))

(defun cdb-speedbar-update ()
  (when (and (boundp 'speedbar-frame) (frame-live-p speedbar-frame))
    ;; Dummy command to update speedbar even when idle.
    (cdb-input "-environment-pwd"
               'cdb-speedbar-timer-fn
               'cdb-speedbar-update)))

(defun cdb-speedbar-timer-fn ()
  (if cdb-speedbar-auto-raise
      (raise-frame speedbar-frame))
  (speedbar-timer-fn))

(defun cdb-var-evaluate-expression-handler (varnum changed)
  (goto-char (point-min))
  (re-search-forward (concat ".*value=\\(" cdb--string-regexp "\\)")
                     nil t)
  (let ((var (assoc varnum cdb-var-list)))
    (when var
      (if changed (setcar (nthcdr 5 var) 'changed))
      (setcar (nthcdr 4 var) (read (match-string 1)))))
  (cdb-speedbar-update))

                                        ; Uses "-var-list-children --all-values".  Needs cdb 6.1 onwards.
(defun cdb-var-list-children (varnum)
  (cdb-input (concat "-var-update " varnum) 'ignore)
  (cdb-input (concat "-var-list-children --all-values " varnum)
	     `(lambda () (cdb-var-list-children-handler ,varnum))))

(defun cdb-var-list-children-handler (varnum)
  (let* ((var-list nil)
	 (output (bindat-get-field (cdb-json-partial-output "child")))
	 (children (bindat-get-field output 'children)))
    (catch 'child-already-watched
      (dolist (var cdb-var-list)
	(if (string-equal varnum (car var))
	    (progn
	      ;; With dynamic varobjs numchild may have increased.
	      (setcar (nthcdr 2 var) (bindat-get-field output 'numchild))
	      (push var var-list)
	      (dolist (child children)
		(let ((varchild (list (bindat-get-field child 'name)
				      (bindat-get-field child 'exp)
				      (bindat-get-field child 'numchild)
				      (bindat-get-field child 'type)
				      (bindat-get-field child 'value)
				      nil
				      (bindat-get-field child 'has_more))))
		  (if (assoc (car varchild) cdb-var-list)
		      (throw 'child-already-watched nil))
		  (push varchild var-list))))
	  (push var var-list)))
      (setq cdb-var-list (nreverse var-list))))
  (cdb-speedbar-update))

(defun cdb-var-set-format (format)
  "Set the output format for a variable displayed in the speedbar."
  (let* ((var (nth (- (count-lines (point-min) (point)) 2) cdb-var-list))
	 (varnum (car var)))
    (cdb-input (concat "-var-set-format " varnum " " format) 'ignore)
    (cdb-var-update)))

(defun cdb-var-delete-1 (var varnum)
  (cdb-input (concat "-var-delete " varnum) 'ignore)
  (setq cdb-var-list (delq var cdb-var-list))
  (dolist (varchild cdb-var-list)
    (if (string-match (concat (car var) "\\.") (car varchild))
	(setq cdb-var-list (delq varchild cdb-var-list)))))

(defun cdb-var-delete ()
  "Delete watch expression at point from the speedbar."
  (interactive)
  (let ((text (speedbar-line-text)))
    (string-match "\\(\\S-+\\)" text)
    (let* ((var (nth (- (count-lines (point-min) (point)) 2) cdb-var-list))
           (varnum (car var)))
      (if (string-match "\\." (car var))
          (message-box "Can only delete a root expression")
        (cdb-var-delete-1 var varnum)))))

(defun cdb-var-delete-children (varnum)
  "Delete children of variable object at point from the speedbar."
  (cdb-input (concat "-var-delete -c " varnum) 'ignore))

(defun cdb-edit-value (_text _token _indent)
  "Assign a value to a variable displayed in the speedbar."
  (let* ((var (nth (- (count-lines (point-min) (point)) 2) cdb-var-list))
	 (varnum (car var))
         (value (read-string "New value: ")))
    (cdb-input (concat "-var-assign " varnum " " value)
	       `(lambda () (cdb-edit-value-handler ,value)))))

(defconst cdb-error-regexp "\\^error,msg=\\(\".+\"\\)")

(defun cdb-edit-value-handler (value)
  (goto-char (point-min))
  (if (re-search-forward cdb-error-regexp nil t)
      (message-box "Invalid number or expression (%s)" value)))

                                        ; Uses "-var-update --all-values".  Needs cdb 6.4 onwards.
(defun cdb-var-update ()
  (cdb-input "-var-update --all-values *"
             'cdb-var-update-handler
             'cdb-var-update))

(defun cdb-var-update-handler ()
  (let ((changelist (bindat-get-field (cdb-json-partial-output) 'changelist)))
    (dolist (var cdb-var-list)
      (setcar (nthcdr 5 var) nil))
    (let ((temp-var-list cdb-var-list))
      (dolist (change changelist)
	(let* ((varnum (bindat-get-field change 'name))
	       (var (assoc varnum cdb-var-list))
	       (new-num (bindat-get-field change 'new_num_children)))
	  (when var
	    (let ((scope (bindat-get-field change 'in_scope))
		  (has-more (bindat-get-field change 'has_more)))
	      (cond ((string-equal scope "false")
		     (if cdb-delete-out-of-scope
			 (cdb-var-delete-1 var varnum)
		       (setcar (nthcdr 5 var) 'out-of-scope)))
		    ((string-equal scope "true")
		     (setcar (nthcdr 6 var) has-more)
		     (when (and (or (not has-more)
				    (string-equal has-more "0"))
				(not new-num)
				(string-equal (nth 2 var) "0"))
		       (setcar (nthcdr 4 var)
			       (bindat-get-field change 'value))
		       (setcar (nthcdr 5 var) 'changed)))
		    ((string-equal scope "invalid")
		     (cdb-var-delete-1 var varnum)))))
	  (let ((var-list nil) var1
		(children (bindat-get-field change 'new_children)))
	    (when new-num
              (setq var1 (pop temp-var-list))
              (while var1
                (if (string-equal varnum (car var1))
                    (let ((new (string-to-number new-num))
                          (previous (string-to-number (nth 2 var1))))
                      (setcar (nthcdr 2 var1) new-num)
                      (push var1 var-list)
                      (cond
                       ((> new previous)
                        ;; Add new children to list.
                        (dotimes (_ previous)
                          (push (pop temp-var-list) var-list))
                        (dolist (child children)
                          (let ((varchild
                                 (list (bindat-get-field child 'name)
                                       (bindat-get-field child 'exp)
                                       (bindat-get-field child 'numchild)
                                       (bindat-get-field child 'type)
                                       (bindat-get-field child 'value)
                                       'changed
                                       (bindat-get-field child 'has_more))))
                            (push varchild var-list))))
                       ;; Remove deleted children from list.
                       ((< new previous)
                        (dotimes (_ new)
                          (push (pop temp-var-list) var-list))
                        (dotimes (_ (- previous new))
                          (pop temp-var-list)))))
                  (push var1 var-list))
                (setq var1 (pop temp-var-list)))
              (setq cdb-var-list (nreverse var-list))))))))
  (cdb-speedbar-update))

(defun cdb-speedbar-expand-node (text token indent)
  "Expand the node the user clicked on.
TEXT is the text of the button we clicked on, a + or - item.
TOKEN is data related to this node.
INDENT is the current indentation depth."
  (cond ((string-match "+" text)        ;expand this node
	 (let* ((var (assoc token cdb-var-list))
		(expr (nth 1 var)) (children (nth 2 var)))
	   (if (or (<= (string-to-number children) cdb-max-children)
		   (y-or-n-p
		    (format "%s has %s children. Continue? " expr children)))
	       (cdb-var-list-children token))))
	((string-match "-" text)	;contract this node
	 (dolist (var cdb-var-list)
	   (if (string-match (concat token "\\.") (car var))
	       (setq cdb-var-list (delq var cdb-var-list))))
	 (cdb-var-delete-children token)
	 (speedbar-change-expand-button-char ?+)
	 (speedbar-delete-subblock indent))
	(t (error "Ooops...  not sure what to do")))
  (speedbar-center-buffer-smartly))

(defun cdb-get-target-string ()
  (with-current-buffer gud-comint-buffer
    gud-target-name))


;;
;; cdb buffers.
;;
;; Each buffer has a TYPE -- a symbol that identifies the function
;; of that particular buffer.
;;
;; The usual cdb interaction buffer is given the type `cdbmi' and
;; is constructed specially.
;;
;; Others are constructed by cdb-get-buffer-create and
;; named according to the rules set forth in the cdb-buffer-rules

(defvar cdb-buffer-rules '())

(defun cdb-rules-name-maker (rules-entry)
  (cadr rules-entry))
(defun cdb-rules-buffer-mode (rules-entry)
  (nth 2 rules-entry))
(defun cdb-rules-update-trigger (rules-entry)
  (nth 3 rules-entry))

(defun cdb-update-buffer-name ()
  "Rename current buffer according to name-maker associated with
it in `cdb-buffer-rules'."
  (let ((f (cdb-rules-name-maker (assoc cdb-buffer-type
                                        cdb-buffer-rules))))
    (when f (rename-buffer (funcall f)))))

(defun cdb-current-buffer-rules ()
  "Get `cdb-buffer-rules' entry for current buffer type."
  (assoc cdb-buffer-type cdb-buffer-rules))

(defun cdb-current-buffer-thread ()
  "Get thread object of current buffer from `cdb-threads-list'.

When current buffer is not bound to any thread, return main
thread."
  (cdr (assoc cdb-thread-number cdb-threads-list)))

(defun cdb-current-buffer-frame ()
  "Get current stack frame object for thread of current buffer."
  (bindat-get-field (cdb-current-buffer-thread) 'frame))

(defun cdb-buffer-type (buffer)
  "Get value of `cdb-buffer-type' for BUFFER."
  (with-current-buffer buffer
    cdb-buffer-type))

(defun cdb-buffer-shows-main-thread-p ()
  "Return t if current cdb buffer shows main selected thread and
is not bound to it."
  (current-buffer)
  (not (local-variable-p 'cdb-thread-number)))

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

(defun cdb-bind-function-to-buffer (expr buffer)
  "Return a function which will evaluate EXPR in BUFFER."
  `(lambda (&rest args)
     (with-current-buffer ,buffer
       (apply ',expr args))))

;; Used to display windows with thread-bound buffers
(defmacro def-cdb-preempt-display-buffer (name buffer &optional doc
					       split-horizontal)
  `(defun ,name (&optional thread)
     ,(when doc doc)
     (message "%s" thread)
     (cdb-preempt-existing-or-display-buffer
      (cdb-get-buffer-create ,buffer thread)
      ,split-horizontal)))

;; This assoc maps buffer type symbols to rules.  Each rule is a list of
;; at least one and possible more functions.  The functions have these
;; roles in defining a buffer type:
;;
;;     NAME - Return a name for this  buffer type.
;;
;; The remaining function(s) are optional:
;;
;;     MODE - called in a new buffer with no arguments, should establish
;;	      the proper mode for the buffer.
;;

(defun cdb-set-buffer-rules (buffer-type &rest rules)
  (let ((binding (assoc buffer-type cdb-buffer-rules)))
    (if binding
	(setcdr binding rules)
      (push (cons buffer-type rules)
	    cdb-buffer-rules))))

(defun cdb-parent-mode ()
  "Generic mode to derive all other cdb buffer modes from."
  (kill-all-local-variables)
  (setq buffer-read-only t)
  (buffer-disable-undo)
  ;; Delete buffer from cdb-buf-publisher when it's killed
  ;; (if it has an associated update trigger)
  (add-hook
   'kill-buffer-hook
   (function
    (lambda ()
      (let ((trigger (cdb-rules-update-trigger
                      (cdb-current-buffer-rules))))
        (when trigger
          (cdb-delete-subscriber
           cdb-buf-publisher
           ;; This should match cdb-add-subscriber done in
           ;; cdb-get-buffer-create
           (cons (current-buffer)
                 (cdb-bind-function-to-buffer trigger (current-buffer))))))))
   nil t))

;; Partial-output buffer : This accumulates output from a command executed on
;; behalf of emacs (rather than the user).
;;
(cdb-set-buffer-rules 'cdb-partial-output-buffer
		      'cdb-partial-output-name)

(defun cdb-partial-output-name ()
  (concat " *partial-output-"
	  (cdb-get-target-string)
	  "*"))


(cdb-set-buffer-rules 'cdb-inferior-io
		      'cdb-inferior-io-name
		      'cdb-inferior-io-mode)

(defun cdb-inferior-io-name ()
  (concat "*input/output of "
	  (cdb-get-target-string)
	  "*"))

(defun cdb-display-io-buffer ()
  "Display IO of debugged program in a separate window."
  (interactive)
  (cdb-display-buffer (cdb-get-buffer-create 'cdb-inferior-io)))

(defun cdb-inferior-io--init-proc (proc)
  ;; Set up inferior I/O.  Needs cdb 6.4 onwards.
  (set-process-filter proc 'cdb-inferior-filter)
  (set-process-sentinel proc 'cdb-inferior-io-sentinel)
  ;; The process can run on a remote host.
  (let ((tty (or (process-get proc 'remote-tty)
		 (process-tty-name proc))))
    (unless (or (null tty)
		(string= tty ""))
      (cdb-input
       (concat "-inferior-tty-set " tty) 'ignore))))

(defun cdb-inferior-io-sentinel (proc _str)
  (when (eq (process-status proc) 'failed)
    ;; When the debugged process exits, Emacs gets an EIO error on
    ;; read from the pty, and stops listening to it.  If the cdb
    ;; process is still running, remove the pty, make a new one, and
    ;; pass it to cdb.
    (let ((io-buffer (process-buffer proc)))
      (when (and (process-live-p (get-buffer-process gud-comint-buffer))
		 (buffer-live-p io-buffer))
	;; `comint-exec' deletes the original process as a side effect.
	(comint-exec io-buffer "cdb-inferior" nil nil nil)
	(cdb-inferior-io--init-proc (get-buffer-process io-buffer))))))

(defcustom cdb-display-buffer-other-frame-action
  '((display-buffer-reuse-window display-buffer-pop-up-frame)
    (reusable-frames . visible)
    (inhibit-same-window . t)
    (pop-up-frame-parameters (height . 14)
			     (width . 80)
			     (unsplittable . t)
			     (tool-bar-lines . nil)
			     (menu-bar-lines . nil)
			     (minibuffer . nil)))
  "`display-buffer' action for displaying cdb utility frames."
  :group 'cdb
  :type display-buffer--action-custom-type
  :risky t
  :version "24.3")

(defun cdb-frame-io-buffer ()
  "Display IO of debugged program in another frame."
  (interactive)
  (display-buffer (cdb-get-buffer-create 'cdb-inferior-io)
		  cdb-display-buffer-other-frame-action))

(defvar cdb-inferior-io-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" 'cdb-io-interrupt)
    (define-key map "\C-c\C-z" 'cdb-io-stop)
    (define-key map "\C-c\C-\\" 'cdb-io-quit)
    (define-key map "\C-c\C-d" 'cdb-io-eof)
    (define-key map "\C-d" 'cdb-io-eof)
    map))

;; We want to use comint because it has various nifty and familiar features.
(define-derived-mode cdb-inferior-io-mode comint-mode "Inferior I/O"
  "Major mode for cdb inferior-io."
  :syntax-table nil :abbrev-table nil
  (make-comint-in-buffer "cdb-inferior" (current-buffer) nil))

(defun cdb-inferior-filter (proc string)
  (unless (string-equal string "")
    (cdb-display-buffer (cdb-get-buffer-create 'cdb-inferior-io)))
  (with-current-buffer (cdb-get-buffer-create 'cdb-inferior-io)
    (comint-output-filter proc string)))

(defun cdb-io-interrupt ()
  "Interrupt the program being debugged."
  (interactive)
  (interrupt-process
   (get-buffer-process gud-comint-buffer) comint-ptyp))

(defun cdb-io-quit ()
  "Send quit signal to the program being debugged."
  (interactive)
  (quit-process
   (get-buffer-process gud-comint-buffer) comint-ptyp))

(defun cdb-io-stop ()
  "Stop the program being debugged."
  (interactive)
  (stop-process
   (get-buffer-process gud-comint-buffer) comint-ptyp))

(defun cdb-io-eof ()
  "Send end-of-file to the program being debugged."
  (interactive)
  (process-send-eof
   (get-buffer-process gud-comint-buffer)))

(defun cdb-clear-inferior-io ()
  (with-current-buffer (cdb-get-buffer-create 'cdb-inferior-io)
    (erase-buffer)))


(defconst breakpoint-xpm-data
  "/* XPM */
static char *magick[] = {
/* columns rows colors chars-per-pixel */
\"10 10 2 1\",
\"  c red\",
\"+ c None\",
/* pixels */
\"+++    +++\",
\"++      ++\",
\"+        +\",
\"          \",
\"          \",
\"          \",
\"          \",
\"+        +\",
\"++      ++\",
\"+++    +++\",
};"
  "XPM data used for breakpoint icon.")

(defconst breakpoint-enabled-pbm-data
  "P1
10 10\",
0 0 0 0 1 1 1 1 0 0 0 0
0 0 0 1 1 1 1 1 1 0 0 0
0 0 1 1 1 1 1 1 1 1 0 0
0 1 1 1 1 1 1 1 1 1 1 0
0 1 1 1 1 1 1 1 1 1 1 0
0 1 1 1 1 1 1 1 1 1 1 0
0 1 1 1 1 1 1 1 1 1 1 0
0 0 1 1 1 1 1 1 1 1 0 0
0 0 0 1 1 1 1 1 1 0 0 0
0 0 0 0 1 1 1 1 0 0 0 0"
  "PBM data used for enabled breakpoint icon.")

(defconst breakpoint-disabled-pbm-data
  "P1
10 10\",
0 0 1 0 1 0 1 0 0 0
0 1 0 1 0 1 0 1 0 0
1 0 1 0 1 0 1 0 1 0
0 1 0 1 0 1 0 1 0 1
1 0 1 0 1 0 1 0 1 0
0 1 0 1 0 1 0 1 0 1
1 0 1 0 1 0 1 0 1 0
0 1 0 1 0 1 0 1 0 1
0 0 1 0 1 0 1 0 1 0
0 0 0 1 0 1 0 1 0 0"
  "PBM data used for disabled breakpoint icon.")

(defvar breakpoint-enabled-icon nil
  "Icon for enabled breakpoint in display margin.")

(defvar breakpoint-disabled-icon nil
  "Icon for disabled breakpoint in display margin.")

(declare-function define-fringe-bitmap "fringe.c"
		  (bitmap bits &optional height width align))

(and (display-images-p)
     ;; Bitmap for breakpoint in fringe
     (define-fringe-bitmap 'breakpoint
       "\x3c\x7e\xff\xff\xff\xff\x7e\x3c")
     ;; Bitmap for gud-overlay-arrow in fringe
     (define-fringe-bitmap 'hollow-right-triangle
       "\xe0\x90\x88\x84\x84\x88\x90\xe0"))

(defface breakpoint-enabled
  '((t
     :foreground "red1"
     :weight bold))
  "Face for enabled breakpoint icon in fringe."
  :group 'cdb)

(defface breakpoint-disabled
  '((((class color) (min-colors 88)) :foreground "grey70")
    ;; Ensure that on low-color displays that we end up something visible.
    (((class color) (min-colors 8) (background light))
     :foreground "black")
    (((class color) (min-colors 8) (background dark))
     :foreground "white")
    (((type tty) (class mono))
     :inverse-video t)
    (t :background "gray"))
  "Face for disabled breakpoint icon in fringe."
  :group 'cdb)


(defvar cdb-control-commands-regexp
  (concat
   "^\\("
   "commands\\|if\\|while\\|define\\|document\\|python\\|"
   "while-stepping\\|stepping\\|ws\\|actions"
   "\\)\\([[:blank:]]+.*\\)?$")
  "Regexp matching cdb commands that enter a recursive reading loop.
As long as cdb is in the recursive reading loop, it does not expect
commands to be prefixed by \"-interpreter-exec console\".")

(defun cdb-strip-string-backslash (string)
  (replace-regexp-in-string "\\\\$" "" string))

(defun cdb-send (proc string)
  "A comint send filter for cdb."
  (with-current-buffer gud-comint-buffer
    (let ((inhibit-read-only t))
      (remove-text-properties (point-min) (point-max) '(face))))
  ;; mimic <RET> key to repeat previous command in cdb
  (if (not (string= "" string))
      (if cdb-continuation
	  (setq cdb-last-command (concat cdb-continuation
					 (cdb-strip-string-backslash string)
					 " "))
	(setq cdb-last-command (cdb-strip-string-backslash string)))
    (if cdb-last-command (setq string cdb-last-command))
    (setq cdb-continuation nil))
  (if (and (not cdb-continuation) (or (string-match "^-" string)
	  (> cdb-control-level 0)))
      ;; Either MI command or we are feeding cdb's recursive reading loop.
      (progn
	(setq cdb-first-done-or-error t)
	(process-send-string proc (concat string "\n"))
	(if (and (string-match "^end$" string)
		 (> cdb-control-level 0))
	    (setq cdb-control-level (1- cdb-control-level))))
    ;; CLI command
    (if (string-match "\\\\$" string)
	(setq cdb-continuation
	      (concat cdb-continuation (cdb-strip-string-backslash
					string)
		      " "))
      (setq cdb-first-done-or-error t)
      (let ((to-send string))
        (if cdb-enable-debug
            (push (cons 'mi-send to-send) cdb-debug-log))
        (process-send-string proc to-send))
      (if (and (string-match "^end$" string)
	       (> cdb-control-level 0))
	  (setq cdb-control-level (1- cdb-control-level)))
      (setq cdb-continuation nil)))
  (if (string-match cdb-control-commands-regexp string)
      (setq cdb-control-level (1+ cdb-control-level))))

(defun cdb-mi-quote (string)
  "Return STRING quoted properly as an MI argument.
The string is enclosed in double quotes.
All embedded quotes, newlines, and backslashes are preceded with a backslash."
  (setq string (replace-regexp-in-string "\\([\"\\]\\)" "\\\\\\&" string))
  (setq string (replace-regexp-in-string "\n" "\\n" string t t))
  (concat "\"" string "\""))

(defun cdb-input (command handler-function &optional trigger-name)
  "Send COMMAND to cdb via the MI interface.
Run the function HANDLER-FUNCTION, with no arguments, once the command is
complete.  Do not send COMMAND to cdb if TRIGGER-NAME is non-nil and
Emacs is still waiting for a reply from another command previously
sent with the same TRIGGER-NAME."
  (when (or (not trigger-name)
            (not (cdb-pending-handler-p trigger-name)))
    (setq cdb-token-number (1+ cdb-token-number))

    (if cdb-enable-debug (push (list 'send-item command handler-function)
                               cdb-debug-log))

    (cdb-add-handler cdb-token-number handler-function trigger-name)

    (if cdbmi-debug-mode (message "cdb-input: %s" command))
    (process-send-string (get-buffer-process gud-comint-buffer)
                         (concat command "\n"))))

;; NOFRAME is used for gud execution control commands
(defun cdb-current-context-command (command)
  "Add --thread to cdb COMMAND when needed."
  (if (and cdb-thread-number
	   cdb-supports-non-stop)
      (concat command " --thread " cdb-thread-number)
    command))

(defun cdb-current-context-buffer-name (name)
  "Add thread information and asterisks to string NAME.

If `cdb-thread-number' is nil, just wrap NAME in asterisks."
  (concat "*" name
          (if (local-variable-p 'cdb-thread-number)
              (format " (bound to thread %s)" cdb-thread-number)
            "")
          "*"))

(defun cdb-current-context-mode-name (mode)
  "Add thread information to MODE which is to be used as `mode-name'."
  (concat mode
          (if cdb-thread-number
              (format " [thread %s]" cdb-thread-number)
            "")))


(defcustom gud-cdb-command-name "cdb -i=mi"
  "Default command to execute an executable under the cdb debugger."
  :type 'string
  :group 'cdb)

(defun cdb-resync()
  (setq gud-running nil)
  (setq cdb-output-sink 'user)
  (cdb-remove-all-pending-triggers))

(defun cdb-update (&optional no-proc)
  "Update buffers showing status of debug session.
If NO-PROC is non-nil, do not try to contact the cdb process."
  (when cdb-first-prompt
    (cdb-force-mode-line-update
     (propertize "initializing..." 'face font-lock-variable-name-face))
    (cdb-init-1)
    (setq cdb-first-prompt nil))

  (unless no-proc
    (cdb-get-main-selected-frame))

  ;; We may need to update cdb-threads-list so we can use
  (cdb-get-buffer-create 'cdb-threads-buffer)
  ;; cdb-break-list is maintained in breakpoints handler
  (cdb-get-buffer-create 'cdb-breakpoints-buffer)

  (unless no-proc
    (cdb-emit-signal cdb-buf-publisher 'update))

  (cdb-get-changed-registers)
  (when (and (boundp 'speedbar-frame) (frame-live-p speedbar-frame))
    (dolist (var cdb-var-list)
      (setcar (nthcdr 5 var) nil))
    (cdb-var-update)))

;; cdb-setq-thread-number and cdb-update-gud-running are decoupled
;; because we may need to update current gud-running value without
;; changing current thread (see cdb-running)
(defun cdb-setq-thread-number (number)
  "Set `cdb-thread-number' to NUMBER.
Only this function must be used to change `cdb-thread-number'
value to NUMBER, because `gud-running' and `cdb-frame-number'
need to be updated appropriately when current thread changes."
  ;; cdb 6.8 and earlier always output thread-id="0" when stopping.
  (unless (string-equal number "0") (setq cdb-thread-number number))
  (setq cdb-frame-number "0")
  (cdb-update-gud-running))

(defun cdb-update-gud-running ()
  "Set `gud-running' according to the state of current thread.

`cdb-frame-number' is set to 0 if current thread is now stopped.

Note that when `cdb-gud-control-all-threads' is t, `gud-running'
cannot be reliably used to determine whether or not execution
control buttons should be shown in menu or toolbar.  Use
`cdb-running-threads-count' and `cdb-stopped-threads-count'
instead.

For all-stop mode, thread information is unavailable while target
is running."
  (let ((old-value gud-running))
    (setq gud-running
          (string= (bindat-get-field (cdb-current-buffer-thread) 'state)
                   "running"))
    ;; Set frame number to "0" when _current_ threads stops.
    (when (and (cdb-current-buffer-thread)
               (not (eq gud-running old-value)))
      (setq cdb-frame-number "0"))))

(defun cdb-show-run-p ()
  "Return t if \"Run/continue\" should be shown on the toolbar."
  (or (not cdb-active-process)
      (and (or
            (not cdb-gud-control-all-threads)
            (not cdb-non-stop))
           (not gud-running))
      (and cdb-gud-control-all-threads
           (> cdb-stopped-threads-count 0))))

(defun cdb-show-stop-p ()
  "Return t if \"Stop\" should be shown on the toolbar."
  (or (and (or
            (not cdb-gud-control-all-threads)
            (not cdb-non-stop))
           gud-running)
      (and cdb-gud-control-all-threads
           (> cdb-running-threads-count 0))))

;; GUD displays the selected cdb frame.  This might might not be the current
;; cdb frame (after up, down etc).  If no cdb frame is visible but the last
;; visited breakpoint is, use that window.
(defun cdb-display-source-buffer (buffer)
  (let* ((last-window (if gud-last-last-frame
                          (get-buffer-window
                           (gud-find-file (car gud-last-last-frame)))))
	 (source-window (or last-window
			    (if (and cdb-source-window
				     (window-live-p cdb-source-window))
				cdb-source-window))))
    (when source-window
      (setq cdb-source-window source-window)
      (set-window-buffer source-window buffer))
    source-window))


(defun cdbmi-start-with (str offset match)
  "Return non-nil if string STR starts with MATCH, else returns nil.
OFFSET is the position in STR at which the comparison takes place."
  (let ((match-length (length match))
	(str-length (- (length str) offset)))
    (when (>= str-length match-length)
      (string-equal match (substring str offset (+ offset match-length))))))

(defun cdbmi-same-start (str offset match)
  "Return non-nil if STR and MATCH are equal up to the end of either strings.
OFFSET is the position in STR at which the comparison takes place."
  (let* ((str-length (- (length str) offset))
	 (match-length (length match))
	 (compare-length (min str-length match-length)))
    (when (> compare-length 0)
      (string-equal (substring str offset (+ offset compare-length))
		    (substring match 0 compare-length)))))

(defun cdbmi-is-number (character)
  "Return non-nil if CHARACTER is a numerical character between 0 and 9."
  (and (>= character ?0)
       (<= character ?9)))


(defvar-local cdbmi-bnf-state 'cdbmi-bnf-output
  "Current cdb/MI output parser state.
The parser is placed in a different state when an incomplete data steam is
received from cdb.
This variable will preserve the state required to resume the parsing
when more data arrives.")

(defvar-local cdbmi-bnf-offset 0
  "Offset in `gud-marker-acc' at which the parser is reading.
This offset is used to be able to parse the cdb/MI message
in-place, without the need of copying the string in a temporary buffer
or discarding parsed tokens by substringing the message.")

(defun cdbmi-bnf-init ()
  "Initialize the cdb/MI message parser."
  (setq cdbmi-bnf-state 'cdbmi-bnf-output)
  (setq cdbmi-bnf-offset 0)
  (setq gud-marker-acc ""))


(defun cdbmi-bnf-output ()
  "Implementation of the following cdb/MI output grammar rule:

  output ==>
       ( out-of-band-record )* [ result-record ] cdb-prompt"

  (cdbmi-bnf-skip-unrecognized)
  (while (cdbmi-bnf-out-of-band-record))
  (cdbmi-bnf-result-record)
  (cdbmi-bnf-cdb-prompt))


(defun cdbmi-bnf-skip-unrecognized ()
  "Skip characters until is encounters the beginning of a valid record.
Used as a protection mechanism in case something goes wrong when parsing
a cdb/MI reply message."
  (let ((acc-length (length gud-marker-acc))
	(prefix-offset cdbmi-bnf-offset)
	(prompt "(cdb) \n"))

    (while (and (< prefix-offset acc-length)
                (cdbmi-is-number (aref gud-marker-acc prefix-offset)))
      (setq prefix-offset (1+ prefix-offset)))

    (if (and (< prefix-offset acc-length)
             (not (memq (aref gud-marker-acc prefix-offset)
                        '(?^ ?* ?+ ?= ?~ ?@ ?&)))
             (not (cdbmi-same-start gud-marker-acc cdbmi-bnf-offset prompt))
             (string-match "\\([^^*+=~@&]+\\)" gud-marker-acc
                           cdbmi-bnf-offset))
        (let ((unrecognized-str (match-string 0 gud-marker-acc)))
          (setq cdbmi-bnf-offset (match-end 0))
	  (if cdbmi-debug-mode
              (message "cdbmi-bnf-skip-unrecognized: %s" unrecognized-str))
          (cdb-shell unrecognized-str)
	  t))))


(defun cdbmi-bnf-cdb-prompt ()
  "Implementation of the following cdb/MI output grammar rule:
  cdb-prompt ==>
       '(cdb)' nl

  nl ==>
       CR | CR-LF"

  (let ((prompt "(cdb) \n"))
    (when (cdbmi-start-with gud-marker-acc cdbmi-bnf-offset prompt)
      (if cdbmi-debug-mode (message "cdbmi-bnf-cdb-prompt: %s" prompt))
      (cdb-cdb prompt)
      (setq cdbmi-bnf-offset (+ cdbmi-bnf-offset (length prompt)))

      ;; Returns non-nil to tell gud-cdbmi-marker-filter we've reached
      ;; the end of a cdb reply message.
      t)))


(defun cdbmi-bnf-result-record ()
  "Implementation of the following cdb/MI output grammar rule:

  result-record ==>
       [ token ] '^' result-class ( ',' result )* nl

  token ==>
       any sequence of digits."

  (cdbmi-bnf-result-and-async-record-impl))


(defun cdbmi-bnf-out-of-band-record ()
  "Implementation of the following cdb/MI output grammar rule:

  out-of-band-record ==>
       async-record | stream-record"

  (or (cdbmi-bnf-async-record)
      (cdbmi-bnf-stream-record)))


(defun cdbmi-bnf-async-record ()
  "Implementation of the following cdb/MI output grammar rules:

  async-record ==>
       exec-async-output | status-async-output | notify-async-output

  exec-async-output ==>
       [ token ] '*' async-output

  status-async-output ==>
       [ token ] '+' async-output

  notify-async-output ==>
       [ token ] '=' async-output

  async-output ==>
       async-class ( ',' result )* nl"

  (cdbmi-bnf-result-and-async-record-impl))


(defun cdbmi-bnf-stream-record ()
  "Implement the following cdb/MI output grammar rule:
  stream-record ==>
       console-stream-output | target-stream-output | log-stream-output

  console-stream-output ==>
       '~' c-string

  target-stream-output ==>
       '@' c-string

  log-stream-output ==>
       '&' c-string"
  (when (< cdbmi-bnf-offset (length gud-marker-acc))
    (if (and (member (aref gud-marker-acc cdbmi-bnf-offset) '(?~ ?@ ?&))
             (string-match (concat "\\([~@&]\\)\\(" cdb--string-regexp "\\)\n")
                           gud-marker-acc
                           cdbmi-bnf-offset))
        (let ((prefix (match-string 1 gud-marker-acc))
              (c-string (match-string 2 gud-marker-acc)))

          (setq cdbmi-bnf-offset (match-end 0))
          (if cdbmi-debug-mode (message "cdbmi-bnf-stream-record: %s"
                                        (match-string 0 gud-marker-acc)))

          (cond ((string-equal prefix "~")
                 (cdbmi-bnf-console-stream-output c-string))
                ((string-equal prefix "@")
                 (cdbmi-bnf-target-stream-output c-string))
                ((string-equal prefix "&")
                 (cdbmi-bnf-log-stream-output c-string)))
	  t))))

(defun cdbmi-bnf-console-stream-output (c-string)
  "Handler for the console-stream-output cdb/MI output grammar rule."
  (cdb-console c-string))

(defun cdbmi-bnf-target-stream-output (_c-string)
  "Handler for the target-stream-output cdb/MI output grammar rule."
  ;; Not currently used.
  )

(defun cdbmi-bnf-log-stream-output (c-string)
  "Handler for the log-stream-output cdb/MI output grammar rule."
  ;; Suppress "No registers."  cdb 6.8 and earlier
  ;; duplicates MI error message on internal stream.
  ;; Don't print to GUD buffer.
  (if (not (string-equal (read c-string) "No registers.\n"))
      (cdb-internals c-string)))


(defconst cdbmi-bnf-result-state-configs
  '(("^" . (("done" . (cdb-done . progressive))
            ("error" . (cdb-error . progressive))
            ("running" . (cdb-starting . atomic))))
    ("*" . (("stopped" . (cdb-stopped . atomic))
            ("running" . (cdb-running . atomic))))
    ("+" . ())
    ("=" . (("thread-created" . (cdb-thread-created . atomic))
            ("thread-selected" . (cdb-thread-selected . atomic))
            ("thread-existed" . (cdb-ignored-notification . atomic))
            ('default . (cdb-ignored-notification . atomic)))))
  "Alist of alists, mapping the type and class of message to a handler function.
Handler functions are all flagged as either `progressive' or `atomic'.
`progressive' handlers are capable of parsing incomplete messages.
They can be called several time with new data chunk as they arrive from cdb.
`progressive' handlers must have an extra argument that is set to a non-nil
value when the message is complete.

Implement the following cdb/MI output grammar rule:
  result-class ==>
       'done' | 'running' | 'connected' | 'error' | 'exit'

  async-class ==>
       'stopped' | others (where others will be added depending on the needs
                           --this is still in development).")

(defun cdbmi-bnf-result-and-async-record-impl ()
  "Common implementation of the result-record and async-record rule.
Both rules share the same syntax.  Those records may be very large in size.
For that reason, the \"result\" part of the  record is parsed by
`cdbmi-bnf-incomplete-record-result', which will keep
receiving characters as they arrive from cdb until the record is complete."
  (let ((acc-length (length gud-marker-acc))
	(prefix-offset cdbmi-bnf-offset))

    (while (and (< prefix-offset acc-length)
                (cdbmi-is-number (aref gud-marker-acc prefix-offset)))
      (setq prefix-offset (1+ prefix-offset)))

    (if (and (< prefix-offset acc-length)
             (member (aref gud-marker-acc prefix-offset) '(?* ?+ ?= ?^))
             (string-match "\\([0-9]*\\)\\([*+=^]\\)\\(.+?\\)\\([,\n]\\)"
                           gud-marker-acc cdbmi-bnf-offset))

        (let ((token (match-string 1 gud-marker-acc))
	      (prefix (match-string 2 gud-marker-acc))
	      (class (match-string 3 gud-marker-acc))
	      (complete (string-equal (match-string 4 gud-marker-acc) "\n"))
	      class-alist
	      class-command)

          (setq cdbmi-bnf-offset (match-end 0))
	  (if cdbmi-debug-mode (message "cdbmi-bnf-result-record: %s"
                                        (match-string 0 gud-marker-acc)))

          (setq class-alist
                (cdr (assoc prefix cdbmi-bnf-result-state-configs)))
          (setq class-command (cdr (assoc class class-alist)))
          (if (null class-command)
              (setq class-command (cdr (assoc 'default class-alist))))

          (if complete
              (if class-command
                  (if (equal (cdr class-command) 'progressive)
                      (funcall (car class-command) token "" complete)
                    (funcall (car class-command) token "")))
            (setq cdbmi-bnf-state
                  (lambda ()
                    (cdbmi-bnf-incomplete-record-result token class-command)))
            (funcall cdbmi-bnf-state))
	  t))))

(defun cdbmi-bnf-incomplete-record-result (token class-command)
  "State of the parser used to progressively parse a result-record or async-record
rule from an incomplete data stream.  The parser will stay in this state until
the end of the current result or async record is reached."
  (when (< cdbmi-bnf-offset (length gud-marker-acc))
    ;; Search the data stream for the end of the current record:
    (let* ((newline-pos (string-match "\n" gud-marker-acc cdbmi-bnf-offset))
	   (is-progressive (equal (cdr class-command) 'progressive))
       (is-complete (not (null newline-pos)))
       result-str)

      (when cdbmi-debug-mode
        (message "cdbmi-bnf-incomplete-record-result: %s"
                 (substring gud-marker-acc cdbmi-bnf-offset newline-pos)))

      ;; Update the cdbmi-bnf-offset only if the current chunk of data can
      ;; be processed by the class-command handler:
      (when (or is-complete is-progressive)
        (setq result-str
              (substring gud-marker-acc cdbmi-bnf-offset newline-pos))

        ;; Move cdbmi-bnf-offset past the end of the chunk.
        (setq cdbmi-bnf-offset (+ cdbmi-bnf-offset (length result-str)))
        (when newline-pos
          (setq cdbmi-bnf-offset (1+ cdbmi-bnf-offset))))

      ;; Update the parsing state before invoking the handler in class-command
      ;; to make sure it's not left in an invalid state if the handler was
      ;; to generate an error.
      (if is-complete
	  (setq cdbmi-bnf-state 'cdbmi-bnf-output))

      (if class-command
	  (if is-progressive
	      (funcall (car class-command) token result-str is-complete)
	    (if is-complete
		(funcall (car class-command) token result-str))))

      (unless is-complete
        ;; Incomplete cdb response: abort parsing until we receive more data.
        (if cdbmi-debug-mode (message "cdbmi-bnf-incomplete-record-result, aborting: incomplete stream"))
        (throw 'cdbmi-incomplete-stream nil))

      is-complete)))


; The following grammar rules are not yet implemented by this cdbMI-BNF parser.
; The handling of those rules is currently done by the handlers registered
; in cdbmi-bnf-result-state-configs
;
; result ==>
;      variable "=" value
;
; variable ==>
;      string
;
; value ==>
;      const | tuple | list
;
; const ==>
;      c-string
;
; tuple ==>
;      "{}" | "{" result ( "," result )* "}"
;
; list ==>
;      "[]" | "[" value ( "," value )* "]" | "[" result ( "," result )* "]"


(defun gud-cdbmi-marker-filter (string)
  "Filter cdb/MI output."

  ;; Record transactions if logging is enabled.
  (when cdb-enable-debug
    (push (cons 'recv string) cdb-debug-log)
    (if (and cdb-debug-log-max
	     (> (length cdb-debug-log) cdb-debug-log-max))
	(setcdr (nthcdr (1- cdb-debug-log-max) cdb-debug-log) nil)))

  ;; Recall the left over gud-marker-acc from last time.
  (setq gud-marker-acc (concat gud-marker-acc string))

  ;; Start accumulating output for the GUD buffer.
  (setq cdb-filter-output "")

  (let ((acc-length (length gud-marker-acc)))
    (catch 'cdbmi-incomplete-stream
      (while (and (< cdbmi-bnf-offset acc-length)
		  (funcall cdbmi-bnf-state)))))

  (when (/= cdbmi-bnf-offset 0)
    (setq gud-marker-acc (substring gud-marker-acc cdbmi-bnf-offset))
    (setq cdbmi-bnf-offset 0))

  (when (and cdbmi-debug-mode (> (length gud-marker-acc) 0))
    (message "gud-cdbmi-marker-filter, unparsed string: %s" gud-marker-acc))

  cdb-filter-output)

(defun cdb-cdb (_output-field))

(defun cdb-shell (output-field)
  (setq cdb-filter-output
        (concat output-field cdb-filter-output)))

(defun cdb-ignored-notification (_token _output-field))

;; cdb-invalidate-threads is defined to accept 'update-threads signal
(defun cdb-thread-created (_token _output-field))
(defun cdb-thread-exited (_token output-field)
  "Handle =thread-exited async record.
Unset `cdb-thread-number' if current thread exited and update threads list."
  (let* ((thread-id (bindat-get-field (cdb-json-string output-field) 'id)))
    (if (string= cdb-thread-number thread-id)
        (cdb-setq-thread-number nil))
    ;; When we continue current thread and it quickly exits,
    ;; the pending triggers in cdb-handler-list left after cdb-running
    ;; disallow us to properly call -thread-info without --thread option.
    ;; Thus we need to use cdb-wait-for-pending.
    (cdb-wait-for-pending
     (cdb-emit-signal cdb-buf-publisher 'update-threads))))

(defun cdb-thread-selected (_token output-field)
  "Handler for =thread-selected MI output record.

Sets `cdb-thread-number' to new id."
  (let* ((result (cdb-json-string output-field))
         (thread-id (bindat-get-field result 'id)))
    (cdb-setq-thread-number thread-id)
    ;; Typing `thread N` in GUD buffer makes cdb emit `^done` followed
    ;; by `=thread-selected` notification. `^done` causes `cdb-update`
    ;; as usually. Things happen to fast and second call (from
    ;; cdb-thread-selected handler) gets cut off by our beloved
    ;; pending triggers.
    ;; Solution is `cdb-wait-for-pending' macro: it guarantees that its
    ;; body will get executed when `cdb-handler-list' if free of
    ;; pending triggers.
    (cdb-wait-for-pending
     (cdb-update))))

(defun cdb-running (_token output-field)
  (let* ((thread-id
          (bindat-get-field (cdb-json-string output-field) 'thread-id)))
    ;; We reset cdb-frame-number to nil if current thread has gone
    ;; running. This can't be done in cdb-thread-list-handler-custom
    ;; because we need correct cdb-frame-number by the time
    ;; -thread-info command is sent.
    (when (or (string-equal thread-id "all")
              (string-equal thread-id cdb-thread-number))
      (setq cdb-frame-number nil)))
  (setq cdb-inferior-status "running")
  (cdb-force-mode-line-update
   (propertize cdb-inferior-status 'face font-lock-type-face))
  (when (not cdb-non-stop)
    (setq gud-running t))
  (setq cdb-active-process t))

(defun cdb-starting (_output-field _result)
  ;; CLI commands don't emit ^running at the moment so use cdb-running too.
  (setq cdb-inferior-status "running")
  (cdb-force-mode-line-update
   (propertize cdb-inferior-status 'face font-lock-type-face))
  (setq cdb-active-process t)
  (setq gud-running t))

;; -break-insert -t didn't give a reason before cdb 6.9

(defun cdb-stopped (_token output-field)
  "Given the contents of *stopped MI async record, select new
current thread and update cdb buffers."
  ;; Reason is available with target-async only
  (let* ((result (cdb-json-string output-field))
         (reason (bindat-get-field result 'reason))
         (thread-id (bindat-get-field result 'thread-id)))

    ;; -data-list-register-names needs to be issued for any stopped
    ;; thread
    (when (not cdb-register-names)
      (cdb-input (concat "-data-list-register-names"
			 (if cdb-supports-non-stop
			     (concat " --thread " thread-id)))
		 'cdb-register-names-handler))

    ;; Don't set gud-last-frame here as it's currently done in
    ;; cdb-frame-handler because synchronous cdb doesn't give these fields
    ;; with CLI.
    ;;(when file
    ;;  (setq
    ;;   ;; Extract the frame position from the marker.
    ;;   gud-last-frame (cons file
    ;;    		    (string-to-number
    ;;    		     (match-string 6 gud-marker-acc)))))

    (setq cdb-inferior-status (or reason "unknown"))
    (cdb-force-mode-line-update
     (propertize cdb-inferior-status 'face font-lock-warning-face))
    (if (string-equal reason "exited-normally")
	(setq cdb-active-process nil))

    ;; Select new current thread.

    ;; Don't switch if we have no reasons selected
    (when cdb-switch-reasons
      ;; Switch from another stopped thread only if we have
      ;; cdb-switch-when-another-stopped:
      (when (or cdb-switch-when-another-stopped
                (not (string= "stopped"
                              (bindat-get-field (cdb-current-buffer-thread) 'state))))
        ;; Switch if current reason has been selected or we have no
        ;; reasons
        (if (or (eq cdb-switch-reasons t)
                (member reason cdb-switch-reasons))
            (when (not (string-equal cdb-thread-number thread-id))
              (message "Switched to thread %s" thread-id)
              (cdb-setq-thread-number thread-id))
          (message "Thread %s stopped" thread-id))))

    ;; Print "(cdb)" to GUD console
    (when cdb-first-done-or-error
      (setq cdb-filter-output (concat cdb-filter-output cdb-prompt-name)))

    ;; In non-stop, we update information as soon as another thread gets
    ;; stopped
    (when (or cdb-first-done-or-error
              cdb-non-stop)
      ;; In all-stop this updates gud-running properly as well.
      (cdb-update)
      (setq cdb-first-done-or-error nil))
    (run-hook-with-args 'cdb-stopped-functions result)))

;; Remove the trimmings from log stream containing debugging messages
;; being produced by cdb's internals, use warning face and send to GUD
;; buffer.
(defun cdb-internals (output-field)
  (setq cdb-filter-output
	(cdb-concat-output
	 cdb-filter-output
	 (if (string= output-field "\"\\n\"")
	     ""
	   (let ((error-message
		  (read output-field)))
	     (put-text-property
	      0 (length error-message)
	      'face font-lock-warning-face
	      error-message)
	     error-message)))))

;; Remove the trimmings from the console stream and send to GUD buffer
;; (frontend MI commands should not print to this stream)
(defun cdb-console (output-field)
  (setq cdb-filter-output
	(cdb-concat-output cdb-filter-output (read output-field))))

(defun cdb-done (token-number output-field is-complete)
  (cdb-done-or-error token-number 'done output-field is-complete))

(defun cdb-error (token-number output-field is-complete)
  (cdb-done-or-error token-number 'error output-field is-complete))

(defun cdb-done-or-error (token-number type output-field is-complete)
  (if (string-equal token-number "")
      ;; Output from command entered by user
      (progn
	(setq cdb-output-sink 'user)
	(setq token-number nil)
	;; MI error - send to minibuffer
	(when (eq type 'error)
          ;; Skip "msg=" from `output-field'
          (message "%s" (read (substring output-field 4)))
          ;; Don't send to the console twice.  (If it is a console error
          ;; it is also in the console stream.)
          (setq output-field nil)))
    ;; Output from command from frontend.
    (setq cdb-output-sink 'emacs))

  ;; The process may already be dead (e.g. C-d at the cdb prompt).
  (let* ((proc (get-buffer-process gud-comint-buffer))
	 (no-proc (or (null proc)
		      (memq (process-status proc) '(exit signal)))))

    (when (and is-complete cdb-first-done-or-error)
      (unless (or token-number gud-running no-proc)
	(setq cdb-filter-output (concat cdb-filter-output cdb-prompt-name)))
      (cdb-update no-proc)
      (setq cdb-first-done-or-error nil))

    (setq cdb-filter-output
	  (cdb-concat-output cdb-filter-output output-field))

    ;; We are done concatenating to the output sink.  Restore it to user sink:
    (setq cdb-output-sink 'user)

    (when (and token-number is-complete)
      (with-current-buffer
	  (cdb-get-buffer-create 'cdb-partial-output-buffer)
	(cdb-handle-reply (string-to-number token-number))))

  (when is-complete
    (cdb-clear-partial-output))))

(defun cdb-concat-output (so-far new)
  (cond
   ((eq cdb-output-sink 'user) (concat so-far new))
   ((eq cdb-output-sink 'emacs)
    (cdb-append-to-partial-output new)
    so-far)))

(defun cdb-append-to-partial-output (string)
  (with-current-buffer (cdb-get-buffer-create 'cdb-partial-output-buffer)
    (goto-char (point-max))
    (insert string)))

(defun cdb-clear-partial-output ()
  (with-current-buffer (cdb-get-buffer-create 'cdb-partial-output-buffer)
    (erase-buffer)))

(defun cdb-jsonify-buffer (&optional fix-key fix-list)
  "Prepare cdb/MI output in current buffer for parsing with `json-read'.

Field names are wrapped in double quotes and equal signs are
replaced with semicolons.

If FIX-KEY is non-nil, strip all \"FIX-KEY=\" occurrences from
partial output.  This is used to get rid of useless keys in lists
in MI messages, e.g.: [key=.., key=..].  -stack-list-frames and
-break-info are examples of MI commands which issue such
responses.

If FIX-LIST is non-nil, \"FIX-LIST={..}\" is replaced with
\"FIX-LIST=[..]\" prior to parsing. This is used to fix broken
-break-info output when it contains breakpoint script field
incompatible with cdb/MI output syntax."
  (save-excursion
    (goto-char (point-min))
    (when fix-key
      (save-excursion
        (while (re-search-forward (concat "[\\[,]\\(" fix-key "=\\)") nil t)
          (replace-match "" nil nil nil 1))))
    (when fix-list
      (save-excursion
        ;; Find positions of braces which enclose broken list
        (while (re-search-forward (concat fix-list "={\"") nil t)
          (let ((p1 (goto-char (- (point) 2)))
                (p2 (progn (forward-sexp)
                           (1- (point)))))
            ;; Replace braces with brackets
            (save-excursion
              (goto-char p1)
              (delete-char 1)
              (insert "[")
              (goto-char p2)
              (delete-char 1)
              (insert "]"))))))
    (goto-char (point-min))
    (insert "{")
    (let ((re (concat "\\([[:alnum:]-_]+\\)=\\({\\|\\[\\|\"\"\\|"
                      cdb--string-regexp "\\)")))
      (while (re-search-forward re nil t)
        (replace-match "\"\\1\":\\2" nil nil)))
    (goto-char (point-max))
    (insert "}")))

(defun cdb-json-read-buffer (&optional fix-key fix-list)
  "Prepare and parse cdb/MI output in current buffer with `json-read'.

FIX-KEY and FIX-LIST work as in `cdb-jsonify-buffer'."
  (cdb-jsonify-buffer fix-key fix-list)
  (save-excursion
    (goto-char (point-min))
    (let ((json-array-type 'list))
      (json-read))))

(defun cdb-json-string (string &optional fix-key fix-list)
  "Prepare and parse STRING containing cdb/MI output with `json-read'.

FIX-KEY and FIX-LIST work as in `cdb-jsonify-buffer'."
  (with-temp-buffer
    (insert string)
    (cdb-json-read-buffer fix-key fix-list)))

(defun cdb-json-partial-output (&optional fix-key fix-list)
  "Prepare and parse cdb-partial-output-buffer with `json-read'.

FIX-KEY and FIX-KEY work as in `cdb-jsonify-buffer'."
  (with-current-buffer (cdb-get-buffer-create 'cdb-partial-output-buffer)
    (cdb-json-read-buffer fix-key fix-list)))

(defun cdb-line-posns (line)
  "Return a pair of LINE beginning and end positions."
  (let ((offset (1+ (- line (line-number-at-pos)))))
    (cons
     (line-beginning-position offset)
     (line-end-position offset))))

(defmacro cdb-mark-line (line variable)
  "Set VARIABLE marker to point at beginning of LINE.

If current window has no fringes, inverse colors on LINE.

Return position where LINE begins."
  `(save-excursion
     (let* ((posns (cdb-line-posns ,line))
            (start-posn (car posns))
            (end-posn (cdr posns)))
       (set-marker ,variable (copy-marker start-posn))
       (when (not (> (car (window-fringes)) 0))
         (put-text-property start-posn end-posn
                            'font-lock-face '(:inverse-video t)))
       start-posn)))

(defun cdb-pad-string (string padding)
  (format (concat "%" (number-to-string padding) "s") string))

;; cdb-table struct is a way to programmatically construct simple
;; tables. It help to reliably align columns of data in cdb buffers
;; and provides
(cl-defstruct cdb-table
  (column-sizes nil)
  (rows nil)
  (row-properties nil)
  (right-align nil))

(defun cdb-table-add-row (table row &optional properties)
  "Add ROW of string to TABLE and recalculate column sizes.

When non-nil, PROPERTIES will be added to the whole row when
calling `cdb-table-string'."
  (let ((rows (cdb-table-rows table))
        (row-properties (cdb-table-row-properties table))
        (column-sizes (cdb-table-column-sizes table))
        (right-align (cdb-table-right-align table)))
    (when (not column-sizes)
      (setf (cdb-table-column-sizes table)
            (make-list (length row) 0)))
    (setf (cdb-table-rows table)
          (append rows (list row)))
    (setf (cdb-table-row-properties table)
          (append row-properties (list properties)))
    (setf (cdb-table-column-sizes table)
          (cl-mapcar (lambda (x s)
                         (let ((new-x
                                (max (abs x) (string-width (or s "")))))
                           (if right-align new-x (- new-x))))
                       (cdb-table-column-sizes table)
                       row))
    ;; Avoid trailing whitespace at eol
    (if (not (cdb-table-right-align table))
        (setcar (last (cdb-table-column-sizes table)) 0))))

(defun cdb-table-string (table &optional sep)
  "Return TABLE as a string with columns separated with SEP."
  (let ((column-sizes (cdb-table-column-sizes table)))
    (mapconcat
     'identity
     (cl-mapcar
      (lambda (row properties)
        (apply 'propertize
               (mapconcat 'identity
                          (cl-mapcar (lambda (s x) (cdb-pad-string s x))
                                       row column-sizes)
                          sep)
               properties))
      (cdb-table-rows table)
      (cdb-table-row-properties table))
     "\n")))

;; bindat-get-field goes deep, cdb-get-many-fields goes wide
(defun cdb-get-many-fields (struct &rest fields)
  "Return a list of FIELDS values from STRUCT."
  (let ((values))
    (dolist (field fields)
      (push (bindat-get-field struct field) values))
    (nreverse values)))

(defmacro def-cdb-auto-update-trigger (trigger-name cdb-command
                                                    handler-name
                                                    &optional signal-list)
  "Define a trigger TRIGGER-NAME which sends cdb-COMMAND and sets
HANDLER-NAME as its handler.  HANDLER-NAME is bound to current
buffer with `cdb-bind-function-to-buffer'.

If SIGNAL-LIST is non-nil, cdb-COMMAND is sent only when the
defined trigger is called with an argument from SIGNAL-LIST.  It's
not recommended to define triggers with empty SIGNAL-LIST.
Normally triggers should respond at least to 'update signal.

Normally the trigger defined by this command must be called from
the buffer where HANDLER-NAME must work.  This should be done so
that buffer-local thread number may be used in cdb-COMMAND (by
calling `cdb-current-context-command').
`cdb-bind-function-to-buffer' is used to achieve this, see
`cdb-get-buffer-create'.

Triggers defined by this command are meant to be used as a
trigger argument when describing buffer types with
`cdb-set-buffer-rules'."
  `(defun ,trigger-name (&optional signal)
     (when
         (or (not ,signal-list)
             (memq signal ,signal-list))
       (cdb-input ,cdb-command
                  (cdb-bind-function-to-buffer ',handler-name (current-buffer))
                  (cons (current-buffer) ',trigger-name)))))

;; Used by disassembly buffer only, the rest use
;; def-cdb-trigger-and-handler
(defmacro def-cdb-auto-update-handler (handler-name custom-defun
                                                    &optional nopreserve)
  "Define a handler HANDLER-NAME calling CUSTOM-DEFUN.

Handlers are normally called from the buffers they put output in.

Erase current buffer and evaluate CUSTOM-DEFUN.
Then call `cdb-update-buffer-name'.

If NOPRESERVE is non-nil, window point is not restored after CUSTOM-DEFUN."
  `(defun ,handler-name ()
     (let* ((inhibit-read-only t)
            ,@(unless nopreserve
                '((window (get-buffer-window (current-buffer) 0))
                  (start (window-start window))
                  (p (window-point window)))))
       (erase-buffer)
       (,custom-defun)
       (cdb-update-buffer-name)
       ,@(when (not nopreserve)
          '((set-window-start window start)
            (set-window-point window p))))))

(defmacro def-cdb-trigger-and-handler (trigger-name cdb-command
                                                    handler-name custom-defun
                                                    &optional signal-list)
  "Define trigger and handler.

TRIGGER-NAME trigger is defined to send cdb-COMMAND.
See `def-cdb-auto-update-trigger'.

HANDLER-NAME handler uses customization of CUSTOM-DEFUN.
See `def-cdb-auto-update-handler'."
  `(progn
     (def-cdb-auto-update-trigger ,trigger-name
       ,cdb-command
       ,handler-name ,signal-list)
     (def-cdb-auto-update-handler ,handler-name
       ,custom-defun)))



;; Breakpoint buffer : This displays the output of `-break-list'.
(def-cdb-trigger-and-handler
  cdb-invalidate-breakpoints "-break-list"
  cdb-breakpoints-list-handler cdb-breakpoints-list-handler-custom
  '(start update))

(cdb-set-buffer-rules
 'cdb-breakpoints-buffer
 'cdb-breakpoints-buffer-name
 'cdb-breakpoints-mode
 'cdb-invalidate-breakpoints)

(defun cdb-breakpoints-list-handler-custom ()
  (let ((breakpoints-list (bindat-get-field
                           (cdb-json-partial-output "bkpt" "script")
                           'BreakpointTable 'body))
        (table (make-cdb-table)))
    (setq cdb-breakpoints-list nil)
    (cdb-table-add-row table '("Num" "Type" "Disp" "Enb" "Addr" "Hits" "What"))
    (dolist (breakpoint breakpoints-list)
      (add-to-list 'cdb-breakpoints-list
                   (cons (bindat-get-field breakpoint 'number)
                         breakpoint))
      (let ((at (bindat-get-field breakpoint 'at))
            (pending (bindat-get-field breakpoint 'pending))
            (func (bindat-get-field breakpoint 'func))
	    (type (bindat-get-field breakpoint 'type)))
        (cdb-table-add-row table
                           (list
                            (bindat-get-field breakpoint 'number)
                            (or type "")
                            (or (bindat-get-field breakpoint 'disp) "")
                            (let ((flag (bindat-get-field breakpoint 'enabled)))
                              (if (string-equal flag "y")
                                  (eval-when-compile
                                    (propertize "y" 'font-lock-face
                                                font-lock-warning-face))
                                (eval-when-compile
                                  (propertize "n" 'font-lock-face
                                              font-lock-comment-face))))
                            (bindat-get-field breakpoint 'addr)
                            (or (bindat-get-field breakpoint 'times) "")
                            (if (and type (string-match ".*watchpoint" type))
                                (bindat-get-field breakpoint 'what)
                              (or pending at
                                  (concat "in "
                                          (propertize (or func "unknown")
                                                      'font-lock-face font-lock-function-name-face)
                                          (cdb-frame-location breakpoint)))))
                           ;; Add clickable properties only for breakpoints with file:line
                           ;; information
                           (append (list 'cdb-breakpoint breakpoint)
                                   (when func '(help-echo "mouse-2, RET: visit breakpoint"
                                                mouse-face highlight))))))
    (insert (cdb-table-string table " "))
    (cdb-place-breakpoints)))

;; Put breakpoint icons in relevant margins (even those set in the GUD buffer).
(defun cdb-place-breakpoints ()
  ;; Remove all breakpoint-icons in source buffers but not assembler buffer.
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (if (and (eq gud-minor-mode 'cdbmi)
               (not (string-match "\\` ?\\*.+\\*\\'" (buffer-name))))
          (cdb-remove-breakpoint-icons (point-min) (point-max)))))
  (dolist (breakpoint cdb-breakpoints-list)
    (let* ((breakpoint (cdr breakpoint)) ; cdb-breakpoints-list is
                                        ; an associative list
           (line (bindat-get-field breakpoint 'line)))
      (when line
        (let ((file (bindat-get-field breakpoint 'fullname))
              (flag (bindat-get-field breakpoint 'enabled))
              (bptno (bindat-get-field breakpoint 'number)))
          (unless (and file (file-exists-p file))
            (setq file (cdr (assoc bptno cdb-location-alist))))
	  (if (or (null file)
		  (string-equal file "File not found"))
	      ;; If the full filename is not recorded in the
	      ;; breakpoint structure or in `cdb-location-alist', use
	      ;; -file-list-exec-source-file to extract it.
	      (when (setq file (bindat-get-field breakpoint 'file))
		(cdb-input (concat "list " file ":1") 'ignore)
		(cdb-input "-file-list-exec-source-file"
			   `(lambda () (cdb-get-location
					,bptno ,line ,flag))))
	    (with-current-buffer (find-file-noselect file 'nowarn)
	      (cdb-init-buffer)
	      ;; Only want one breakpoint icon at each location.
	      (cdb-put-breakpoint-icon (string-equal flag "y") bptno
				       (string-to-number line)))))))))

(defconst cdb-source-file-regexp
  (concat "fullname=\\(" cdb--string-regexp "\\)"))

(defun cdb-get-location (bptno line flag)
  "Find the directory containing the relevant source file.
Put in buffer and place breakpoint icon."
  (goto-char (point-min))
  (catch 'file-not-found
    (if (re-search-forward cdb-source-file-regexp nil t)
	(delete (cons bptno "File not found") cdb-location-alist)
      ;; FIXME: Why/how do we use (match-string 1) when the search failed?
      (push (cons bptno (match-string 1)) cdb-location-alist)
      (cdb-resync)
      (unless (assoc bptno cdb-location-alist)
	(push (cons bptno "File not found") cdb-location-alist)
	(message-box "Cannot find source file for breakpoint location.
Add directory to search path for source files using the cdb command, dir."))
      (throw 'file-not-found nil))
    (with-current-buffer (find-file-noselect (match-string 1))
      (cdb-init-buffer)
      ;; only want one breakpoint icon at each location
      (cdb-put-breakpoint-icon (eq flag ?y) bptno (string-to-number line)))))

(add-hook 'find-file-hook 'cdb-find-file-hook)

(defun cdb-find-file-hook ()
  "Set up buffer for debugging if file is part of the source code
of the current session."
  (if (and (buffer-name gud-comint-buffer)
	   ;; in case gud or cdb-ui is just loaded
	   gud-comint-buffer
	   (eq (buffer-local-value 'gud-minor-mode gud-comint-buffer)
	       'cdbmi))
      (if (member buffer-file-name cdb-source-file-list)
	  (with-current-buffer (find-buffer-visiting buffer-file-name)
	    (cdb-init-buffer)))))

(declare-function gud-remove "cdb-mi" t t) ; gud-def
(declare-function gud-break  "cdb-mi" t t) ; gud-def
(declare-function fringe-bitmaps-at-pos "fringe.c" (&optional pos window))

(defun cdb-mouse-set-clear-breakpoint (event)
  "Set/clear breakpoint in left fringe/margin at mouse click.
If not in a source or disassembly buffer just set point."
  (interactive "e")
  (mouse-minibuffer-check event)
  (let ((posn (event-end event)))
    (with-selected-window (posn-window posn)
      (if (or (buffer-file-name) (derived-mode-p 'cdb-disassembly-mode))
	  (if (numberp (posn-point posn))
	      (save-excursion
		(goto-char (posn-point posn))
		(if (or (posn-object posn)
			(eq (car (fringe-bitmaps-at-pos (posn-point posn)))
			    'breakpoint))
		    (gud-remove nil)
		  (gud-break nil)))))
      (posn-set-point posn))))

(defun cdb-mouse-toggle-breakpoint-margin (event)
  "Enable/disable breakpoint in left margin with mouse click."
  (interactive "e")
  (mouse-minibuffer-check event)
  (let ((posn (event-end event)))
    (if (numberp (posn-point posn))
	(with-selected-window (posn-window posn)
	  (save-excursion
	    (goto-char (posn-point posn))
	    (if	(posn-object posn)
		(gud-basic-call
		 (let ((bptno (get-text-property
			       0 'cdb-bptno (car (posn-string posn)))))
		   (concat
		    (if (get-text-property
			 0 'cdb-enabled (car (posn-string posn)))
			"-break-disable "
		      "-break-enable ")
		    bptno)))))))))

(defun cdb-mouse-toggle-breakpoint-fringe (event)
  "Enable/disable breakpoint in left fringe with mouse click."
  (interactive "e")
  (mouse-minibuffer-check event)
  (let* ((posn (event-end event))
	 (pos (posn-point posn))
	 obj)
    (when (numberp pos)
      (with-selected-window (posn-window posn)
	(with-current-buffer (window-buffer)
	  (goto-char pos)
	  (dolist (overlay (overlays-in pos pos))
	    (when (overlay-get overlay 'put-break)
	      (setq obj (overlay-get overlay 'before-string))))
	  (when (stringp obj)
	    (gud-basic-call
	     (concat
	      (if (get-text-property 0 'cdb-enabled obj)
		  "-break-disable "
		"-break-enable ")
              (get-text-property 0 'cdb-bptno obj)))))))))

(defun cdb-breakpoints-buffer-name ()
  (concat "*breakpoints of " (cdb-get-target-string) "*"))

(defun cdb-display-breakpoints-buffer (&optional thread)
  "Display cdb breakpoints."
  (interactive)
  (cdb-display-buffer (cdb-get-buffer-create 'cdb-breakpoints-buffer thread)))

(defun cdb-frame-breakpoints-buffer (&optional thread)
  "Display cdb breakpoints in another frame."
  (interactive)
  (display-buffer (cdb-get-buffer-create 'cdb-breakpoints-buffer thread)
		  cdb-display-buffer-other-frame-action))

(defvar cdb-breakpoints-mode-map
  (let ((map (make-sparse-keymap))
	(menu (make-sparse-keymap "Breakpoints")))
    (define-key menu [quit] '("Quit"   . cdb-delete-frame-or-window))
    (define-key menu [goto] '("Goto"   . cdb-goto-breakpoint))
    (define-key menu [delete] '("Delete" . cdb-delete-breakpoint))
    (define-key menu [toggle] '("Toggle" . cdb-toggle-breakpoint))
    (suppress-keymap map)
    (define-key map [menu-bar breakpoints] (cons "Breakpoints" menu))
    (define-key map " " 'cdb-toggle-breakpoint)
    (define-key map "D" 'cdb-delete-breakpoint)
    ;; Don't bind "q" to kill-this-buffer as we need it for breakpoint icons.
    (define-key map "q" 'cdb-delete-frame-or-window)
    (define-key map "\r" 'cdb-goto-breakpoint)
    (define-key map "\t" (lambda ()
                           (interactive)
                           (cdb-set-window-buffer
                            (cdb-get-buffer-create 'cdb-threads-buffer) t)))
    (define-key map [mouse-2] 'cdb-goto-breakpoint)
    (define-key map [follow-link] 'mouse-face)
    map))

(defun cdb-delete-frame-or-window ()
  "Delete frame if there is only one window.  Otherwise delete the window."
  (interactive)
  (if (one-window-p) (delete-frame)
    (delete-window)))

;;from make-mode-line-mouse-map
(defun cdb-make-header-line-mouse-map (mouse function) "\
Return a keymap with single entry for mouse key MOUSE on the header line.
MOUSE is defined to run function FUNCTION with no args in the buffer
corresponding to the mode line clicked."
  (let ((map (make-sparse-keymap)))
    (define-key map (vector 'header-line mouse) function)
    (define-key map (vector 'header-line 'down-mouse-1) 'ignore)
    map))

(defmacro cdb-propertize-header (name buffer help-echo mouse-face face)
  `(propertize ,name
	       'help-echo ,help-echo
	       'mouse-face ',mouse-face
	       'face ',face
	       'local-map
	       (cdb-make-header-line-mouse-map
		'mouse-1
		(lambda (event) (interactive "e")
		  (save-selected-window
		    (select-window (posn-window (event-start event)))
                    (cdb-set-window-buffer
                     (cdb-get-buffer-create ',buffer) t) )))))


;; uses "-thread-info". Needs cdb 7.0 onwards.
;;; Threads view

(defun cdb-threads-buffer-name ()
  (concat "*threads of " (cdb-get-target-string) "*"))

(defun cdb-display-threads-buffer (&optional thread)
  "Display cdb threads."
  (interactive)
  (cdb-display-buffer (cdb-get-buffer-create 'cdb-threads-buffer thread)))

(defun cdb-frame-threads-buffer (&optional thread)
  "Display cdb threads in another frame."
  (interactive)
  (display-buffer (cdb-get-buffer-create 'cdb-threads-buffer thread)
		  cdb-display-buffer-other-frame-action))

(def-cdb-trigger-and-handler
  cdb-invalidate-threads (cdb-current-context-command "-thread-info")
  cdb-thread-list-handler cdb-thread-list-handler-custom
  '(start update update-threads))

(cdb-set-buffer-rules
 'cdb-threads-buffer
 'cdb-threads-buffer-name
 'cdb-threads-mode
 'cdb-invalidate-threads)

(defvar cdb-threads-font-lock-keywords
  '(("in \\([^ ]+\\)"  (1 font-lock-function-name-face))
    (" \\(stopped\\)"  (1 font-lock-warning-face))
    (" \\(running\\)"  (1 font-lock-string-face))
    ("\\(\\(\\sw\\|[_.]\\)+\\)="  (1 font-lock-variable-name-face)))
  "Font lock keywords used in `cdb-threads-mode'.")

(defvar cdb-threads-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\r" 'cdb-select-thread)
    (define-key map "f" 'cdb-display-stack-for-thread)
    (define-key map "F" 'cdb-frame-stack-for-thread)
    (define-key map "l" 'cdb-display-locals-for-thread)
    (define-key map "L" 'cdb-frame-locals-for-thread)
    (define-key map "r" 'cdb-display-registers-for-thread)
    (define-key map "R" 'cdb-frame-registers-for-thread)
    (define-key map "d" 'cdb-display-disassembly-for-thread)
    (define-key map "D" 'cdb-frame-disassembly-for-thread)
    (define-key map "i" 'cdb-interrupt-thread)
    (define-key map "c" 'cdb-continue-thread)
    (define-key map "s" 'cdb-step-thread)
    (define-key map "\t"
      (lambda ()
        (interactive)
        (cdb-set-window-buffer
         (cdb-get-buffer-create 'cdb-breakpoints-buffer) t)))
    (define-key map [mouse-2] 'cdb-select-thread)
    (define-key map [follow-link] 'mouse-face)
    map))

(defvar cdb-threads-header
  (list
   (cdb-propertize-header
    "Breakpoints" cdb-breakpoints-buffer
    "mouse-1: select" mode-line-highlight mode-line-inactive)
   " "
   (cdb-propertize-header "Threads" cdb-threads-buffer
			  nil nil mode-line)))

(define-derived-mode cdb-threads-mode cdb-parent-mode "Threads"
  "Major mode for cdb threads."
  (setq cdb-thread-position (make-marker))
  (add-to-list 'overlay-arrow-variable-list 'cdb-thread-position)
  (setq header-line-format cdb-threads-header)
  (set (make-local-variable 'font-lock-defaults)
       '(cdb-threads-font-lock-keywords))
  'cdb-invalidate-threads)

(defun cdb-thread-list-handler-custom ()
  (let ((threads-list (bindat-get-field (cdb-json-partial-output) 'threads))
        (table (make-cdb-table))
        (marked-line nil))
    (setq cdb-threads-list nil)
    (setq cdb-running-threads-count 0)
    (setq cdb-stopped-threads-count 0)
    (set-marker cdb-thread-position nil)

    (dolist (thread (reverse threads-list))
      (let ((running (equal (bindat-get-field thread 'state) "running")))
        (add-to-list 'cdb-threads-list
                     (cons (bindat-get-field thread 'id)
                           thread))
        (cl-incf (if running
                     cdb-running-threads-count
                   cdb-stopped-threads-count))

        (cdb-table-add-row
         table
         (list
          (bindat-get-field thread 'id)
          (concat
           (if cdb-thread-buffer-verbose-names
               (concat (bindat-get-field thread 'target-id) " ") "")
           (bindat-get-field thread 'state)
           ;; Include frame information for stopped threads
           (if (not running)
               (concat
                " in " (bindat-get-field thread 'frame 'func)
                (if cdb-thread-buffer-arguments
                    (concat
                     " ("
                     (let ((args (bindat-get-field thread 'frame 'args)))
                       (mapconcat
                        (lambda (arg)
                          (apply #'format "%s=%s"
                                 (cdb-get-many-fields arg 'name 'value)))
                        args ","))
                     ")")
                  "")
                (if cdb-thread-buffer-locations
                    (cdb-frame-location (bindat-get-field thread 'frame)) "")
                (if cdb-thread-buffer-addresses
                    (concat " at " (bindat-get-field thread 'frame 'addr)) ""))
             "")))
         (list
          'cdb-thread thread
          'mouse-face 'highlight
          'help-echo "mouse-2, RET: select thread")))
      (when (string-equal cdb-thread-number
                          (bindat-get-field thread 'id))
        (setq marked-line (length cdb-threads-list))))
    (insert (cdb-table-string table " "))
    (when marked-line
      (cdb-mark-line marked-line cdb-thread-position)))
  ;; We update gud-running here because we need to make sure that
  ;; cdb-threads-list is up-to-date
  (cdb-update-gud-running)
  (cdb-emit-signal cdb-buf-publisher 'update-disassembly))

(defmacro def-cdb-thread-buffer-command (name custom-defun &optional doc)
  "Define a NAME command which will act upon thread on the current line.

CUSTOM-DEFUN may use locally bound `thread' variable, which will
be the value of 'cdb-thread property of the current line.
If `cdb-thread' is nil, error is signaled."
  `(defun ,name (&optional event)
     ,(when doc doc)
     (interactive (list last-input-event))
     (if event (posn-set-point (event-end event)))
     (save-excursion
       (beginning-of-line)
       (let ((thread (get-text-property (point) 'cdb-thread)))
         (if thread
             ,custom-defun
           (error "Not recognized as thread line"))))))

(defmacro def-cdb-thread-buffer-simple-command (name buffer-command
                                                     &optional doc)
  "Define a NAME which will call BUFFER-COMMAND with id of thread
on the current line."
  `(def-cdb-thread-buffer-command ,name
     (,buffer-command (bindat-get-field thread 'id))
     ,doc))

(def-cdb-thread-buffer-command cdb-select-thread
  (let ((new-id (bindat-get-field thread 'id)))
    (cdb-setq-thread-number new-id)
    (cdb-input (concat "-thread-select " new-id) 'ignore)
    (cdb-update))
  "Select the thread at current line of threads buffer.")

(def-cdb-thread-buffer-simple-command
  cdb-display-stack-for-thread
  cdb-preemptively-display-stack-buffer
  "Display stack buffer for the thread at current line.")

(def-cdb-thread-buffer-simple-command
  cdb-display-locals-for-thread
  cdb-preemptively-display-locals-buffer
  "Display locals buffer for the thread at current line.")

(def-cdb-thread-buffer-simple-command
  cdb-display-registers-for-thread
  cdb-preemptively-display-registers-buffer
  "Display registers buffer for the thread at current line.")

(def-cdb-thread-buffer-simple-command
  cdb-display-disassembly-for-thread
  cdb-preemptively-display-disassembly-buffer
  "Display disassembly buffer for the thread at current line.")

(def-cdb-thread-buffer-simple-command
  cdb-frame-stack-for-thread
  cdb-frame-stack-buffer
  "Display another frame with stack buffer for thread at current line.")

(def-cdb-thread-buffer-simple-command
  cdb-frame-locals-for-thread
  cdb-frame-locals-buffer
  "Display another frame with locals buffer for thread at current line.")

(def-cdb-thread-buffer-simple-command
  cdb-frame-registers-for-thread
  cdb-frame-registers-buffer
  "Display another frame with registers buffer for the thread at current line.")

(def-cdb-thread-buffer-simple-command
  cdb-frame-disassembly-for-thread
  cdb-frame-disassembly-buffer
  "Display another frame with disassembly buffer for the thread at current line.")

(defmacro def-cdb-thread-buffer-gud-command (name gud-command &optional doc)
  "Define a NAME which will execute GUD-COMMAND with
`cdb-thread-number' locally bound to id of thread on the current
line."
  `(def-cdb-thread-buffer-command ,name
     (if cdb-non-stop
         (let ((cdb-thread-number (bindat-get-field thread 'id))
               (cdb-gud-control-all-threads nil))
           (call-interactively #',gud-command))
       (error "Available in non-stop mode only, customize `cdb-non-stop-setting'"))
     ,doc))

(def-cdb-thread-buffer-gud-command
  cdb-interrupt-thread
  gud-stop-subjob
  "Interrupt thread at current line.")

;; Defined opaquely in M-x cdb via gud-def.
(declare-function gud-cont "cdb-mi" (arg) t)

(def-cdb-thread-buffer-gud-command
  cdb-continue-thread
  gud-cont
  "Continue thread at current line.")

(declare-function gud-step "cdb-mi" (arg) t)

(def-cdb-thread-buffer-gud-command
  cdb-step-thread
  gud-step
  "Step thread at current line.")


;;; Memory view

(defcustom cdb-memory-rows 8
  "Number of data rows in memory window."
  :type 'integer
  :group 'gud
  :version "23.2")

(defcustom cdb-memory-columns 4
  "Number of data columns in memory window."
  :type 'integer
  :group 'gud
  :version "23.2")

(defcustom cdb-memory-format "x"
  "Display format of data items in memory window."
  :type '(choice (const :tag "Hexadecimal" "x")
          (const :tag "Signed decimal" "d")
          (const :tag "Unsigned decimal" "u")
          (const :tag "Octal" "o")
          (const :tag "Binary" "t"))
  :group 'gud
  :version "22.1")

(defcustom cdb-memory-unit 4
  "Unit size of data items in memory window."
  :type '(choice (const :tag "Byte" 1)
          (const :tag "Halfword" 2)
          (const :tag "Word" 4)
          (const :tag "Giant word" 8))
  :group 'gud
  :version "23.2")

(def-cdb-trigger-and-handler
  cdb-invalidate-memory
  (format "-data-read-memory %s %s %d %d %d"
          cdb-memory-address
          cdb-memory-format
          cdb-memory-unit
          cdb-memory-rows
          cdb-memory-columns)
  cdb-read-memory-handler
  cdb-read-memory-custom
  '(start update))

(cdb-set-buffer-rules
 'cdb-memory-buffer
 'cdb-memory-buffer-name
 'cdb-memory-mode
 'cdb-invalidate-memory)

(defun cdb-memory-column-width (size format)
  "Return length of string with memory unit of SIZE in FORMAT.

SIZE is in bytes, as in `cdb-memory-unit'.  FORMAT is a string as
in `cdb-memory-format'."
  (let ((format-base (cdr (assoc format
                                 '(("x" . 16)
                                   ("d" . 10) ("u" . 10)
                                   ("o" . 8)
                                   ("t" . 2))))))
    (if format-base
        (let ((res (ceiling (log (expt 2.0 (* size 8)) format-base))))
          (cond ((string-equal format "x")
                 (+ 2 res)) ; hexadecimal numbers have 0x in front
                ((or (string-equal format "d")
                     (string-equal format "o"))
                 (1+ res))
                (t res)))
      (error "Unknown format"))))

(defun cdb-read-memory-custom ()
  (let* ((res (cdb-json-partial-output))
         (err-msg (bindat-get-field res 'msg)))
    (if (not err-msg)
        (let ((memory (bindat-get-field res 'memory)))
          (setq cdb-memory-address (bindat-get-field res 'addr))
          (setq cdb-memory-next-page (bindat-get-field res 'next-page))
          (setq cdb-memory-prev-page (bindat-get-field res 'prev-page))
          (setq cdb-memory-last-address cdb-memory-address)
          (dolist (row memory)
            (insert (concat (bindat-get-field row 'addr) ":"))
            (dolist (column (bindat-get-field row 'data))
              (insert (cdb-pad-string column
                                      (+ 2 (cdb-memory-column-width
                                            cdb-memory-unit
                                            cdb-memory-format)))))
            (newline)))
      ;; Show last page instead of empty buffer when out of bounds
      (progn
        (let ((cdb-memory-address cdb-memory-last-address))
          (cdb-invalidate-memory 'update)
          (error err-msg))))))

(defvar cdb-memory-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map "q" 'kill-this-buffer)
    (define-key map "n" 'cdb-memory-show-next-page)
    (define-key map "p" 'cdb-memory-show-previous-page)
    (define-key map "a" 'cdb-memory-set-address)
    (define-key map "t" 'cdb-memory-format-binary)
    (define-key map "o" 'cdb-memory-format-octal)
    (define-key map "u" 'cdb-memory-format-unsigned)
    (define-key map "d" 'cdb-memory-format-signed)
    (define-key map "x" 'cdb-memory-format-hexadecimal)
    (define-key map "b" 'cdb-memory-unit-byte)
    (define-key map "h" 'cdb-memory-unit-halfword)
    (define-key map "w" 'cdb-memory-unit-word)
    (define-key map "g" 'cdb-memory-unit-giant)
    (define-key map "R" 'cdb-memory-set-rows)
    (define-key map "C" 'cdb-memory-set-columns)
    map))

(defun cdb-memory-set-address-event (event)
  "Handle a click on address field in memory buffer header."
  (interactive "e")
  (save-selected-window
    (select-window (posn-window (event-start event)))
    (cdb-memory-set-address)))

;; Non-event version for use within keymap
(defun cdb-memory-set-address ()
  "Set the start memory address."
  (interactive)
  (let ((arg (read-from-minibuffer "Memory address: ")))
    (setq cdb-memory-address arg))
  (cdb-invalidate-memory 'update))

(defmacro def-cdb-set-positive-number (name variable echo-string &optional doc)
  "Define a function NAME which reads new VAR value from minibuffer."
  `(defun ,name (event)
     ,(when doc doc)
     (interactive "e")
     (save-selected-window
       (select-window (posn-window (event-start event)))
       (let* ((arg (read-from-minibuffer ,echo-string))
              (count (string-to-number arg)))
         (if (<= count 0)
             (error "Positive number only")
           (customize-set-variable ',variable count)
           (cdb-invalidate-memory 'update))))))

(def-cdb-set-positive-number
  cdb-memory-set-rows
  cdb-memory-rows
  "Rows: "
  "Set the number of data rows in memory window.")

(def-cdb-set-positive-number
  cdb-memory-set-columns
  cdb-memory-columns
  "Columns: "
  "Set the number of data columns in memory window.")

(defmacro def-cdb-memory-format (name format doc)
  "Define a function NAME to switch memory buffer to use FORMAT.

DOC is an optional documentation string."
  `(defun ,name () ,(when doc doc)
     (interactive)
     (customize-set-variable 'cdb-memory-format ,format)
     (cdb-invalidate-memory 'update)))

(def-cdb-memory-format
  cdb-memory-format-binary "t"
  "Set the display format to binary.")

(def-cdb-memory-format
  cdb-memory-format-octal "o"
  "Set the display format to octal.")

(def-cdb-memory-format
  cdb-memory-format-unsigned "u"
  "Set the display format to unsigned decimal.")

(def-cdb-memory-format
  cdb-memory-format-signed "d"
  "Set the display format to decimal.")

(def-cdb-memory-format
  cdb-memory-format-hexadecimal "x"
  "Set the display format to hexadecimal.")

(defvar cdb-memory-format-map
  (let ((map (make-sparse-keymap)))
    (define-key map [header-line down-mouse-3] 'cdb-memory-format-menu-1)
    map)
  "Keymap to select format in the header line.")

(defvar cdb-memory-format-menu
  (let ((map (make-sparse-keymap "Format")))

    (define-key map [binary]
      '(menu-item "Binary" cdb-memory-format-binary
        :button (:radio . (equal cdb-memory-format "t"))))
    (define-key map [octal]
      '(menu-item "Octal" cdb-memory-format-octal
        :button (:radio . (equal cdb-memory-format "o"))))
    (define-key map [unsigned]
      '(menu-item "Unsigned Decimal" cdb-memory-format-unsigned
        :button (:radio . (equal cdb-memory-format "u"))))
    (define-key map [signed]
      '(menu-item "Signed Decimal" cdb-memory-format-signed
        :button (:radio . (equal cdb-memory-format "d"))))
    (define-key map [hexadecimal]
      '(menu-item "Hexadecimal" cdb-memory-format-hexadecimal
        :button (:radio . (equal cdb-memory-format "x"))))
    map)
  "Menu of display formats in the header line.")

(defun cdb-memory-format-menu (event)
  (interactive "@e")
  (x-popup-menu event cdb-memory-format-menu))

(defun cdb-memory-format-menu-1 (event)
  (interactive "e")
  (save-selected-window
    (select-window (posn-window (event-start event)))
    (let* ((selection (cdb-memory-format-menu event))
	   (binding (and selection (lookup-key cdb-memory-format-menu
					       (vector (car selection))))))
      (if binding (call-interactively binding)))))

(defmacro def-cdb-memory-unit (name unit-size doc)
  "Define a function NAME to switch memory unit size to UNIT-SIZE.

DOC is an optional documentation string."
  `(defun ,name () ,(when doc doc)
     (interactive)
     (customize-set-variable 'cdb-memory-unit ,unit-size)
     (cdb-invalidate-memory 'update)))

(def-cdb-memory-unit cdb-memory-unit-giant 8
  "Set the unit size to giant words (eight bytes).")

(def-cdb-memory-unit cdb-memory-unit-word 4
  "Set the unit size to words (four bytes).")

(def-cdb-memory-unit cdb-memory-unit-halfword 2
  "Set the unit size to halfwords (two bytes).")

(def-cdb-memory-unit cdb-memory-unit-byte 1
  "Set the unit size to bytes.")

(defmacro def-cdb-memory-show-page (name address-var &optional doc)
  "Define a function NAME which show new address in memory buffer.

The defined function switches Memory buffer to show address
stored in ADDRESS-VAR variable.

DOC is an optional documentation string."
  `(defun ,name
     ,(when doc doc)
     (interactive)
     (let ((cdb-memory-address ,address-var))
       (cdb-invalidate-memory))))

(def-cdb-memory-show-page cdb-memory-show-previous-page
  cdb-memory-prev-page)

(def-cdb-memory-show-page cdb-memory-show-next-page
  cdb-memory-next-page)

(defvar cdb-memory-unit-map
  (let ((map (make-sparse-keymap)))
    (define-key map [header-line down-mouse-3] 'cdb-memory-unit-menu-1)
    map)
  "Keymap to select units in the header line.")

(defvar cdb-memory-unit-menu
  (let ((map (make-sparse-keymap "Unit")))
    (define-key map [giantwords]
      '(menu-item "Giant words" cdb-memory-unit-giant
        :button (:radio . (equal cdb-memory-unit 8))))
    (define-key map [words]
      '(menu-item "Words" cdb-memory-unit-word
        :button (:radio . (equal cdb-memory-unit 4))))
    (define-key map [halfwords]
      '(menu-item "Halfwords" cdb-memory-unit-halfword
        :button (:radio . (equal cdb-memory-unit 2))))
    (define-key map [bytes]
      '(menu-item "Bytes" cdb-memory-unit-byte
        :button (:radio . (equal cdb-memory-unit 1))))
    map)
  "Menu of units in the header line.")

(defun cdb-memory-unit-menu (event)
  (interactive "@e")
  (x-popup-menu event cdb-memory-unit-menu))

(defun cdb-memory-unit-menu-1 (event)
  (interactive "e")
  (save-selected-window
    (select-window (posn-window (event-start event)))
    (let* ((selection (cdb-memory-unit-menu event))
	   (binding (and selection (lookup-key cdb-memory-unit-menu
					       (vector (car selection))))))
      (if binding (call-interactively binding)))))

(defvar cdb-memory-font-lock-keywords
  '(;; <__function.name+n>
    ("<\\(\\(\\sw\\|[_.]\\)+\\)\\(\\+[0-9]+\\)?>"
     (1 font-lock-function-name-face)))
  "Font lock keywords used in `cdb-memory-mode'.")

(defvar cdb-memory-header
  '(:eval
    (concat
     "Start address["
     (propertize "-"
                 'face font-lock-warning-face
                 'help-echo "mouse-1: decrement address"
                 'mouse-face 'mode-line-highlight
                 'local-map (cdb-make-header-line-mouse-map
                             'mouse-1
                             #'cdb-memory-show-previous-page))
     "|"
     (propertize "+"
                 'face font-lock-warning-face
                 'help-echo "mouse-1: increment address"
                 'mouse-face 'mode-line-highlight
                 'local-map (cdb-make-header-line-mouse-map
                             'mouse-1
                             #'cdb-memory-show-next-page))
     "]: "
     (propertize cdb-memory-address
                 'face font-lock-warning-face
                 'help-echo "mouse-1: set start address"
                 'mouse-face 'mode-line-highlight
                 'local-map (cdb-make-header-line-mouse-map
                             'mouse-1
                             #'cdb-memory-set-address-event))
     "  Rows: "
     (propertize (number-to-string cdb-memory-rows)
                 'face font-lock-warning-face
                 'help-echo "mouse-1: set number of columns"
                 'mouse-face 'mode-line-highlight
                 'local-map (cdb-make-header-line-mouse-map
                             'mouse-1
                             #'cdb-memory-set-rows))
     "  Columns: "
     (propertize (number-to-string cdb-memory-columns)
                 'face font-lock-warning-face
                 'help-echo "mouse-1: set number of columns"
                 'mouse-face 'mode-line-highlight
                 'local-map (cdb-make-header-line-mouse-map
                             'mouse-1
                             #'cdb-memory-set-columns))
     "  Display Format: "
     (propertize cdb-memory-format
                 'face font-lock-warning-face
                 'help-echo "mouse-3: select display format"
                 'mouse-face 'mode-line-highlight
                 'local-map cdb-memory-format-map)
     "  Unit Size: "
     (propertize (number-to-string cdb-memory-unit)
                 'face font-lock-warning-face
                 'help-echo "mouse-3: select unit size"
                 'mouse-face 'mode-line-highlight
                 'local-map cdb-memory-unit-map)))
  "Header line used in `cdb-memory-mode'.")

(define-derived-mode cdb-memory-mode cdb-parent-mode "Memory"
  "Major mode for examining memory."
  (setq header-line-format cdb-memory-header)
  (set (make-local-variable 'font-lock-defaults)
       '(cdb-memory-font-lock-keywords))
  'cdb-invalidate-memory)

(defun cdb-memory-buffer-name ()
  (concat "*memory of " (cdb-get-target-string) "*"))

(defun cdb-display-memory-buffer (&optional thread)
  "Display cdb memory contents."
  (interactive)
  (cdb-display-buffer (cdb-get-buffer-create 'cdb-memory-buffer thread)))

(defun cdb-frame-memory-buffer ()
  "Display memory contents in another frame."
  (interactive)
  (display-buffer (cdb-get-buffer-create 'cdb-memory-buffer)
		  cdb-display-buffer-other-frame-action))


;;; Disassembly view

(defun cdb-disassembly-buffer-name ()
  (cdb-current-context-buffer-name
   (concat "disassembly of " (cdb-get-target-string))))

(defun cdb-display-disassembly-buffer (&optional thread)
  "Display cdb disassembly information."
  (interactive)
  (cdb-display-buffer (cdb-get-buffer-create 'cdb-disassembly-buffer thread)))

(def-cdb-preempt-display-buffer
  cdb-preemptively-display-disassembly-buffer
  'cdb-disassembly-buffer)

(defun cdb-frame-disassembly-buffer (&optional thread)
  "Display cdb disassembly information in another frame."
  (interactive)
  (display-buffer (cdb-get-buffer-create 'cdb-disassembly-buffer thread)
		  cdb-display-buffer-other-frame-action))

(def-cdb-auto-update-trigger cdb-invalidate-disassembly
  (let* ((frame (cdb-current-buffer-frame))
         (file (bindat-get-field frame 'fullname))
         (line (bindat-get-field frame 'line)))
    (if file
      (format "-data-disassemble -f %s -l %s -n -1 -- 0" file line)
    ;; If we're unable to get a file name / line for $PC, simply
    ;; follow $PC, disassembling the next 10 (x ~15 (on IA) ==
    ;; 150 bytes) instructions.
    "-data-disassemble -s $pc -e \"$pc + 150\" -- 0"))
  cdb-disassembly-handler
  ;; We update disassembly only after we have actual frame information
  ;; about all threads, so no there's `update' signal in this list
  '(start update-disassembly))

(def-cdb-auto-update-handler
  cdb-disassembly-handler
  cdb-disassembly-handler-custom
  t)

(cdb-set-buffer-rules
 'cdb-disassembly-buffer
 'cdb-disassembly-buffer-name
 'cdb-disassembly-mode
 'cdb-invalidate-disassembly)

(defvar cdb-disassembly-font-lock-keywords
  '(;; <__function.name+n>
    ("<\\(\\(\\sw\\|[_.]\\)+\\)\\(\\+[0-9]+\\)?>"
     (1 font-lock-function-name-face))
    ;; 0xNNNNNNNN <__function.name+n>: opcode
    ("^0x[0-9a-f]+ \\(<\\(\\(\\sw\\|[_.]\\)+\\)\\+[0-9]+>\\)?:[ \t]+\\(\\sw+\\)"
     (4 font-lock-keyword-face))
    ;; %register(at least i386)
    ("%\\sw+" . font-lock-variable-name-face)
    ("^\\(Dump of assembler code for function\\) \\(.+\\):"
     (1 font-lock-comment-face)
     (2 font-lock-function-name-face))
    ("^\\(End of assembler dump\\.\\)" . font-lock-comment-face))
  "Font lock keywords used in `cdb-disassembly-mode'.")

(defvar cdb-disassembly-mode-map
  ;; TODO
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "q" 'kill-this-buffer)
    map))

(define-derived-mode cdb-disassembly-mode cdb-parent-mode "Disassembly"
  "Major mode for cdb disassembly information."
  ;; TODO Rename overlay variable for disassembly mode
  (add-to-list 'overlay-arrow-variable-list 'cdb-disassembly-position)
  (setq fringes-outside-margins t)
  (set (make-local-variable 'cdb-disassembly-position) (make-marker))
  (set (make-local-variable 'font-lock-defaults)
       '(cdb-disassembly-font-lock-keywords))
  'cdb-invalidate-disassembly)

(defun cdb-disassembly-handler-custom ()
  (let* ((instructions (bindat-get-field (cdb-json-partial-output) 'asm_insns))
         (address (bindat-get-field (cdb-current-buffer-frame) 'addr))
         (table (make-cdb-table))
         (marked-line nil))
    (dolist (instr instructions)
      (cdb-table-add-row table
                         (list
                          (bindat-get-field instr 'address)
                          (let
                              ((func-name (bindat-get-field instr 'func-name))
                               (offset (bindat-get-field instr 'offset)))
                            (if func-name
                                (format "<%s+%s>:" func-name offset)
                              ""))
                          (bindat-get-field instr 'inst)))
      (when (string-equal (bindat-get-field instr 'address)
                          address)
        (progn
          (setq marked-line (length (cdb-table-rows table)))
          (setq fringe-indicator-alist
                (if (string-equal cdb-frame-number "0")
                    nil
                  '((overlay-arrow . hollow-right-triangle)))))))
    (insert (cdb-table-string table " "))
    (cdb-disassembly-place-breakpoints)
    ;; Mark current position with overlay arrow and scroll window to
    ;; that point
    (when marked-line
      (let ((window (get-buffer-window (current-buffer) 0)))
        (set-window-point window (cdb-mark-line marked-line
                                                cdb-disassembly-position))))
    (setq mode-name
          (cdb-current-context-mode-name
           (concat "Disassembly: "
                   (bindat-get-field (cdb-current-buffer-frame) 'func))))))

(defun cdb-disassembly-place-breakpoints ()
  (cdb-remove-breakpoint-icons (point-min) (point-max))
  (dolist (breakpoint cdb-breakpoints-list)
    (let* ((breakpoint (cdr breakpoint))
           (bptno (bindat-get-field breakpoint 'number))
           (flag (bindat-get-field breakpoint 'enabled))
           (address (bindat-get-field breakpoint 'addr)))
      (save-excursion
        (goto-char (point-min))
        (if (re-search-forward (concat "^" address) nil t)
            (cdb-put-breakpoint-icon (string-equal flag "y") bptno))))))


(defvar cdb-breakpoints-header
  (list
   (cdb-propertize-header "Breakpoints" cdb-breakpoints-buffer
			  nil nil mode-line)
   " "
   (cdb-propertize-header "Threads" cdb-threads-buffer
			  "mouse-1: select" mode-line-highlight
                          mode-line-inactive)))

;;; Breakpoints view
(define-derived-mode cdb-breakpoints-mode cdb-parent-mode "Breakpoints"
  "Major mode for cdb breakpoints."
  (setq header-line-format cdb-breakpoints-header)
  'cdb-invalidate-breakpoints)

(defun cdb-toggle-breakpoint ()
  "Enable/disable breakpoint at current line of breakpoints buffer."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((breakpoint (get-text-property (point) 'cdb-breakpoint)))
      (if breakpoint
          (gud-basic-call
           (concat (if (equal "y" (bindat-get-field breakpoint 'enabled))
                       "-break-disable "
                     "-break-enable ")
                   (bindat-get-field breakpoint 'number)))
        (error "Not recognized as break/watchpoint line")))))

(defun cdb-delete-breakpoint ()
  "Delete the breakpoint at current line of breakpoints buffer."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((breakpoint (get-text-property (point) 'cdb-breakpoint)))
      (if breakpoint
          (gud-basic-call (concat "-break-delete "
                                  (bindat-get-field breakpoint 'number)))
        (error "Not recognized as break/watchpoint line")))))

(defun cdb-goto-breakpoint (&optional event)
  "Go to the location of breakpoint at current line of breakpoints buffer."
  (interactive (list last-input-event))
  (if event (posn-set-point (event-end event)))
  ;; Hack to stop cdb-goto-breakpoint displaying in GUD buffer.
  (let ((window (get-buffer-window gud-comint-buffer)))
    (if window (save-selected-window  (select-window window))))
  (save-excursion
    (beginning-of-line)
    (let ((breakpoint (get-text-property (point) 'cdb-breakpoint)))
      (if breakpoint
          (let ((bptno (bindat-get-field breakpoint 'number))
                (file  (bindat-get-field breakpoint 'fullname))
                (line  (bindat-get-field breakpoint 'line)))
            (save-selected-window
              (let* ((buffer (find-file-noselect
                              (if (file-exists-p file) file
                                (cdr (assoc bptno cdb-location-alist)))))
                     (window (or (cdb-display-source-buffer buffer)
                                 (display-buffer buffer))))
                (setq cdb-source-window window)
                (with-current-buffer buffer
                  (goto-char (point-min))
                  (forward-line (1- (string-to-number line)))
                  (set-window-point window (point))))))
        (error "Not recognized as break/watchpoint line")))))


;; Frames buffer.  This displays a perpetually correct backtrack trace.
;;
(def-cdb-trigger-and-handler
  cdb-invalidate-frames (cdb-current-context-command "kn")
  cdb-stack-list-frames-handler cdb-stack-list-frames-custom
  '(start update))

(cdb-set-buffer-rules
 'cdb-stack-buffer
 'cdb-stack-buffer-name
 'cdb-frames-mode
 'cdb-invalidate-frames)

(defun cdb-frame-location (frame)
  "Return \" of file:line\" or \" of library\" for structure FRAME.

FRAME must have either \"file\" and \"line\" members or \"from\"
member."
  (let ((file (bindat-get-field frame 'file))
        (line (bindat-get-field frame 'line))
        (from (bindat-get-field frame 'from)))
    (let ((res (or (and file line (concat file ":" line))
                   from)))
      (if res (concat " of " res) ""))))

(defun cdb-stack-list-frames-custom ()
	"Do stuff."
  (let ((stack (bindat-get-field (cdb-json-partial-output "frame") 'stack))
        (table (make-cdb-table)))
    (set-marker cdb-stack-position nil)
    (dolist (frame stack)
      (cdb-table-add-row table
                         (list
                          (bindat-get-field frame 'level)
                          "in"
                          (concat
                           (bindat-get-field frame 'func)
                           (if cdb-stack-buffer-locations
                               (cdb-frame-location frame) "")
                           (if cdb-stack-buffer-addresses
                               (concat " at " (bindat-get-field frame 'addr)) "")))
                         `(mouse-face highlight
                                      help-echo "mouse-2, RET: Select frame"
                                      cdb-frame ,frame)))
    (insert (cdb-table-string table " ")))
  (when (and cdb-frame-number
             (cdb-buffer-shows-main-thread-p))
    (cdb-mark-line (1+ (string-to-number cdb-frame-number))
                   cdb-stack-position))
  (setq mode-name
        (cdb-current-context-mode-name "Frames")))

(defun cdb-stack-buffer-name ()
  (cdb-current-context-buffer-name
   (concat "stack frames of " (cdb-get-target-string))))

(defun cdb-display-stack-buffer (&optional thread)
  "Display cdb backtrace for current stack."
  (interactive)
  (cdb-display-buffer (cdb-get-buffer-create 'cdb-stack-buffer thread)))

(def-cdb-preempt-display-buffer
  cdb-preemptively-display-stack-buffer
  'cdb-stack-buffer nil t)

(defun cdb-frame-stack-buffer (&optional thread)
  "Display cdb backtrace for current stack in another frame."
  (interactive)
  (display-buffer (cdb-get-buffer-create 'cdb-stack-buffer thread)
		  cdb-display-buffer-other-frame-action))

(defvar cdb-frames-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "q" 'kill-this-buffer)
    (define-key map "\r" 'cdb-select-frame)
    (define-key map [mouse-2] 'cdb-select-frame)
    (define-key map [follow-link] 'mouse-face)
    map))

(defvar cdb-frames-font-lock-keywords
  '(("in \\([^ ]+\\)"  (1 font-lock-function-name-face)))
  "Font lock keywords used in `cdb-frames-mode'.")

(define-derived-mode cdb-frames-mode cdb-parent-mode "Frames"
  "Major mode for cdb call stack."
  (setq cdb-stack-position (make-marker))
  (add-to-list 'overlay-arrow-variable-list 'cdb-stack-position)
  (setq truncate-lines t)  ;; Make it easier to see overlay arrow.
  (set (make-local-variable 'font-lock-defaults)
       '(cdb-frames-font-lock-keywords))
  'cdb-invalidate-frames)

(defun cdb-select-frame (&optional event)
  "Select the frame and display the relevant source."
  (interactive (list last-input-event))
  (if event (posn-set-point (event-end event)))
  (let ((frame (get-text-property (point) 'cdb-frame)))
    (if frame
        (if (cdb-buffer-shows-main-thread-p)
            (let ((new-level (bindat-get-field frame 'level)))
              (setq cdb-frame-number new-level)
              (cdb-input (concat "-stack-select-frame " new-level)
			 'ignore)
              (cdb-update))
          (error "Could not select frame for non-current thread"))
      (error "Not recognized as frame line"))))


;; Locals buffer.
;; uses "-stack-list-locals --simple-values". Needs cdb 6.1 onwards.
(def-cdb-trigger-and-handler
  cdb-invalidate-locals
  (concat (cdb-current-context-command "-stack-list-locals")
          " --simple-values")
  cdb-locals-handler cdb-locals-handler-custom
  '(start update))

(cdb-set-buffer-rules
 'cdb-locals-buffer
 'cdb-locals-buffer-name
 'cdb-locals-mode
 'cdb-invalidate-locals)

(defvar cdb-locals-watch-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "\r" 'gud-watch)
    (define-key map [mouse-2] 'gud-watch)
    map)
  "Keymap to create watch expression of a complex data type local variable.")

(defvar cdb-edit-locals-map-1
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "\r" 'cdb-edit-locals-value)
    (define-key map [mouse-2] 'cdb-edit-locals-value)
    map)
  "Keymap to edit value of a simple data type local variable.")

(defun cdb-edit-locals-value (&optional event)
  "Assign a value to a variable displayed in the locals buffer."
  (interactive (list last-input-event))
  (save-excursion
    (if event (posn-set-point (event-end event)))
    (beginning-of-line)
    (let* ((var (bindat-get-field
                 (get-text-property (point) 'cdb-local-variable) 'name))
	   (value (read-string (format "New value (%s): " var))))
      (gud-basic-call
       (concat  "-cdb-set variable " var " = " value)))))

;; Don't display values of arrays or structures.
;; These can be expanded using gud-watch.
(defun cdb-locals-handler-custom ()
  (let ((locals-list (bindat-get-field (cdb-json-partial-output) 'locals))
        (table (make-cdb-table)))
    (dolist (local locals-list)
      (let ((name (bindat-get-field local 'name))
            (value (bindat-get-field local 'value))
            (type (bindat-get-field local 'type)))
        (if (or (not value)
                (string-match "\\0x" value))
            (add-text-properties 0 (length name)
                                 `(mouse-face highlight
                                              help-echo "mouse-2: create watch expression"
                                              local-map ,cdb-locals-watch-map)
                                 name)
          (add-text-properties 0 (length value)
                               `(mouse-face highlight
                                            help-echo "mouse-2: edit value"
                                            local-map ,cdb-edit-locals-map-1)
                               value))
        (cdb-table-add-row
         table
         (list
          (propertize type 'font-lock-face font-lock-type-face)
          (propertize name 'font-lock-face font-lock-variable-name-face)
          value)
         `(cdb-local-variable ,local))))
    (insert (cdb-table-string table " "))
    (setq mode-name
          (cdb-current-context-mode-name
           (concat "Locals: "
                   (bindat-get-field (cdb-current-buffer-frame) 'func))))))

(defvar cdb-locals-header
  (list
   (cdb-propertize-header "Locals" cdb-locals-buffer
			  nil nil mode-line)
   " "
   (cdb-propertize-header "Registers" cdb-registers-buffer
			  "mouse-1: select" mode-line-highlight
                          mode-line-inactive)))

(defvar cdb-locals-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "q" 'kill-this-buffer)
    (define-key map "\t" (lambda ()
                           (interactive)
                           (cdb-set-window-buffer
                            (cdb-get-buffer-create
                             'cdb-registers-buffer
                             cdb-thread-number) t)))
    map))

(define-derived-mode cdb-locals-mode cdb-parent-mode "Locals"
  "Major mode for cdb locals."
  (setq header-line-format cdb-locals-header)
  'cdb-invalidate-locals)

(defun cdb-locals-buffer-name ()
  (cdb-current-context-buffer-name
   (concat "locals of " (cdb-get-target-string))))

(defun cdb-display-locals-buffer (&optional thread)
  "Display the local variables of current cdb stack."
  (interactive)
  (cdb-display-buffer (cdb-get-buffer-create 'cdb-locals-buffer thread)))

(def-cdb-preempt-display-buffer
  cdb-preemptively-display-locals-buffer
  'cdb-locals-buffer nil t)

(defun cdb-frame-locals-buffer (&optional thread)
  "Display the local variables of the current cdb stack in another frame."
  (interactive)
  (display-buffer (cdb-get-buffer-create 'cdb-locals-buffer thread)
		  cdb-display-buffer-other-frame-action))


;; Registers buffer.

(def-cdb-trigger-and-handler
  cdb-invalidate-registers
  (concat (cdb-current-context-command "-data-list-register-values") " x")
  cdb-registers-handler
  cdb-registers-handler-custom
  '(start update))

(cdb-set-buffer-rules
 'cdb-registers-buffer
 'cdb-registers-buffer-name
 'cdb-registers-mode
 'cdb-invalidate-registers)

(defun cdb-registers-handler-custom ()
  (when cdb-register-names
    (let ((register-values
           (bindat-get-field (cdb-json-partial-output) 'register-values))
          (table (make-cdb-table)))
      (dolist (register register-values)
        (let* ((register-number (bindat-get-field register 'number))
               (value (bindat-get-field register 'value))
               (register-name (nth (string-to-number register-number)
                                   cdb-register-names)))
          (cdb-table-add-row
           table
           (list
            (propertize register-name
                        'font-lock-face font-lock-variable-name-face)
            (if (member register-number cdb-changed-registers)
                (propertize value 'font-lock-face font-lock-warning-face)
              value))
           `(mouse-face highlight
                        help-echo "mouse-2: edit value"
                        cdb-register-name ,register-name))))
      (insert (cdb-table-string table " ")))
    (setq mode-name
          (cdb-current-context-mode-name "Registers"))))

(defun cdb-edit-register-value (&optional event)
  "Assign a value to a register displayed in the registers buffer."
  (interactive (list last-input-event))
  (save-excursion
    (if event (posn-set-point (event-end event)))
    (beginning-of-line)
    (let* ((var (bindat-get-field
                 (get-text-property (point) 'cdb-register-name)))
	   (value (read-string (format "New value (%s): " var))))
      (gud-basic-call
       (concat  "-cdb-set variable $" var " = " value)))))

(defvar cdb-registers-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "\r" 'cdb-edit-register-value)
    (define-key map [mouse-2] 'cdb-edit-register-value)
    (define-key map "q" 'kill-this-buffer)
    (define-key map "\t" (lambda ()
                           (interactive)
                           (cdb-set-window-buffer
                            (cdb-get-buffer-create
                             'cdb-locals-buffer
                             cdb-thread-number) t)))
    map))

(defvar cdb-registers-header
  (list
   (cdb-propertize-header "Locals" cdb-locals-buffer
			  "mouse-1: select" mode-line-highlight
                          mode-line-inactive)
   " "
   (cdb-propertize-header "Registers" cdb-registers-buffer
			  nil nil mode-line)))

(define-derived-mode cdb-registers-mode cdb-parent-mode "Registers"
  "Major mode for cdb registers."
  (setq header-line-format cdb-registers-header)
  'cdb-invalidate-registers)

(defun cdb-registers-buffer-name ()
  (cdb-current-context-buffer-name
   (concat "registers of " (cdb-get-target-string))))

(defun cdb-display-registers-buffer (&optional thread)
  "Display cdb register contents."
  (interactive)
  (cdb-display-buffer (cdb-get-buffer-create 'cdb-registers-buffer thread)))

(def-cdb-preempt-display-buffer
  cdb-preemptively-display-registers-buffer
  'cdb-registers-buffer nil t)

(defun cdb-frame-registers-buffer (&optional thread)
  "Display cdb register contents in another frame."
  (interactive)
  (display-buffer (cdb-get-buffer-create 'cdb-registers-buffer thread)
		  cdb-display-buffer-other-frame-action))

;; Needs cdb 6.4 onwards (used to fail with no stack).
(defun cdb-get-changed-registers ()
  (when (cdb-get-buffer 'cdb-registers-buffer)
    (cdb-input "-data-list-changed-registers"
               'cdb-changed-registers-handler
               'cdb-get-changed-registers)))

(defun cdb-changed-registers-handler ()
  (setq cdb-changed-registers nil)
  (dolist (register-number
           (bindat-get-field (cdb-json-partial-output) 'changed-registers))
    (push register-number cdb-changed-registers)))

(defun cdb-register-names-handler ()
  ;; Don't use pending triggers because this handler is called
  ;; only once (in cdb-init-1)
  (setq cdb-register-names nil)
  (dolist (register-name
           (bindat-get-field (cdb-json-partial-output) 'register-names))
    (push register-name cdb-register-names))
  (setq cdb-register-names (reverse cdb-register-names)))


(defun cdb-get-source-file-list ()
  "Create list of source files for current cdb session.
If buffers already exist for any of these files, `gud-minor-mode'
is set in them."
  (goto-char (point-min))
  (while (re-search-forward cdb-source-file-regexp nil t)
    (push (read (match-string 1)) cdb-source-file-list))
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (member buffer-file-name cdb-source-file-list)
	(cdb-init-buffer)))))

(defun cdb-get-main-selected-frame ()
  "Trigger for `cdb-frame-handler' which uses main current thread.
Called from `cdb-update'."
  (cdb-input (cdb-current-context-command "-stack-info-frame")
             'cdb-frame-handler
             'cdb-get-main-selected-frame))

(defun cdb-frame-handler ()
  "Set `cdb-selected-frame' and `cdb-selected-file' to show
overlay arrow in source buffer."
  (let ((frame (bindat-get-field (cdb-json-partial-output) 'frame)))
    (when frame
      (setq cdb-selected-frame (bindat-get-field frame 'func))
      (setq cdb-selected-file (bindat-get-field frame 'fullname))
      (setq cdb-frame-number (bindat-get-field frame 'level))
      (setq cdb-frame-address (bindat-get-field frame 'addr))
      (let ((line (bindat-get-field frame 'line)))
        (setq cdb-selected-line (and line (string-to-number line)))
        (when (and cdb-selected-file cdb-selected-line)
          (setq gud-last-frame (cons cdb-selected-file cdb-selected-line))
          (gud-display-frame)))
      (if gud-overlay-arrow-position
          (let ((buffer (marker-buffer gud-overlay-arrow-position))
                (position (marker-position gud-overlay-arrow-position)))
            (when buffer
              (with-current-buffer buffer
                (setq fringe-indicator-alist
                      (if (string-equal cdb-frame-number "0")
                          nil
                        '((overlay-arrow . hollow-right-triangle))))
                (setq gud-overlay-arrow-position (make-marker))
                (set-marker gud-overlay-arrow-position position))))))))

(defconst cdb-prompt-name-regexp
  (concat "value=\\(" cdb--string-regexp "\\)"))

(defun cdb-get-prompt ()
  "Find prompt for cdb session."
  (goto-char (point-min))
  (setq cdb-prompt-name nil)
  (re-search-forward cdb-prompt-name-regexp nil t)
  (setq cdb-prompt-name (read (match-string 1)))
  ;; Insert first prompt.
  (setq cdb-filter-output (concat cdb-filter-output cdb-prompt-name)))

;;;; Window management
(defun cdb-display-buffer (buf)
  "Show buffer BUF, and make that window dedicated."
  (let ((window (display-buffer buf)))
    (set-window-dedicated-p window t)
    window))

  ;; (let ((answer (get-buffer-window buf 0)))
  ;;   (if answer
  ;; 	(display-buffer buf nil 0) ;Deiconify frame if necessary.
  ;;     (let ((window (get-lru-window)))
  ;; 	(if (eq (buffer-local-value 'gud-minor-mode (window-buffer window))
  ;;               'cdbmi)
  ;; 	    (let ((largest (get-largest-window)))
  ;; 	      (setq answer (split-window largest))
  ;; 	      (set-window-buffer answer buf)
  ;; 	      (set-window-dedicated-p answer t)
  ;; 	      answer)
  ;; 	  (set-window-buffer window buf)
  ;; 	  window)))))


(defun cdb-preempt-existing-or-display-buffer (buf &optional split-horizontal)
  "Find window displaying a buffer with the same
`cdb-buffer-type' as BUF and show BUF there.  If no such window
exists, just call `cdb-display-buffer' for BUF.  If the window
found is already dedicated, split window according to
SPLIT-HORIZONTAL and show BUF in the new window."
  (if buf
      (when (not (get-buffer-window buf))
        (let* ((buf-type (cdb-buffer-type buf))
               (existing-window
                (get-window-with-predicate
                 #'(lambda (w)
                     (and (eq buf-type
                              (cdb-buffer-type (window-buffer w)))
                          (not (window-dedicated-p w)))))))
          (if existing-window
              (set-window-buffer existing-window buf)
            (let ((dedicated-window
                   (get-window-with-predicate
                    #'(lambda (w)
                        (eq buf-type
                            (cdb-buffer-type (window-buffer w)))))))
              (if dedicated-window
                  (set-window-buffer
                   (split-window dedicated-window nil split-horizontal) buf)
                (cdb-display-buffer buf))))))
    (error "Null buffer")))

;;; Shared keymap initialization:

(let ((menu (make-sparse-keymap "cdb-Windows")))
  (define-key gud-menu-map [displays]
    `(menu-item "cdb-Windows" ,menu
		:visible (eq gud-minor-mode 'cdbmi)))
  (define-key menu [cdb] '("cdb" . cdb-display-cdb-buffer))
  (define-key menu [threads] '("Threads" . cdb-display-threads-buffer))
  (define-key menu [memory] '("Memory" . cdb-display-memory-buffer))
  (define-key menu [disassembly]
    '("Disassembly" . cdb-display-disassembly-buffer))
  (define-key menu [registers] '("Registers" . cdb-display-registers-buffer))
  (define-key menu [inferior]
    '("IO" . cdb-display-io-buffer))
  (define-key menu [locals] '("Locals" . cdb-display-locals-buffer))
  (define-key menu [frames] '("Stack" . cdb-display-stack-buffer))
  (define-key menu [breakpoints]
    '("Breakpoints" . cdb-display-breakpoints-buffer)))

(let ((menu (make-sparse-keymap "cdb-Frames")))
  (define-key gud-menu-map [frames]
    `(menu-item "cdb-Frames" ,menu
		:visible (eq gud-minor-mode 'cdbmi)))
  (define-key menu [cdb] '("cdb" . cdb-frame-cdb-buffer))
  (define-key menu [threads] '("Threads" . cdb-frame-threads-buffer))
  (define-key menu [memory] '("Memory" . cdb-frame-memory-buffer))
  (define-key menu [disassembly]
    '("Disassembly" . cdb-frame-disassembly-buffer))
  (define-key menu [registers] '("Registers" . cdb-frame-registers-buffer))
  (define-key menu [inferior]
    '("IO" . cdb-frame-io-buffer))
  (define-key menu [locals] '("Locals" . cdb-frame-locals-buffer))
  (define-key menu [frames] '("Stack" . cdb-frame-stack-buffer))
  (define-key menu [breakpoints]
    '("Breakpoints" . cdb-frame-breakpoints-buffer)))

(let ((menu (make-sparse-keymap "cdb-MI")))
  (define-key menu [cdb-customize]
    '(menu-item "Customize" (lambda () (interactive) (customize-group 'cdb))
      :help "Customize cdb Graphical Mode options."))
  (define-key menu [cdb-many-windows]
    '(menu-item "Display Other Windows" cdb-many-windows
      :help "Toggle display of locals, stack and breakpoint information"
      :button (:toggle . cdb-many-windows)))
  (define-key menu [cdb-restore-windows]
    '(menu-item "Restore Window Layout" cdb-restore-windows
      :help "Restore standard layout for debug session."))
  (define-key menu [sep1]
    '(menu-item "--"))
  (define-key menu [all-threads]
    '(menu-item "GUD controls all threads"
      (lambda ()
        (interactive)
        (setq cdb-gud-control-all-threads t))
      :help "GUD start/stop commands apply to all threads"
      :button (:radio . cdb-gud-control-all-threads)))
  (define-key menu [current-thread]
    '(menu-item "GUD controls current thread"
      (lambda ()
        (interactive)
        (setq cdb-gud-control-all-threads nil))
      :help "GUD start/stop commands apply to current thread only"
      :button (:radio . (not cdb-gud-control-all-threads))))
  (define-key menu [sep2]
    '(menu-item "--"))
  (define-key menu [cdb-customize-reasons]
    '(menu-item "Customize switching..."
      (lambda ()
        (interactive)
        (customize-option 'cdb-switch-reasons))))
  (define-key menu [cdb-switch-when-another-stopped]
    (menu-bar-make-toggle cdb-toggle-switch-when-another-stopped
                          cdb-switch-when-another-stopped
                          "Automatically switch to stopped thread"
                          "cdb thread switching %s"
                          "Switch to stopped thread"))
  (define-key gud-menu-map [mi]
    `(menu-item "cdb-MI" ,menu :visible (eq gud-minor-mode 'cdbmi))))

;; TODO Fit these into tool-bar-local-item-from-menu call in gud.el.
;; cdb-MI menu will need to be moved to gud.el. We can't use
;; tool-bar-local-item-from-menu here because it appends new buttons
;; to toolbar from right to left while we want our A/T throttle to
;; show up right before Run button.
(define-key-after gud-tool-bar-map [all-threads]
  '(menu-item "Switch to non-stop/A mode" cdb-control-all-threads
    :image (find-image '((:type xpm :file "gud/thread.xpm")))
    :visible (and (eq gud-minor-mode 'cdbmi)
                  cdb-non-stop
                  (not cdb-gud-control-all-threads)))
  'run)

(define-key-after gud-tool-bar-map [current-thread]
  '(menu-item "Switch to non-stop/T mode" cdb-control-current-thread
    :image (find-image '((:type xpm :file "gud/all.xpm")))
    :visible (and (eq gud-minor-mode 'cdbmi)
                  cdb-non-stop
                  cdb-gud-control-all-threads))
  'all-threads)

(defun cdb-frame-cdb-buffer ()
  "Display GUD buffer in another frame."
  (interactive)
  (display-buffer-other-frame gud-comint-buffer))

(defun cdb-display-cdb-buffer ()
  "Display GUD buffer."
  (interactive)
  (pop-to-buffer gud-comint-buffer nil 0))

(defun cdb-set-window-buffer (name &optional ignore-dedicated window)
  "Set buffer of selected window to NAME and dedicate window.

When IGNORE-DEDICATED is non-nil, buffer is set even if selected
window is dedicated."
  (unless window (setq window (selected-window)))
  (when ignore-dedicated
    (set-window-dedicated-p window nil))
  (set-window-buffer window (get-buffer name))
  (set-window-dedicated-p window t))

(defun cdb-setup-windows ()
  "Layout the window pattern for option `cdb-many-windows'."
  (cdb-get-buffer-create 'cdb-locals-buffer)
  (cdb-get-buffer-create 'cdb-stack-buffer)
  (cdb-get-buffer-create 'cdb-breakpoints-buffer)
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

(define-minor-mode cdb-many-windows
  "If nil just pop up the GUD buffer unless `cdb-show-main' is t.
In this case it starts with two windows: one displaying the GUD
buffer and the other with the source file with the main routine
of the debugged program.  Non-nil means display the layout shown for
`cdb'."
  :global t
  :group 'cdb
  :version "22.1"
  (if (and gud-comint-buffer
           (buffer-name gud-comint-buffer))
      (ignore-errors
        (cdb-restore-windows))))

(defun cdb-restore-windows ()
  "Restore the basic arrangement of windows used by cdb.
This arrangement depends on the value of option `cdb-many-windows'."
  (interactive)
  (switch-to-buffer gud-comint-buffer) ;Select the right window and frame.
  (delete-other-windows)
  (if cdb-many-windows
      (cdb-setup-windows)
    (when (or gud-last-last-frame cdb-show-main)
      (let ((win (split-window)))
        (set-window-buffer
         win
         (if gud-last-last-frame
             (gud-find-file (car gud-last-last-frame))
           (gud-find-file cdb-main-file)))
        (setq cdb-source-window win)))))

;; Called from `gud-sentinel' in gud.el:
(defun cdb-reset ()
  "Exit a debugging session cleanly.
Kills the cdb buffers, and resets variables and the source buffers."
  ;; The cdb-inferior buffer has a pty hooked up to the main cdb
  ;; process.  This pty must be deleted explicitly.
  (let ((pty (get-process "cdb-inferior")))
    (if pty (delete-process pty)))
  ;; Find cdb-mi buffers and kill them.
  (dolist (buffer (buffer-list))
    (unless (eq buffer gud-comint-buffer)
      (with-current-buffer buffer
        (if (eq gud-minor-mode 'cdbmi)
            (if (string-match "\\` ?\\*.+\\*\\'" (buffer-name))
                (kill-buffer nil)
              (cdb-remove-breakpoint-icons (point-min) (point-max) t)
              (setq gud-minor-mode nil)
              (kill-local-variable 'tool-bar-map)
              (kill-local-variable 'cdb-define-alist))))))
  (setq cdb-disassembly-position nil)
  (setq overlay-arrow-variable-list
        (delq 'cdb-disassembly-position overlay-arrow-variable-list))
  (setq fringe-indicator-alist '((overlay-arrow . right-triangle)))
  (setq cdb-stack-position nil)
  (setq overlay-arrow-variable-list
        (delq 'cdb-stack-position overlay-arrow-variable-list))
  (setq cdb-thread-position nil)
  (setq overlay-arrow-variable-list
        (delq 'cdb-thread-position overlay-arrow-variable-list))
  (if (boundp 'speedbar-frame) (speedbar-timer-fn))
  (setq gud-running nil)
  (setq cdb-active-process nil)
  (remove-hook 'after-save-hook 'cdb-create-define-alist t))

(defun cdb-get-source-file ()
  "Find the source file where the program starts and display it with related
buffers, if required."
  (goto-char (point-min))
  (if (re-search-forward cdb-source-file-regexp nil t)
      (setq cdb-main-file (read (match-string 1))))
  (if cdb-many-windows
      (cdb-setup-windows)
    (cdb-get-buffer-create 'cdb-breakpoints-buffer)
    (and cdb-show-main
	 cdb-main-file
	 (display-buffer (gud-find-file cdb-main-file))))
  (cdb-force-mode-line-update
   (propertize "ready" 'face font-lock-variable-name-face)))

;;from put-image
(defun cdb-put-string (putstring pos &optional dprop &rest sprops)
  "Put string PUTSTRING in front of POS in the current buffer.
PUTSTRING is displayed by putting an overlay into the current buffer with a
`before-string' string that has a `display' property whose value is
PUTSTRING."
  (let ((string (make-string 1 ?x))
        (buffer (current-buffer)))
    (setq putstring (copy-sequence putstring))
    (let ((overlay (make-overlay pos pos buffer))
          (prop (or dprop
                    (list (list 'margin 'left-margin) putstring))))
      (put-text-property 0 1 'display prop string)
      (if sprops
          (add-text-properties 0 1 sprops string))
      (overlay-put overlay 'put-break t)
      (overlay-put overlay 'before-string string))))

;;from remove-images
(defun cdb-remove-strings (start end &optional buffer)
  "Remove strings between START and END in BUFFER.
Remove only strings that were put in BUFFER with calls to `cdb-put-string'.
BUFFER nil or omitted means use the current buffer."
  (unless buffer
    (setq buffer (current-buffer)))
  (dolist (overlay (overlays-in start end))
    (when (overlay-get overlay 'put-break)
      (delete-overlay overlay))))

(defun cdb-put-breakpoint-icon (enabled bptno &optional line)
  (let* ((posns (cdb-line-posns (or line (line-number-at-pos))))
         (start (- (car posns) 1))
         (end (+ (cdr posns) 1))
         (putstring (if enabled "B" "b"))
         (source-window (get-buffer-window (current-buffer) 0)))
    (add-text-properties
     0 1 '(help-echo "mouse-1: clear bkpt, mouse-3: enable/disable bkpt")
     putstring)
    (if enabled
        (add-text-properties
         0 1 `(cdb-bptno ,bptno cdb-enabled t) putstring)
      (add-text-properties
       0 1 `(cdb-bptno ,bptno cdb-enabled nil) putstring))
    (cdb-remove-breakpoint-icons start end)
    (if (display-images-p)
        (if (>= (or left-fringe-width
                    (if source-window (car (window-fringes source-window)))
                    cdb-buffer-fringe-width) 8)
            (cdb-put-string
             nil (1+ start)
             `(left-fringe breakpoint
                           ,(if enabled
                                'breakpoint-enabled
                              'breakpoint-disabled))
             'cdb-bptno bptno
             'cdb-enabled enabled)
          (when (< left-margin-width 2)
            (save-current-buffer
              (setq left-margin-width 2)
              (if source-window
                  (set-window-margins
                   source-window
                   left-margin-width right-margin-width))))
          (put-image
           (if enabled
               (or breakpoint-enabled-icon
                   (setq breakpoint-enabled-icon
                         (find-image `((:type xpm :data
                                        ,breakpoint-xpm-data
                                        :ascent 100 :pointer hand)
                                       (:type pbm :data
                                        ,breakpoint-enabled-pbm-data
                                        :ascent 100 :pointer hand)))))
             (or breakpoint-disabled-icon
                 (setq breakpoint-disabled-icon
                       (find-image `((:type xpm :data
                                      ,breakpoint-xpm-data
                                      :conversion disabled
                                      :ascent 100 :pointer hand)
                                     (:type pbm :data
                                      ,breakpoint-disabled-pbm-data
                                      :ascent 100 :pointer hand))))))
           (+ start 1)
           putstring
           'left-margin))
      (when (< left-margin-width 2)
        (save-current-buffer
          (setq left-margin-width 2)
          (let ((window (get-buffer-window (current-buffer) 0)))
            (if window
                (set-window-margins
                 window left-margin-width right-margin-width)))))
      (cdb-put-string
       (propertize putstring
                   'face (if enabled
                             'breakpoint-enabled 'breakpoint-disabled))
       (1+ start)))))

(defun cdb-remove-breakpoint-icons (start end &optional remove-margin)
  (cdb-remove-strings start end)
  (if (display-images-p)
      (remove-images start end))
  (when remove-margin
    (setq left-margin-width 0)
    (let ((window (get-buffer-window (current-buffer) 0)))
      (if window
          (set-window-margins
           window left-margin-width right-margin-width)))))


;;; Functions for inline completion.

(defvar gud-cdb-fetch-lines-in-progress)
(defvar gud-cdb-fetch-lines-string)
(defvar gud-cdb-fetch-lines-break)
(defvar gud-cdb-fetched-lines)

(defun gud-cdbmi-completions (context command)
  "Completion table for cdb/MI commands.
COMMAND is the prefix for which we seek completion.
CONTEXT is the text before COMMAND on the line."
  (let ((gud-cdb-fetch-lines-in-progress t)
	(gud-cdb-fetch-lines-string nil)
	(gud-cdb-fetch-lines-break (length context))
	(gud-cdb-fetched-lines nil)
	;; This filter dumps output lines to `gud-cdb-fetched-lines'.
	(gud-marker-filter #'gud-cdbmi-fetch-lines-filter))
    (with-current-buffer (cdb-get-buffer 'cdb-partial-output-buffer)
      (cdb-input (concat "complete " context command)
		 (lambda () (setq gud-cdb-fetch-lines-in-progress nil)))
      (while gud-cdb-fetch-lines-in-progress
	(accept-process-output (get-buffer-process gud-comint-buffer))))
    (gud-cdb-completions-1 gud-cdb-fetched-lines)))

(defun gud-cdbmi-fetch-lines-filter (string)
  "Custom filter function for `gud-cdbmi-completions'."
  (setq string (concat gud-cdb-fetch-lines-string
		       (gud-cdbmi-marker-filter string)))
  (while (string-match "\n" string)
    (push (substring string gud-cdb-fetch-lines-break (match-beginning 0))
	  gud-cdb-fetched-lines)
    (setq string (substring string (match-end 0))))
  "")

(provide 'cdb-mi)

;;; cdb-mi.el ends here
