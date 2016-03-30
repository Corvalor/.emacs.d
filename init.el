;;; init.el -- The main nitialization

;;; Commentary:
;; This mainly calls the different initialization elisp files to initialize emacs

;;; Code:

;; Prepare necessary paths
(add-to-list 'load-path (expand-file-name "init/" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "etc/plugins/" user-emacs-directory))
(add-to-list 'load-path "~/share/emacs/site-lisp")

;; Benchmark the load process
;;(require 'init-benchmark)

;; Ensure that flycheck gets the required paths

;; Custom default functions
(require 'init-default-functions)

;; Default settings
(require 'init-default-settings)

;; Packages
(require 'init-packages)
(require 'init-custom-packages)

;; modeline
(require 'init-mode-line)

;; auto insertion
(require 'init-auto-insert)

;; msbuild mode
(require 'msbuild-mode)

;; custom stuff, optional
(require 'init-custom nil t)

(provide 'init)
;;; init.el ends here
