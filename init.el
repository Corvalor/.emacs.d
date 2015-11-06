;;; init.el -- The main nitialization

;;; Commentary:
;; This mainly calls the different initialization elisp files to initialize emacs

;;; Code:

;; Prepare necessary paths
(add-to-list 'load-path (expand-file-name "init/" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "etc/plugins/" user-emacs-directory))

;; Benchmark the load process
;;(require 'init-benchmark)

;; Ensure that flycheck gets the required paths

;; Custom default functions
(require 'init-default-functions)

;; Default settings
(require 'init-default-settings)

;; Packages
(require 'init-packages)

;; modeline
(require 'init-mode-line)

;; msbuild mode
(require 'msbuild-mode)

(provide 'init)
;;; init.el ends here
