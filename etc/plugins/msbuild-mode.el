(defcustom msbuild-mode-configuration "Debug" "")
(defcustom msbuild-mode-platform "Win32" "")

(defcustom msbuild-mode-mode-line
'(:eval (format " Projectile[%s]" "Test"))
 ""
  :type 'sexp)

(define-minor-mode msbuild-mode
  "Handles the compilation via msbuild"
  :lighter (:eval (format " %s|%s" msbuild-mode-configuration msbuild-mode-platform))
  )

(provide 'msbuild-mode)