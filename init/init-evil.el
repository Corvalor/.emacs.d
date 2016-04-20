;; init-evil.el -- initializaion of the custom evil functions

;;; Commentary:
;; The custom evil functions

;;; Code:

(evil-define-operator evil-delete-custom (beg end type register yank-handler)
"Delete text from BEG to END with TYPE.
Save in Register or in the kill-ring with YANK-Handler.
Uses black hole Register if non specified."
(interactive "<r><x><y>")
(unless register
(setq register ?_))
(evil-delete beg end type register yank-handler))

(evil-define-operator evil-delete-line-custom (beg end type register yank-handler)
"Delete text from BEG to END with TYPE.
Save in Register or in the kill-ring with YANK-Handler.
Uses black hole Register if non specified."
(interactive "<r><x><y>")
(unless register
(setq register ?_))
(evil-delete-line beg end type register yank-handler))

(evil-define-operator evil-change-custom (beg end &optional type register yank-handler delete-func)
"Delete text from BEG to END with TYPE.
Save in Register or in the kill-ring with YANK-Handler.
Uses black hole Register if non specified."
(interactive "<r><x><y>")
(unless register
(setq register ?_))
(evil-change beg end type register yank-handler delete-func))

(evil-define-operator evil-change-line-custom (beg end &optional type register yank-handler)
"Delete text from BEG to END with TYPE.
Save in Register or in the kill-ring with YANK-Handler.
Uses black hole Register if non specified."
(interactive "<r><x><y>")
(unless register
(setq register ?_))
(evil-change-line beg end type register yank-handler))
	
(evil-define-operator evil-paste-after-custom (count &optional register yank-handler)
"Pasted the latest yanked text behind point.
The return value is the yanked text.
Uses first Register if non specified.
Prevents yanking."
(interactive "P<x>")
(unless register
(setq register ?0))
(evil-paste-after count register yank-handler))

(provide 'init-evil)
;;; init-evil.el ends here
