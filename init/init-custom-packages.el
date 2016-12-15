;; init-custom-packages.el -- initializaion of the packages

;;; Commentary:
;; The packages I am using plus initialization

;;; Code:

;(if (string-prefix-p "i3" (shell-command-to-string "wmctrl -m | sed -n -e 's/^.*Name: //p'"))
    ;(require 'init-i3-packages))

;; (defadvice windmove-do-window-select (around stumpwm-do-window-select-wrapper activate)
;;   "Let windmove do its own thing, if there is an error, try stumpwm in that direction."
;;   (condition-case err
;;       ad-do-it
;;     (error
;;      ( let ((dir (cond ((eq (ad-get-arg 0) 'left) "left")
;;                        ((eq (ad-get-arg 0) 'up) "up")
;;                        ((eq (ad-get-arg 0) 'down) "down")
;;                        ((eq (ad-get-arg 0) 'right) "right"))))
;;          (shell-command (concat "stumpish 'eval (stumpwm::run-commands \"move-focus " dir "\")'" ))))))

(provide 'init-custom-packages)
;;; init-custom-packages.el ends here
