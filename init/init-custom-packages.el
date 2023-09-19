;; init-custom-packages.el -- initializaion of the packages

;;; Commentary:
;; The packages I am using plus initialization

;;; Code:

;(if (string-prefix-p "i3" (shell-command-to-string "wmctrl -m | sed -n -e 's/^.*Name: //p'"))
    ;(require 'init-i3-packages))

(defadvice windmove-do-window-select (around stumpwm-do-window-select-wrapper activate)
  "Let windmove do its own thing, if there is an error, try stumpwm in that direction."
  (condition-case err
      ad-do-it
    (error
     ( let ((dir (cond ((eq (ad-get-arg 0) 'left) "1")
                       ((eq (ad-get-arg 0) 'up) "0")
                       ((eq (ad-get-arg 0) 'down) "0")
                       ((eq (ad-get-arg 0) 'right) "-1"))))
         (shell-command (concat "echo 'require(\"awful\").screen.focus_relative(" dir ")' | awesome-client" ))))))

(provide 'init-custom-packages)
;;; init-custom-packages.el ends here
