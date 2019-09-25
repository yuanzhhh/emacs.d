;;
(use-package git-timemachine)

;; https://melpa.org/#/git-messenger
;; Pop up last commit information of current line
(require 'git-messenger)
(global-set-key (kbd "C-, g m") 'git-messenger:popup-message)

(provide 'git-init)
