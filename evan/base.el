

(defun openMyInitConfigFile()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(global-set-key (kbd "<f1>") 'openMyInitConfigFile)

(defun openMySpacemacsConfigFile()
  (interactive)
  (find-file "~/.spacemacs.d/init.el"))

(global-set-key (kbd "<f2>") 'openMySpacemacsConfigFile)

(require 'reveal-in-osx-finder)
(global-set-key (kbd "C-c z") 'reveal-in-osx-finder)

;; Linum mode 行数多了会卡
(global-nlinum-mode)

(setq backup-inhibited t
      make-backup-files nil
      auto-save-list-file-prefix nil
      auto-save-default nil
      create-lockfiles nil)

(autopair-global-mode t)

(setq initial-frame-alist (quote ((fullscreen . maximized))))

;; all-the-icons for https://github.com/domtronn/all-the-icons.el
(use-package neotree
  :init
  (setq neo-theme 'icons
        neo-smart-open t
        neo-window-position 'left
        neo-autorefresh 1
        neo-mode-line-type 'none
        neo-hidden-regexp-list '("^\\." "\\.pyc$" "~$" "^#.*#$" "\\.elc$" "node_modules")))

(setq projectile-switch-project-action 'neotree-projectile-action)

;; 更改 quick look 键
(with-eval-after-load 'neotree
  (define-key neotree-mode-map (kbd "f") 'neotree-quick-look))

;; (define-key global-map (kbd "C-:") 'avy-goto-char)
;; (define-key global-map (kbd "C-'") 'avy-goto-char2)

;; (nyan-mode t)

(require 'multi-term)
(setq multi-term-program "/bin/zsh")

(defun iterm-goto-probject-local ()
  "Go to present working dir and focus iterm"
  (interactive)
  (do-applescript
   (concat
    " tell application \"iTerm2\"\n"
    "   tell the current session of current window\n"
    (format "     write text \"cd %s\" \n"
            ;; string escaping madness for applescript
            (replace-regexp-in-string "\\\\" "\\\\\\\\"
                                      (shell-quote-argument (or (projectile-project-root default-directory)))))
    "   end tell\n"
    " end tell\n"
    " do shell script \"open -a iTerm\"\n"
    ))
  )

(global-set-key (kbd "C-, i") 'iterm-goto-probject-local)

(defun iterm-focus ()
  (interactive)
  (do-applescript
   " do shell script \"open -a iTerm\"\n"
   ))

(global-set-key (kbd "C-, f") 'iterm-focus)

(use-package company-tabnine :ensure t)
(require 'company-tabnine)
(use-package company
  :init
  (setq company-idle-delay 0
        company-echo-delay 0
        company-show-numbers t
        company-minimum-prefix-length 2
        company-dabbrev-downcase nil
        company-selection-wrap-around t
        ;;company-global-modes '(not org-mode)
        )
  :config
  (global-company-mode)
  (add-to-list 'company-backends #'company-tabnine)
  :bind
  )

(defun company//sort-by-tabnine (candidates)
  (if (or (functionp company-backend)
          (not (and (listp company-backend) (memq 'company-tabnine company-backend))))
      candidates
    (let ((candidates-table (make-hash-table :test #'equal))
          candidates-1
          candidates-2)
      (dolist (candidate candidates)
        (if (eq (get-text-property 0 'company-backend candidate)
                'company-tabnine)
            (unless (gethash candidate candidates-table)
              (push candidate candidates-2))
          (push candidate candidates-1)
          (puthash candidate t candidates-table)))
      (setq candidates-1 (nreverse candidates-1))
      (setq candidates-2 (nreverse candidates-2))
      (nconc (seq-take candidates-1 2)
             (seq-take candidates-2 2)
             (seq-drop candidates-1 2)
             (seq-drop candidates-2 2)))))

(add-to-list 'company-transformers 'company//sort-by-tabnine t)

;; The free version of TabNine is good enough,
;; and below code is recommended that TabNine not always
;; prompt me to purchase a paid version in a large project.
(defadvice company-echo-show (around disable-tabnine-upgrade-message activate)
  (let ((company-message-func (ad-get-arg 0)))
    (when (and company-message-func
               (stringp (funcall company-message-func)))
      (unless (string-match "The free version of TabNine only indexes up to" (funcall company-message-func))
        ad-do-it))))

(company-tng-configure-default)
(setq company-frontends
      '(company-tng-frontend
        company-pseudo-tooltip-frontend
        company-echo-metadata-frontend))
