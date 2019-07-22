;; Disable backup, auto-save and lockfiles
(setq backup-inhibited t
      make-backup-files nil
      auto-save-list-file-prefix nil
      auto-save-default nil
      create-lockfiles nil)

;; 显示行号
;; Linum mode 行数多了会卡
(global-nlinum-mode)

(use-package company-tabnine :ensure t)

(require 'company-tabnine)

;; 补全
(use-package company
  :init
  (setq company-idle-delay 0.1
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

(company-tng-configure-default)
(setq company-frontends
      '(company-tng-frontend
        company-pseudo-tooltip-frontend
        company-echo-metadata-frontend))

;; autopair 匹配括号引号 to enable in all buffers
(autopair-global-mode t)

;; 全屏
(setq initial-frame-alist (quote ((fullscreen . maximized))))

;; neotree config
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

;; ace-jump-mode
;; (autoload
;;   'ace-jump-mode
;;   "ace-jump-mode" t)
;; (eval-after-load "ace-jump-mode"
;;   '(ace-jump-mode-enable-mark-sync))
;; ;; 跳入
;; (define-key global-map (kbd "C-c f") 'ace-jump-mode)
;; ;; 跳回
;; (define-key global-map (kbd "C-c b") 'ace-jump-mode-pop-mark)

(define-key global-map (kbd "C-:") 'avy-goto-char)

(define-key global-map (kbd "C-'") 'avy-goto-char2)

;; 低配版彩虹猫
;; (nyan-mode t)

;; 定义F1 来打开配置文件
(defun openMyInitConfigFile()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(global-set-key (kbd "<f1>") 'openMyInitConfigFile)

;; 定义F2 打开spacemacs配置文件
(defun openMySpacemacsConfigFile()
  (interactive)
  (find-file "~/.emacs.d/.spacemacs.d/init.el"))

(global-set-key (kbd "<f2>") 'openMySpacemacsConfigFile)

;; multi-term
(require 'multi-term)
(setq multi-term-program "/bin/zsh")

;; Themes
(defun setDoomThemes()
  (interactive)
  (require 'doom-themes)
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled

  ;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each theme
  ;; may have their own settings.
  ;; vibrant
  (load-theme 'doom-vibrant t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (doom-themes-treemacs-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)
  )

;; (setDoomThemes)

(load-theme 'gruvbox t)

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

(provide 'main-init)
