(setq global-default-tab-width 2)
(defvar my/indentation-size 2)
(setq standard-indent my/indentation-size)
(setq-default indent-tabs-mode nil
              tab-width my/indentation-size)

(defun openMyNoteFile()
  (interactive)
  (find-file "~/note/work.org"))

(global-set-key (kbd "<f1>") 'openMyNoteFile)

(defun openMySpacemacsConfigFile()
  (interactive)
  (find-file "~/.spacemacs.d/evan/base.org"))

(global-set-key (kbd "<f2>") 'openMySpacemacsConfigFile)

(defun openMyInitConfigFile()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(global-set-key (kbd "<f3>") 'openMyInitConfigFile)

(require 'all-the-icons)

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
  (setq
        neo-theme 'arrow
        neo-smart-open t
        neo-window-position 'left
        neo-autorefresh 1
        neo-mode-line-type 'none
        neo-hidden-regexp-list '("^\\." "\\.pyc$" "~$" "^#.*#$" "\\.elc$" "node_modules"))
  :bind
    ("<tab>" . neotree-quick-look))

(setq projectile-switch-project-action 'neotree-projectile-action)
;; 更改 quick look 键
;; (with-eval-after-load 'neotree
;;   (define-key neotree-mode-map (kbd "<tab>") 'neotree-quick-look))

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
    " tell application \"iTerm\"\n"
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

(require 'smartparens-config)
(add-hook 'web-mode-hook #'smartparens-mode)

;; 退出vim输入模式
(global-set-key (kbd "C-c C-g") 'evil-escape)

;; (require 'doom-modeline)
;; (doom-modeline-mode 1)

(require 'deft)

(setq deft-extension "org")
(setq deft-text-mode 'org-mode)
(setq deft-directory "~/note")
;; 允许子目录递归索引
(setq deft-recursive t)
(setq deft-use-filename-as-title t)
(defun ironman-deft-search-for(filter)
  (interactive "MFilter: ")
  (deft)
  (deft-filter filter t)
)
(global-set-key [f4] 'deft)
(global-set-key [f5] 'ironman-deft-search-for)



(use-package git-timemachine
  :custom-face
  (git-timemachine-minibuffer-author-face ((t (:inherit success))))
  (git-timemachine-minibuffer-detail-face ((t (:inherit warning))))
  :bind (:map vc-prefix-map
        ("t" . git-timemachine)))

(use-package git-messenger
  :bind (:map vc-prefix-map
        ("p" . git-messenger:popup-message)
        :map git-messenger-map
        ("o" . git-messenger:copy-message))
  :init (setq git-messenger:show-detail t
              git-messenger:use-magit-popup t)
  :config
  (with-no-warnings
    (with-eval-after-load 'hydra
      (defhydra git-messenger-hydra (:color blue)
        ("s" git-messenger:popup-show "show")
        ("c" git-messenger:copy-commit-id "copy hash")
        ("m" git-messenger:copy-message "copy message")
        ("," (catch 'git-messenger-loop (git-messenger:show-parent)) "go parent")
        ("q" git-messenger:popup-close "quit")))

    (defun my-git-messenger:format-detail (vcs commit-id author message)
      (if (eq vcs 'git)
          (let ((date (git-messenger:commit-date commit-id))
                (colon (propertize ":" 'face 'font-lock-comment-face)))
            (concat
            (format "%s%s %s \n%s%s %s\n%s  %s %s \n"
                    (propertize "Commit" 'face 'font-lock-keyword-face) colon
                    (propertize (substring commit-id 0 8) 'face 'font-lock-comment-face)
                    (propertize "Author" 'face 'font-lock-keyword-face) colon
                    (propertize author 'face 'font-lock-string-face)
                    (propertize "Date" 'face 'font-lock-keyword-face) colon
                    (propertize date 'face 'font-lock-string-face))
            (propertize (make-string 38 ?─) 'face 'font-lock-comment-face)
            message
            (propertize "\nPress q to quit" 'face '(:inherit (font-lock-comment-face italic)))))
        (git-messenger:format-detail vcs commit-id author message)))

    (defun my-git-messenger:popup-message ()
      "Popup message with `posframe', `pos-tip', `lv' or `message', and dispatch actions with `hydra'."
      (interactive)
      (let* ((vcs (git-messenger:find-vcs))
            (file (buffer-file-name (buffer-base-buffer)))
            (line (line-number-at-pos))
            (commit-info (git-messenger:commit-info-at-line vcs file line))
            (commit-id (car commit-info))
            (author (cdr commit-info))
            (msg (git-messenger:commit-message vcs commit-id))
            (popuped-message (if (git-messenger:show-detail-p commit-id)
                                  (my-git-messenger:format-detail vcs commit-id author msg)
                                (cl-case vcs
                                  (git msg)
                                  (svn (if (string= commit-id "-")
                                          msg
                                        (git-messenger:svn-message msg)))
                                  (hg msg)))))
        (setq git-messenger:vcs vcs
              git-messenger:last-message msg
              git-messenger:last-commit-id commit-id)
        (run-hook-with-args 'git-messenger:before-popup-hook popuped-message)
        (git-messenger-hydra/body)
        (cond ((and (fboundp 'posframe-workable-p) (posframe-workable-p))
              (let ((buffer-name "*git-messenger*"))
                (posframe-show buffer-name
                                :string popuped-message
                                :left-fringe 8
                                :right-fringe 8
                                :internal-border-color (face-foreground 'default)
                                :internal-border-width 1)
                (unwind-protect
                    (push (read-event) unread-command-events)
                  (posframe-delete buffer-name))))
              ((and (fboundp 'pos-tip-show) (display-graphic-p))
              (pos-tip-show popuped-message))
              ((fboundp 'lv-message)
              (lv-message popuped-message)
              (unwind-protect
                  (push (read-event) unread-command-events)
                (lv-delete-window)))
              (t (message "%s" popuped-message)))
        (run-hook-with-args 'git-messenger:after-popup-hook popuped-message)))
    (advice-add #'git-messenger:popup-close :override #'ignore)
    (advice-add #'git-messenger:popup-message :override #'my-git-messenger:popup-message)))

(use-package flycheck
    :ensure t
    :init
    (global-flycheck-mode t)
    :custom
    (flycheck-check-syntax-automatically '(mode-enabled save))
    :config
    (flycheck-add-mode 'javascript-eslint 'web-mode))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode)
  (prettier-js-mode)
  (tide-hl-identifier-mode)
  (add-to-list 'company-backends '(company-tide :with company-tabnine :separate)))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; 当tide被加载后
(with-eval-after-load 'tide
  ;; evil模式下绑定 g d 跳转定义
  (evil-define-key '(normal) tide-mode-map (kbd "g d") 'tide-jump-to-definition))

(setq tide-format-options '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t :placeOpenBraceOnNewLineForFunctions nil))

(use-package tide :ensure t)

(use-package web-mode
  :mode
  ("\\.html\\'" "\\.tsx\\'" "\\.vue\\'" "\\.svelte\\'" "\\.twig\\'")
  :init
  (add-to-list 'magic-mode-alist '("import.*react" . web-mode))
  :custom
  (web-mode-block-padding my/indentation-size)
  (web-mode-style-padding my/indentation-size)
  (web-mode-script-padding my/indentation-size)
  (web-mode-attr-indent-offset my/indentation-size)
  (web-mode-attr-value-indent-offset my/indentation-size)
  (web-mode-enable-current-element-highlight t)
  (web-mode-content-types-alist '(("jsx" . "\\.[jt]?s[x]?\\'")
                                  ("vue" . "\\.vue\\'")))
  :config
  (set-face-background 'web-mode-current-element-highlight-face "#AF3A03")
  :hook
  (web-mode . emmet-mode))

  (add-hook 'web-mode-hook
      (lambda ()
        ;; `:separate`  使得不同 backend 分开排序

        (setup-tide-mode)
        (setq web-mode-code-indent-offset 2)
        (setq-local web-mode-enable-auto-quoting nil)))

  (add-hook 'web-mode-hook
            (lambda ()
              (pcase web-mode-content-type
                ("jsx" (progn
                        (setq-local emmet-expand-jsx-className? t)
                        (setq-local web-mode-enable-auto-quoting nil)
                        (setup-tide-mode)))
                ("vue" (setup-tide-mode))
                ("html" (prettier-js-mode)))))

;; (add-hook 'web-mode-hook
;;   (lambda ()
;;     (when (string-equal "tsx" (file-name-extension buffer-file-name))
;;       (setq-local emmet-expand-jsx-className? t)
;;       (flycheck-add-mode 'typescript-tslint 'web-mode)
;;       (flycheck-add-next-checker 'javascript-eslint 'jsx-tide 'append))

;;     (when (string-equal "ts" (file-name-extension buffer-file-name))
;;       (flycheck-add-mode 'typescript-tslint 'web-mode)
;;       (flycheck-add-next-checker 'javascript-eslint))

;;     (setup-tide-mode)
;;     ))

(use-package typescript-mode
  :custom
  (typescript-indent-level my/indentation-size)
  :hook
  (typescript-mode . setup-tide-mode))

(flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append)
(add-hook 'web-mode-hook
  (lambda ()
    (when (equal web-mode-content-type "jsx")
      (setq-local emmet-expand-jsx-className? t)
      (flycheck-add-mode 'javascript-eslint 'web-mode)
      (flycheck-add-next-checker 'javascript-eslint 'jsx-tide 'append)
      (setup-tide-mode))
    ))

(use-package js2-mode
  :mode "\\.js\\'"
  :custom
  (js2-basic-offset my/indentation-size)
  (js2-highlight-level 3)
  (js2-mode-show-parse-errors nil)
  (js2-mode-show-strict-warnings nil)
  :hook
  (js2-mode . setup-tide-mode))

(require 'vue-mode)
(require 'lsp-mode)

(defun vuejs-custom ()
  (lsp)
  (flycheck-mode t)
  (company-mode))

(add-hook 'vue-mode-hook 'vuejs-custom)

;; (require 'doom-themes)

;; (defgroup doom-dracula-alt-theme nil
;;   "Options for doom-themes"
;;   :group 'doom-themes)

;; (defcustom doom-dracula-alt-brighter-modeline nil
;;   "If non-nil, more vivid colors will be used to style the mode-line."
;;   :group 'doom-dracula-alt-theme
;;   :type 'boolean)

;; (defcustom doom-dracula-alt-brighter-comments nil
;;   "If non-nil, comments will be highlighted in more vivid colors."
;;   :group 'doom-dracula-alt-theme
;;   :type 'boolean)

;; (defcustom doom-dracula-alt-colorful-headers nil
;;   "If non-nil, headers in org-mode will be more colorful; which is truer to the
;; original Dracula Emacs theme."
;;   :group 'doom-dracula-alt-theme
;;   :type 'boolean)

;; (defcustom doom-dracula-alt-comment-bg doom-dracula-alt-brighter-comments
;;   "If non-nil, comments will have a subtle, darker background. Enhancing their
;; legibility."
;;   :group 'doom-dracula-alt-theme
;;   :type 'boolean)

;; (defcustom doom-dracula-alt-padded-modeline doom-themes-padded-modeline
;;   "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
;; determine the exact padding."
;;   :group 'doom-dracula-alt-theme
;;   :type '(or integer boolean))

;; (def-doom-theme doom-dracula-alt
;;     "A dark theme inspired by Atom One Dark"

;;     ;; name        default   256       16
;;     ((bg         '("#282a36" nil       nil            ))
;;     (bg-alt     '("#1E2029" nil       nil            ))
;;     (base0      '("#1E2029" "#1E2029"   "black"        ))
;;     (base1      '("#282a36" "#282a36" "brightblack"  ))
;;     (base2      '("#373844" "#373844" "brightblack"  ))
;;     (base3      '("#44475a" "#44475a" "brightblack"  ))
;;     (base4      '("#565761" "#565761" "brightblack"  ))
;;     (base5      '("#6272a4" "#6272a4" "brightblack"  ))
;;     (base6      '("#b6b6b2" "#b6b6b2" "brightblack"  ))
;;     (base7      '("#ccccc7" "#ccccc7" "brightblack"  ))
;;     (base8      '("#f8f8f2" "#f8f8f2" "white"        ))
;;     (fg         '("#f8f8f2" "#f8f8f2" "white"        ))
;;     (fg-alt     '("#e2e2dc" "#e2e2dc" "brightwhite"  ))

;;     (grey       base4)
;;     (red        '("#ff5555" "#ff6655" "red"          ))
;;     (orange     '("#ffb86c" "#ffb86c" "brightred"    ))
;;     (green      '("#50fa7b" "#50fa7b" "green"        ))
;;     (teal       '("#0189cc" "#0189cc" "brightgreen"  ))
;;     (yellow     '("#f1fa8c" "#f1fa8c" "yellow"       ))
;;     (blue       '("#61bfff" "#61bfff" "brightblue"   ))
;;     (dark-blue  '("#0189cc" "#0189cc" "blue"         ))
;;     (magenta    '("#ff79c6" "#ff79c6" "magenta"      ))
;;     (violet     '("#bd93f9" "#bd93f9" "brightmagenta"))
;;     (cyan       '("#8be9fd" "#8be9fd" "brightcyan"   ))
;;     (dark-cyan  '("#8be9fd" "#8be9fd" "cyan"         ))

;;     ;; face categories -- required for all themes
;;     (highlight      violet)
;;     (vertical-bar   (doom-darken base1 0.1))
;;     (selection      dark-blue)
;;     (builtin        orange)
;;     (comments       (if doom-dracula-alt-brighter-comments dark-cyan base5))
;;     (doc-comments   (doom-lighten (if doom-dracula-alt-brighter-comments dark-cyan base5) 0.25))
;;     (constants      cyan)
;;     (functions      green)
;;     (keywords       magenta)
;;     (methods        teal)
;;     (operators      violet)
;;     (type           blue)
;;     (strings        yellow)
;;     (variables      base8)
;;     (numbers        red)
;;     (region         base3)
;;     (error          red)
;;     (warning        yellow)
;;     (success        green)
;;     (vc-modified    orange)
;;     (vc-added       green)
;;     (vc-deleted     red)

;;     ;; custom categories
;;     (level1 magenta)
;;     (level2 violet)
;;     (level3 (if doom-dracula-alt-colorful-headers green   (doom-lighten violet 0.35)))
;;     (level4 (if doom-dracula-alt-colorful-headers yellow  (doom-lighten magenta 0.35)))
;;     (level5 (if doom-dracula-alt-colorful-headers cyan    (doom-lighten violet 0.6)))
;;     (level6 (if doom-dracula-alt-colorful-headers orange  (doom-lighten magenta 0.6)))
;;     (level7 (if doom-dracula-alt-colorful-headers blue    (doom-lighten violet 0.85)))
;;     (level8 (if doom-dracula-alt-colorful-headers magenta (doom-lighten magenta 0.85)))
;;     (level9 (if doom-dracula-alt-colorful-headers violet  (doom-lighten violet 0.95)))

;;     (hidden     base1)
;;     (-modeline-bright doom-dracula-alt-brighter-modeline)
;;     (-modeline-pad
;;       (when doom-dracula-alt-padded-modeline
;;         (if (integerp doom-dracula-alt-padded-modeline) doom-dracula-alt-padded-modeline 4)))

;;     (modeline-fg     nil)
;;     (modeline-fg-alt base5)

;;     (modeline-bg

;;       (if -modeline-bright
;;           (doom-darken  magenta 0.675)
;;         (doom-darken bg 0.1))
;;       )
;;     (modeline-bg-l
;;       (if -modeline-bright
;;           (doom-darken magenta 0.6)
;;         `(,(doom-darken (car bg) 0.075) ,@(cdr base1))
;;         ))
;;     (modeline-bg-inactive   (doom-darken bg 0.1))
;;     (modeline-bg-inactive-l `(,(doom-darken (car bg) 0.075) ,@(cdr base1))))


;;     ;; --- extra faces ------------------------
;;     ((elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")

;;     ;; ((line-number &override) :foreground base4)
;;     ;; ((line-number-current-line &override) :foreground fg)
;;     ((line-number &override) :foreground base5 :distant-foreground nil)
;;     ((line-number-current-line &override) :foreground base7 :distant-foreground nil)

;;     (font-lock-comment-face
;;       :foreground comments
;;       :background (if doom-dracula-alt-comment-bg (doom-lighten bg 0.05)))
;;     (font-lock-doc-face
;;       :inherit 'font-lock-comment-face
;;       :foreground doc-comments)
;;     (solaire-hl-line-face :background base2)
;;     (doom-modeline-bar :background (if -modeline-bright modeline-bg highlight))
;;     (mode-line
;;       :background modeline-bg :foreground modeline-fg
;;       :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
;;     (mode-line-inactive
;;       :background modeline-bg-inactive :foreground modeline-fg-alt
;;       :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
;;     (mode-line-emphasis
;;       :foreground (if -modeline-bright base8 highlight))

;;     (solaire-mode-line-face
;;       :inherit 'mode-line
;;       :background modeline-bg-l
;;       :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
;;     (solaire-mode-line-inactive-face
;;       :inherit 'mode-line-inactive
;;       :background modeline-bg-inactive-l
;;       :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))

;;     ;; --- major-mode faces -------------------
;;     ;; css-mode / scss-mode
;;     (css-proprietary-property :foreground orange)
;;     (css-property             :foreground green)
;;     (css-selector             :foreground blue)

;;     ;; markdown-mode
;;     (markdown-markup-face :foreground base5)
;;     (markdown-header-face :inherit 'bold :foreground red)
;;     (markdown-code-face :background (doom-lighten base3 0.05))

;;     ;; org-mode
;;     (org-level-1 :background nil :foreground level1 :height 1.2 :weight 'bold)
;;     (org-level-2 :foreground level2 :weight 'bold)
;;     (org-level-3 :inherit 'org-level-2 :foreground level3)
;;     (org-level-4 :inherit 'org-level-2 :foreground level4)
;;     (org-level-5 :inherit 'org-level-2 :foreground level5)
;;     (org-level-6 :inherit 'org-level-2 :foreground level6)
;;     (org-level-7 :inherit 'org-level-2 :foreground level7)
;;     (org-todo :foreground orange :bold 'inherit :background (doom-darken base1 0.02))
;;     (org-done :foreground green :strike-through nil :background base2 :bold t)
;;     (org-headline-done :foreground base4 :strike-through nil)
;;     ((org-tag &override) :foreground (doom-lighten orange 0.3))
;;     (org-agenda-date :foreground cyan)
;;     (org-agenda-dimmed-todo-face :foreground comments)
;;     (org-agenda-done :foreground base4)
;;     (org-agenda-structure :foreground violet)
;;     (org-block            :background nil :foreground violet)
;;     (org-block-begin-line :background nil :foreground comments)
;;     (org-code :foreground yellow)
;;     (org-column :background base1)
;;     (org-column-title :background base1 :bold t :underline t)
;;     (org-date :foreground cyan)
;;     (org-document-info :foreground blue)
;;     (org-document-info-keyword :foreground comments)
;;     (org-ellipsis :foreground comments)
;;     (org-footnote :foreground blue)
;;     (org-headline-base :foreground comments :strike-through t :bold nil)
;;     (org-link :foreground orange :underline t :weight 'bold)
;;     (org-priority :foreground cyan)
;;     (org-scheduled :foreground green)
;;     (org-scheduled-previously :foreground yellow)
;;     (org-scheduled-today :foreground orange)
;;     (org-sexp-date :foreground base4)
;;     (org-special-keyword :foreground yellow)
;;     (org-table :foreground violet)
;;     (org-upcoming-deadline :foreground yellow)
;;     (org-warning :foreground magenta)
;;     )
;;   )

(require 'doom-themes)

(defgroup doom-solarized-light-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom doom-solarized-light-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-solarized-light-theme
  :type 'boolean)

(defcustom doom-solarized-light-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-solarized-light-theme
  :type 'boolean)

(defcustom doom-solarized-light-comment-bg doom-solarized-light-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'doom-solarized-light-theme
  :type 'boolean)

(defcustom doom-solarized-light-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-solarized-light-theme
  :type '(choice integer boolean))

;;
(def-doom-theme doom-solarized-light
    "A light theme inspired by Solarized light"

    ;; name        default   256       16
    ((bg         '("#FDF6E3" nil       nil            ))
    (bg-alt     '("#FFFBEA" nil       nil            ))
    (base0      '("#FFFBF0" "black"   "black"        ))
    (base1      '("#FCF8ED" "#1e1e1e" "brightblack"  ))
    (base2      '("#FCF7E8" "#2e2e2e" "brightblack"  ))
    (base3      '("#F2E6CE" "#262626" "brightblack"  ))
    (base4      '("#E1DBCD" "#3f3f3f" "brightblack"  ))
    (base5      '("#D6D6D6" "#525252" "brightblack"  ))
    (base6      '("#96A7A9" "#6b6b6b" "brightblack"  ))
    (base7      '("#788484" "#979797" "brightblack"  ))
    (base8      '("#626C6C" "#dfdfdf" "white"        ))
    (fg         '("#556b72" "#2d2d2d" "white"        ))
    (fg-alt     '("#7B8787" "#bfbfbf" "brightwhite"  ))

    (grey       base4)
    (red        '("#dc322f" "#ff6655" "red"          ))
    (orange     '("#cb4b16" "#dd8844" "brightred"    ))
    (green      '("#859900" "#99bb66" "green"        ))
    (teal       '("#35a69c" "#33aa99" "brightgreen"  ))
    (yellow     '("#b58900" "#ECBE7B" "yellow"       ))
    (blue       '("#268bd2" "#51afef" "brightblue"   ))
    (dark-blue  '("#3F88AD" "#2257A0" "blue"         ))
    (magenta    '("#d33682" "#c678dd" "magenta"      ))
    (violet     '("#6c71c4" "#a9a1e1" "brightmagenta"))
    (cyan       '("#2aa198" "#46D9FF" "brightcyan"   ))
    (dark-cyan  '("#204052" "#5699AF" "cyan"         ))

    ;; face categories -- required for all themes
    (highlight      blue)
    (vertical-bar   base4)
    (selection      dark-blue)
    (builtin        magenta)
    (comments       (if doom-solarized-light-brighter-comments
                        (doom-lighten teal 0.25)
                      base6))
    (doc-comments   teal)
    (constants      violet)
    (functions      magenta)
    (keywords       green)
    (methods        cyan)
    (operators      blue)
    (type           yellow)
    (strings        cyan)
    (variables      blue)
    (numbers        violet)
    (region         `(,(doom-darken (car bg-alt) 0.1) ,@(doom-darken (cdr base0) 0.1)))
    (error          red)
    (warning        yellow)
    (success        green)
    (vc-modified    orange)
    (vc-added       green)
    (vc-deleted     red)

    ;; custom categories
    (hidden     `(,(car bg) "black" "black"))
    (-modeline-bright doom-solarized-light-brighter-modeline)
    (-modeline-pad
      (when doom-solarized-light-padded-modeline
        (if (integerp doom-solarized-light-padded-modeline) doom-solarized-light-padded-modeline 4)))

    (modeline-fg     nil)
    (modeline-fg-alt base6)

    (modeline-bg
      (if -modeline-bright
          (doom-lighten bg 0.7)
        (doom-lighten base3 0.2)))
    (modeline-bg-l
      (if -modeline-bright
          (doom-lighten bg 0.7)
        (doom-darken bg 0.05)))
    (modeline-bg-inactive   (doom-darken bg 0.02))
    (modeline-bg-inactive-l (doom-darken bg 0.025)))


    ;; --- extra faces ------------------------
    ((elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")

    (hl-line :background base3)

    ((line-number &override) :foreground base6)
    ((line-number-current-line &override) :foreground fg :background region :weight 'bold)

    (org-block :background (doom-blend yellow bg 0.04))
    (org-block-background :background (doom-blend yellow bg 0.04))
    (org-block-begin-line :background (doom-blend yellow bg 0.08))
    (org-block-end-line :background (doom-blend yellow bg 0.08))
    (lsp-ui-sideline-code-action :foreground blue)

    (font-lock-comment-face
      :slant 'italic
      :foreground comments
      :background (if doom-solarized-light-comment-bg (doom-blend teal base0 0.07)))
    ((font-lock-doc-face &override) :foreground doc-comments)
    ((font-lock-type-face &override) :slant 'italic)
    ((font-lock-builtin-face &override) :slant 'italic)
    ((font-lock-function-name-face &override) :foreground type)

    (font-lock-keyword-face
      :weight 'bold
      :foreground keywords)

    (font-lock-constant-face
      :weight 'bold
      :foreground constants)


    (doom-modeline-bar :background (if -modeline-bright modeline-bg highlight))

    (mode-line
      :background modeline-bg :foreground modeline-fg
      :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
    (mode-line-inactive
      :background modeline-bg-inactive :foreground modeline-fg-alt
      :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
    (mode-line-emphasis
      :foreground (if -modeline-bright base8 highlight))

    (solaire-mode-line-face
      :inherit 'mode-line
      :background modeline-bg-l
      :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
    (solaire-mode-line-inactive-face
      :inherit 'mode-line-inactive
      :background modeline-bg-inactive-l
      :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))

    ;; --- major-mode faces -------------------
    ;; css-mode / scss-mode
    (css-proprietary-property :foreground orange)
    (css-property             :foreground green)
    (css-selector             :foreground blue)

    ;; markdown-mode
    (markdown-markup-face :foreground base5)
    (markdown-header-face :inherit 'bold :foreground red)
    ((markdown-code-face &override) :background (doom-lighten base3 0.05))

    ;; ivy-mode
    (ivy-current-match :background (doom-lighten yellow 0.65) :distant-foreground fg)
    (ivy-minibuffer-match-face-1 :foreground blue :background base3 :weight 'bold)
    (ivy-minibuffer-match-face-2 :foreground magenta :background base3 :weight 'bold)
    (ivy-minibuffer-match-face-3 :foreground green   :background base3 :weight 'bold)
    (ivy-minibuffer-match-face-4 :foreground yellow  :background base3 :weight 'bold)
    (ivy-minibuffer-match-highlight :foreground violet :weight 'bold)
    (swiper-match-face-1 :inherit 'ivy-minibuffer-match-face-1)
    (swiper-match-face-2 :inherit 'ivy-minibuffer-match-face-2)
    (swiper-match-face-3 :inherit 'ivy-minibuffer-match-face-3)
    (swiper-match-face-4 :inherit 'ivy-minibuffer-match-face-4)

    ;; posframe
    (ivy-posframe :background modeline-bg-l)
    ;; org-mode
    (org-hide :foreground hidden)
    (solaire-org-hide-face :foreground hidden)

    ;; helm
    (helm-selection :foreground base0 :weight 'bold :background blue)

    ;; company
    (company-tooltip-selection :background blue
                                :foreground base3)

    ;; widget
    (widget-field :foreground fg :background base3)
    (widget-single-line-field :foreground fg :background base3)

    ;; latex
    (font-latex-sedate-face :foreground base6)

    )
  )

(require 'kaolin-themes)

(defgroup kaolin-valley-light nil
  "Kaolin valley light theme options."
  :group 'kaolin-themes)

(defcustom kaolin-valley-light-alt-bg nil
  "Use white background color."
  :type 'boolean
  :group 'kaolin-valley-light)

(define-kaolin-theme valley-light  "Light variant of kaolin-valley-dark theme."

  ;; Palette modification
  (
  ;; Colors
  (spring-green6 "#3e594e")
  (aquamarine4   "#518270")
  (orange0       "#d1832e")
  (orange3       "#F3AE6C")
  (cerulean4     "#47629E")
  (ultramarine1  "#744DF7")

  (azure1     "#0070CC")
  (teal0      "#0D7A75")
  (capri1     "#0F79BF")
  (harlequin1 "#417E2A")
  (harlequin3 "#4B8500")
  (crimson3   "#EE4970")
  (amber3     "#F3CB41")
  (harlequin2 "#2C820D")
  (erin2      "#18803A")
  (Lime2      "#5B7709")

  ;; Color vars
  ;; 背景色
  (bg0 "#FFFAE6")
  (bg1 (if kaolin-valley-light-alt-bg "#FFFAE6" "#FFFAE6"))
  (bg2 (if kaolin-valley-light-alt-bg white0 "#F3E7D3"))
  (bg3 (if kaolin-valley-light-alt-bg white1 "#F0DFCA"))
  (bg4 (if kaolin-valley-light-alt-bg white2 "#EBD7BE"))

  (pane orange9)

  ;; TODO
  ;; 普通代码字体颜色
  (fg1 "#48555E")
  (fg2 "#6b6560")
  (fg3 "#79716c")
  (fg4 "#867e78")

  (keyword     teal0)
  (builtin     erin2)

  (var         crimson0)
  (const       crimson0)
  (functions   capri1)
  (type        orange0)

  (comment     brown8)
  (comment-alt teal7)

  (str         ultramarine1)
  (str-alt     ultramarine4)
  (doc         str-alt)

  (prep        vermilion0)
  (num         vermilion0)
  (bool        num)
  (warning     orange0)
  (err         red3)

  (dim-buffer white0)
  (hl         azure1)
  (hl-line    (if kaolin-themes-hl-line-colored green9 bg3))
  (hl-indent  white4)
  (selection green9)
  (pulse bg4)

  (todo red3)
  (done erin2)

  (tooltip-fg fg3)
  (tooltip-hl-bg bg3)
  (tooltip-hl-fg hl)


    ;; TODO:
  (rb1 teal1)
  (rb2 aquamarine1)
  (rb3 violet4)
  (rb4 ultramarine4)
  (rb5 vermilion4)
  (rb6 brown3)
  (rb7 capri4)
  (rb8 magenta3)
  (rb9 yellow3)

  (diff-add spring-green3)
  (diff-mod vermilion3)
  (diff-rem red3)

    ;; Mode-line
  (line-fg           fg4)
  (line-color2       brown4)
  (line-bg1          bg2)
  (line-bg2          bg3)
  (line-border       (if kaolin-themes-modeline-border bg3 line-bg1))

  ;; Telephone-line
  (segment-active    gray2)
  (segment-inactive  gray2)

  (win-border    bg3)
  (line-num-fg   brown8)
  (line-num-hl   amber1)

  (cursor       gray3)

  (ivy1          gray9)
  (search1       cerise0)
  (search2       amber0)
  (search3       red3))

  (

  ;; TODO:
  (highlight-quoted-symbol  (:foreground builtin))

  ;; (org-level-1            (:foreground teal0 :bold bold :height 1.1))
  ;; (org-level-2            (:foreground violet4  :bold nil))

  ;; (org-level-4            (:foreground vermilion4 :bold nil))
  (org-code               (:foreground keyword))
  (org-verbatim           (:foreground orange2))
  (org-date               (:foreground erin2 :underline kaolin-themes-underline)))

  (when kaolin-themes-git-gutter-solid
    (custom-theme-set-faces
    'kaolin-valley-light
    `(git-gutter:added     ((t (:background ,diff-add :foreground ,diff-add))))
    `(git-gutter:modified  ((t (:background ,diff-mod :foreground ,diff-mod))))
    `(git-gutter:deleted   ((t (:background ,diff-rem :foreground ,diff-rem)))))))


