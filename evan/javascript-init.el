;; flycheck
(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode t))

(setq global-default-tab-width 2)

;; web-mode
(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.js[x]?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode))
  (setq web-mode-content-types-alist
        '(("jsx" . ".*\\.js[x]?\\'"))))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  ;; 保存的时候进行检查
  ;; flycheck doc http://www.flycheck.org/en/latest/user/syntax-checks.html
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1))

;; tide
(use-package tide
  :ensure t
  :config
  (flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append)
  ;; formats the buffer before saving
  ;; (add-hook 'before-save-hook 'tide-format-before-save)
  (add-hook 'web-mode-hook
            (lambda ()
              ;; JSX
              (when (equal web-mode-content-type "jsx")
                (setq-local emmet-expand-jsx-className? t)
                (flycheck-add-mode 'javascript-eslint 'web-mode)
                (flycheck-add-next-checker 'javascript-eslint 'jsx-tide 'append)
                (setup-tide-mode))

              ;; TSX
              (when (string-equal "tsx" (file-name-extension buffer-file-name))
                (setq-local emmet-expand-jsx-className? t)
                (flycheck-add-mode 'typescript-tslint 'web-mode)
                (flycheck-add-next-checker 'typescript-tslint 'jsx-tide 'append)
                (setup-tide-mode))

              ;; Typescript
              (when (string-equal "ts" (file-name-extension buffer-file-name))
                (flycheck-add-mode 'typescript-tslint 'web-mode)
                (setup-tide-mode))

              ;; `:separate`  使得不同 backend 分开排序
              (add-to-list 'company-backends '(company-tide :with company-tabnine :separate))
              (setq web-mode-code-indent-offset 2)
              (setq-local web-mode-enable-auto-quoting nil))))


;; 当tide被加载后
(with-eval-after-load 'tide
  ;; evil模式下绑定 g d 跳转定义
  (evil-define-key '(normal) tide-mode-map (kbd "g d") 'tide-jump-to-definition))

(setq tide-format-options '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t :placeOpenBraceOnNewLineForFunctions nil))

(provide 'javascript-init)

;; vue
(require 'vue-mode)
(require 'lsp-mode)

(defun vuejs-custom ()
  (lsp)
  (flycheck-mode t)
  (company-mode))

(add-hook 'vue-mode-hook 'vuejs-custom)
