;;; customizations/config/lsp.el -*- lexical-binding: t; -*-

;; (define-key evil-normal-state-map "\C-n" nil)
;; (define-key acm-mode-map "\C-n" #'acm-select-next)
(use-package! lsp-bridge
  :config
  (require 'yasnippet)
  (yas-global-mode 1)
  (global-lsp-bridge-mode))

(with-eval-after-load 'lsp-bridge
  (evil-define-key 'insert lsp-bridge-mode-map (kbd "C-n") 'acm-select-next)
  (evil-define-key 'insert lsp-bridge-mode-map (kbd "C-p") 'acm-select-prev)
  (add-hook 'acm-mode-hook #'evil-normalize-keymaps)
  )
