;;; customizations/config/lsp.el -*- lexical-binding: t; -*-

(use-package! lsp-bridge
  :config
  (require 'yasnippet)
  (yas-global-mode 1)
  (global-lsp-bridge-mode))

(with-eval-after-load 'lsp-bridge
  (evil-define-key 'insert lsp-bridge-mode-map (kbd "C-n") 'acm-select-next)
  (evil-define-key 'insert lsp-bridge-mode-map (kbd "C-p") 'acm-select-prev)
  (evil-define-key 'insert lsp-bridge-mode-map (kbd "C-d") 'acm-doc-scroll-down)
  (evil-define-key 'insert lsp-bridge-mode-map (kbd "C-f") 'acm-doc-scroll-up)
  (evil-define-key 'normal lsp-bridge-mode-map (kbd "K") 'lsp-bridge-show-documentation)
  )
