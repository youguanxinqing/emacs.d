;;; customizations/init.el -*- lexical-binding: t; -*-

(setq doom-font (font-spec :size 22))
(setq rustic-lsp-server 'rust-analyzer)

;; close evil-snipe-s
(evil-define-key '(normal motion) evil-snipe-local-mode-map
  "s" nil
  "S" nil)

;; rime
(use-package! rime
  :config
  (setq default-input-method "rime"
        rime-show-candidate 'posframe))
(global-set-key (kbd "C-x C-d") 'toggle-input-method)
(global-set-key (kbd "M-j") 'rime-inline-ascii)

(global-set-key (kbd "C-s") 'evil-avy-goto-word-1)

(map! :n "C-t" nil)
(define-key evil-motion-state-map (kbd "C-t") 'evil-jump-backward-swap)
(global-set-key [remap evil-jump-to-tag] nil)
(define-key evil-motion-state-map (kbd "C-]") 'evil-jump-to-tag)

(map! :leader

      :desc "Find file from here"  "SPC"  #'find-file

      (:prefix-map ("f" . "file")
       :desc "Search project"  "w"  #'+default/search-project
       :desc "Find file from workspace"  "f"  #'+default/find-file-under-here
       :desc "Find file from here"  "F"  #'find-file
       :desc "Search current directory"  "d"  #'+default/search-cwd)
      )


;; high priority config
(setq private-custom-file "~/.config/doom/private/custom.el")
(if (file-exists-p private-custom-file)
    (load private-custom-file) nil)
