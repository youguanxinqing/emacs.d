;;; customizations/init.el -*- lexical-binding: t; -*-

(setq doom-font (font-spec :size 22))
(setq rustic-lsp-server 'rust-analyzer)


;; rime ----------- start

(defun guan/--get-input-method (action)
  (cond ((string-equal action "switch") (if (rime--ascii-mode-p)
                                            "Rime: To English"
                                            "Rime: To Chinese"))
        ((string-equal action "show") (if (rime--ascii-mode-p)
                                          "Rime: English"
                                          "Rime: Chinese"))
        ))

(defun guan/switch-input-method ()
  "switch input method: english or chinese"
  (interactive)
  (rime-inline-ascii)
  (rime--message-display-content (guan/--get-input-method "switch")))

(defun guan/show-input-method ()
  "show rime current input method"
  (interactive)
  (rime--message-display-content (guan/--get-input-method "show")))

(use-package! rime
  :config
  (setq default-input-method "rime"
        rime-show-candidate 'posframe)
  (global-set-key (kbd "C-x C-d") 'toggle-input-method)
  (global-set-key (kbd "M-j") 'guan/switch-input-method))

;; rime ----------- end

(global-set-key (kbd "C-s") 'evil-avy-goto-word-1)


;; close evil-snipe-s
(evil-define-key '(normal motion) evil-snipe-local-mode-map
  "s" nil
  "S" nil)

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
