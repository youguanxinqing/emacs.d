(require 'package)
(setq package-archives '(("gnu" . "https://mirrors.ustc.edu.cn/elpa/gnu/")
                         ("melpa" . "https://mirrors.ustc.edu.cn/elpa/melpa/")
                         ("nongnu" . "https://mirrors.ustc.edu.cn/elpa/nongnu/")))
(package-initialize)

(add-to-list 'load-path "site-lisp/open-newline/")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(modus-operandi))
 '(custom-safe-themes
   '("21e3d55141186651571241c2ba3c665979d1e886f53b2e52411e9e96659132d4" default))
 '(package-selected-packages '(org-roam-ui org-roam vertico modus-themes))
 '(warning-suppress-log-types '((emacs)))
 '(warning-suppress-types '((emacs))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Courier New" :foundry "outline" :slant normal :weight regular :height 90 :width normal)))))

(set-frame-font "Courier New 14" nil t)
(set-fontset-font "fontset-default" 'han "微软雅黑" nil 'append)

(use-package vertico
  :init
  (vertico-mode)
  (setq vertico-count 10)
  (setq vertico-cycle t)
  )

(use-package olivetti
  :hook (org-mode . olivetti-mode)
  )

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "d:/notes/org-roam"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))


(require 'open-newline)
(global-set-key (kbd "C-o") 'open-newline-below)
(global-set-key (kbd "C-S-o") 'open-newline-above)

(global-set-key (kbd "C-h") 'delete-backward-char)



