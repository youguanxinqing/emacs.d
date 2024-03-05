(require 'package)
(setq package-archives '(("gnu" . "https://mirrors.ustc.edu.cn/elpa/gnu/")
                         ("melpa" . "https://mirrors.ustc.edu.cn/elpa/melpa/")
                         ("nongnu" . "https://mirrors.ustc.edu.cn/elpa/nongnu/")))
(package-initialize)

;; --- begin add site-lisp ---
(add-to-list 'load-path "~/.emacs.d/site-lisp/open-newline/")
;; --- end 

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(modus-operandi))
 '(custom-safe-themes
   '("4b026ac68a1aa4d1a91879b64f54c2490b4ecad8b64de5b1865bca0addd053d9" "d92c1c36a5181cf629749bf6feee1886cf6bce248ab075c9d1b1f6096fea9539" "98ef36d4487bf5e816f89b1b1240d45755ec382c7029302f36ca6626faf44bbd" "ba323a013c25b355eb9a0550541573d535831c557674c8d59b9ac6aa720c21d3" "046a2b81d13afddae309930ef85d458c4f5d278a69448e5a5261a5c78598e012" "7b8f5bbdc7c316ee62f271acf6bcd0e0b8a272fdffe908f8c920b0ba34871d98" "d445c7b530713eac282ecdeea07a8fa59692c83045bf84dd112dd738c7bcad1d" "bbb13492a15c3258f29c21d251da1e62f1abb8bbd492386a673dcfab474186af" "7fd8b914e340283c189980cd1883dbdef67080ad1a3a9cc3df864ca53bdc89cf" "21e3d55141186651571241c2ba3c665979d1e886f53b2e52411e9e96659132d4" default))
 '(package-selected-packages
   '(rg orderless doom-modeline color-theme org-roam-ui org-roam vertico modus-themes))
 '(warning-suppress-log-types '((emacs)))
 '(warning-suppress-types '((emacs))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Courier New" :foundry "outline" :slant normal :weight regular :height 90 :width normal)))))


;; --- begin basic conf ---

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8)
(setq-default default-buffer-file-coding-system 'utf-8)

(set-frame-font "Courier New 14" nil t)
(set-fontset-font "fontset-default" 'han "微软雅黑" nil 'append)
(load-theme 'modus-operandi-tinted t)
(tool-bar-mode 0)

;; keymap
(global-set-key (kbd "C-h") 'delete-backward-char)

;; --- end 

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

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))



(require 'open-newline)
(global-set-key (kbd "C-o") 'open-newline-below)
(global-set-key (kbd "C-S-o") 'open-newline-above)

