;;; customizations/init.el -*- lexical-binding: t; -*-

(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(load "+guan-color.el")
;; font --------- start
(setq doom-font (font-spec :size 21))


(defface custom-line-highlight '((t (:background "#8FBC8F" :foreground "#000000" :extend t))) "")
(add-hook
 'treemacs-mode-hook
 (defun channge-hl-line-mode ()
   (setq-local hl-line-face 'custom-line-highlight)
   (overlay-put hl-line-overlay 'face hl-line-face)
   ))

;; banner

(defun banner-of-guan ()
   (let* ((banner '(
    "          ▀████▀▄▄              ▄█ "
    "            █▀    ▀▀▄▄▄▄▄    ▄▄▀▀█ "
    "    ▄        █          ▀▀▀▀▄  ▄▀  "
    "   ▄▀ ▀▄      ▀▄              ▀▄▀  "
    "  ▄▀    █     █▀   ▄█▀▄      ▄█    "
    "  ▀▄     ▀▄  █     ▀██▀     ██▄█   "
    "   ▀▄    ▄▀ █   ▄██▄   ▄  ▄  ▀▀ █  "
    "    █  ▄▀  █    ▀██▀    ▀▀ ▀▀  ▄▀  "
    "   █   █  █      ▄▄           ▄▀   by guan "
    "                                   "
    "                                   "
    "      Don't mind not knowing.      "
    "                                   "
                  ))
         (longest-line (apply #'max (mapcar #'length banner))))
    (put-text-property
     (point)
     (dolist (line banner (point))
       (insert (+doom-dashboard--center
                +doom-dashboard--width
                (concat line (make-string (max 0 (- longest-line (length line))) 32)))
               "\n"))
     'face 'doom-dashboard-banner)))
(setq +doom-dashboard-ascii-banner-fn #'banner-of-guan)

;;

;; set search path
(defun set-env-path (search-paths)
  (dolist (path search-paths)
    (if (not (member path exec-path))
        (add-to-list 'exec-path path)
        nil))
  (let ((path-list (string-split (getenv "PATH") ":")))
    (dolist (path search-paths)
        (if (not (member path path-list))
                (push path path-list )
                nil))
    (setenv "PATH" (string-join path-list ":")))
  )
(set-env-path (string-split (shell-command-to-string "printenv PATH") ":"))

;;

(defun guan/--font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))
;; (guan/--font-installed-p "LXGWWenKaiMonoScreen")

(defun guan/enable-auto-set-font ()
  (when (display-graphic-p)
   (cl-loop for font in '("Source Code Pro" "Fira Code"
                          "Menlo" "Monaco" "Dejavu Sans Mono"
                           "Lucida Console" "Consolas" "SAS Monospace")
           when (guan/--font-installed-p font)
           return (set-face-attribute
                   'default nil
                   :font (font-spec :family font
                                   :weight 'normal
                                   :slant 'normal
                                   :size (cond ((eq system-type 'gnu/linux) 14.0)
                                                ((eq system-type 'darwin) 20.0)
                                                ((eq system-type 'windows-nt) 12.5)))))
   (cl-loop for font in '("OpenSansEmoji" "Noto Color Emoji" "Segoe UI Emoji"
                           "EmojiOne Color" "Apple Color Emoji" "Symbola" "Symbol")
           when (guan/--font-installed-p font)
           return (set-fontset-font t 'unicode
                                   (font-spec :family font
                                           :size (cond ((eq system-type 'gnu/linux) 16.5)
                                                       ((eq system-type 'darwin) 20.0)
                                                       ((eq system-type 'windows-nt) 15.0)))
                                   nil 'prepend))
   (cl-loop for font in '("LXGWWenKaiScreen" "LXGWWenKaiMonoScreen" "LXGWWenKaiMonoGBScreen"
                          "noto-fonts-cjk" "思源黑体 CN" "思源宋体 CN" "微软雅黑 CN"
                           "Source Han Sans CN" "Source Han Serif CN"
                           "WenQuanYi Micro Hei" "文泉驿等宽微米黑"
                           "Microsoft Yahei UI" "Microsoft Yahei")
           when (guan/--font-installed-p font)
           return (set-fontset-font t '(#x4e00 . #x9fff)
                                   (font-spec :name font
                                           :weight 'normal
                                           :slant 'normal
                                           :size (cond ((eq system-type 'gnu/linux) 14.0)
                                                       ((eq system-type 'darwin) 20.0)
                                                       ((eq system-type 'windows-nt) 15.0)))))
   (cl-loop for font in '("HanaMinB" "SimSun-ExtB")
        when (guan/--font-installed-p font)
        return (set-fontset-font t '(#x20000 . #x2A6DF)
                                (font-spec :name font
                                        :weight 'normal
                                        :slant 'normal
                                        :size (cond ((eq system-type 'gnu/linux) 16.5)
                                                    ((eq system-type 'darwin) 20.0)
                                                    ((eq system-type 'windows-nt) 15.0))))))
        )
(guan/enable-auto-set-font)

;; font --------- end

(setq rustic-lsp-server 'rust-analyzer)


;; lsp treemacs symbol ----- start

(defun guan/--kill-lsp-treemacs-symbols-buffer ()
  "Kill symbols view"
  (if-let (buf (get-buffer lsp-treemacs-symbols-buffer-name))
      (kill-buffer buf)))

;;;###autoload
(defun guan/toggle-symbols-outline ()
  "Toggle symbols view"
  (interactive)
  (let ((buf (get-buffer lsp-treemacs-symbols-buffer-name)))
    (cond ((null buf) (lsp-treemacs-symbols))
          (t (guan/--kill-lsp-treemacs-symbols-buffer)))))

;; rime ----------- start
(defun guan/--get-input-method (action)
  (cond ((string-equal action "switch") (if (rime--ascii-mode-p)
                                            "Rime: To English"
                                            "Rime: To Chinese"))
        ((string-equal action "show") (if (rime--ascii-mode-p)
                                          "Rime: English"
                                          "Rime: Chinese"))
        ))

;;;###autoload
(defun guan/switch-input-method ()
  "switch input method: english or chinese"
  (interactive)
  (rime-inline-ascii)
  (rime--message-display-content (guan/--get-input-method "switch")))

;;;###autoload
(defun guan/show-input-method ()
  "show rime current input method"
  (interactive)
  (rime--message-display-content (guan/--get-input-method "show")))

(use-package! rime
  :defer 2
  :config
  (setq default-input-method "rime"
        rime-show-candidate 'posframe)
  (global-set-key (kbd "C-x C-d") 'toggle-input-method)
  (global-set-key (kbd "M-j") 'guan/switch-input-method))

;; rime ----------- end

(global-set-key (kbd "C-s") 'evil-avy-goto-char)


;; close evil-snipe-s
(evil-define-key '(normal motion) evil-snipe-local-mode-map
  "s" nil
  "S" nil)

;; map f, F
(evil-define-key '(normal motion) evil-snipe-override-local-mode-map
  "f" 'evil-avy-goto-char-in-line
  "F" 'evil-avy-goto-char-in-line
  "t" 'evil-avy-goto-char-in-line
  "T" 'evil-avy-goto-char-in-line)

(map! :n "C-t" nil)
(define-key evil-motion-state-map (kbd "C-t") 'evil-jump-backward-swap)
(global-set-key [remap evil-jump-to-tag] nil)
(define-key evil-motion-state-map (kbd "C-]") 'evil-jump-to-tag)

;; highlight symbol
(require 'hi-lock)

(setq color-candinates '('guan-match-light-green
                         'guan-match-green-yellow
                         'guan-match-yellow
                         'guan-match-salmon
                         'guan-match-medium-violet-red
                         'guan-match-dark-orange
                         'guan-match-brown
                         'guan-match-deep-sky-blue))
(setq color-candinates-length (length color-candinates))
(setq use-color-position 0)

(defun guan/--color-from-candinates ()
  "Extract color in cycle"
  (if (< use-color-position color-candinates-length)
      (let ((position use-color-position))
        (setq use-color-position (+ 1 use-color-position))
        (nth position color-candinates))
      (progn
        (setq use-color-position 1)
        (nth 0 color-candinates))
    ))

(defun guan/--is-valid-regexp (regexp)
  (cond ((null regexp) nil)
        ((string-match regexp "") nil)
        (t regexp)))

(defun guan/--un-highlight-regexp (regexp)
  (unhighlight-regexp regexp))

(defun guan/--highlight-regexp (regexp)
  (let ((face (guan/--color-from-candinates)))
    (unless hi-lock-mode (hi-lock-mode 1))
    (hi-lock-set-pattern
     regexp face nil nil
     (if (and case-fold-search search-upper-case)
         (isearch-no-upper-case-p regexp t)
       case-fold-search))))

;;;###autoload
(defun guan/toggle-highlight-symbol-at-point ()
  "Toggle highlight symbol at point"
  (interactive)
  (let* ((regexp (guan/--is-valid-regexp (find-tag-default-as-symbol-regexp)))
         (existed-regexp-list (mapcar (lambda (pattern)
                   (or (car (rassq pattern hi-lock-interactive-lighters))
                             (car pattern)))
                 hi-lock-interactive-patterns)))
    (if regexp
        (if (and existed-regexp-list (member regexp existed-regexp-list))
            (guan/--un-highlight-regexp regexp)
            (guan/--highlight-regexp regexp))
        (display-message-or-buffer "invalid symbol"))
    ))

;;;###autoload
(defun guan/un-highlight-all-symbols ()
  "Un-Highlight all symbols"
  (interactive)
  (hi-lock-unface-buffer t))

;;;###autoload
(evil-define-operator guan/evilnc-comment-operator (start end type)
  (cond ((string-equal type 'inclusive)
           (let* ((start (progn (goto-char start) (line-beginning-position)))
                  (end (progn (goto-char end) (line-end-position))))
             (evilnc-comment-operator start end type)))
        (t (evilnc-comment-operator start end type))))

(map! :leader

      :desc "Find file from here"  "SPC"  #'find-file
      :desc "Toggle highlight symbol at point"  "k"  #'guan/toggle-highlight-symbol-at-point
      :desc "Locate file on treemacs sider" "e" #'treemacs-find-file
      :desc "Kill buffer"  "x"  #'kill-current-buffer

      (:prefix-map ("f" . "file")
       :desc "Search project"  "w"  #'+default/search-project
       :desc "Find file from workspace"  "f"  #'+default/find-file-under-here
       :desc "Find file from here"  "F"  #'find-file
       :desc "Search current directory"  "d"  #'+default/search-cwd)

      (:prefix-map ("w" . "workspaces/windows")
       :desc "Select from multi windows"  "s"  #'ace-window
       :desc "Enlargen current window"  "o"  #'doom/window-enlargen
       :desc "Normalize window size"  "n"  #'balance-windows
       :desc "Swap window"  "w"  #'ace-swap-window)

      (:prefix-map ("c" . "code")
       :desc "Pop up scratch from buffer"  "n"  #'doom/open-scratch-buffer)

      (:prefix-map ("q" . "quit/restart")
       :desc "Quit Emacs"  "a"  #'evil-quit-all)

      )

(map!
      :nv "gc" #'guan/evilnc-comment-operator)



;; high priority config ---- end --------------------------
(setq private-custom-file "~/.config/doom/private/custom.el")
(if (file-exists-p private-custom-file)
    (load private-custom-file) nil)
