;;; customizations/init.el -*- lexical-binding: t; -*-

(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(add-to-list 'load-path "~/.config/doom/customizations/utils")
(load "+guan-color.el")

(add-to-list 'load-path "~/.config/doom/customizations/config")
(dolist (module-name '("upload-server"
                       "run-anything"
                       "grpc"
                       "flash"))
  (load (format "%s.el" module-name)))

(add-hook
 'treemacs-mode-hook
 (defun channge-hl-line-mode ()
   (setq-local hl-line-face 'custom-line-highlight)
   (overlay-put hl-line-overlay 'face hl-line-face)

   ))

(setq +format-on-save-enabled-modes
      '(go-mode rust-mode))
(setq-hook! 'emacs-lisp-mode-hook +format-with-lsp nil)

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

(defun guan/enable-auto-set-font (&optional font-size1 font-size2 font-size3 font-size4)
  (let ((font-size1 (if (equal nil font-size1) 14.5 font-size1))
        (font-size2 (if (equal nil font-size2) 14.5 font-size2))
        (font-size3 (if (equal nil font-size3) 14.5 font-size3))
        (font-size4 (if (equal nil font-size4) 14.5 font-size4)))
    (when (display-graphic-p)
      (cl-loop for font in '("LXGWWenKaiMonoScreen" "Source Code Pro" "Fira Code"
                             "Menlo" "Monaco" "Dejavu Sans Mono"
                             "Lucida Console" "Consolas" "SAS Monospace")
               when (guan/--font-installed-p font)
               return (set-face-attribute
                       'default nil
                       :font (font-spec :family font
                                        :weight 'normal
                                        :slant 'normal
                                        :size  font-size1)))
      (cl-loop for font in '("OpenSansEmoji" "Noto Color Emoji" "Segoe UI Emoji"
                             "EmojiOne Color" "Apple Color Emoji" "Symbola" "Symbol")
               when (guan/--font-installed-p font)
               return (set-fontset-font t 'unicode
                                        (font-spec :family font
                                                   :size font-size2)
                                        nil 'prepend))
      (cl-loop for font in '("LXGWWenKaiMonoScreen" "LXGWWenKaiMonoGBScreen"
                             "noto-fonts-cjk" "思源黑体 CN" "思源宋体 CN" "微软雅黑 CN"
                             "Source Han Sans CN" "Source Han Serif CN"
                             "WenQuanYi Micro Hei" "文泉驿等宽微米黑"
                             "Microsoft Yahei UI" "Microsoft Yahei")
               when (guan/--font-installed-p font)
               return (set-fontset-font t '(#x4e00 . #x9fff)
                                        (font-spec :name font
                                                   :weight 'normal
                                                   :slant 'normal
                                                   :size font-size3)))
      (cl-loop for font in '("HanaMinB" "SimSun-ExtB")
               when (guan/--font-installed-p font)
               return (set-fontset-font t '(#x20000 . #x2A6DF)
                                        (font-spec :name font
                                                   :weight 'normal
                                                   :slant 'normal
                                                   :size font-size4)))
      )))
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
  (message (guan/--get-input-method "switch")))

;;;###autoload
(defun guan/show-input-method ()
  "show rime current input method"
  (interactive)
  (message (guan/--get-input-method "show")))

(use-package! rime
  :defer 2
  :config
  (setq default-input-method "rime"
        rime-show-candidate 'posframe)
  (global-set-key (kbd "C-x C-d") 'toggle-input-method)
  (global-set-key (kbd "M-j") 'guan/switch-input-method))

;; dont hook on macos
(if (not (eq system-type 'darwin))
    (add-hook 'minibuffer-setup-hook (lambda ()
                                       (activate-input-method "rime")
                                       (if (not (rime--ascii-mode-p))
                                           (rime-inline-ascii)))))

;; rime ----------- end

(global-set-key (kbd "C-s") 'guan/flash-jump)


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

;;;###autoload
(require 'treemacs)
(defun guan/toggle-treemacs ()
  "Toggle treemacs and locate current position"
  (interactive)
  (let ((buffer (current-buffer))
        (state (treemacs-current-visibility)))
    (cond ((or (string-equal state "none") (string-equal state "exists"))
           (progn (+treemacs/toggle)
                  (switch-to-buffer buffer)
                  (treemacs-find-file)))
          (t (+treemacs/toggle)))))

;;;###autoload
(defun guan/treemacs-find-file ()
  "Find file in sider and switch to the window."
  (interactive)
  (treemacs-find-file)
  (let ((state (treemacs-current-visibility)))
    (if (not (string-equal state "visible"))
        (guan/toggle-treemacs)
      nil)
    (select-window (treemacs-get-local-window))
    ))

;;;###autoload
(defun guan/ace-swap-window ()
  "Swap window."
  (interactive)
  (let ((this-window (selected-window)))
    (ace-swap-window)
    (dolist (window (window-list))
      (if (eq this-window window)
          (select-window window) nil))
    ))

;;;###autoload
(defun guan/vsplit-and-goto-definition ()
  "Vsplit, then goto definition"
  (interactive)
  (let ((new-window (split-window-right)))
    (select-window new-window)
    (+lookup/definition (symbol-at-point))))

;;;###autoload
(defun guan/window-enlargen ()
  "Enlargen the current window and shrinks others."
  (interactive)
  (let ((this-window (selected-window)))
    (dolist (window (window-list))
      (if (not (eq this-window window))
          (progn
            (select-window window)
            (forward-line 0))))
    (select-window this-window)
    (doom/window-enlargen)))

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
       :desc "Enlargen current window"  "o"  #'guan/window-enlargen
       :desc "Normalize window size"  "n"  #'balance-windows
       :desc "Swap window"  "w"  #'guan/ace-swap-window)

      (:prefix-map ("c" . "code")
       :desc "Pop up scratch from buffer"  "n"  #'doom/open-scratch-buffer)

      (:prefix-map ("q" . "quit/restart")
       :desc "Quit Emacs"  "a"  #'evil-quit-all)

      (:prefix-map ("o" . "open")
                   (:when (modulep! :ui treemacs)
                     :desc "Project sidebar"  "p"  #'guan/toggle-treemacs))

      )
(map!
 :nv "gc" #'guan/evilnc-comment-operator
 :n "gr" #'+lookup/references)

(global-set-key (kbd "C-j") nil)
(global-set-key (kbd "M-i") nil)

(define-key evil-window-map "v" '+evil/window-vsplit-and-follow)
(define-key evil-window-map "V" 'evil-window-vsplit)
(define-key evil-window-map "s" '+evil/window-split-and-follow)
(define-key evil-window-map "S" 'evil-window-split)
(define-key evil-window-map "\C-]" 'guan/vsplit-and-goto-definition)
(define-key evil-window-map "o" 'guan/window-enlargen)

(require 'protobuf-mode)


;; high priority config ---- end --------------------------
(setq private-custom-file "~/.config/doom/private/custom.el")
(if (file-exists-p private-custom-file)
    (load private-custom-file) nil)
