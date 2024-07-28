;;; customizations/config/flash.el -*- lexical-binding: t; -*-

(defface guan-background-face
  '((t (:foreground "gray40")))
  "Face for whole window background during selection.")

(setq guan--overlay-state nil)

(defun guan/--toggle-grey-background ()
  (if (equal nil guan--overlay-state)
      (let* ((beg (window-start))
             (end (window-end))
             (ov (make-overlay beg end (window-buffer))))
        (overlay-put ov 'face 'guan-background-face)
        (setq guan--overlay-state ov))
    (progn
      (delete-overlay guan--overlay-state)
      (setq guan--overlazy-state nil)
      )
    )
  )

;; (guan/--toggle-grey-background)

(defun jump ()
  (interactive)
  ())
