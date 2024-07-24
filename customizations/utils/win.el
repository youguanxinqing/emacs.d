;;; package --- Summary -*- lexical-binding: t; -*-
;;; Commentary:
;;; customizations/utils/win.el

;;; Code:
(defun guan/display-content-to-window (window-name title content)
  "Display content in a window that named WINDOW-NAME."
  (if (not (string-equal window-name (buffer-name)))
      (switch-to-buffer-other-window window-name)
    nil)
  (erase-buffer)
  (insert title)
  (insert "\n----\n\n")
  (insert content)
  (fill-paragraph)
  )

(provide 'win)
;;; win.el ends here
