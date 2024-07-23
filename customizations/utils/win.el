;;; package --- Summary -*- lexical-binding: t; -*-
;;; Commentary:
;;; customizations/utils/win.el

;;; Code:
(defun guan/display-content-to-window (window-name title content)
  "Display content in a window that named WINDOW-NAME."
  (switch-to-buffer-other-window window-name)
  (erase-buffer)
  (insert title)
  (insert "\n----\n\n")
  (insert content)
  (fill-paragraph)
  )

(provide 'win)
;;; win.el ends here
