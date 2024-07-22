;;; package --- Summary -*- lexical-binding: t; -*-
;;; Commentary:
;;; customizations/utils/io.el

;;; Code:
(defun guan/read-file (path)
  "Read contents from PATH that passed."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(provide 'io)
;;; io.el ends here
