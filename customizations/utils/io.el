;;; package --- Summary -*- lexical-binding: t; -*-
;;; Commentary:
;;; customizations/utils/io.el

(defun guan/read-file (path)
  "Read contents from filepath."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(provide 'io)
;;; io.el ends here
