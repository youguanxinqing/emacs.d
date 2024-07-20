;;; package --- Summary -*- lexical-binding: t; -*-
;;; Commentary:
;;; customizations/utils/path.el

(require 'projectile)
(require 's)

;;; Code:
(defun guan/project-root-dir-name ()
  "Get name of root directory under current project."
  (car (last (seq-filter  (lambda (item)
                            (if (length> item 0)
                                item)) (string-split (projectile-project-root) "/")))
       ))

(defun guan/project-root-dir-abspath ()
  "Get absolute path of root directory under current project."
  (projectile-project-root))

(defun guan/current-file-name ()
  "Get name of current file."
  ())

(defun guan/current-file-abspath ()
  "Get absolute path of current file."
  (buffer-file-name))

(defun guan/current-file-relpath ()
  "Get relative path of current file."
  (s-replace (guan/project-root-dir-abspath) "" (guan/current-file-abspath))
  )

(provide 'path)
;;; path.el ends here
