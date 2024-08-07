;;; package --- Summary -*- lexical-binding: t; -*-
;;; Commentary:
;;; customizations/utils/pyenv.el

(require 'path)
(require 'f)
(require 'io)

;;; Code:
(defun guan/with-pyenv (cmd)
  "Add whole path for python CMD if there is a .python-version file in root dir."
  (let* ((pyenv-path "~/.pyenv/versions/")
         (version-file ".python-version")
         (version-file-abspath (concat (guan/project-root-dir-abspath) "/" version-file)))
    (if (not (file-exists-p version-file-abspath))
        cmd
      (let ((py-with-python (concat pyenv-path (string-trim (guan/read-file version-file-abspath)) "/bin/python")))
        (string-join (mapcar (lambda (item)
                               (if (string-match "^python" item)
                                   py-with-python
                                 item))  (string-split cmd " "))
                     " ")
        )
      )
    ))


(provide 'pyenv)
;;; pyenv.el ends here
