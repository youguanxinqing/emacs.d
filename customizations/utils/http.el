;;; package --- Summary -*- lexical-binding: t; -*-
;;; Commentary:
;;; customizations/utils/http.el

;;; Code:
(defun guan/http-post-json (url post-data)
  "Quickly trigger http post request."
  (let ((url-request-method "POST")
        (url-request-extra-headers `(("Content-Type" . "application/json")))
        (url-request-data post-data)
        )
    (with-current-buffer (url-retrieve-synchronously url)
      (let ((resp (buffer-string)))
        (kill-buffer)
        resp))
    )
  )

(provide 'http)
;;; http.el ends here
