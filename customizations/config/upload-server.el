;;; customizations/config/upload-server.el -*- lexical-binding: t; -*-

(require 'path)

(defun guan-server-select (project-name target-root-dir options)
  `(,project-name ,target-root-dir ,options))

(defun guan-server-item (name addr)
  `(,name ,addr))

(defvar guan-upload-configs `(("doom"
                               "/tmp/test/"
                               (("test1" "127.0.0.1:9091")
                                ("test2" "127.0.0.1:9092")))
                              )
  )

(defun guan/--find-servers-from-configs (project-name)
  "Find server options from variable `guan-upload-configs'."
  (let ((index 0)
        (target-root-dir (nth 1 (assoc project-name guan-upload-configs)))
        (servers (nth 2 (assoc project-name guan-upload-configs))))
    (mapcar  (lambda (option)
               (setq index (+ index 1))
               (let ((key (format "%d. %s -> %s"
                                  index
                                  (nth 0 option)
                                  (nth 1 option)
                                  )))
                 (push target-root-dir option)
                 `(,key . ,option)
                 ))
             servers)
    )
  )

;; guan/--do-upload-action
;; shell command:
;;   sync-client --addr [remote_host]:[remote_port] \
;;               --local-file-path [local-file-path] \
;;               --remote-file-path [remote-file-path] \
(defun guan/--do-upload-action (target-root-dir name addr)
  (let* ((local-file-path (guan/current-file-abspath))
         (remote-file-path (format "%s%s" target-root-dir (guan/current-file-relpath)))
         (cmd-string (format "sync-client --addr %s --local-file-path %s --remote-file-path %s"
                             addr
                             local-file-path
                             remote-file-path
                             )))
    (shell-command cmd-string)
    )
  )

;;;###autoload
(defun guan/upload-file ()
  "Upload current file to target sever."
  (interactive)
  (let* ((options (guan/--find-servers-from-configs (guan/project-root-dir-name)))
         (choice-key (completing-read "Choose Target Server: " options))
         (choice-value (cdr (assoc choice-key options))))
    (apply 'guan/--do-upload-action choice-value)
    ))
