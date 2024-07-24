;;; customizations/config/grpc.el -*- lexical-binding: t; -*-

(require 'path)
(require 'pyenv)

(defun guan-py-grpc-item (includes python-out grpc-python-out after-callback)
  `(
    ("includes" . ,includes)
    ("python-out" . ,python-out)
    ("grpc-python-out" . ,grpc-python-out)
    ("after-hook" . ,after-callback)
    )
  )

(defun guan-py-grpc-config (name item) `(,name . ,item))


(defvar py-grpc-configs (list
                         (guan-py-grpc-config
                          "sec_third_server"
                          (guan-py-grpc-item '("/tmp/" "/tmp/") "/tmp/" "/tmp/" (lambda () (print "after callback"))))
                         ))


(defun guan/--make-grpc-command (grpc-config)
  (let ((includes-chunk (string-join (mapcar (lambda (item)
                                               (format "-I=%s" item)
                                               )
                                             (cdr (assoc "includes" grpc-config))) " "))
        (python-out-chunk (format "--python_out=%s" (cdr (assoc "python-out" grpc-config))))
        (grpc-python-out-chunk (format "--grpc_python_out=%s" (cdr (assoc "python-out" grpc-config)))))
    (guan/with-pyenv (format "python -m grpc_tools.protoc %s %s %s %s"
                             includes-chunk
                             python-out-chunk
                             grpc-python-out-chunk
                             (guan/current-file-abspath)))
    ))

;;; python -m grpc_tools.protoc \
;;;   -I=./a/b/ \
;;;   -I=. \
;;;   --python_out=./x/y/ \
;;;   --grpc_python_out=./x/y/ \
;;;   ./xxx.proto
;;;###autoload
(defun guan/py-gen-grpc-file ()
  (interactive)
  (let ((ext (guan/current-filename-ext))
        (filename (guan/current-filename-without-ext)))
    (if (not (string-equal "proto" ext))
        (message (format "%s is not proto file" (guan/current-filename)))
      (let ((grpc-config (assoc filename py-grpc-configs)))
        (if (eq nil grpc-config)
            (message (format "not found \"%s\" config" filename))
          (print (guan/--make-grpc-command (cdr grpc-config)))
          (funcall (cdr (assoc "after-hook" (cdr grpc-config))))
          )
        )
      )
    ))
