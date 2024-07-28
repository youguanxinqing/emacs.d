;;; customizations/config/flash.el -*- lexical-binding: t; -*-

(defface guan-background-face
  '((t (:foreground "gray40")))
  "Face for whole window background during selection.")

(defface guan-highlight-face
  '((t (:inherit highlight)))
  "Face for matches during reading chars using `avy-goto-char-timer'.")

(defcustom guan-del-last-char-by '(?\b ?\d)
  "List of event types, i.e. key presses, that delete the last
character read.  The default represents `C-h' and `DEL'.  See
`event-convert-list'."
  :type 'list)

(setq guan--overlay-state nil)

(defvar guan--overlays '())

(defun guan/--toggle-grey-background ()
  (if (equal nil guan--overlay-state)
      (let* ((beg (window-start))
             (end (window-end))
             (ov (make-overlay beg end (window-buffer))))
        (overlay-put ov 'face 'guan-background-face)
        (setq guan--overlay-state ov))
    (progn
      (delete-overlay guan--overlay-state)
      (setq guan--overlay-state nil)
      (print guan--overlay-state)
      )
    )
  )

(defun guan/--clear-all-highlights ()
  (when (< 0 (length guan--overlays))
    (dolist (ov guan--overlays)
      (print ov)
      (delete-overlay ov))
    )
  (setq guan--overlays '()))

(defun search-and-hightlight (text start end)
  (save-excursion
    (progn
      (guan/--clear-all-highlights)
      (goto-char start)
      (while (re-search-forward text end t)
        (let ((ov (make-overlay (- (point) (length text)) (point))))
          (overlay-put ov 'face 'guan-highlight-face)
          (push ov guan--overlays)
          )
        )
      )
    )
  )

(defun live-grepper ()
  (let ((start-point (window-start))
        (end-point (window-end))
        (search-text "")
        char break)
    (progn
      (guan/--toggle-grey-background)
      (while (and (not break)
                  (setq char
                        (read-char (format "%d  char%s: "
                                           (length search-text)
                                           (if (string= search-text "")
                                               search-text
                                             (format " (%s)" search-text)))
                                   t
                                   )
                        )
                  )
        (cond
         ;; Handle ESC
         ((= char 27)
          (progn
            (guan/--toggle-grey-background)
            (guan/--clear-all-highlights)
            (keyboard-quit)))
         ;; Handle RET
         ((= char 13)
          (progn
            (guan/--toggle-grey-background)
            (guan/--clear-all-highlights)
            (keyboard-quit)))
         ;; Handle C-h, DEL
         ((memq char guan-del-last-char-by)
          (let ((l (length search-text)))
            (when (>= l 1) (setq search-text (substring search-text 0 (1- l)))
                  (if (> (length search-text) 0)
                      (search-and-hightlight search-text start-point end-point)
                    (guan/--clear-all-highlights))
                  ))
          )
         (t
          (progn
            (setq search-text (concat search-text (list char)))
            ;; Highlight
            (when (> (length search-text) 0)
              (search-and-hightlight search-text start-point end-point))
            ))
         ))
      )))


;;;###autoload
(defun guan/flash-jump ()
  (interactive)
  (condition-case nil
      (live-grepper)
    (quit (progn
            (guan/--toggle-grey-background)
            (guan/--clear-all-highlights)
            (keyboard-quit)))))
