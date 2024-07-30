;;; customizations/config/flash.el -*- lexical-binding: t; -*-

(defface guan-background-face
  '((t (:foreground "gray40")))
  "Face for whole window background during selection.")

(defface guan-highlight-face
  '((t (:inherit highlight)))
  "Face for matches during reading chars using `avy-goto-char-timer'.")

(defface guan-lead-face
  '((t (:foreground "white" :background "#4f57f9")))
  "Face used for first non-terminating leading chars.")

(defcustom guan-del-last-char-by '(?\b ?\d)
  "List of event types, i.e. key presses, that delete the last
character read.  The default represents `C-h' and `DEL'.  See
`event-convert-list'."
  :type 'list)

(setq guan--overlay-state nil)

(defvar guan--overlays '())

;; guan--leader-keys: ((CHAR . POSITION))
(setq guan--label-positions '())

;; abcdefghijklmnopqrstuvwxyz
(defvar guan-flash-labels '(
                            ?a ?b ?c ?d ?e
                            ?f ?g ?h ?i ?j
                            ?k ?l ?m ?n ?o
                            ?p ?q ?r ?s ?t
                            ?u ?v ?w ?x ?y ?z
                            ))

(defun guan/--toggle-grey-background (&optional start end)
  (if (equal nil guan--overlay-state)
      (let* ((beg (if (equal nil start) (window-start) start))
             (end (if (equal nil end) (window-end) end))
             (ov (make-overlay beg end (window-buffer))))
        (overlay-put ov 'face 'guan-background-face)
        (setq guan--overlay-state ov)
        )
    (progn
      (delete-overlay guan--overlay-state)
      (setq guan--overlay-state nil)
      )
    )
  )

(defun guan/--clear-all-highlights ()
  (when (< 0 (length guan--overlays))
    (dolist (ov guan--overlays)
      (delete-overlay ov))
    )
  (setq guan--overlays '()))

;; search-and-hightlight
;; CANDIDATES '((MATCH-START . OV))
(defun search-and-hightlight (text start end)
  (let ((current-pos (point))
        candidates real-tails usable-labels
        tmp-label tmp-ov)
    (save-excursion
      (progn
        (guan/--clear-all-highlights)
        (setq guan--label-positions '())
        (goto-char start)
        ;; search all matches
        (while (re-search-forward text end t)
          (let* ((match-start (- (point) (length text)))
                 (ov (make-overlay match-start (point)))
                 (tail (char-after (point))))
            (if (not (member tail real-tails))
                (push tail real-tails))
            (push (list match-start ov) candidates)
            )
          )
        ;; sort candidates by distance
        (setq candidates (cl-sort candidates #'< :key (lambda (one-candidate) (abs (- current-pos (car one-candidate))))))
        ;; final usable label set
        (dolist (label guan-flash-labels)
          (when (not (member label real-tails))
            (push label usable-labels)))
        ;; highlight search text
        (while (> (length candidates) 0)
          (setq tmp-ov (car (cdr (car candidates))))
          (overlay-put tmp-ov 'face 'guan-highlight-face)
          (push tmp-ov guan--overlays)
          (when (> (length usable-labels) 0)
            (setq tmp-label (car usable-labels))
            (overlay-put tmp-ov 'after-string (propertize (char-to-string tmp-label) 'face 'guan-lead-face))
            (push (list tmp-label (car (car candidates))) guan--label-positions)
            ;; delete one from usable-labels
            (setq usable-labels (cdr usable-labels))
            )
          ;; delete one from candidates
          (setq candidates (cdr candidates))
          )
        )
      )
    )
  )

(defun guan/--clean-up ()
  (guan/--toggle-grey-background)
  (guan/--clear-all-highlights)
  (setq guan--label-positions '())
  )

(defun live-grepper ()
  (let ((start-point (window-start))
        (end-point (window-end))
        (search-text "")
        char break labels)
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
        (setq labels (mapcar (lambda (one) (car one)) guan--label-positions))
        (cond
         ;; Handle ESC
         ((= char 27) (keyboard-quit))
         ;; Handle RET
         ((= char 13) (keyboard-quit))
         ;; Handle C-h, DEL
         ((memq char guan-del-last-char-by)
          (let ((l (length search-text)))
            (when (>= l 1) (setq search-text (substring search-text 0 (1- l)))
                  (if (> (length search-text) 0)
                      (search-and-hightlight search-text start-point end-point)
                    (guan/--clear-all-highlights))
                  ))
          )
         ((memq char labels)
          (progn
            (goto-char (car (cdr (assoc char guan--label-positions))))
            (keyboard-quit)
            )
          )
         (t
          (progn
            (setq search-text (concat search-text (char-to-string char)))
            ;; Highlight
            (when (> (length search-text) 0)
              (search-and-hightlight search-text start-point end-point)))
          )
         ))
      )
    ))


(defun guan/flash-jump ()
  (interactive)
  (condition-case nil
      (live-grepper)
    (t (guan/--clean-up))
    )
  )
