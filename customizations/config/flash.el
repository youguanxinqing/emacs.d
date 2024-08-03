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

(setq guan--grey-overlays nil)

(defvar guan--overlays '())

;; guan--label-positions: ((LABEL WINDOW POSITION))
(setq guan--label-positions '())

(defun --extract-label-position (key label-position)
  (cond ((string-equal key "label") (nth 0 label-position))
        ((string-equal key "window") (nth 1 label-position))
        ((string-equal key "position") (nth 2 label-position))))

(defun --make-label-position (label position window)
  `(,label ,position ,window))

;; abcdefghijklmnopqrstuvwxyz
(defvar guan-flash-labels '(
                            ?a ?b ?c ?d ?e
                            ?f ?g ?h ?i ?j
                            ?k ?l ?m ?n ?o
                            ?p ?q ?r ?s ?t
                            ?u ?v ?w ?x ?y ?z
                            ))

(defun guan/--toggle-grey-background (&optional start end)
  (if (> (length guan--grey-overlays) 0)
      (while (> (length guan--grey-overlays) 0)
        (delete-overlay (car guan--grey-overlays))
        (setq guan--grey-overlays (cdr guan--grey-overlays))
        )
    (dolist (window (--windows))
      (save-window-excursion
        (when (not (equal window (selected-window)))
          (select-window window))
        (let* ((beg (if (equal nil start) (window-start) start))
               (end (if (equal nil end) (window-end) end))
               (ov (make-overlay beg end (window-buffer))))
          (overlay-put ov 'face 'guan-background-face)
          (push ov guan--grey-overlays))
        )
      )
    )
  )

(defun guan/--clear-all-highlights ()
  (when (< 0 (length guan--overlays))
    (dolist (ov guan--overlays)
      (delete-overlay ov))
    )
  (setq guan--overlays '()))

(defun --windows ()
  (let (windows)
    (save-window-excursion
      (dolist (window (window-list))
        (select-window window)
        (unless (string-equal (buffer-name) "*Messages*")
          (push window windows))))
    windows
    ))

(defun --extract-candidate (key candidate)
  (cond ((string-equal key "window") (nth 0 candidate))
        ((string-equal key "position") (nth 1 candidate))
        ((string-equal key "ov") (nth 2 candidate))))

(defun --make-candidate (window position ov)
  `(,window ,position ,ov))

;; search-and-hightlight
;; CANDIDATES '((WINDOW MATCH-START OV))
(defun search-and-hightlight (text)
  (let ((current-pos (point))
        (focus-window (selected-window))
        candidates duplicate-queue
        real-tails usable-labels
        tmp-label tmp-ov)
    (save-excursion
      (save-window-excursion
        (progn
          (guan/--clear-all-highlights)
          (setq guan--label-positions '())

          (dolist (window (--windows))
            (select-window window)

            (goto-char (window-start))
            ;; search all matches
            (while (re-search-forward text (window-end) t)
              (let* ((match-start (- (point) (length text)))
                     (ov (make-overlay match-start (point)))
                     (tail (char-after (point)))
                     (uniq-key (format "%s-%d" (buffer-name) match-start)))
                (if (not (member tail real-tails))
                    (push tail real-tails))
                (unless (or (and (eq match-start current-pos) (eq window focus-window))
                            (member uniq-key duplicate-queue) )
                  (push (--make-candidate (selected-window) match-start ov)  candidates)
                  (push uniq-key  duplicate-queue))
                )))
          ;; sort candidates by distance
          (setq candidates (cl-sort candidates
                                    (lambda (a b)
                                      (and (<= (nth 0 a) (nth 0 b))
                                           (<= (nth 1 a) (nth 1 b))))
                                    :key (lambda (one-candidate)
                                           (list (--window-to-number focus-window (--extract-candidate "window" one-candidate))
                                                 (abs (- current-pos (--extract-candidate "position" one-candidate)))))
                                    ))
          ;; final usable label set
          (dolist (label guan-flash-labels)
            (when (not (member label real-tails))
              (push label usable-labels)))
          ;; highlight search text
          (while (> (length candidates) 0)
            (setq tmp-ov (--extract-candidate "ov" (car candidates)))
            (overlay-put tmp-ov 'face 'guan-highlight-face)
            (overlay-put tmp-ov 'priority 999)
            (push tmp-ov guan--overlays)
            (when (> (length usable-labels) 0)
              (setq tmp-label (car usable-labels))
              (overlay-put tmp-ov 'after-string (propertize (char-to-string tmp-label) 'face 'guan-lead-face))
              (push (--make-label-position tmp-label
                                           (--extract-candidate "window" (car candidates))
                                           (--extract-candidate "position" (car candidates)))
                    guan--label-positions)
              ;; delete one from usable-labels
              (setq usable-labels (cdr usable-labels)))
            ;; delete one from candidates
            (setq candidates (cdr candidates)))
          )
        )
      )
    )
  )

;; window string format: #<window id on ...>
;; return 0 if window that passed is current window
(defun --window-to-number (current-window window)
  (cond ((eq window current-window) 0)
        (t (let (number)
             (save-match-data
               (and (string-match "#<window \\([0-9]+\\) on .*" (format "%s" (selected-window)))
                    (setq number (match-string 1 (format "%s" (selected-window))))))
             (string-to-number number)
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
  (let ((search-text "")
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
                      (search-and-hightlight search-text)
                    (guan/--clear-all-highlights))
                  ))
          )
         ((memq char labels)
          (let* ((label-position (assoc char guan--label-positions))
                 (target-window (--extract-label-position "window" label-position))
                 (target-position (--extract-label-position "position" label-position)))
            (when (not (eq (selected-window) target-window))
              (select-window target-window))
            (goto-char target-position)
            (keyboard-quit)
            ))
         (t
          (progn
            (setq search-text (concat search-text (char-to-string char)))
            ;; Highlight
            (when (> (length search-text) 0)
              (search-and-hightlight search-text)))
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
