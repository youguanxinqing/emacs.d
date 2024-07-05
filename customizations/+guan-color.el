;;; customizations/color.el -*- lexical-binding: t; -*-

;;; Code:
(defface guan-match-light-green
  '((((class color) (background light))
     :foreground "#000000"
     :background "#90EE90")
    (((class color) (background dark))
     :foreground "#000000"
     :background "#90EE90")
    (t
     :inverse-video t))
  "Used for multi highlight symbols."
  :group 're-builder)

(defface guan-match-green-yellow
  '((((class color) (background light))
     :foreground "#000000"
     :background "#ADFF2F")
    (((class color) (background dark))
     :foreground "#000000"
     :background "#ADFF2F")
    (t
     :inverse-video t))
  "Used for multi highlight symbols."
  :group 're-builder)

(defface guan-match-gold
  '((((class color) (background light))
     :foreground "#000000"
     :background "#FFD700")
    (((class color) (background dark))
     :foreground "#000000"
     :background "#FFD700")
    (t
     :inverse-video t))
  "Used for multi highlight symbols."
  :group 're-builder)

(defface guan-match-salmon
  '((((class color) (background light))
     :foreground "#000000"
     :background "#FA8072")
    (((class color) (background dark))
     :foreground "#000000"
     :background "#FA8072")
    (t
     :inverse-video t))
  "Used for multi highlight symbols."
  :group 're-builder)

(defface guan-match-medium-violet-red
  '((((class color) (background light))
     :foreground "#000000"
     :background "#C71585")
    (((class color) (background dark))
     :foreground "#000000"
     :background "#C71585")
    (t
     :inverse-video t))
  "Used for multi highlight symbols."
  :group 're-builder)

(defface guan-match-dark-orange
  '((((class color) (background light))
     :foreground "#000000"
     :background "#FF8C00")
    (((class color) (background dark))
     :foreground "#000000"
     :background "#FF8C00")
    (t
     :inverse-video t))
  "Used for multi highlight symbols."
  :group 're-builder)

(defface guan-match-brown
  '((((class color) (background light))
     :foreground "#000000"
     :background "#A52A2A")
    (((class color) (background dark))
     :foreground "#000000"
     :background "#A52A2A")
    (t
     :inverse-video t))
  "Used for multi highlight symbols."
  :group 're-builder)

(defface guan-match-deep-sky-blue
  '((((class color) (background light))
     :foreground "#000000"
     :background "#00BFFF")
    (((class color) (background dark))
     :foreground "#000000"
     :background "#00BFFF")
    (t
     :inverse-video t))
  "Used for multi highlight symbols."
  :group 're-builder)

(provide '+guan-color.el)
;;; +guan-color.el ends here
