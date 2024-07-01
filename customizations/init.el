;;; customizations/init.el -*- lexical-binding: t; -*-

(setq doom-font (font-spec :size 22))

(setq private-custom-file "~/.config/doom/private/custom.el")
(if (file-exists-p private-custom-file)
    (load private-custom-file) nil)
