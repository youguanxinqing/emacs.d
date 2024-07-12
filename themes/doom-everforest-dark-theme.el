;;; doom-everforest-dark-theme.el --- inspired by Tomas Restrepo's everforest-dark -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Added: September 15, 2016 (32f8b5ae5feb)
;; Author: Henrik Lissner <https://github.com/hlissner>
;; Maintainer:
;; Source: https://github.com/tomasr/everforest-dark
;;
;;; Commentary:
;;; Code:

(require 'doom-themes)


;;
;;; Variables

(defgroup doom-everforest-dark-theme nil
  "Options for the `doom-everforest-dark' theme."
  :group 'doom-themes)

(defcustom doom-everforest-dark-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-everforest-dark-theme
  :type 'boolean)

(defcustom doom-everforest-dark-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-everforest-dark-theme
  :type 'boolean)

(defcustom doom-everforest-dark-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'doom-everforest-dark-theme
  :type '(choice integer boolean))


;;
;;; Theme definition

(def-doom-theme doom-everforest-dark
  "A dark, vibrant theme inspired by Textmate's Monokai."

  ;; name        gui       256       16
  ((bg         '("#232a2e" "black"   "black"        ))
   (fg         '("#d3c6aa" "#d3c6aa" "brightwhite"  ))

   ;; These are off-color variants of bg/fg, used primarily for `solaire-mode',
   ;; but can also be useful as a basis for subtle highlights (e.g. for hl-line
   ;; or region), especially when paired with the `doom-darken', `doom-lighten',
   ;; and `doom-blend' helper functions.
   (bg-alt     '("#343f44" "black"   "black"        ))
   (fg-alt     '("#514045" "#514045" "white"        ))

   ;; These should represent a spectrum from bg to fg, where base0 is a starker
   ;; bg and base8 is a starker fg. For example, if bg is light grey and fg is
   ;; dark grey, base0 should be white and base8 should be black.
   (base0      '("#2d353b" "black"   "black"        ))
   (base1      '("#343f44" "#343f44" "brightblack"  ))
   (base2      '("#3d484d" "#3d484d" "brightblack"  ))
   (base3      '("#475258" "#475258" "brightblack"  ))
   (base4      '("#4f585e" "#4f585e" "brightblack"  ))
   (base5      '("#56635f" "#56635f" "brightblack"  ))
   (base6      '("#543a48" "#543a48" "brightblack"  ))
   (base7      '("#4d4c43" "#4d4c43" "brightblack"  ))
   (base8      '("#425047" "#425047" "brightwhite"  ))
   (bg-visual      '("#514045" "#514045" "brightwhite"  ))

   (grey       '("#859289" "#859289" "brightblack"  ))
   (red        '("#e67e80" "#e67e80" "red"          ))
   (orange     '("#e69875" "#e69875" "brightred"    ))
   (green      '("#a7c080" "#a7c080" "green"        ))
   (teal       green)
   (aqua       '("#83c092" "#83c092" "brightcyan"   ))
   (yellow     '("#dbbc7f" "#dbbc7f" "yellow"       ))
   (blue       '("#7fbbb3" "#7fbbb3" "brightblue"   ))
   (dark-blue  '("#7fbbb3" "#7fbbb3" "blue"         ))
   (magenta    '("#d699b6" "#d699b6" "magenta"      ))
   (violet     '("#d699b6" "#d699b6" "brightmagenta"))
   (purple     '("#d699b6" "#d699b6" "brightmagenta"))
   (cyan       '("#83c092" "#83c092" "brightcyan"   ))
   (dark-cyan  '("#83c092" "#83c092" "cyan"         ))

   ;; These are the "universal syntax classes" that doom-themes establishes.
   ;; These *must* be included in every doom themes, or your theme will throw an
   ;; error, as they are used in the base theme defined in doom-themes-base.
   (highlight      orange)
   (vertical-bar   (doom-lighten bg 0.1))
   (selection      base5)
   (builtin        purple)
   (comments       (if doom-everforest-dark-brighter-comments violet base5))
   (doc-comments   (if doom-everforest-dark-brighter-comments
                       (doom-lighten violet 0.1)
                     (doom-lighten base5 0.25)))
   (constants      orange)
   (functions      green)
   (keywords       red)
   (methods        green)
   (operators      violet)
   (type           aqua)
   (strings        green)
   (variables      blue)
   (numbers        fg)
   (region         bg-visual)
   (error          red)
   (warning        purple)
   (success        green)
   (vc-modified    cyan)
   (vc-added       (doom-darken green 0.15))
   (vc-deleted     red)

   ;; These are extra color variables used only in this theme; i.e. they aren't
   ;; mandatory for derived themes.
   (modeline-fg          fg)
   (modeline-fg-alt      base4)
   (modeline-bg          (if doom-everforest-dark-brighter-modeline base4 base3))
   (modeline-bg-inactive (doom-darken (if doom-everforest-dark-brighter-modeline
                                          base3
                                        base2)
                                      0.2))
   (org-quote `(,(doom-lighten (car bg) 0.05) "#1f1f1f"))

   (-modeline-pad
    (when doom-everforest-dark-padded-modeline
      (if (integerp doom-everforest-dark-padded-modeline) doom-everforest-dark-padded-modeline 4))))


  ;;;; Base theme face overrides
  ((cursor :background purple)
   (lazy-highlight :background purple :foreground base0 :distant-foreground base0 :bold bold)
   ((line-number &override) :foreground base5 :distant-foreground 'unspecified)
   ((line-number-current-line &override) :foreground base7 :distant-foreground 'unspecified)
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color modeline-bg-inactive)))
   (isearch :foreground base0 :background green)

   ;;;; centaur-tabs
   (centaur-tabs-active-bar-face :background green)
   (centaur-tabs-modified-marker-selected :inherit 'centaur-tabs-selected :foreground green)
   (centaur-tabs-modified-marker-unselected :inherit 'centaur-tabs-unselected :foreground green)
   ;;;; css-mode <built-in> / scss-mode
   (css-proprietary-property :foreground keywords)
   ;;;; doom-modeline
   (doom-modeline-bar :background green)
   (doom-modeline-buffer-file :inherit 'mode-line-buffer-id :weight 'bold)
   (doom-modeline-buffer-path :inherit 'bold :foreground green)
   (doom-modeline-buffer-project-root :foreground green :weight 'bold)
   (doom-modeline-buffer-modified :inherit 'bold :foreground orange)
   ;;;; ediff <built-in>
   (ediff-fine-diff-A :background (doom-blend purple bg 0.3) :weight 'bold)
   ;;;; evil
   (evil-search-highlight-persist-highlight-face :background purple)
   ;;;; evil-snipe
   (evil-snipe-first-match-face :foreground base0 :background green)
   (evil-snipe-matches-face     :foreground green :underline t)
   ;;;; flycheck
   (flycheck-error   :underline `(:style wave :color ,red)    :background base3)
   (flycheck-warning :underline `(:style wave :color ,yellow) :background base3)
   (flycheck-info    :underline `(:style wave :color ,green)  :background base3)
   ;;;; helm
   (helm-swoop-target-line-face :foreground purple :inverse-video t)
   ;;;; ivy
   (ivy-current-match :background base3)
   (ivy-minibuffer-match-face-1 :background base1 :foreground base4)
   ;;;; markdown-mode
   (markdown-blockquote-face :inherit 'italic :foreground dark-blue)
   (markdown-list-face :foreground purple)
   (markdown-pre-face  :foreground cyan)
   (markdown-link-face :inherit 'bold :foreground blue)
   ((markdown-code-face &override) :background (doom-lighten base2 0.045))
   ;;;; neotree
   (neo-dir-link-face   :foreground cyan)
   (neo-expand-btn-face :foreground purple)
   ;;;; outline <built-in>
   ((outline-1 &override) :foreground purple)
   ((outline-2 &override) :foreground orange)
   ;;;; org <built-in>
   (org-ellipsis :foreground orange)
   (org-tag :foreground yellow :bold nil)
   ((org-quote &override) :foreground base7)
   (org-todo :foreground yellow :bold 'inherit)
   (org-list-dt :foreground yellow)
   ;;;; rainbow-delimiters
   (rainbow-delimiters-depth-1-face :foreground purple)
   (rainbow-delimiters-depth-2-face :foreground orange)
   (rainbow-delimiters-depth-3-face :foreground green)
   (rainbow-delimiters-depth-4-face :foreground cyan)
   (rainbow-delimiters-depth-5-face :foreground purple)
   (rainbow-delimiters-depth-6-face :foreground orange)
   (rainbow-delimiters-depth-7-face :foreground green))

  ;;;; Base theme variable overrides
  ;; ()
  )

;;; doom-everforest-dark-theme.el ends here
