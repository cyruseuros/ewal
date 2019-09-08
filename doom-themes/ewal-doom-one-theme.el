;;; ewal-doom-one-theme.el -*- lexical-binding: t; -*-

(require 'ewal-doom-themes)

(defgroup ewal-doom-one-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom ewal-doom-one-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'ewal-doom-one-theme
  :type 'boolean)

(defcustom ewal-doom-one-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'ewal-doom-one-theme
  :type 'boolean)

(defcustom ewal-doom-one-comment-bg ewal-doom-one-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'ewal-doom-one-theme
  :type 'boolean)

(defcustom ewal-doom-one-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'ewal-doom-one-theme
  :type '(choice integer boolean))

(ewal-load-colors)

;;
(def-doom-theme ewal-doom-one
  "A dark theme inspired by Atom One Dark, cutomized with `ewal'."

  ;; name        default   256       16
  ((bg         (ewal-doom-themes-get-color 'background  0))
   (bg-alt     (ewal-doom-themes-get-color 'background -1))
   (base0      (ewal-doom-themes-get-color 'background -5))
   (base1      (ewal-doom-themes-get-color 'background -4))
   (base2      (ewal-doom-themes-get-color 'background -3))
   (base3      (ewal-doom-themes-get-color 'background -2))
   (base4      (ewal-doom-themes-get-color 'background +2))
   (base5      (ewal-doom-themes-get-color 'background +3))
   (base6      (ewal-doom-themes-get-color 'background +4))
   (base7      (ewal-doom-themes-get-color 'background +5))
   (base8      (ewal-doom-themes-get-color 'foreground +1))
   (fg         (ewal-doom-themes-get-color 'foreground  0))
   (fg-alt     (ewal-doom-themes-get-color 'foreground -1))

   (grey       base4)
   (red        (ewal-doom-themes-get-color 'red      0))
   (orange     (ewal-doom-themes-get-color 'red     +2))
   (green      (ewal-doom-themes-get-color 'green    0))
   (teal       (ewal-doom-themes-get-color 'green   +2))
   (yellow     (ewal-doom-themes-get-color 'yellow   0))
   (blue       (ewal-doom-themes-get-color 'blue    +2))
   (dark-blue  (ewal-doom-themes-get-color 'blue     0))
   (magenta    (ewal-doom-themes-get-color 'magenta +2))
   (violet     (ewal-doom-themes-get-color 'magenta  0))
   (cyan       (ewal-doom-themes-get-color 'cyan    +2))
   (dark-cyan  (ewal-doom-themes-get-color 'cyan     0))

   ;; face categories -- required for all themes
   (highlight      blue)
   (vertical-bar   (doom-darken base1 0.1))
   (selection      dark-blue)
   (builtin        magenta)
   (comments       (if ewal-doom-one-brighter-comments dark-cyan base5))
   (doc-comments   (doom-lighten (if ewal-doom-one-brighter-comments dark-cyan base5) 0.25))
   (constants      violet)
   (functions      magenta)
   (keywords       blue)
   (methods        cyan)
   (operators      blue)
   (type           yellow)
   (strings        green)
   (variables      (doom-lighten magenta 0.4))
   (numbers        orange)
   (region         `(,(doom-lighten (car bg-alt) 0.15) ,@(doom-lighten (cdr base1) 0.35)))
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-bright ewal-doom-one-brighter-modeline)
   (-modeline-pad
    (when ewal-doom-one-padded-modeline
      (if (integerp ewal-doom-one-padded-modeline) doom-one-padded-modeline 4)))

   (modeline-fg     nil)
   (modeline-fg-alt base5)

   (modeline-bg
    (if -modeline-bright
        (doom-darken blue 0.475)
      `(,(doom-darken (car bg-alt) 0.15) ,@(cdr base0))))
   (modeline-bg-l
    (if -modeline-bright
        (doom-darken blue 0.45)
      `(,(doom-darken (car bg-alt) 0.1) ,@(cdr base0))))
   (modeline-bg-inactive   `(,(doom-darken (car bg-alt) 0.1) ,@(cdr bg-alt)))
   (modeline-bg-inactive-l `(,(car bg-alt) ,@(cdr base1))))


  ;; --- extra faces ------------------------
  ((elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")

   (evil-goggles-default-face :inherit 'region :background (doom-blend region bg 0.5))

   ((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground fg)

   (font-lock-comment-face
    :foreground comments
    :background (if ewal-doom-one-comment-bg (doom-lighten bg 0.05)))
   (font-lock-doc-face
    :inherit 'font-lock-comment-face
    :foreground doc-comments)

   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis
    :foreground (if -modeline-bright base8 highlight))

   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))

   ;; Doom modeline
   (doom-modeline-bar :background (if -modeline-bright modeline-bg highlight))
   (doom-modeline-buffer-file :inherit 'mode-line-buffer-id :weight 'bold)
   (doom-modeline-buffer-path :inherit 'mode-line-emphasis :weight 'bold)
   (doom-modeline-buffer-project-root :foreground green :weight 'bold)

   ;; ivy-mode
   (ivy-current-match :background dark-blue :distant-foreground base0 :weight 'normal)

   ;; --- major-mode faces -------------------
   ;; css-mode / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)

   ;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   (markdown-code-face :background (doom-lighten base3 0.05))

   ;; org-mode
   (org-hide :foreground hidden)
   (solaire-org-hide-face :foreground hidden))


  ;; --- extra variables ---------------------
  ()
  )

(provide-theme 'ewal-doom-one)

;;; ewal-doom-one-theme.el ends here
