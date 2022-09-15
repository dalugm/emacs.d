;;; init-theme.el --- Emacs themes -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Theme configuration.
;;

;;; Code:

;; ---------------------------------------------------------
;; themes
;; ---------------------------------------------------------

(defvar my-dark-theme-alist '(
                              alect-black
                              alect-black-alt
                              alect-dark
                              alect-dark-alt
                              ample
                              ample-flat
                              atom-one-dark
                              challenger-deep
                              chocolate
                              cyberpunk
                              darktooth
                              darktooth-dark
                              darktooth-darker
                              deeper-blue
                              doom-1337
                              doom-Iosvkem
                              doom-acario-dark
                              doom-ayu-dark
                              doom-ayu-mirage
                              doom-badger
                              doom-challenger-deep
                              doom-city-lights
                              doom-dark+
                              doom-dracula
                              doom-ephemeral
                              doom-fairy-floss
                              doom-gruvbox
                              doom-henna
                              doom-homage-black
                              doom-horizon
                              doom-ir-black
                              doom-lantern
                              doom-laserwave
                              doom-manegarm
                              doom-material
                              doom-material-dark
                              doom-meltbus
                              doom-miramare
                              doom-molokai
                              doom-monokai-classic
                              doom-monokai-machine
                              doom-monokai-octagon
                              doom-monokai-pro
                              doom-monokai-ristretto
                              doom-monokai-spectrum
                              doom-moonlight
                              doom-nord
                              doom-nord-aurora
                              doom-nova
                              doom-oceanic-next
                              doom-old-hope
                              doom-one
                              doom-opera
                              doom-outrun-electric
                              doom-palenight
                              doom-peacock
                              doom-plain-dark
                              doom-rouge
                              doom-shades-of-purple
                              doom-snazzy
                              doom-solarized-dark
                              doom-solarized-dark-high-contrast
                              doom-sourcerer
                              doom-spacegrey
                              doom-tokyo-night
                              doom-tomorrow-night
                              doom-vibrant
                              doom-wilmersdorf
                              doom-xcode
                              doom-zenburn
                              dracula
                              ef-autumn
                              ef-dark
                              ef-deuteranopia-dark
                              ef-duo-dark
                              ef-trio-dark
                              ef-night
                              ef-winter
                              green-is-the-new-black
                              gruvbox
                              gruvbox-dark-hard
                              gruvbox-dark-medium
                              gruvbox-dark-soft
                              kaolin-aurora
                              kaolin-blossom
                              kaolin-bubblegum
                              kaolin-dark
                              kaolin-eclipse
                              kaolin-galaxy
                              kaolin-mono-dark
                              kaolin-ocean
                              kaolin-shiva
                              kaolin-temple
                              kaolin-valley-dark
                              leuven-dark
                              manoj-dark
                              material
                              misterioso
                              modus-vivendi
                              moe-dark
                              monokai
                              nord
                              sanityinc-solarized-dark
                              sanityinc-tomorrow-blue
                              sanityinc-tomorrow-bright
                              sanityinc-tomorrow-eighties
                              sanityinc-tomorrow-night
                              sinolor-black
                              sinolor-dark
                              sinolor-eva
                              sinolor-green
                              sinolor-palace
                              solarized-dark
                              solarized-dark-high-contrast
                              solarized-gruvbox-dark
                              solarized-selenized-black
                              solarized-selenized-dark
                              solarized-wombat-dark
                              solarized-zenburn
                              spacemacs-dark
                              srcery
                              tango-dark
                              tao-yin
                              tsdh-dark
                              wheatgrass
                              wombat
                              zenburn
                              )
  "Dark themes.")

(defvar my-black-theme-alist '(
                               alect-black
                               alect-black-alt
                               cyberpunk
                               doom-homage-black
                               doom-ir-black
                               doom-meltbus
                               ef-dark
                               manoj-dark
                               modus-vivendi
                               sanityinc-tomorrow-bright
                               sinolor-black
                               sinolor-eva
                               wheatgrass
                               )
  "Themes used in the light-less black environment.")

(defvar my-light-theme-alist '(
                               adwaita
                               alect-light
                               alect-light-alt
                               ample-light
                               dichromacy
                               doom-acario-light
                               doom-ayu-light
                               doom-earl-grey
                               doom-flatwhite
                               doom-gruvbox-light
                               doom-homage-white
                               doom-nord-light
                               doom-one-light
                               doom-opera-light
                               doom-plain
                               doom-solarized-light
                               doom-tomorrow-day
                               ef-day
                               ef-deuteranopia-light
                               ef-duo-light
                               ef-light
                               ef-spring
                               ef-summer
                               ef-trio-light
                               gruvbox-light-hard
                               gruvbox-light-medium
                               gruvbox-light-soft
                               kaolin-breeze
                               kaolin-light
                               kaolin-mono-light
                               kaolin-valley-light
                               leuven
                               light-blue
                               material-light
                               modus-operandi
                               moe-light
                               organic-green
                               sanityinc-solarized-light
                               sanityinc-tomorrow-day
                               sinolor-light
                               solarized-gruvbox-light
                               solarized-light
                               solarized-light-high-contrast
                               solarized-selenized-light
                               solarized-selenized-white
                               spacemacs-light
                               tango
                               tao-yang
                               tsdh-light
                               whiteboard
                               )
  "Light themes.")

;; ---------------------------------------------------------
;; variables
;; ---------------------------------------------------------

(defvar my-theme-alist (append my-light-theme-alist my-dark-theme-alist)
  "Total themes.")

(defvar my--fallback-theme 'sinolor-eva
  "Fallback theme if user theme cannot be applied.")

(defvar my--current-theme nil
  "Internal variable storing currently loaded theme.")

(defvar my-current-theme-index 0
  "Mark current theme index.")

(defvar my-cycle-themes (mapcar #'symbol-name my-theme-alist)
  "Get theme names.")

;; ---------------------------------------------------------
;; functions
;; ---------------------------------------------------------

(defun my--cycle-theme (index)
  "According to INDEX cycle through `my-theme-alist'."
  (setq my-current-theme-index
        (+ index (cl-position
                  (car (mapcar #'symbol-name my-theme-alist))
                  my-cycle-themes :test 'equal)))
  (when (>= my-current-theme-index (length my-cycle-themes))
    (setq my-current-theme-index 0))
  (when (< my-current-theme-index 0)
    (setq my-current-theme-index (- (length my-cycle-themes) 1)))
  (let* ((my--current-theme (nth my-current-theme-index my-cycle-themes))
         (progress-reporter
          (make-progress-reporter
           (format "Loading theme %s..." my--current-theme))))
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme (intern my--current-theme) t)
    (progress-reporter-done progress-reporter)))

(defun my--random-theme (themes)
  "Pickup random color theme from a list of THEMES.
If want to cycle through self-choose themes, e.g. `my-theme-alist',
then use `(my--random-theme my-theme-alist)'.  If use all available
theme, then use `(my--random-theme (custom-available-themes))'."
  (let* ((available-themes (mapcar #'symbol-name themes))
         (theme (nth (random (length available-themes)) available-themes)))
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme (intern theme) t)
    (message "Theme [%s] loaded." theme)))

(provide 'init-theme)

;;; init-theme.el ends here
