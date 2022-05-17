;;;;;;;;;;;;;;;;;;;;;;;;;; THEME MANAGEMENT ;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; THEMES SETUP AND CUSTOMIZATIONS ;;;;

(use-package leuven-theme
  :ensure t
  :init
  (eval-after-load 'leuven-theme
    (lambda ()
      (face-spec-set 'minibuffer-line '((t :foreground "blue"
                                           :background "white")))
      (face-spec-set 'hl-line '((t :inherit unspecified
                                   :background "light cyan"))))))

(eval-after-load 'whiteboard-theme
  (lambda ()
    (face-spec-set 'hl-line '((t :inherit unspecified
                                 :background "white")))))

(eval-after-load 'dichromacy-theme
  (lambda ()
    (face-spec-set 'minibuffer-line '((t :foreground "dark violet"
                                         :background "white")))
    (face-spec-set 'hl-line '((t :inherit unspecified
                                 :background "light cyan")))))

(eval-after-load 'moe-light-theme
  (lambda ()
    ;; (face-spec-set 'region '((t :inherit unspecified
    ;;                             :foreground unspecified
    ;;                             :background "yellow")))
    (face-spec-set 'minibuffer-line '((t :foreground "dark violet"
                                         :background "#fdfde7")))
    (face-spec-set 'hl-line '((t :inherit unspecified
                                 :background "light cyan")))))

(eval-after-load 'moe-dark-theme
  (lambda ()
    ;; (face-spec-set 'region '((t :inherit unspecified
    ;;                             :foreground unspecified
    ;;                             :background "#224455")))
    (face-spec-set 'minibuffer-line '((t :foreground "#ff4ea3"
                                         :background "#303030")))
    (face-spec-set 'hl-line '((t :inherit unspecified
                                 :background "gray13")))))

(use-package cyberpunk-theme
  :ensure t
  :init
  (eval-after-load 'cyberpunk-theme
    (lambda ()
      (face-spec-set 'minibuffer-line '((t :foreground "green"
                                           :background "black")))
      (face-spec-set 'hl-line '((t :inherit unspecified
                                   :background "gray13"))))))

;;;; THEME-SWITCHING UTILITIES ;;;;

(defvar *bsc-themes-by-darkness* '("leuven         ; (FAVOURITE) light-theme, white bg, sensible colours"
                                   "whiteboard     ; light-theme, off-white bg, muted colour palette"
                                   "dichromacy     ; light-theme, white bg, high contrast"
                                   "moe-light      ; light-theme, pale bg, super-cute candy colours"
                                   "alect-light    ; light-theme, cream bg, low-contrast, easy on the eyes"
                                   "misterioso     ; mid-theme, grey-blue bg, jewel tones"
                                   "moe-dark       ; mid-theme, mid-grey bg, nice colours, ok contrast"
                                   "ample-flat     ; mid-theme, mid-tones, low-contrast, greyish"
                                   "tango-dark     ; mid-theme, dark-grey bg, rich colours (jewel tones)"
                                   "tsdh-dark      ; mid-theme, 1970s, brownish & orange vibes"
                                   "material       ; mid-theme, dark bluish-grey bg"
                                   "flatland       ; mid-theme, tasteful, dark-grey bg"
                                   "monokai        ; mid-theme, 1970s, brown & yellow vibes"
                                   "dracula        ; mid-theme, purplish"
                                   "deeper-blue    ; mid/dark-theme, dark-blue bg"
                                   "paganini       ; mid/dark-theme, 1970s, grey & blue with orange & green touches"
                                   "toxi           ; dark-theme, dark bg, high contrast, garish colours, toxic green, yellow & red, purple selection"
                                   "cherry-blossom ; dark-theme, dark bg, high contrast, jewel tones (purple & pink)"
                                   "cyberpunk      ; (FAVOURITE) dark-theme, black bg, high contrast, bright colours"
                                   )
  "list of themes ordered from light to dark. Each entry is the
  theme name, followed by at least one space and then a short
  description.")

(defun bsc-disable-all-themes ()
  "Disables all currently enabled themes"
  (interactive)
  (mapc #'disable-theme custom-enabled-themes))

(defadvice load-theme (before theme-dont-propogate activate)
  "disable other theme before loading new one"
  (bsc-disable-all-themes))

(defun bsc-choose-theme ()
  "Switches theme, prompting user to choose from a pre-determined list."
  (interactive)
  (ivy-read "Switch to theme (arranged light to dark):"
            *bsc-themes-by-darkness*
            :action (lambda (x)
                      ;; separate theme name from description
                      (let ((theme-name (car (split-string x " "))))
                        ;; INTERN gets the SYMBOL whose name is X
                        ;; ... otherwise we're just passing a string
                        (load-theme (intern theme-name))))))

;;;; LOAD THEME ON STARTUP ;;;;
;; light or dark theme depending on time of day
;; TODO: maybe measure light level from webcam

;; load light or dark theme based on time of day
(let ((hour (string-to-number (format-time-string "%H"))))
  (if
      (and (>= hour 6) (< hour 18))
      ;; day time
      (progn
        (message "day time: loading light theme")
        (load-theme 'leuven))
    ;; night time
    (progn
      (message "night time: loading dark theme")
      (load-theme 'cyberpunk))))

;;(load-theme 'cyberpunk)

;;;;;;;;;;;;;;;;;;; MISC APPEARANCE RELATED THINGS ;;;;;;;;;;;;;;;;;;;

(scroll-bar-mode -1) ; hide scroll bar

(column-number-mode) ; display column number

(show-paren-mode) ; hilights matching parentheses

(use-package fill-column-indicator
  :ensure t)

;; keep track of the cursor, but not in terminal mode because it tends to
;; obscure the text of the current line
(if (display-graphic-p) ; returns nil if running in terminal mode
    (progn
      ;; hilight line where the active cursor is
      (global-hl-line-mode 1)))
