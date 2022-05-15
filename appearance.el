;;;;;;;;;;;;;;;;;;;;;;;;;; THEME MANAGEMENT ;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; THEMES SETUP AND CUSTOMIZATIONS ;;;;

(use-package cyberpunk-theme
  :ensure t
  :init
  (eval-after-load 'cyberpunk-theme
    (lambda ()
      (face-spec-set 'minibuffer-line '((t :foreground "green"
                                           :background "black")))
      (face-spec-set 'hl-line '((t :background "gray13"))))))

(use-package leuven-theme
  :ensure t
  :init
  (eval-after-load 'leuven-theme
    (lambda ()
      (face-spec-set 'minibuffer-line '((t :foreground "blue"
                                           :background "white")))
      (face-spec-set 'hl-line '((t :inherit unspecified
                               :background "light cyan"))))))

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

(eval-after-load 'whiteboard-theme
  (lambda ()
    (face-spec-set
     'hl-line '((t :background "white")))))

;;;; THEME-SWITCHING UTILITIES ;;;;

(defvar *bsc-themes-by-darkness* '(leuven         ; FAVOURITE (white bg, good sensible colours, good mode support (org, minibuffer, completions etc))
                                   whiteboard
                                   dichromacy     ; FAVOURITE (white bg, high contrast)
                                   moe-light      ; FAVOURITE (pale bg, cute colours)
                                   alect-light    ; GOOD (cream bg)
                                   misterioso     ; grey-blue bg
                                   moe-dark       ; FAVOURITE (mid-grey bg, nice colours, ok contrast)
                                   ample-flat     ; mid-tones, low-contrast, greyish
                                   tango-dark
                                   flatland
                                   tsdh-dark      ; GOOD
                                   material
                                   monokai        ; GOOD (TODO: selection/hl-line)
                                   dracula        ; GOOD (purplish) (TODO: selection)
                                   deeper-blue    ; GOOD (TODO: selection/hl-line)
                                   paganini       ; GOOD (high contrast, dark bg)
                                   cherry-blossom ; GOOD (high contrast, dark bg)
                                   toxi           ; GOOD (garish colours, dark bg)
                                   cyberpunk)     ; FAVOURITE (black bg, bright colours high contrast)
  "list of themes ordered from light to dark")

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
            ;; INTERN gets the SYMBOL whose name is X
            ;; ... otherwise we're just passing a string
            :action (lambda (x) (load-theme (intern x)))))

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
