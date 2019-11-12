;;;;;;;;;;;;;;;;;;;;;;;;;; MASTER INIT FILE ;;;;;;;;;;;;;;;;;;;;;;;;;;

;; streamline frame in GUI mode
;; do this first, to prevent them from momentarily showing during startup
(tool-bar-mode -1) ; hide tool bar
(menu-bar-mode -1) ; hide menu bar (press f10 to pop up menu)

;; package and use-package are needed for practically everything else
(require 'package)
;; add package repositories
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize) ; required before use-package will work
(require 'use-package)

;; unset C- and M- digit keys
;; ... can still use C-M-<DIGIT> for numeric prefix...
;; ... do this now (before other config) so that there's no need to worry about
;; un-setting any user-set keybindings
(dotimes (n 10)
  (global-unset-key (kbd (format "C-%d" n)))
  (global-unset-key (kbd (format "M-%d" n))))



;;;;;;;;;;;;;;;;;;;;;;; LOAD OTHER INIT FILES ;;;;;;;;;;;;;;;;;;;;;;;;

(defvar bsc-init-dir (file-name-directory (or load-file-name buffer-file-name)))

;; setup appearance first, so that window doesn't temporarily look different
;; during initialisation
(load-file (concat bsc-init-dir "appearance.el"))

;; setup packages which I use
(load-file (concat bsc-init-dir "packages.el"))

;; custom editing functions etc
(load-file (concat bsc-init-dir "functions.el"))

(load-file (concat bsc-init-dir "keybindings.el"))

;; anything else...
(load-file (concat bsc-init-dir "misc.el"))
