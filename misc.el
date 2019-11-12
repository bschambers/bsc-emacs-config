(setq-default indent-tabs-mode nil) ; make indentation always use spaces (not tabs)

;; (setq mark-even-if-inactive t)

;; remember history of window configurations...
;; ... undo/redo with C-c <left>/C-c <right
(winner-mode t)

;; remember last position of point in every file visited
(save-place-mode t)

;; save bookmarks every time one is added, instead of only when emacs exits
(setq bookmark-save-flag t)

(global-subword-mode t) ; move through camel-case words nicely

;; keep list of recent files
(recentf-mode t)
;; increase the number of recent files stored
(setq recentf-max-menu-items 50)
(setq recentf-max-saved-items 50)

;; Save whatever’s in the current (system) clipboard before replacing it with
;; the Emacs’ text.  https://github.com/dakrone/eos/blob/master/eos.org
(setq save-interprogram-paste-before-kill t)

;; weblinks will open in eww...
;; ... press '&' at any time to open the page in the system defaut browser
(setq browse-url-browser-function 'eww-browse-url)

;; always add tags tables, rather than starting a new one, and don't ask for permission
(setq tags-add-tables t)
