;;;;;;;;;;;;;;;;;;;;;;;;;;; KEYBINDINGS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set up keybindings which I use, including some hydras, and guide-key.
;; Mode-specific keybindings are also defined in other init files.



;; guide-key pops up a list of keybindings after 1 second for the specified prefix keys
(use-package guide-key
  :disabled
  :ensure t
  :config
  (setq guide-key/guide-key-sequence
        '("C-h"
          "C-x n" "C-x p" "C-x r" "C-x v" "C-x 4" "C-x 5" "C-x 6" "C-x 8"
          "C-c"
          "C-S-c"
          "M-o"
          "M-s"))
  (guide-key-mode 1)
  (setq guide-key/recursive-key-sequence-flag t)
  (setq guide-key/popup-window-position 'bottom))



;;;;;;;;;;;;;;;;;;;;; GENERAL GLOBAL KEYBINDINGS ;;;;;;;;;;;;;;;;;;;;;

;; move between panes using SHIFT-[UP/DOWN/LEFT/RIGHT]
(windmove-default-keybindings)

;; use ibuffer instead of list-buffers
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; jump to current file in dired
(global-set-key (kbd "C-x C-j") 'dired-jump)

(global-set-key (kbd "C-M-q") 'bsc-unfill-paragraph)

(global-set-key (kbd "M-K") 'kill-paragraph)

;; set <f5> as compilation shortcut
;; ... set project specific command using compile-command in .dir-locals.el
(global-set-key (kbd "<f5>") 'recompile)

;; useful for navigating files and directories
(global-set-key (kbd "<f8>") 'speedbar)



;;;;;;;;;;;;;;;;;;;;;;;; ADDITIONAL HELP KEYS ;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-h r") 'describe-char)



;;;;;;;;;;;;;;;;;;;;;;; LAPTOP KEYBOARD FIXES ;;;;;;;;;;;;;;;;;;;;;;;;

;; for US keyboard layout (without pound sterling symbol)
(defun bsc-insert-pound-sign () (interactive) (insert "£"))
(global-set-key (kbd "M-2") 'bsc-insert-pound-sign)

;; for using UK keyboard layout on a US keyboard
;; (defun bsc-insert-backslash () (interactive) (insert "\\"))
;; (global-set-key (kbd "M-2") 'bsc-insert-backslash)
;; (defun bsc-insert-pipe () (interactive) (insert "|"))
;; (global-set-key (kbd "M-3") 'bsc-insert-pipe)

(global-set-key (kbd "M-3") 'indent-region)



;;;;;;;;;;;;;;;;;;;;; MODE-SPECIFIC KEYBINDINGS ;;;;;;;;;;;;;;;;;;;;;;

;; default keybinding M-<tab> is overridden by operating system
(eval-after-load 'calc
  '(define-key calc-mode-map (kbd "C-<tab>") 'calc-roll-up))

(eval-after-load 'outline
  '(define-key outline-minor-mode-map (kbd "<f9>") 'outline-toggle-children))



;;;;;;;;;;;;;;;;;;;;;;;;;;;; SOME HYDRAS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; use hydra to define mini keymaps, with hinting and several other neat
;; features

;; most of these borrowed or adapted from the hydra example file, or various web
;; articles

(use-package hydra
  :ensure t)

(global-set-key
 (kbd "C-c f")
 (defhydra hydra-bsc-favourites (:color blue)
   "misc favourites"
   ("a" align-regexp "align regexp")
   ("j" bsc-javadoc-format "format javadoc")
   ("s" bsc-insert-separator-comment "separator comment")
   ("t" bsc-set-global-text-size "global text size")
   ("h" bsc-substitute-html-entities "substitute html entities")
   ("i" bsc-insert-time "insert time")
   ("w" delete-trailing-whitespace "delete whitespace")
   ("T" bsc-insert-tag "insert sgml tag")
   ("W" bsc-wrap-markup "wrap in sgml tag")))

(defhydra hydra-toggle (:color blue)
    "toggle"
    ("c" company-mode "auto-completion")
    ("C" global-company-mode "global auto-completion")
    ("d" toggle-debug-on-error "debug on error")
    ("w" global-subword-mode "subword mode")
    ("SPC" whitespace-mode "whitespace mode")
    ("l" global-linum-mode "line numbers")
    ("n" nameless-mode "nameless mode")
    ("f" auto-fill-mode "auto-fill")
    ("t" toggle-truncate-lines "truncate lines")
    ("r" rectangle-mark-mode "rectangle mark")
    ("p" smartparens-global-mode "smartparens")
    ("o" outline-minor-mode "outline-minor-mode")
    ("y" yas-global-mode "yasnippet")
    ("m" minibuffer-line-mode "minibuffer-line")
    ("M" bsc-toggle-use-meghanada "use meghanada"))

(global-set-key (kbd "C-c t") 'hydra-toggle/body)

(defhydra hydra-apropos (:color blue
                         :hint nil)
  "
_a_propos        _c_ommand
_d_ocumentation  _l_ibrary
_v_ariable       _u_ser-option
^ ^          valu_e_"
  ("a" apropos)
  ("d" apropos-documentation)
  ("v" apropos-variable)
  ("c" apropos-command)
  ("l" apropos-library)
  ("u" apropos-user-option)
  ("e" apropos-value))

(global-set-key (kbd "C-c h") 'hydra-apropos/body)

(global-set-key
 (kbd "M-g")
 (defhydra hydra-error (:color red)
   "goto-error"
   ("a" first-error "first")
   ("n" next-error "next")
   ("p" previous-error "prev")
   ("l" recenter-top-bottom "recenter")
   ("g" goto-line "goto-line" :exit t)
   ("q" nil "quit")))

(global-set-key
 (kbd "C-c s")
 (defhydra hydra-window-splitter (:color red)
  "window splitter"
  ("<left>" hydra-move-splitter-left "left")
  ("<right>" hydra-move-splitter-right "right")
  ("<up>" hydra-move-splitter-up "up")
  ("<down>" hydra-move-splitter-down "down")
  ("b" balance-windows "balance" :exit t)
  ("q" nil "finalise")))

(defun hydra-move-splitter-left (arg)
  "Move window splitter left."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (shrink-window-horizontally arg)
    (enlarge-window-horizontally arg)))

(defun hydra-move-splitter-right (arg)
  "Move window splitter right."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (enlarge-window-horizontally arg)
    (shrink-window-horizontally arg)))

(defun hydra-move-splitter-up (arg)
  "Move window splitter up."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (enlarge-window arg)
    (shrink-window arg)))

(defun hydra-move-splitter-down (arg)
  "Move window splitter down."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (shrink-window arg)
    (enlarge-window arg)))

(defhydra hydra-eww (:color blue)
  "
_&_: external browser   _B_: list bookmarks    _H_: history           _v_: view source
_G_: search             _b_: add bookmark      _l_: back (last) URL   _C_: URL list cookies
_g_: reload             _M-p_: prev bookmark   _r_: forward URL       _D_: paragraph direction
_R_: readable view      _M-n_: next bookmark   _u_: up URL            _E_: character encoding
_w_: copy page URL                           _t_: top URL           _F_: toggle fonts
_d_: download                                _S_: list buffers
"
  ("&" eww-browse-with-external-browser)
  ("G" eww)
  ("g" eww-reload)
  ("R" eww-readable)

  ("B" eww-list-bookmarks)
  ("b" eww-add-bookmark)
  ("M-p" eww-previous-bookmark)
  ("M-n" eww-next-bookmark)

  ("H" eww-list-histories)
  ("l" eww-back-url)
  ("r" eww-forward-url)
  ("u" eww-up-url)
  ("t" eww-top-url)
  ("S" eww-list-buffers)

  ("w" eww-copy-page-url)
  ("d" eww-download)
  ("v" eww-view-source)

  ("C" url-cookie-list)
  ("D" eww-toggle-paragraph-direction)
  ("E" eww-set-character-encoding)
  ("F" eww-toggle-fonts))

(eval-after-load 'eww
  '(progn
     (define-key eww-mode-map "h" 'hydra-eww/body)))
