;;;;;;;;;;;;;;;;;;; SETUP FOR PACKAGES WHICH I USE ;;;;;;;;;;;;;;;;;;;

;; FOR PACKAGES NOT IN ARCHIVES
;; put elisp package additions in this dir:
(add-to-list 'load-path "~/.emacs.d/lisp")



;;;;;;;;;;;;;;;;;;;;;;;;;; VARIOUS PACKAGES ;;;;;;;;;;;;;;;;;;;;;;;;;;

;; use hydra to define mini keymaps, with hinting and several other neat
;; features
(use-package hydra
  :ensure t)

;; JUMP TO DEFINITION: dumb-jump works for 50+ programming languages
;; with no config required.
;;
;; Uses uses ag, rg or grep behind the scenes - make sure that ag is
;; installed on system for good performance:
;; $ sudo apt-get install silversearcher-ag
(use-package dumb-jump
  :ensure t
  :config
  ;; use dumb-jump as backend for xref
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  ;; a selection of useful commands
  (defhydra dumb-jump-hydra (:color blue :columns 3)
    "Dumb Jump"
    ("j" dumb-jump-go "Go")
    ("o" dumb-jump-go-other-window "Other window")
    ("e" dumb-jump-go-prefer-external "Go external")
    ("x" dumb-jump-go-prefer-external-other-window "Go external other window")
    ("i" dumb-jump-go-prompt "Prompt")
    ("l" dumb-jump-quick-look "Quick look")
    ("b" dumb-jump-back "Back"))
  (global-set-key (kbd "C-M-,") 'dumb-jump-hydra/body))

(use-package shrink-whitespace
  :ensure t
  :bind (("M-\\" . shrink-whitespace)
         ("M-4" . shrink-whitespace)))

;; NOTE: yasnippet may not play well with org-mode (see web for a workaround)
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode t) ; enable yasnippet everywhere
  (global-set-key (kbd "C-c & d") 'yas-describe-tables)
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"
                           "~/.emacs.d/elpa/java-snippets-20160626.1952")))

;; use company for autocompletion
(use-package company
  :ensure t
  :config
  (global-company-mode t)     ; activate company everywhere
  ;; DISABLED company-quickhelp-mode because triggers debugger every time
  ;(company-quickhelp-mode t) ; enable documentation popups
  )

;; completion framework
(use-package ivy
  :ensure t
  :bind (("C-c C-r" . ivy-resume))
  :config
  ;;(ivy-mode t) ; I prefer default behaviour for some things
  (setq ivy-use-virtual-buffers t)) ; includes bookmarks/recent etc in find file

;; extended functionality built on top of ivy
(use-package counsel
  :ensure t
  :bind (("M-x"       . counsel-M-x)
         ("C-x x"     . execute-extended-command)
         ("C-M-y"     . counsel-yank-pop)
         ("C-x C-M-f" . counsel-find-file)
         ("<f7>"      . counsel-recentf)
         ("C-x r b"   . counsel-bookmark)))

;; interactive isearch in current buffer using ivy
(use-package swiper
  :ensure t
  :bind (("C-s" . swiper)
         ("M-s s" . isearch-forward)))

;; replaces ace-jump-mode
(use-package avy
  :ensure t
  :bind (("C-;" . avy-goto-word-or-subword-1)
         ("C-:" . avy-goto-char)))

;; replace keybinding for zap-to-char
(use-package avy-zap
  :ensure t
  :bind (("M-z" . avy-zap-to-char)))

;; swap buffers between two windows i.e. transpose windows
;; ... also see: ace-delete-window & ace-window
(use-package ace-window
  :ensure t
  :bind (("C-c w" . ace-swap-window)))

;; simple project-management
(use-package projectile
  :ensure t
  :config
  (projectile-mode 1)
  (define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map))

;; use ivy/counsel for projectile completion
(use-package counsel-projectile
  :ensure t
  :config (counsel-projectile-mode t))

(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)
         ("C--" . er/contract-region)))

(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C-S-c C-S-n" . mc/mark-next-like-this)
         ("C-S-c C-S-p" . mc/mark-previous-like-this)
         ("C-S-c C-}" . mc/skip-to-next-like-this)
         ("C-S-c C-{" . mc/skip-to-previous-like-this)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)))

(use-package magit
  :ensure t
  :config
  (global-set-key (kbd "C-x g") 'magit-status))

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode t))

;; show useful info in the echo area when minibuffer not active
;; ... good for full screen mode
;; ... set minibuffer-line face in customise
;; M-x customize-face minibuffer-line
(use-package minibuffer-line
  :ensure t
  :config
  ;; do setup first, so that it will display right as soon as it's activated
  (display-battery-mode t)
  (setq minibuffer-line-refresh-interval 20) ; refresh every 20 seconds
  (setq minibuffer-line-format
        '(""
          (:eval (format-time-string "%R | %d-%m-%Y")) ;; time | date
          (:eval (battery-format
                  " | BATTERY: %b%p%%  time=%t"
                  (funcall battery-status-function)))))
  (minibuffer-line-mode t))

;; interface to external debuggers
(use-package realgud
  :ensure t)



;;;;;;;;;;;;;;;;;;;;;;;;;;;; SMARTPARENS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package smartparens
  :ensure t
  :config
  (smartparens-global-mode 1)
  (show-smartparens-global-mode 1)
  (sp-use-smartparens-bindings))

(sp-with-modes sp--lisp-modes
  ;; disable ', it's the quote character!
  (sp-local-pair "'" nil :actions nil)
  ;; also only use the pseudo-quote inside strings where it serve as
  ;; hyperlink.
  (sp-local-pair "`" "'" :when '(sp-in-string-p sp-in-comment-p))
  (sp-local-pair "`" nil
                 :skip-match (lambda (ms mb me)
                               (cond
                                ((equal ms "'")
                                 (or (sp--org-skip-markup ms mb me)
                                     (not (sp-point-in-string-or-comment))))
                                (t (not (sp-point-in-string-or-comment)))))))

(sp-local-pair 'java-mode "/**" "*/")

(sp-local-pair 'python-mode "\"\"\"" "\"\"\"")
(sp-local-pair 'python-mode "'''" "'''")



;;;;;;;;;;;;;;;;;;;;;;;;;; LISP PROGRAMING ;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 'compilation' key for lisp modes
(define-key lisp-mode-map (kbd "<f6>") 'load-file)
(define-key emacs-lisp-mode-map (kbd "<f6>") 'load-file)

;; EMACS LISP
;; Use nameless to simulate namespaces in elisp
(use-package nameless
  :ensure t)
(add-hook 'emacs-lisp-mode-hook #'nameless-mode)
(add-hook 'emacs-lisp-mode-hook 'fci-mode)

;; COMMON LISP (and SLIME)
;; Installation instructions quicklisp, slime and ccl in:
;;     backup-and-new-install.org
;;(load (expand-file-name "~/quicklisp/slime-helper.el"))
;;(setq inferior-lisp-program "ccl64")
;; can now do M-x slime

;; ERT (Emacs Regression Testing)
(defun bsc-ert-list-tests ()
  "Lists all current tests..."
  (interactive)
  (message "%s" (mapcar 'ert-test-name (ert-select-tests t t))))



;;;;;;;;;;;;;;;;;;;;;;;;;; JAVA PROGRAMMING ;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package gradle-mode
  :ensure t)

(use-package meghanada
  :ensure t)

;; If non-nil meghanada-mode will be started with java-mode
(setq bsc-use-meghanada t)

(defun bsc-toggle-use-meghanada ()
  (interactive)
  (if bsc-use-meghanada
      (setq bsc-use-meghanada nil)
    (setq bsc-use-meghanada t))
  (message "Start meghanada-mode with java-mode: %s" bsc-use-meghanada))

;; activate and setup gradle and meghanada minor modes with java-mode
(add-hook 'java-mode-hook
          (lambda ()
            (setq c-basic-offset 4) ; tab-indentation width
            (setq fill-column 90) ; java code can be a bit verbose (who has an 80 width now?)
            (gradle-mode t)
            (define-key gradle-mode-map (kbd "<f6>") 'gradle-execute)
            (setq gradle-use-gradlew t)
            (if bsc-use-meghanada
                (meghanada-mode t))
            ;; use code format
            ;; (add-hook 'before-save-hook 'meghanada-code-beautify-before-save)
            ))

(add-hook 'java-mode-hook 'fci-mode)

;; javadoc-lookup
(global-set-key (kbd "C-h j") 'javadoc-lookup)



;;;;;;;;;;;;;;;;;;;;;;;;; PYTHON PROGRAMMING ;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package elpy
     :ensure t
     :disabled
     :setup
     (elpy-enable))

(add-hook 'python-mode-hook 'fci-mode)



;;;;;;;;;;;;;;;;;;;;;;;; ARDUINO PROGRAMMING ;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; NOTE: arduino-cli-mode is kind of broken at the moment
;;;; - LIST BOARDS works ok but COMPILE & UPLOAD don't

(defhydra hydra-arduino (:color blue)
  "arduino cli"
  ("c" arduino-cli-compile "compile")
  ("u" arduino-cli-upload "upload")
  ("l" arduino-cli-board-list "list boards"))

(use-package arduino-cli-mode
  :ensure t
  ;; :hook arduino-mode
  :mode "\\.ino\\'"
  :bind (("C-c a" . hydra-arduino/body))
  :custom
  (arduino-cli-warnings 'all)
  (arduino-cli-verify t))



;;;;;;;;;;;;;;;;;;;;;;;;;; MISC PROGRAMMING ;;;;;;;;;;;;;;;;;;;;;;;;;;

;; WEB MODE
;; major mode which can mix html, php, and various other web languages...
(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (defun my-web-mode-hook ()
    "Hooks for Web mode."
    (setq web-mode-markup-indent-offset 2) ; set indentation level
    (add-hook 'web-mode-hook  'my-web-mode-hook)))

;; KOTLIN
(use-package kotlin-mode
  :disabled
  :ensure t)

;; SCALA
;; autoload scala-mode when it is first activated
(autoload 'scala-mode "scala-mode" "Scala mode" t)
;; automatically use scala mode for editing files with .scala suffix
(setq auto-mode-alist (append '(("\\.scala$" . scala-mode)) auto-mode-alist))

;; OCTAVE/MATLAB
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

;; JAVASCRIPT
(use-package js
  :config
  (setf js-indent-level 2))

;; GLSL
(use-package glsl-mode
  :ensure t)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ORG MODE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package org
  :ensure t
  :config
  (setq org-log-done 'time) ; insert timestamp when TODO is DONE
  ;; make windmove work in areas where there is no special functionality on S-<cursor>
  (add-hook 'org-shiftup-final-hook    'windmove-up)
  (add-hook 'org-shiftdown-final-hook  'windmove-down)
  (add-hook 'org-shiftleft-final-hook  'windmove-left)
  (add-hook 'org-shiftright-final-hook 'windmove-right)
  ;; require org-export to be loaded, otherwise my custom export backend
  ;; definitions will cause an error on org-mode startup
  (require 'ox-org)
  ;; enable code-block execution
  (require 'ob-shell) ; used for org babel shell code snippets
  ;;(require 'ob-kotlin)
  (org-babel-do-load-languages
   'org-babel-load-languages '((C . t)
                               (shell . t)
                               (java . t)
                               (python . t)
                               ;;(kotlin . t)
			       ))
  ;; directory for notes.org
  (setq org-directory "~/.emacs.d/org")
  (setq org-default-notes-file (concat org-directory "/notes.org"))
  (setq org-journal-file (concat org-directory "/journal.org"))
  (setq org-quotations-file (concat org-directory "/quotations.org"))
  ;; org-roam setup


  ;; global keybindings accessible in all other modes
  (global-set-key (kbd "C-c a") 'org-agenda) ; show agenda views
  (global-set-key (kbd "C-c c") 'org-capture)
  (global-set-key (kbd "C-c l") 'org-store-link) ; store org link to current location
  (global-set-key (kbd "C-c b") 'org-iswitchb) ; switch between org buffers
  ;;org-capture templates
  (setq org-capture-templates
        '(("t"
           "Todo"
           entry
           (file+headline org-default-notes-file "Tasks")
           "* TODO %?\n %i\n %a")
          ("b"
           "Tidbits"
           entry
           (file+headline org-default-notes-file "Tidbits")
           "* %U %^{Title}\n%?")
          ("a"
           "Art (research/inspiration etc)"
           entry
           (file+headline org-default-notes-file "Art")
           "* %?")
          ("j"
           "Journal entry"
           entry
           (file+datetree org-journal-file)
           "* %U %^{Title}\n%?")
          ("u"
           "Quotation"
           entry
           (file org-quotations-file)
           "* %U %^{Title}\n%?"))))

;; (eval-after-load 'org-mode
;;   '(push '("html" . web) org-src-lang-modes))

;; HYDRA FOR ORG-AGENDA
(defun org-agenda-cts ()
  (and (eq major-mode 'org-agenda-mode)
       (let ((args (get-text-property
                    (min (1- (point-max)) (point))
                    'org-last-args)))
         (nth 2 args))))

(defhydra hydra-org-agenda-view (:hint none)
  "
_d_: ?d? day        _g_: time grid=?g?  _a_: arch-trees
_w_: ?w? week       _[_: inactive       _A_: arch-files
_t_: ?t? fortnight  _f_: follow=?f?     _r_: clock report=?r?
_m_: ?m? month      _e_: entry text=?e? _D_: include diary=?D?
_y_: ?y? year       _q_: quit           _L__l__c_: log = ?l?"
  ("SPC" org-agenda-reset-view)
  ("d" org-agenda-day-view (if (eq 'day (org-agenda-cts)) "[x]" "[ ]"))
  ("w" org-agenda-week-view (if (eq 'week (org-agenda-cts)) "[x]" "[ ]"))
  ("t" org-agenda-fortnight-view (if (eq 'fortnight (org-agenda-cts)) "[x]" "[ ]"))
  ("m" org-agenda-month-view (if (eq 'month (org-agenda-cts)) "[x]" "[ ]"))
  ("y" org-agenda-year-view (if (eq 'year (org-agenda-cts)) "[x]" "[ ]"))
  ("l" org-agenda-log-mode (format "% -3S" org-agenda-show-log))
  ("L" (org-agenda-log-mode '(4)))
  ("c" (org-agenda-log-mode 'clockcheck))
  ("f" org-agenda-follow-mode (format "% -3S" org-agenda-follow-mode))
  ("a" org-agenda-archives-mode)
  ("A" (org-agenda-archives-mode 'files))
  ("r" org-agenda-clockreport-mode (format "% -3S" org-agenda-clockreport-mode))
  ("e" org-agenda-entry-text-mode (format "% -3S" org-agenda-entry-text-mode))
  ("g" org-agenda-toggle-time-grid (format "% -3S" org-agenda-use-time-grid))
  ("D" org-agenda-toggle-diary (format "% -3S" org-agenda-include-diary))
  ("!" org-agenda-toggle-deadlines)
  ("[" (let ((org-agenda-include-inactive-timestamps t))
         (org-agenda-check-type t 'timeline 'agenda)
         (org-agenda-redo)
         (message "Display now includes inactive timestamps as well")))
  ("q" (message "Abort") :exit t)
  ("v" nil))

(eval-after-load 'org-agenda
  '(progn
     (define-key org-agenda-mode-map "v" 'hydra-org-agenda-view/body)))



;;;;;;;;;;;;;;;;;;;;; ORX-EXPORT CUSTOM BACKENDS ;;;;;;;;;;;;;;;;;;;;;

(defun bst-org-plain-text-headline (headline contents info)
  (message "bst----headline")
  ;; (if (not (org-string-nw-p )))
  ;; (format "headline contents: %s" (org-element-contents headline)))
  ;; (format "headline info: %s" info))

  "headline")
  ;; ;; "headline21")
  ;; ;; (get-text-property 0 :raw-value headline))
  ;; (if (stringp headline)
  ;;     (concat "FOUND A STRING: " (get-text-property 0 :title headline) "\n")
  ;;   (format "not-string --> type=%s\nraw-value=%s\ncontents=%s\ninfo=%s\nheadline=%s"
  ;;           (type-of headline)
  ;;           ;; (cdr headline)
  ;;           "raw" ;;(plist-get (plist-get headline 'headline) :raw-value)
  ;;           contents
  ;;           "info"
  ;;           "headline")))

(defun bst-org-plain-text-line-break (_line-break _contents _info)
  "Transcode a LINE-BREAK object from Org to ASCII.
CONTENTS is nil.  INFO is a plist holding contextual
  information."
  (message "bst-----line-break")
  " ")

(defun bst-org-plain-text-paragraph (paragraph contents info)
  "Transcode a PARAGRAPH element from Org to ASCII.
CONTENTS is the contents of the paragraph, as a string.  INFO is
the plist used as a communication channel."
                                        ;(replace-regexp-in-string "\\`[ \t]+" "" contents)
  (message "bst----paragraph")
  "paragraph")

(defun bst-org-plain-text-plain-text (text info)
  (let ((unwrapped (replace-regexp-in-string "\n" "" text)))
    (message "bst----plain-text (text=%s)" unwrapped)
    (format "PLAIN-TEXT: %s" unwrapped)))

(defun bst-org-plain-text-section (section contents info)
  (message "bst----section")
  "section")
  ;; (format "section contents: %s\nsection: %s" contents section))

(defun bst-org-export-as-plain-text
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a text buffer.

Export is done in a buffer named \"*Org PLAIN-TEXT Export*\", which
will be displayed when `org-export-show-temporary-export-buffer'
is non-nil."
  (interactive)
  (message "running bst-org-export-as-plain-text")
  (org-export-to-buffer 'bst-plain-text "*Org PLAIN-TEXT Export*"
    async subtreep visible-only body-only ext-plist (lambda () (text-mode))))

;; CUSTOM PLAIN TEXT BACKEND FOR ORG-EXPORT
;; very simple org-export backend - just removes word wrap
;;
;;
;; HIERACHY OF ELEMENTS:
;; headline
;;   section
;;     paragraph
;;
(add-hook 'org-mode-hook
 (lambda ()
   (org-export-define-backend
    'bst-plain-text
    ;; translation alist
    '((headline . bst-org-plain-text-headline)
      (line-break . bst-org-plain-text-line-break)
      (paragraph . bst-org-plain-text-paragraph)
      (plain-text . bst-org-plain-text-plain-text)
      (section . bst-org-plain-text-section))
    :menu-entry
    '(?t 1
	 ((?n "no word-wrap (DOESN'T WORK)"
	      (lambda (a s v b)
		(bst-org-export-as-plain-text))))))))



;; ANOTHER GO AT IT

(defun bst-org-export-unwrap-text (text backend info)
  ;; (concat "TEXT = "
  ;;         (replace-regexp-in-string "\n" " " text))
  ;; )
  ;; (concat "PLAIN>>>> " text))
  (replace-regexp-in-string "\n" " " text))

(defun bst-org-export-filter-para-headlines (text backend info)
  "Remove any headline which has the tag :PARA: - It's contents
will still be included.

I created this filter so that I could use paragraph headings as
an essay-writing technique and then automatically remove them at
export."
  (if (string-match ":PARA:" text)
      ;; omit first line of text
      (substring text (+ 1 (string-match "\n" text)))
    text))

(defun bst-org-export-as-org-no-wrap
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to an Org buffer.

No word-wrap in plain-text sections.

Exclude headings with :PARA: tag (leaving their contents in)."
  (interactive)
  (org-export-to-buffer 'bst-org-no-wrap "*Org ORG Export*"
    async subtreep visible-only body-only ext-plist (lambda () (org-mode))))

;;;; org-export-before-processing-hook
;;;; org-export-before-parsing-hook

(add-hook 'org-mode-hook
	  (lambda ()
            (org-export-define-derived-backend 'bst-org-no-wrap 'org
              :menu-entry
              '(?O 1
                   ((?n "As Org buffer (no word-wrap/exclude :PARA: headings)"
                        (lambda (a s v b)
                          (bst-org-export-as-org-no-wrap)))))
              ;; :translate-alist
              ;; '()
              :filters-alist
              '((:filter-plain-text . bst-org-export-unwrap-text)
                (:filter-headline . bst-org-export-filter-para-headlines)))
            ))

(defun bst-org-no-wrap-headline (headline contents info)
  (message "bst-org-no-wrap-headline")
  ;; (if (not (org-string-nw-p )))
  ;; (format "headline contents: %s" (org-element-contents headline)))
  ;; (format "headline info: %s" info))
  ;; "headline")
  (let* ((title (org-ascii--build-title headline info 10 nil))
         (body (replace-regexp-in-string "\\(.\\)\n\\(.\\)" "\\1 \\2" contents)))
    (concat title "\n\n" body)))

(defun bst-org-export-as-ascii-no-wrap
  (&optional async subtreep visible-only body-only ext-plist)
  (interactive)
  (org-export-to-buffer 'bst-org-ascii-no-wrap "*Org ASCII Export*"
    async subtreep visible-only body-only ext-plist (lambda () (text-mode))))

(add-hook 'org-mode-hook
;; (add-hook 'org-export-before-processing-hook
 (lambda ()
   (org-export-define-derived-backend 'bst-org-ascii-no-wrap 'ascii
				      :menu-entry
				      '(?t 1
					   ((?N "As ASCII buffer (no word-wrap) (WORK IN PROGRESS)"
						(lambda (a s v b)
						  (bst-org-export-as-ascii-no-wrap)))))
				      :translate-alist
				      '((headline . bst-org-no-wrap-headline)
					;; (plain-text . bst-org-no-wrap-plain-text)
					)
				      :filters-alist
				      '((:filter-plain-text . bst-org-export-unwrap-text)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MARKDOWN ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package markdown
;;   :ensure t)



;;;;;;;;;;;;;;;;;; BBDB (FOR CONTACTS/ADDRESS BOOK) ;;;;;;;;;;;;;;;;;;

(use-package bbdb
  :ensure t
  ;; :bind (("a" . bsc-bbdb-add-mail-alias))
  :config
  (bbdb-initialize)
  (add-hook 'mail-setup-hook 'bbdb-mail-aliases)
  (add-hook 'message-setup-hook 'bbdb-mail-aliases))

;;;; CUSTOM BBDB EXTENSIONS

;; attempt to fix issue of * a
;; TODO: fix this!
(defun bsc-bbdb-add-mail-alias (records alias)
  "Add same ALIAS to RECORDS."
  (interactive (list (bbdb-do-records)
                     (bbdb-read-string "Alias: ")))
  (dolist (record records)
    (bbdb-add-mail-alias record alias nil)))

;; (defun bsc-bbdb--month-str-to-number (month-str)
;;   (let ((str (downcase month-str))
;;         (return-val 0))
;;       (if (equal str "jan") (setq return-val 1))
;;       (if (equal str "feb") (setq return-val 2))
;;       (if (equal str "mar") (setq return-val 3))
;;       (if (equal str "apr") (setq return-val 4))
;;       (if (equal str "may") (setq return-val 5))
;;       (if (equal str "jun") (setq return-val 6))
;;       (if (equal str "jul") (setq return-val 7))
;;       (if (equal str "aug") (setq return-val 8))
;;       (if (equal str "sep") (setq return-val 9))
;;       (if (equal str "oct") (setq return-val 10))
;;       (if (equal str "nov") (setq return-val 11))
;;       (if (equal str "dec") (setq return-val 12))
;;       return-val))

(defun bsc--month-str-to-number (month-str)
  (let ((str (downcase month-str))
        (return-val 0))
      (if (equal str "jan") (setq return-val 1))
      (if (equal str "feb") (setq return-val 2))
      (if (equal str "mar") (setq return-val 3))
      (if (equal str "apr") (setq return-val 4))
      (if (equal str "may") (setq return-val 5))
      (if (equal str "jun") (setq return-val 6))
      (if (equal str "jul") (setq return-val 7))
      (if (equal str "aug") (setq return-val 8))
      (if (equal str "sep") (setq return-val 9))
      (if (equal str "oct") (setq return-val 10))
      (if (equal str "nov") (setq return-val 11))
      (if (equal str "dec") (setq return-val 12))
      return-val))

(defun bsc-bbdb--sort-by-dob-ignore-year (a b)
    "Sorting-function for use with SEQ-SORT. Compares two BBDB
  records by 'dob' (date of birth) field. Compares by month and
  day, ignoring the year of birth.

Returns non-nil if A comes before B."
    ;; get the two DOB field values
    (let* ((return-val nil)
           (dob-a (bbdb-record-field a 'dob))
           (dob-b (bbdb-record-field b 'dob))
           ;; extract the month and day from DOB
           (month-a (string-to-number (substring dob-a 3 5)))
           (month-b (string-to-number (substring dob-b 3 5)))
           (day-a (string-to-number (substring dob-a 0 2)))
           (day-b (string-to-number (substring dob-b 0 2))))
      ;; compare month
      (if (< month-a month-b)
          (setq return-val 't))
      ;; if month is equal, then compare day
      (if (and (= month-a month-b) (< day-a day-b))
          (setq return-val 't))
      return-val))

(defun bsc-bbdb-list-birthdays ()
    "Examines the BBDB database and lists all birthdays in order of
  the soonest from the current date. Any record which doesn't
  have a 'dob' field is ignored."
    (interactive)
    (require 'bbdb)
    ;; get sorted list of records and current date
    (let* ((records (seq-sort
                     'bsc-bbdb--sort-by-dob-ignore-year
                     (bbdb-search (bbdb-records) :xfield (cons 'dob ".+"))))
           (time-str (current-time-string))
           ;; (current-month (bsc-bbdb--month-str-to-number (substring time-str 4 7)))
           (current-month (bsc--month-str-to-number (substring time-str 4 7)))
           (current-day (string-to-number (substring time-str 8 10)))
           (current-position 0))

      ;; find soonest birthday relative to today's date and count the number of steps
      (dolist (current records)
        (let* ((name (bbdb-record-name current))
               (dob (bbdb-record-field current 'dob))
               (month (string-to-number (substring dob 3 5)))
               (day (string-to-number (substring dob 0 2))))
          ;; step on current position if date is earlier than current date
          (if (or (< month current-month)
                  (and (= month current-month) (< day current-day)))
              (setq current-position (+ 1 current-position)))))

      ;; rotate list by the required number of steps
      (dotimes (_ current-position)
        (let ((first-record (car records)))
          ;; remove from start
          (setq records (cdr records))
          ;; add to end
          (setf (cdr (last records)) (cons first-record nil))))

      ;; display results in new buffer
      (switch-to-buffer-other-window "*upcoming-birthdays*")
      (erase-buffer) ; erase any existing content in results buffer

      ;; headling message
      (insert (format "UPCOMING BIRTHDAYS (%d found in bbdb database)\n\n"
                      (length records)))
      (insert (format "Current time: %s\n\n" (current-time-string)))

      ;; insert formatted list in buffer, iterating list destructively
      (let ((counter 1)
            (new-year-mark (- (length records) current-position))
            (beginning (point)))
        (while records
          (let* ((current (car records))
                 (name (bbdb-record-name current))
                 (dob (bbdb-record-field current 'dob))
                 (month (string-to-number (substring dob 3 5)))
                 (day (string-to-number (substring dob 0 2))))

            (insert (format "%s: %s DOB: %s\n"
                            counter
                            name
                            dob))

            (if (= counter new-year-mark)
                (insert (format "NEW YEAR\n")))

            (setq counter (+ 1 counter))
            (setq records (cdr records))))

        ;; align date of birth in column layout
        ;; NOTE: parenthesised sub-expression denotes the whitespace to be replaced
        (align-regexp beginning (point) "\\(\\s-*\\)DOB:"))))



;;;;;;;;;;;;;;;;;;;;;;;;;;; FUN AND GAMES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; graded one minute typing test
(use-package typit
  :ensure t)

(defun typit ()
  (interactive)
  (ivy-read "Which kind of typing test:? "
            '(typit-basic-test
              typit-advanced-test
              typit-from-file)
            :action (lambda (x) (funcall (intern x)))))
