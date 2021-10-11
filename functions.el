;;;;;;;;;;;;;;;;;;;; CUSTOM EDITING FUNCTIONS ETC ;;;;;;;;;;;;;;;;;;;;

(defun bsc-set-global-text-size (size)
  "Sets text height attribute globally.

Text height is given in 1/10ths of a point, so 100 = 10pt."
  (interactive
   (list (string-to-number
          (read-from-minibuffer
           (format "Current global text size is %d --- set new size: "
                   (face-attribute 'default :height))))))
  (set-face-attribute 'default nil :height size))

(defun bsc-insert-time ()
  "Inserts the current time in string format at the cursor position."
  (interactive)
  (insert (current-time-string)))

(defun bsc-millis-to-readable-time (num)
  "Converts milliseconds to verbose human-readable time and displays in minibuffer.

Conversion guide:
1 second = 1000 millis
1 minute = 60000 millis
1 hour   = 3600000 millis
2 hours  = 7200000 millis
24 hours = 86400000 millis"
  (interactive "nEnter milliseconds to convert: ")
  (let* ((millis (% num 1000))
         (total-seconds (/ num 1000))
         (seconds (% total-seconds 60))
         (total-minutes (/ total-seconds 60))
         (minutes (% total-minutes 60))
         (hours (/ total-minutes 60)))
  (message "%s milliseconds = %s hours, %s minutes, %s seconds, %s milliseconds"
           num hours minutes seconds millis)))

(defun bsc-unfill-paragraph ()
  "Gets rid of all newlines in paragraph at point.

Paragraph is terminated by two newlines in a row.

UNDER CONSTRUCTION: use regex to make more robust"
  (interactive)
  (save-excursion
  (let ((start (search-backward "\n\n" nil t))
        (end (search-forward "\n\n" nil t 2))) ; 2nd instance (we are on top of the first)
    (narrow-to-region (+ start 2)
                      (- end 2))
   ;; (search-backward "\n\n" nil t)
   ;; (search-forward "\n\n" nil t 2)
    (goto-char (point-min))
    (while (search-forward "\n" nil t)
      (replace-match " " nil t))
    (widen))))

(defun bsc-substitute-from-hash (hash rStart rEnd)
  "Substitute each instance of key in hash with it's value.

Operates on region bounded by rStart and rEnd."
  (let ((keys (hash-table-keys hash)))
    (save-excursion
      (narrow-to-region rStart rEnd)
      (dolist (k keys)
        (goto-char (point-min))
        (while (search-forward k nil t)
          (replace-match (gethash k hash) nil t)))
      (widen))))

(defun indented-insert (text)
  "Inserts text, and indents according to mode's normal <TAB> behaviour."
  (indent-for-tab-command)
  (insert text))

(defvar bsc-nonstandard-symbol-substitution-table (make-hash-table :test 'equal))
(puthash "“" "\"" bsc-nonstandard-symbol-substitution-table)
(puthash "”" "\"" bsc-nonstandard-symbol-substitution-table)
(puthash "‘" "'" bsc-nonstandard-symbol-substitution-table)
(puthash "’" "'" bsc-nonstandard-symbol-substitution-table)
(puthash " " "_" bsc-nonstandard-symbol-substitution-table)
(puthash "…" "..." bsc-nonstandard-symbol-substitution-table) ; elipsis
(puthash "–" "-" bsc-nonstandard-symbol-substitution-table)
(puthash "—" "-" bsc-nonstandard-symbol-substitution-table)
(puthash "é" "e" bsc-nonstandard-symbol-substitution-table) ; acute accent
(puthash "è" "e" bsc-nonstandard-symbol-substitution-table) ; grave accent
(puthash "⁂" "*" bsc-nonstandard-symbol-substitution-table)
(puthash "Æ" "AE" bsc-nonstandard-symbol-substitution-table)
;; (puthash "_" "" bsc-nonstandard-symbol-substitution-table)

(defun bsc-substitute-nonstandard-symbols (rStart rEnd)
  "Substitutes non-standard characters in region with their
ascii equivalents.

Character and replacement definitions taken from
bsc-nonstandard-symbol-substitution-table."
  (interactive "r")
  (bsc-substitute-from-hash bsc-nonstandard-symbol-substitution-table rStart rEnd))



;;;;;;;;;;;;;;;;;;;;;;;;; SEPARATOR COMMENTS ;;;;;;;;;;;;;;;;;;;;;;;;;

(defun insert-separator-comment-generic (label start-str pad-char end-str)
  "Inserts a single line separator comment, in the format: start-str-------end-str."
  (let* ((line-len (- 70 (length start-str) (length end-str)))
         ;; if label is empty string, don't put spaces around it
         (adjusted-label (if (= 0 (length label))
                             label
                             (concat " " label " ")))
	 (label-len (length adjusted-label))
	 (part-len (/ (- line-len label-len) 2)))
    (indented-insert (concat start-str
                             (make-string part-len pad-char)
			     adjusted-label
			     (make-string (+ part-len (% label-len 2)) pad-char)
                             end-str "\n"))))

(defun insert-separator-comment-generic-3-line (label start-str pad-char end-str)
  "Inserts a three line separator comment, in the format: start-str-------end-str."
  (let* ((line-len (- 70 4))
	 (label-len (+ 2 (length label)))
	 (part-len (/ (- line-len label-len) 2)))
    (indented-insert (concat start-str (make-string line-len pad-char) end-str "\n"))
    (indented-insert (concat start-str (make-string part-len pad-char)
			     " " label " "
			     (make-string (+ part-len (% label-len 2)) pad-char) end-str "\n"))
    (indented-insert (concat start-str (make-string line-len pad-char) end-str "\n"))))

(defun insert-separator-comment-python (label)
  "Inserts a three line separator comment, prompting user for label text.
 Uses #----# format, suitable python."
  (interactive "sLabel: ")
  (insert-separator-comment-generic label "##" ?# "##"))

(defun insert-separator-comment-c (label)
  "Inserts a three line separator comment, prompting user for label text.
 Uses #----# format, suitable C, C++, java, Scala etc."
  (interactive "sLabel: ")
  (insert-separator-comment-generic label "/*" ?- "*/"))

(defun insert-separator-comment-lisp (label)
  "Inserts a three line separator comment, prompting user for label text.
 Uses #----# format, suitable Lisp."
  (interactive "sLabel: ")
  (insert-separator-comment-generic label ";;" ?\; ";;"))

(defun insert-separator-comment-html (label)
  "Inserts a single line separator comment, prompting user for label text."
  (interactive "sLabel: ")
  (insert-separator-comment-generic label "<!-- " ?- " -->"))

(defun insert-separator-comment-org (label)
  "Inserts a single line separator comment, prompting user for label text."
  (interactive "sLabel: ")
  (insert-separator-comment-generic label "* " ?= ""))

(defun bsc-insert-separator-comment (label)
  "Inserts a three line separator comment, prompting user for label text.
Looks at current major mode and tries to insert correct comment type.
Currently supports: c, java, python, lisp, html..."
  (interactive "sLabel: ")
  (cond ((or (eq major-mode 'python-mode)
             (eq major-mode 'perl-mode)
             (eq major-mode 'ruby-mode)
             (eq major-mode 'shell-script-mode)) (insert-separator-comment-python label))
        ((or (eq major-mode 'c-mode)
             (eq major-mode 'css-mode)
             (eq major-mode 'php-mode)
             (eq major-mode 'java-mode)) (insert-separator-comment-c label))
        ((or (eq major-mode 'emacs-lisp-mode)
             (eq major-mode 'lisp-mode)
             (eq major-mode 'scheme-mode)) (insert-separator-comment-lisp label))
        ((or (eq major-mode 'html-mode)
             (eq major-mode 'web-mode)) (insert-separator-comment-html label))
        ((eq major-mode 'org-mode) (insert-separator-comment-org label))
        (t (insert-separator-comment-generic label "==" ?= "==")))) ; default



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; HTML ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bsc-insert-tag (tagname)
  "Inserts a sgml style tag <tagname></tagname> around the cursor point."
  (interactive "sInsert tag: ")
  (let ((opentag (concat "<" tagname ">"))
	(closetag (concat "</" tagname ">")))
    (insert (concat opentag closetag))
    (backward-char (length closetag))))

(defun bsc-wrap-markup (tagname)
  "Inserts a markup <tagname></tagname> around the selected region."
  (interactive "sWrap Markup: ")
  (let ((opentag (concat "<" tagname ">"))
	(closetag (concat "</" tagname ">")))
    ;; get position for end tag
    (setq jump-dest (+ (region-beginning)
		       (length opentag)
		       (- (region-end) (region-beginning)))) ; region length
    (goto-char (region-beginning)) (insert opentag)
    (goto-char jump-dest) (insert closetag)))

(defvar bsc-char-html-entities (make-hash-table :test 'equal))
(puthash "“" "&ldquo;" bsc-char-html-entities) ; &#8220;
(puthash "”" "&rdquo;" bsc-char-html-entities) ; &#8221;
(puthash "‘" "&#8216;" bsc-char-html-entities)
(puthash "’" "&#8217;" bsc-char-html-entities)
;;(puthash """ "&#8217;" bsc-char-html-entities)
(puthash " " " " bsc-char-html-entities)
(puthash "…" "&hellip;" bsc-char-html-entities) ; &hellip;
(puthash "(" "&#40;" bsc-char-html-entities)
(puthash ")" "&#41;" bsc-char-html-entities)
(puthash "–" "-" bsc-char-html-entities)
(puthash "é" "&#233;" bsc-char-html-entities) ; acute accent
(puthash "è" "&#232;" bsc-char-html-entities) ; grave accent

(defun bsc-substitute-html-entities (rStart rEnd)
  "Substitutes several special characters in region with their
html entity equivalents.

Character and replacement definitions taken from bsc-char-html-entities."
  (interactive "r")
  (bsc-substitute-from-hash bsc-char-html-entities rStart rEnd))



;;;;;;;;;;;;;;;;;;;;;;;;;; FUN/SILLY STUFF ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bsc-animate-text (text)
  "Uses animate.el to display animated multi-line text."
  (interactive "sInput text to animate (use semicolon for newline): ")
  (switch-to-buffer
   ;; make buffer, or switch to it if it already exists
   (get-buffer-create "*bsc-animate-text-buffer*"))
  (erase-buffer)
  (let ((parts (split-string text ";"))
        (n 0))
    (while parts
      (sit-for 1) ; pause for one second
      (animate-string (car parts) (+ n 10))
      (setq parts (cdr parts))
      (setq n (+ 2 n)))))
