;;; org-other-agenda-day.el --- Calendar and time blocks (widgets/faces) -*- lexical-binding: t; -*-
;; Render with faces instead of character art for an HTML-like appearance.
;;; Commentary:

;; This file provides rendering primitives for the day and week views:
;; a calendar grid, duration-proportional time blocks, and multi-day bars.
;; It also defines their faces; page assembly lives in
;; `org-other-agenda.el'.

;;; Code:
(require 'cl-lib)
(require 'calendar)
(require 'textui)

;; -- Faces -------------------------------------------------------------
(defface ooa-day-title '((t (:height 1.25 :weight bold)))
  "A large date heading.

Scaling is safe because this line stands alone and shares no columns aligned by
character cells.  Never set `:height' on a face used for grid alignment.  Emacs
counts full-size character cells, while a scaled glyph draws only about 87% of
the width, so subsequent columns drift."
  :group 'org-other-agenda)
(defface ooa-day-meta
  '((((background dark)) (:foreground "#98A1AC"))
    (t (:foreground "#5D6672")))
  "Statistics shown beside the heading."
  :group 'org-other-agenda)
(defface ooa-hour
  '((((background dark)) (:foreground "#6B747F"))
    (t (:foreground "#9AA2AC")))
  "The label shown at exact hours."
  :group 'org-other-agenda)
(defface ooa-rule
  '((((background dark)) (:underline (:color "#262C35" :position 0)))
    (t (:underline (:color "#E2E6EB" :position 0))))
  "A hairline drawn with an underline instead of a character."
  :group 'org-other-agenda)
(defface ooa-blk-work
  '((((background dark)) (:background "#1E3340" :foreground "#DCE8EF"))
    (t (:background "#DCE8EF" :foreground "#16323F")))
  "The background for a work block."
  :group 'org-other-agenda)
(defface ooa-blk-life
  '((((background dark)) (:background "#271F2C" :foreground "#E7DCEC"))
    (t (:background "#EDE6F0" :foreground "#3A2C42")))
  "The background for a life block."
  :group 'org-other-agenda)
(defface ooa-blk-habit
  '((((background dark)) (:background "#1C2A22" :foreground "#DDEFE2"))
    (t (:background "#E1EDE5" :foreground "#24382C")))
  "The background for a habit block."
  :group 'org-other-agenda)
(defface ooa-stripe-work
  '((((background dark)) (:background "#79B0CC")) (t (:background "#2C5F7C")))
  "The left stripe for a work block."
  :group 'org-other-agenda)
(defface ooa-stripe-life
  '((((background dark)) (:background "#B197BE")) (t (:background "#7A5C86")))
  "The left stripe for a life block."
  :group 'org-other-agenda)
(defface ooa-stripe-habit
  '((((background dark)) (:background "#7FB394")) (t (:background "#4A7A5E")))
  "The left stripe for a habit block."
  :group 'org-other-agenda)
(defface ooa-blk-time
  '((((background dark)) (:foreground "#9FC4D6"))
    (t (:foreground "#4A6D80")))
  "Small time text inside a block."
  :group 'org-other-agenda)
(defface ooa-cal-sel
  '((((background dark)) (:box (:line-width (-1 . -1) :color "#79B0CC")
                          :foreground "#79B0CC" :weight bold))
    (t (:box (:line-width (-1 . -1) :color "#2C5F7C")
        :foreground "#2C5F7C" :weight bold)))
  "The selected calendar date uses a box instead of a character for its frame."
  :group 'org-other-agenda)
(defface ooa-cal-today
  '((((background dark)) (:foreground "#D6A65C" :weight bold))
    (t (:foreground "#9C6212" :weight bold)))
  "The current date."
  :group 'org-other-agenda)
(defface ooa-cal-out
  '((((background dark)) (:foreground "#3A424C")) (t (:foreground "#C8CFD6")))
  "A date outside the current month."
  :group 'org-other-agenda)
(defface ooa-cal-dow
  '((((background dark)) (:foreground "#6B747F"))
    (t (:foreground "#9AA2AC")))
  "The weekday header."
  :group 'org-other-agenda)
(defface ooa-dot-sched
  '((((background dark)) (:foreground "#79B0CC"))
    (t (:foreground "#2C5F7C")))
  "A dot indicating scheduled work."
  :group 'org-other-agenda)
(defface ooa-dot-dead
  '((((background dark)) (:foreground "#D6A65C"))
    (t (:foreground "#9C6212")))
  "A dot indicating a deadline."
  :group 'org-other-agenda)

(defun ooa-text--non-empty (value)
  "Return `VALUE', or a single space when `VALUE' is nil or empty."
  (if (or (null value) (string-empty-p value)) " " value))

(define-widget 'ooa-text 'item
  "A line of plain text that preserves text properties.

The `ooa-text' widget exists because TextUI's built-in `item' widget drops
text properties, including `face', from `value' during rendering. Rows that
need coloring must therefore use this widget type.

TextUI rejects a native widget that renders zero characters, so an empty
`value' (an entry without tags, for example) renders as one space; TextUI
then pads the cell to its declared `:width'."
  :format "%v"
  :value-create (lambda (w) (insert (ooa-text--non-empty (widget-get w :value)))))

(define-widget 'ooa-date-cell 'push-button
  "A single date cell in the calendar."
  :button-prefix "" :button-suffix "" :format "%[%v%]"
  :value-create (lambda (w) (insert (widget-value w)))
  :action (lambda (w &optional _e)
            (textui-set-state (current-buffer) :date (widget-get w :ca-abs))))

(defun ooa-day--stripe (kind)
  "Return a one-character colored stripe for `KIND'."
  (propertize " " 'face (cond ((eq kind 'life) 'ooa-stripe-life)
                              ((eq kind 'habit) 'ooa-stripe-habit)
                              (t 'ooa-stripe-work))))
(defun ooa-day--blkface (kind)
  "Return the block face associated with `KIND'."
  (cond ((eq kind 'life) 'ooa-blk-life) ((eq kind 'habit) 'ooa-blk-habit)
        (t 'ooa-blk-work)))

(defun ooa-day--object-value (value hd &optional keymap)
  "Add the rendered Org-heading object and KEYMAP for marker `HD' to `VALUE'.

When KEYMAP is nil, use the entry-cell map when the main package has defined
one."
  (let ((value (copy-sequence (or value ""))))
    (when (markerp hd)
      (let* ((keymap (or keymap
                         (and (boundp 'org-other-agenda-entry-cell-map)
                              (symbol-value 'org-other-agenda-entry-cell-map))))
             (properties
              (list 'org-other-agenda-object (cons 'org-heading hd))))
        (when keymap
          (setq properties (append properties (list 'keymap keymap))))
        (add-text-properties 0 (length value) properties value)))
    value))


;; -- Calendar ----------------------------------------------------------
(defun ooa-day--month-grid (sel-abs today-abs busy dead)
  "Return a list of calendar rows for `SEL-ABS'.  `TODAY-ABS' selects today.

`BUSY' and `DEAD' are hash tables keyed by absolute dates."
  (let* ((g (calendar-gregorian-from-absolute sel-abs))
         (mo (nth 0 g)) (yr (nth 2 g))
         (first (calendar-absolute-from-gregorian (list mo 1 yr)))
         (lead (mod (- (calendar-day-of-week (calendar-gregorian-from-absolute first)) 1) 7))
         (start (- first lead))
         rows)
    (push (list :type 'ooa-text
                :value (propertize (format "  %s %d" (calendar-month-name mo) yr)
                                   'face '(:weight bold)))
          rows)
    (push (list :type 'ooa-text
                :value (concat "  " (mapconcat (lambda (d) (propertize (format "%-4s" d)
                                                                       'face 'ooa-cal-dow))
                                               '("Mo" "Tu" "We" "Th" "Fr" "Sa" "Su") "")))
          rows)
    (dotimes (wk 6)
      (let (cells)
        (dotimes (dw 7)
          (let* ((abs (+ start (* wk 7) dw))
                 (gg (calendar-gregorian-from-absolute abs))
                 (day (nth 1 gg))
                 (out (/= (nth 0 gg) mo))
                 (face (cond ((= abs sel-abs) 'ooa-cal-sel)
                             ((= abs today-abs) 'ooa-cal-today)
                             (out 'ooa-cal-out)
                             (t 'default)))
                 (dot (concat (if (gethash abs busy) (propertize "·" 'face 'ooa-dot-sched) " ")
                              (if (gethash abs dead) (propertize "·" 'face 'ooa-dot-dead) " "))))
            (push (list :type 'ooa-date-cell
                        :value (concat (propertize (format "%2d" day) 'face face) dot)
                        :ca-abs abs
                        :layout (list :focus-id (list 'cal abs)))
                  cells)))
        (push (list :type :flex :direction :row :gap 0
                    :children (cons (list :type 'ooa-text :value "  ")
                                    (nreverse cells)))
              rows)))
    (push (list :type 'ooa-text
                :value (concat "  " (propertize "·" 'face 'ooa-dot-sched)
                               (propertize " scheduled   " 'face 'ooa-day-meta)
                               (propertize "·" 'face 'ooa-dot-dead)
                               (propertize " deadline" 'face 'ooa-day-meta)))
          rows)
    (nreverse rows)))

;; -- Time blocks -------------------------------------------------------
(defcustom ooa-day-max-block-width 34
  "Maximum width of one task block, measured in character cells.

Without a cap, a block spans its entire lane, and the lane grows with the
window, making the layout visually insubstantial.  Width beyond the cap remains
blank, while exact-hour hairlines still span the full grid to preserve its grid
structure."
  :type 'integer :group 'convenience)

(defun ooa-day--timeblock (blocks width h0 h1 slot)
  "Render `BLOCKS' between `H0' and `H1' using `SLOT' minute increments.

`WIDTH' is the available width.

Each block is a property list containing `:title', `:start', `:end',
`:kind', `:lane', and `:nlanes'; all times are measured in minutes."
  (let* ((labw 7)
         (avail (max 20 (- width labw 2)))
         (nlanes (max 1 (apply #'max 1 (mapcar (lambda (b) (plist-get b :nlanes)) blocks))))
         ;; Cap `lw' at `ooa-day-max-block-width'. Leave surplus width blank
         ;; instead of letting one block span the entire region.
         (lw (min ooa-day-max-block-width (/ avail nlanes)))
         out)
    (cl-loop for mins from (* h0 60) below (* h1 60) by slot do
      (let* ((h (/ mins 60)) (m (% mins 60))
             ;; `labw' is 7: the exact-hour label "HH:MM " occupies six
             ;; cells, so pad it to 7. Otherwise exact-hour and half-hour rows
             ;; start one column apart.
             (label (if (zerop m)
                        (propertize (format "%02d:%02d  " h m) 'face 'ooa-hour)
                      (propertize "       " 'face 'ooa-hour)))
             (cells
              (cl-loop for i from 0 below nlanes collect
                (let ((b (cl-find-if
                          (lambda (x) (and (= (plist-get x :lane) i)
                                           (<= (plist-get x :start) mins)
                                           (< mins (plist-get x :end))))
                          blocks)))
                  (if (null b)
                      (make-string lw ?\s)
                    (let* ((kind (plist-get b :kind))
                           (top (= mins (plist-get b :start)))
                           (body
                            ;; Show the time range only when `lw' is wide
                            ;; enough; otherwise put the title on the first row.
                            (cond
                             ((and top (>= lw 22))
                              (propertize
                               (format "%02d:%02d–%02d:%02d"
                                       (/ (plist-get b :start) 60) (% (plist-get b :start) 60)
                                       (/ (plist-get b :end) 60) (% (plist-get b :end) 60))
                               'face 'ooa-blk-time))
                             (top (truncate-string-to-width (plist-get b :title) (- lw 3) 0 ?\s))
                             ((and (>= lw 22) (= mins (+ (plist-get b :start) slot)))
                              (truncate-string-to-width (plist-get b :title) (- lw 3) 0 ?\s))
                             (t ""))))
                      (ooa-day--object-value
                       (concat (ooa-day--stripe kind)
                               (propertize
                                (concat " " (truncate-string-to-width body (- lw 2) 0 ?\s))
                                'face (ooa-day--blkface kind)))
                       (plist-get b :hd)))))))
             (line (concat "  " label (mapconcat #'identity cells ""))))
        ;; Extend the hairline across the full grid width; otherwise the grid
        ;; pattern stops halfway after the block-width cap.
        (when (< (string-width line) (+ 2 avail))
          (setq line (concat line (make-string (- (+ 2 avail) (string-width line)) ?\s))))
        ;; At exact hours, layer the hairline with `add-face-text-property'
        ;; and `APPEND' set to t. Using `propertize' directly would overwrite
        ;; each block's background.
        (when (zerop m)
          (setq line (copy-sequence line))
          (add-face-text-property 0 (length line) 'ooa-rule t line))
        (push (list :type 'ooa-text :value line) out)))
    (nreverse out)))


;; -- Week view: multi-day bars ----------------------------------------
(defface ooa-bar-work
  '((((background dark)) (:background "#1E3340" :foreground "#DCE8EF"))
    (t (:background "#DCE8EF" :foreground "#16323F")))
  "A work bar spanning multiple days."
  :group 'org-other-agenda)
(defface ooa-bar-learn
  '((((background dark)) (:background "#1C2A22" :foreground "#DDEFE2"))
    (t (:background "#E1EDE5" :foreground "#24382C")))
  "A learning bar spanning multiple days."
  :group 'org-other-agenda)
(defface ooa-bar-travel
  '((((background dark)) (:background "#271F2C" :foreground "#E7DCEC"))
    (t (:background "#EDE6F0" :foreground "#3A2C42")))
  "A travel bar spanning multiple days."
  :group 'org-other-agenda)
(defface ooa-wk-today
  '((((background dark)) (:foreground "#D6A65C" :weight bold))
    (t (:foreground "#9C6212" :weight bold)))
  "Today in the week view."
  :group 'org-other-agenda)
(defface ooa-wk-head
  '((((background dark)) (:foreground "#98A1AC")) (t (:foreground "#5D6672")))
  "The header in the week view."
  :group 'org-other-agenda)

(defun ooa-day--strip (txt todo)
  "Strip formatting markers from `TXT'.

Remove the `TODO' keyword, priority cookie, trailing `:tag:' text, and
alignment padding."
  (let ((s (or txt "")))
    (when (and todo (string-prefix-p todo s))
      (setq s (string-trim-left (substring s (length todo)))))
    (setq s (replace-regexp-in-string "\\`([0-9]+/[0-9]+):[ \t]*" "" s))
    (setq s (replace-regexp-in-string "\\`\\[#[A-Za-z0-9]\\][ \t]*" "" s))
    (setq s (replace-regexp-in-string "[ \t]+:[[:alnum:]_@#%:]+:[ \t]*\\'" "" s))
    (string-trim s)))

(defun ooa-day--spans (entries)
  "Recover multi-day tasks from `ENTRIES'.  Return a list of span plists.

Org Agenda splits each multi-day task into one entry per day.  The `type'
field is \"block\", and `extra' has the form \"(N/M): \".  Use those fields to
infer the absolute start and end dates."
  (let (seen out)
    (dolist (e entries)
      (when (and (equal (plist-get e :etype) "block")
                 (plist-get e :extra)
                 (string-match "(\\([0-9]+\\)/\\([0-9]+\\))" (plist-get e :extra)))
        (let* ((n (string-to-number (match-string 1 (plist-get e :extra))))
               (m (string-to-number (match-string 2 (plist-get e :extra))))
               (hd (plist-get e :hd))
               (day (plist-get e :dayabs)))
          (unless (member hd seen)
            (push hd seen)
            (let ((tags (plist-get e :tags)))
              (push (list :title (ooa-day--strip (plist-get e :txt)
                                               (plist-get e :todo))
                          :start (- day (1- n)) :end (+ (- day (1- n)) (1- m))
                          :total m
                          :hd hd
                          :kind (cond ((member "travel" tags) 'travel)
                                      ((member "learn" tags) 'learn)
                                      (t 'work)))
                    out))))))
    (nreverse out)))

(defun ooa-day--barface (kind)
  "Return the multi-day bar face associated with `KIND'."
  (cond ((eq kind 'travel) 'ooa-bar-travel)
        ((eq kind 'learn) 'ooa-bar-learn)
        (t 'ooa-bar-work)))

(defun ooa-day--week-bars (spans mon colw labw)
  "Render `SPANS' as bar rows for a week.

`MON' is Monday's absolute date, `COLW' is each day column's width, and
`LABW' is the label column's width."
  (let (lanes rows)
    (dolist (sp (sort (copy-sequence spans)
                      (lambda (a b) (< (plist-get a :start) (plist-get b :start)))))
      (let ((i 0) (done nil))
        (while (and (not done) (< i (length lanes)))
          (when (cl-every (lambda (x) (or (< (plist-get x :end) (plist-get sp :start))
                                          (> (plist-get x :start) (plist-get sp :end))))
                          (nth i lanes))
            (setf (nth i lanes) (append (nth i lanes) (list sp)))
            (setq done t))
          (setq i (1+ i)))
        (unless done (setq lanes (append lanes (list (list sp)))))))
    (dolist (lane lanes)
      (let ((line (make-string (+ labw (* 7 colw)) ?\s)))
        (dolist (sp lane)
          (let* ((s (max 0 (- (plist-get sp :start) mon)))
                 (e (min 6 (- (plist-get sp :end) mon)))
                 (x (+ labw (* s colw)))
                 (w (* (1+ (- e s)) colw))
                 (clipl (< (plist-get sp :start) mon))
                 (clipr (> (plist-get sp :end) (+ mon 6)))
                 (txt (concat (if clipl "◄" " ")
                              (truncate-string-to-width
                               (plist-get sp :title) (- w 3) 0 ?\s)
                              (if clipr "►" " "))))
            (setq txt (truncate-string-to-width txt w 0 ?\s))
            (add-face-text-property 0 (length txt)
                                    (ooa-day--barface (plist-get sp :kind)) nil txt)
            (setq txt (ooa-day--object-value txt (plist-get sp :hd)))
            (setq line (concat (substring line 0 x) txt
                               (substring line (min (length line) (+ x w)))))))
        (push (list :type 'ooa-text :value line) rows)))
    (nreverse rows)))

(provide 'org-other-agenda-day)
;;; org-other-agenda-day.el ends here
