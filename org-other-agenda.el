;;; org-other-agenda.el --- A different view of your Org agenda  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 chenyibin

;; Author: chenyibin
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (textui "0.5.1"))
;; Keywords: calendar, outlines, convenience
;; URL: https://github.com/yibie/org-other-agenda
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Org's agenda is a list.  This package renders the same data as a board:
;; a month calendar you can click, a day view with proportional time blocks,
;; a week view where multi-day tasks are drawn as spanning bars, and a search
;; page backed by Org's full-text and tag query engines.
;;
;; Nothing about Org changes.  A hidden agenda buffer is produced by Org
;; itself, its per-line text properties are read back, and the result is
;; re-laid out with TextUI.  Sorting, searching, and every `org-agenda-*'
;; command keep their own semantics; entry identity stays `org-hd-marker'.
;;
;; Run `M-x org-other-agenda'.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-agenda)
(require 'org-habit)
(require 'textui)
(require 'org-other-agenda-day)

(defgroup org-other-agenda nil
  "Render the Org agenda as a board."
  :group 'org
  :prefix "org-other-agenda-")

(defconst org-other-agenda--shadow " *org-other-agenda-shadow*")
(defconst org-other-agenda--query-shadow " *org-other-agenda-query-shadow*")
(defvar org-other-agenda-series)

;;; ---------------------------------------------------------------- Semantic extraction

(defun org-other-agenda--query-command ()
  "Return the active Org query command, or nil when no query is active.

The query lives in the TextUI buffer state.  A leading `#' has already been
classified by `org-other-agenda-search' as a tags query, so the command
receives the remainder exactly as Org expects it."
  (let* ((state (and (boundp 'textui-state) textui-state))
         (query (plist-get state :query))
         (kind (plist-get state :query-kind)))
    (when (and (stringp query) (string-match-p "\\S-" query))
      (list (if (eq kind 'tags) 'tags 'search) query))))

(defun org-other-agenda--base-series (&optional start-day)
  "Return the configured base series, optionally starting at `START-DAY'."
  (if start-day
      (mapcar (lambda (block)
                (if (eq (car block) 'agenda)
                    (append block
                            (list `((org-agenda-start-day
                                     ,start-day))))
                  block))
              org-other-agenda-series)
    org-other-agenda-series))

(defun org-other-agenda--run-series (series buffer-name)
  "Generate `SERIES' in the hidden agenda buffer `BUFFER-NAME'."
  (when series
    (let ((org-agenda-buffer-name buffer-name)
          (org-agenda-sticky nil)
          (org-agenda-window-setup 'current-window))
      (with-current-buffer (get-buffer-create buffer-name)
        (catch 'exit
          (org-agenda-run-series
           "org-other-agenda"
           (list series org-other-agenda-series)))))))

(defun org-other-agenda--generate (&optional start-day query-only)
  "Generate a hidden agenda buffer.

`START-DAY' is passed to Org unchanged and defaults to today.
When `QUERY-ONLY' is non-nil, generate only the active search or tags block
in the separate query shadow buffer.  Otherwise generate only the configured
base series in the ordinary shadow buffer.  Keeping the two buffers separate
means clearing a query can restore the last base result without running Org
again.

The four context requirements below are all essential.  A private buffer name
keeps the view away from `*Org Agenda*'.  `org-agenda-sticky' must be nil, or
the second generation silently throws instead of rebuilding.
`org-agenda-window-setup' must not be `reorganize-frame', or
`org-agenda-fit-window-to-buffer' displays the hidden buffer.  The buffer must
already be current via `with-current-buffer', so
`org-agenda-prepare-window' short-circuits."
  (if query-only
      (org-other-agenda--run-series
       (let ((query-command (org-other-agenda--query-command)))
         (and query-command (list query-command)))
       org-other-agenda--query-shadow)
    (org-other-agenda--run-series
     (org-other-agenda--base-series start-day)
     org-other-agenda--shadow)))

(defun org-other-agenda--todo-state-at-marker (hd)
  "Return the TODO state at heading marker HD, or nil.

Some Org agenda block producers do not attach a `todo-state' text property.
The source heading remains authoritative, so use it as the generic fallback."
  (when (and (markerp hd) (buffer-live-p (marker-buffer hd)))
    (with-current-buffer (marker-buffer hd)
      (save-excursion
        (goto-char hd)
        (org-get-todo-state)))))

(defun org-other-agenda--entries (&optional buffer)
  "Extract entry semantics from hidden agenda `BUFFER'.

When `BUFFER' is nil, use the ordinary base shadow buffer."
  (with-current-buffer (or buffer org-other-agenda--shadow)
    (let (out (day nil))
      (goto-char (point-min))
      (while (not (eobp))
        (when (get-text-property (line-beginning-position) 'org-agenda-date-header)
          (setq day (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position))))
        (when (org-get-at-bol 'org-marker)
          (let* ((txt (or (org-get-at-bol 'txt) ""))
                 (hd  (org-get-at-bol 'org-hd-marker))
                 (todo (or (org-get-at-bol 'todo-state)
                           (org-other-agenda--todo-state-at-marker hd)))
                 (raw (buffer-substring-no-properties
                       (line-beginning-position) (line-end-position))))
            (push (list :txt (string-trim txt)
                        :hd hd
                        :type (org-get-at-bol 'org-agenda-type)
                        :todo todo
                        :prio (org-get-at-bol 'priority)
                        :tags (org-get-at-bol 'tags)
                        :cat (org-get-at-bol 'org-category)
                        :time (org-get-at-bol 'time-of-day)
                        :dur (org-get-at-bol 'duration)
                        :etype (org-get-at-bol 'type)
                        :extra (org-get-at-bol 'extra)
                        :dayabs (org-get-at-bol 'day)
                        :blocked (org-get-at-bol 'org-todo-blocked)
                        :habit (org-get-at-bol 'org-habit-p)
                        :day day
                        ;; Repeated overdue entries use Org's "Sched. Nx" prefix.
                        :overdue (and (string-match "Sched\\. \\([0-9]+\\)x" raw)
                                      (string-to-number (match-string 1 raw)))
                        ;; Clocking: the row's `org-hd-marker' equals
                        ;; `org-clock-hd-marker' (4106-4107).
                        :clocking (and (markerp org-clock-hd-marker)
                                       (marker-buffer org-clock-hd-marker)
                                       (equal hd org-clock-hd-marker)))
                  out)))
        (forward-line 1))
      (nreverse out))))

;;; ---------------------------------------------------------------- Clickable row widgets

(define-widget 'org-other-agenda-row 'push-button
  "A clickable agenda entry row.

The entire row responds to mouse clicks and RET."
  :button-prefix ""
  :button-suffix ""
  :format "%[%v%]"
  :value-create
  (lambda (widget) (insert (ooa-text--non-empty (widget-value widget))))
  :action
  (lambda (widget &optional event)
    (let ((hd (widget-get widget :cool-hd)))
      (org-other-agenda-visit hd event))))

(defun org-other-agenda-visit (hd &optional _event)
  "Visit the Org location referenced by `HD'.

The real implementation forwards the native `org-agenda' command here.
`EVENT' is accepted for that interface."
  (if (and (markerp hd) (marker-buffer hd))
      (let ((heading (org-with-point-at hd (org-get-heading t t t t))))
        (message "→ %s" heading)
        (unless noninteractive
          (pop-to-buffer (marker-buffer hd))
          (widen)
          (goto-char (marker-position hd))
          (org-fold-show-context 'agenda)
          (recenter 3))
        heading)
    (message "This row has no org-hd-marker.")
    nil))

(defun org-other-agenda--row-widget (e title-width tag-width todo-width
                                      &optional scope ordinal)
  "Build a clickable widget element for `E'.

`TITLE-WIDTH', `TAG-WIDTH', and `TODO-WIDTH' set the fixed column widths.
`SCOPE' disambiguates entries that can occur in multiple blocks, such as
agenda, tags, and search.  TextUI requires each `:focus-id' to be unique
within a frame, so the focus ID must include the block scope.  `ORDINAL'
disambiguates repeated rows from the same block."
  (list :type 'org-other-agenda-row
        :value (org-other-agenda--row e title-width tag-width todo-width)
        :cool-hd (plist-get e :hd)
        :layout (list :focus-id (list (or scope (plist-get e :type))
                                      (plist-get e :hd)
                                      (plist-get e :day)
                                      (or ordinal 0)))))

;;; ---------------------------------------------------------------- Rendering helpers

(defface org-other-agenda-dim '((t (:foreground "#6b7280")))
  "Secondary information.")
(defface org-other-agenda-day '((t (:foreground "#7dcfff" :weight bold)))
  "A date header.")
(defface org-other-agenda-today '((t (:foreground "#ffc777" :weight bold)))
  "Today.")
(defface org-other-agenda-a '((t (:foreground "#ff757f" :weight bold)))
  "Priority A.")
(defface org-other-agenda-blocked '((t (:foreground "#4b5563")))
  "A blocked entry.")
(defface org-other-agenda-clock '((t (:foreground "#c3e88d" :weight bold)))
  "An entry currently being clocked.")
(defface org-other-agenda-tag '((t (:foreground "#82aaff")))
  "An entry tag.")
(defface org-other-agenda-active-tab
  '((t (:background "#7dcfff" :foreground "#0b0f14" :weight bold)))
  "The active tab.")
(defface org-other-agenda-tab
  '((t (:foreground "#6b7280")))
  "An inactive tab.")
(defface org-other-agenda-overdue
  '((t (:foreground "#ffc777")))
  "An overdue group heading.")

(defun org-other-agenda--prio-cell (e)
  "Return the priority cell for `E'.

1000 is the default priority, not a high priority."
  (let ((p (plist-get e :prio)))
    (cond ((and p (>= p 2000)) (propertize " A " 'face 'org-other-agenda-a))
          ((and p (<= p 0))    (propertize " C " 'face 'org-other-agenda-dim))
          (t "   "))))

(defconst org-other-agenda--todo-column-min-width 4
  "Minimum width of the TODO keyword column.")

(defconst org-other-agenda--todo-column-max-width 8
  "Maximum width of the TODO keyword column.")

(defun org-other-agenda--todo-column-width (entries)
  "Return one TODO column width for all `ENTRIES'.

The source Org buffers own the buffer-local `org-todo-keywords-1' values.
Use the widest configured keyword across those buffers, bounded by the
renderer's maximum, so every row in a frame shares one width."
  (let ((width org-other-agenda--todo-column-min-width)
        (buffers nil))
    (dolist (e entries)
      (let ((hd (plist-get e :hd)))
        (when (and (markerp hd) (buffer-live-p (marker-buffer hd)))
          (cl-pushnew (marker-buffer hd) buffers))))
    (dolist (buffer buffers)
      (with-current-buffer buffer
        (dolist (keyword org-todo-keywords-1)
          (when (stringp keyword)
            (setq width (max width (string-width keyword)))))))
    (min width org-other-agenda--todo-column-max-width)))

(defun org-other-agenda--todo-face (e)
  "Return Org's TODO face for `E'.

`E' supplies a heading marker for `org-get-todo-face' in the source Org
buffer, where buffer-local TODO configuration is available."
  (let ((todo (plist-get e :todo))
        (hd (plist-get e :hd)))
    (when (and (stringp todo) (> (length todo) 0))
      (if (and (markerp hd) (buffer-live-p (marker-buffer hd)))
          (with-current-buffer (marker-buffer hd)
            (org-get-todo-face todo))
        (org-get-todo-face todo)))))

(defun org-other-agenda--state-cell (e width)
  "Return the TODO cell for `E', padded to `WIDTH' columns.

Do not map keywords through a package-owned glyph table.  Custom keywords must
remain visible, and `org-get-todo-face' must supply Org's own face from the
source buffer."
  (let ((todo (plist-get e :todo)))
    (if (and (stringp todo) (> (length todo) 0))
        (let* ((text (truncate-string-to-width todo width 0 ?\s))
               (value (concat text
                              (make-string
                               (max 0 (- width (string-width text))) ?\s))))
          (propertize value 'face (org-other-agenda--todo-face e)))
      (make-string width ?\s))))

(defun org-other-agenda--condition-cell (e)
  "Return the independent one-column condition indicator for `E'.

Clocking, blocking and habit state are orthogonal to TODO state.  When more
than one applies, the visual priority is clocking, then blocked, then habit."
  (cond ((plist-get e :clocking)
         (propertize "▶" 'face 'org-other-agenda-clock))
        ((plist-get e :blocked)
         (propertize "⊘" 'face 'org-other-agenda-blocked))
        ((plist-get e :habit)
         (propertize "◈" 'face 'org-other-agenda-dim))
        (t " ")))

(defun org-other-agenda--time-cell (e)
  "Return the five-column time cell for `E'."
  (let ((tm (plist-get e :time)))
    (if tm (propertize (format "%5s" (format "%d:%02d" (/ tm 100) (% tm 100)))
                       'face 'org-other-agenda-dim)
      "     ")))

(defun org-other-agenda--tags-cell (e width)
  "Return the tag cell for `E', truncated to `WIDTH' columns."
  (let* ((tags (plist-get e :tags))
         (s (if tags (mapconcat (lambda (tg) (concat "#" tg)) tags " ") "")))
    (propertize (truncate-string-to-width s width 0 ?\s) 'face 'org-other-agenda-tag)))

(defun org-other-agenda--clean-title (e)
  "Remove the `TODO' keyword and [#X] cookie from `E'.

Each already has a dedicated column."
  (let ((s (plist-get e :txt))
        (kw (plist-get e :todo)))
    (when (and kw (string-prefix-p kw s))
      (setq s (string-trim-left (substring s (length kw)))))
    (setq s (replace-regexp-in-string "\\`\\[#[A-Za-z0-9]\\][ \t]*" "" s))
    ;; The `txt' property ends with alignment padding and a :tag: string.
    ;; Tags have their own column, so remove both.
    (setq s (replace-regexp-in-string "[ \t]+:[[:alnum:]_@#%:]+:[ \t]*\\'" "" s))
    (string-trim s)))

(defun org-other-agenda--row (e title-width tag-width todo-width)
  "Build a legacy string row from `E' for batch comparison.

`TITLE-WIDTH', `TAG-WIDTH', and `TODO-WIDTH' control character-cell padding.
The GUI path is unreliable: columns drift when CJK glyph widths do not match
their cell counts.  Keep this implementation for regression comparisons."
  (let* ((title (org-other-agenda--clean-title e)))
    (format "%s %s %s %s %s %s"
            (org-other-agenda--prio-cell e)
            (org-other-agenda--state-cell e todo-width)
            (org-other-agenda--condition-cell e)
            (org-other-agenda--time-cell e)
            (truncate-string-to-width title title-width 0 ?\s)
            (org-other-agenda--tags-cell e tag-width))))

(defun org-other-agenda--cell (value width &optional face)
  "Build a fixed-width cell from `VALUE'.

`WIDTH' sets the cell width, and optional `FACE' supplies its face."
  (list :type 'ooa-text
        :value (if face (propertize value 'face face) value)
        :layout (list :width width)))

(defun org-other-agenda--flex-row (e scope todo-width &optional ordinal)
  "Render `E' as a flex row with a clickable title cell.

TextUI aligns the columns.  `SCOPE' disambiguates the row's `:focus-id', and
`TODO-WIDTH' fixes the state column width.  `ORDINAL', when supplied, is the
entry's position in the complete source list; it keeps repeated Org rows
focusable even when they share a heading marker and date."
  (let* ((title (org-other-agenda--clean-title e))
         (tags (plist-get e :tags))
         (tagstr (if tags (mapconcat (lambda (tg) (concat "#" tg)) tags " ") "")))
    (list :type :flex :direction :row :gap 1
          :children
          (list
           (org-other-agenda--cell (org-other-agenda--prio-cell e) 3)
           (org-other-agenda--cell (org-other-agenda--state-cell e todo-width)
                                   todo-width)
           (org-other-agenda--cell (org-other-agenda--condition-cell e) 1)
           (org-other-agenda--cell (org-other-agenda--time-cell e) 5)
           (list :type 'org-other-agenda-row
                 :value (if (plist-get e :blocked)
                            (propertize title 'face 'org-other-agenda-blocked)
                          title)
                 :cool-hd (plist-get e :hd)
                 :layout (list :grow 1
                               :focus-id (list scope (plist-get e :hd)
                                               (plist-get e :day)
                                               (or ordinal 0))))
           (org-other-agenda--cell tagstr 18 'org-other-agenda-tag)))))

(defconst org-other-agenda--slot 15
  "The number of minutes represented by one time-block row.")
(defconst org-other-agenda--h0 8
  "The first hour rendered in a time block.")
(defconst org-other-agenda--h1 18
  "The hour at which a time block ends.")
(defconst org-other-agenda--circ
  ["①" "②" "③" "④" "⑤" "⑥" "⑦" "⑧" "⑨"])

(defun org-other-agenda--slots (e)
  "Return the half-open slot range occupied by `E', or nil when it has no time."
  (let ((tod (plist-get e :time)))
    (when tod
      (let* ((mins (+ (* 60 (/ tod 100)) (% tod 100)))
             (dur (or (plist-get e :dur) 45))
             (s (/ mins org-other-agenda--slot))
             (n (max 1 (round (/ (float dur) org-other-agenda--slot)))))
        (cons s (+ s n))))))

(defun org-other-agenda--assign-lanes (timed nlanes)
  "Assign `TIMED' entries to `NLANES' lanes greedily.

Return (`LANES' . `OVERFLOW'); entries in a lane never overlap."
  (let ((lanes (make-vector nlanes nil)) over)
    (dolist (e timed)
      (let* ((r (org-other-agenda--slots e)) (placed nil) (i 0))
        (while (and (not placed) (< i nlanes))
          (when (cl-every (lambda (x)
                            (let ((xr (org-other-agenda--slots x)))
                              (or (<= (cdr xr) (car r)) (>= (car xr) (cdr r)))))
                          (aref lanes i))
            (aset lanes i (append (aref lanes i) (list e)))
            (setq placed t))
          (setq i (1+ i)))
        (unless placed (push e over))))
    (cons (append lanes nil) (nreverse over))))

(defface org-other-agenda-blk-a
  '((((background dark))  (:background "#1E3A4A" :foreground "#CFE6F2" :extend nil))
    (t (:background "#DCE8EF" :foreground "#16323F")))
  "A work time block.")
(defface org-other-agenda-blk-b
  '((((background dark))  (:background "#2A3A24" :foreground "#D8ECC8"))
    (t (:background "#E3EFD8" :foreground "#2C3D20")))
  "A life time block.")
(defface org-other-agenda-blk-c
  '((((background dark))  (:background "#3A2F1C" :foreground "#F0DFC0"))
    (t (:background "#F4EAD6" :foreground "#4A3A1C")))
  "An other time block.")
(defface org-other-agenda-hour
  '((t (:foreground "#8A9099")))
  "An exact-hour label.")
(defface org-other-agenda-halfhour
  '((((background dark)) (:foreground "#3A424C"))
    (t (:foreground "#C8CFD6")))
  "A half-hour label.")

(defun org-other-agenda--blk-face (e)
  "Return the time-block face selected by the tags in `E'."
  (let ((tags (plist-get e :tags)))
    (cond ((member "life" tags) 'org-other-agenda-blk-b)
          ((member "learn" tags) 'org-other-agenda-blk-b)
          ((member "work" tags) 'org-other-agenda-blk-a)
          (t 'org-other-agenda-blk-c))))

(defun org-other-agenda--timeblock (entries width)
  "Render timed entries from `ENTRIES' in `WIDTH' columns.

The blocks have no borders, and empty gaps collapse into one row."
  (let* ((timed (sort (cl-remove-if-not #'org-other-agenda--slots entries)
                      (lambda (a b)
                        (let ((ra (org-other-agenda--slots a))
                              (rb (org-other-agenda--slots b)))
                          (if (= (car ra) (car rb))
                              (> (- (cdr ra) (car ra)) (- (cdr rb) (car rb)))
                            (< (car ra) (car rb)))))))
         (gutter 4)
         (avail (max 24 (- width 12 gutter)))
         (nlanes (max 1 (min 3 (/ avail 22))))
         (lw (/ avail nlanes))
         (split (org-other-agenda--assign-lanes timed nlanes))
         (lanes (car split))
         (over (cdr split))
         (s0 (/ (* 60 org-other-agenda--h0) org-other-agenda--slot))
         (s1 (/ (* 60 org-other-agenda--h1) org-other-agenda--slot))
         (empty-run 0)
         out)
    (cl-loop for slot from s0 below s1 do
      (let* ((mins (* slot org-other-agenda--slot))
             (h (/ mins 60)) (m (% mins 60))
             (busy (cl-some (lambda (lane)
                              (cl-find-if
                               (lambda (x) (let ((r (org-other-agenda--slots x)))
                                             (and (<= (car r) slot) (< slot (cdr r)))))
                               lane))
                            lanes)))
        (if (not busy)
            (setq empty-run (1+ empty-run))
          ;; Collapse the accumulated empty interval into one row.
          (when (> empty-run 0)
            (push (list :type 'ooa-text
                        :value (propertize
                                (format "  %s  ⋯ %d minutes free"
                                        (make-string 5 ?\s)
                                        (* empty-run org-other-agenda--slot))
                                'face 'org-other-agenda-halfhour))
                  out)
            (setq empty-run 0))
          (let* ((label (cond ((zerop m) (propertize (format "%02d:%02d" h m)
                                                     'face 'org-other-agenda-hour))
                              ((= m 30) (propertize (format "  :%02d" m)
                                                    'face 'org-other-agenda-halfhour))
                              (t "     ")))
                 (cells
                  (mapcar
                   (lambda (lane)
                     (let ((hit (cl-find-if
                                 (lambda (x) (let ((r (org-other-agenda--slots x)))
                                               (and (<= (car r) slot) (< slot (cdr r)))))
                                 lane)))
                       (if (null hit)
                           (make-string lw ?\s)
                         (let* ((r (org-other-agenda--slots hit))
                                (top (= slot (car r)))
                                (txt (if top
                                         (concat " " (org-other-agenda--clean-title hit))
                                       "")))
                           ;; Fill the entire cell background; this is the
                           ;; visual body of a block.
                           (propertize (truncate-string-to-width txt lw 0 ?\s)
                                       'face (org-other-agenda--blk-face hit))))))
                   lanes))
                 (ov (cl-remove-if-not
                      (lambda (x) (let ((r (org-other-agenda--slots x)))
                                    (and (<= (car r) slot) (< slot (cdr r)))))
                      over))
                 (gut (mapconcat
                       (lambda (x) (aref org-other-agenda--circ
                                         (min 8 (cl-position x over))))
                       ov "")))
            (push (list :type 'ooa-text
                        :value (concat "  " label " "
                                       (mapconcat #'identity cells " ")
                                       " "
                                       (propertize gut 'face 'org-other-agenda-overdue)))
                  out)))))
    (setq out (nreverse out))
    (when over
      (setq out (append out
                        (list (list :type 'ooa-text :value " "))
                        (cl-loop for x in over for i from 0 collect
                                 (list :type 'ooa-text
                                       :value (concat
                                               "     "
                                               (propertize
                                                (aref org-other-agenda--circ (min 8 i))
                                                'face 'org-other-agenda-overdue)
                                               " "
                                               (org-other-agenda--clean-title x)
                                               (propertize "  overlaps main lane"
                                                           'face 'org-other-agenda-dim)))))))
    out))

(defun org-other-agenda--day-page (all width todo-width)
  "Render the Today page for `ALL'.

Place a calendar on the left, time blocks on the right, and remaining tasks
below.  `WIDTH' and `TODO-WIDTH' control the layout."
  (let* ((sel (or (plist-get textui-state :date) (org-today)))
         (today (org-today))
         (busy (make-hash-table)) (dead (make-hash-table))
         (agenda (cl-remove-if-not (lambda (e) (eq (plist-get e :type) 'agenda)) all))
         (day-entries (cl-remove-if-not
                       (lambda (e) (equal (plist-get e :dayabs) sel)) agenda))
         (timed (cl-remove-if-not (lambda (e) (plist-get e :time)) day-entries))
         (untimed (cl-remove-if (lambda (e) (plist-get e :time)) day-entries))
         blocks)
    (dolist (e agenda)
      (let ((d (plist-get e :dayabs)))
        (when d
          (puthash d t busy)
          ;; Mark only actual deadline dates.  `upcoming-deadline' repeats
          ;; throughout its warning period; marking every occurrence would
          ;; create one continuous band and lose its meaning.
          (when (equal (plist-get e :etype) "deadline")
            (puthash d t dead)))))
    ;; Convert entries into blocks and assign lanes.
    (setq timed (sort timed (lambda (a b) (< (plist-get a :time) (plist-get b :time)))))
    (dolist (e timed)
      (let* ((tod (plist-get e :time))
             (st (+ (* 60 (/ tod 100)) (% tod 100)))
             ;; `duration' is a float; truncate it before taking a remainder.
             (du (truncate (or (plist-get e :dur) 45)))
             (tags (plist-get e :tags)))
        (push (list :title (org-other-agenda--clean-title e)
                    :hd (plist-get e :hd)
                    :start st :end (+ st du)
                    :kind (cond ((member "life" tags) 'life)
                                ((member "habit" tags) 'habit)
                                (t 'work))
                    :lane 0 :nlanes 1)
              blocks)))
    (setq blocks (nreverse blocks))
    ;; Give pinned entries first claim on lanes, pushing former main-lane
    ;; occupants into overflow.
    (let ((pinned (plist-get textui-state :pinned)))
      (when pinned
        (setq blocks (append
                      (cl-remove-if-not (lambda (b) (member (plist-get b :hd) pinned)) blocks)
                      (cl-remove-if (lambda (b) (member (plist-get b :hd) pinned)) blocks)))))
    (let* ((calw 30)
           ;; Without borders, subtract only the flex gap and right-side slack.
           (tbw (max 34 (- width calw 6)))
           ;; Cap lane count by available width: at least 20 cells per lane,
           ;; at most 3 lanes.
           (cap (max 1 (min 3 (/ (- tbw 9) 20))))
           (lanes nil) (over nil))
      (dolist (b blocks)
        (let ((i 0) (done nil))
          (while (and (not done) (< i (length lanes)))
            (when (cl-every (lambda (x) (or (<= (plist-get x :end) (plist-get b :start))
                                            (>= (plist-get x :start) (plist-get b :end))))
                            (nth i lanes))
              (setf (nth i lanes) (append (nth i lanes) (list b)))
              (plist-put b :lane i) (setq done t))
            (setq i (1+ i)))
          (unless done
            (if (< (length lanes) cap)
                (progn (setq lanes (append lanes (list (list b))))
                       (plist-put b :lane (1- (length lanes))))
              (push b over)))))
      (setq over (nreverse over))
      (setq blocks (cl-remove-if (lambda (b) (memq b over)) blocks))
      (dolist (b blocks) (plist-put b :nlanes (max 1 (length lanes))))
      (setq textui-state (plist-put textui-state :day-overflow over)))
    (let* ((calw 30)
           (tbw (max 34 (- width calw 6)))
           (over (plist-get textui-state :day-overflow))
           ;; Lines consumed above the list.  Counted from the elements that
           ;; were actually produced, not estimated: the time block collapses
           ;; empty stretches, so an arithmetic guess from the hour range
           ;; overshoots, and an overshoot collapses the list to a single row
           ;; while leaving the page half empty.
           (calendar-elements (ooa-day--month-grid sel today busy dead))
           (timeblock-elements
            (ooa-day--timeblock blocks tbw 8 18 30))
           (fixed-lines (+ 2           ; tab bar and query bar
                           (max (length calendar-elements)
                                (+ 1 (length timeblock-elements)
                                   (if over (1+ (length over)) 0)))
                           2))         ; spacer and the list heading
           (other-elements
            (org-other-agenda--windowed
             untimed
             (org-other-agenda--viewport-offset 'day)
             'day fixed-lines
             (lambda (e ordinal)
               (org-other-agenda--flex-row e 'dayrest todo-width ordinal))))
           (g (calendar-gregorian-from-absolute sel)))
      (list
       (list :type :flex :direction :row :gap 2
             :children
             (list
              (list :type :flex :direction :column :gap 0
                    :layout (list :width calw)
                    :children calendar-elements)
              (list :type :flex :direction :column :gap 0
                    :layout (list :width tbw :grow 1)
                    :children
                    (append
                     (list (list :type 'ooa-text
                                 :value (concat
                                         (propertize (format "%s %d " (calendar-month-name (nth 0 g) t) (nth 1 g))
                                                     'face 'ooa-day-title)
                                         (propertize
                                          (format " %d blocks · %d todos"
                                                  (length blocks) (length untimed))
                                          'face 'ooa-day-meta))))
                     timeblock-elements
                     (when over
                       (cons (list :type 'ooa-text :value " ")
                             (cl-loop for x in over for i from 0 collect
                                      (list :type 'org-other-agenda-pin
                                            :cool-hd (plist-get x :hd)
                                            :layout (list :focus-id
                                                          (list 'pin (plist-get x :hd)))
                                            :value (concat
                                                    "  "
                                                    (propertize
                                                     (aref org-other-agenda--circ (min 8 i))
                                                     'face 'ooa-cal-today)
                                                    " "
                                                    (propertize
                                                     (format "%02d:%02d "
                                                             (/ (plist-get x :start) 60)
                                                             (% (plist-get x :start) 60))
                                                     'face 'ooa-blk-time)
                                                    (plist-get x :title)
                                                    (propertize "  click to take main lane"
                                                                'face 'ooa-day-meta))))))))))
       (list :type 'ooa-text :value " ")
       (list :type 'ooa-text
             :value (propertize
                     (format "  Other items today (%d)    ⊘ blocked · ▶ clocking · ◈ habit"
                             (length untimed))
                                'face 'ooa-day-meta))
       (list :type :flex :direction :column :gap 0
             :children (or other-elements
                           (list (list :type 'ooa-text :value " "))))))))

(defun org-other-agenda--tag-face (e)
  "Return a foreground face for the primary tag in `E'.

When the day column is too narrow, color replaces the tag text."
  (let ((tags (plist-get e :tags)))
    (cond ((member "life" tags) 'ooa-stripe-life)
          ((member "habit" tags) 'ooa-stripe-habit)
          ((member "travel" tags) 'ooa-stripe-life)
          ((member "learn" tags) 'ooa-stripe-habit)
          (t 'default))))

(defun org-other-agenda--week-page (all width todo-width)
  "Render the Week page for `ALL'.

The page contains a day-list header, multi-day bars, and daily entries.
`WIDTH' sets available width, and `TODO-WIDTH' sets the state-column width."
  (let* ((sel (or (plist-get textui-state :date) (org-today)))
         (dow (mod (- (calendar-day-of-week
                       (calendar-gregorian-from-absolute sel)) 1) 7))
         (mon (- sel dow))
         (agenda (cl-remove-if-not (lambda (e) (eq (plist-get e :type) 'agenda)) all))
         (spans (ooa-day--spans agenda))
         (labw 2)
         (colw (max 12 (/ (- width labw 2) 7)))
         (bars (or (ooa-day--week-bars spans mon colw labw)
                   (list (list :type 'ooa-text
                               :value (propertize "  No multi-day tasks this week"
                                                  'face 'ooa-day-meta)))))
         (today (org-today))
         head)
    ;; Header.
    (setq head (concat (make-string labw ?\s)))
    (dotimes (i 7)
      (let* ((abs (+ mon i))
             (g (calendar-gregorian-from-absolute abs))
             (txt (truncate-string-to-width
                   (format "%s %d" (nth i '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))
                           (nth 1 g))
                   colw 0 ?\s)))
        (setq head (concat head
                           (propertize txt 'face
                                       (if (= abs today) 'ooa-wk-today 'ooa-wk-head))))))
    (append
     (list (list :type 'ooa-text
                 :value (concat (propertize "  This week " 'face 'ooa-day-title)
                                (propertize (format "· %d multi-day" (length spans))
                                            'face 'ooa-day-meta)))
           (list :type 'ooa-text :value head)
           (list :type 'ooa-text
                 :value (let ((r (concat (make-string labw ?\s)
                                         (make-string (* 7 colw) ?\s))))
                          (add-face-text-property 0 (length r) 'ooa-rule nil r) r)))
     bars
     (list (list :type 'ooa-text :value " "))
     ;; Daily entries are arranged side by side by column.
     (list
      (list :type :flex :direction :row :gap 0
            :children
            (cons (list :type 'ooa-text :value (make-string labw ?\s))
                  (cl-loop for i from 0 below 7 collect
                    (let* ((abs (+ mon i))
                           (items (cl-remove-if-not
                                   (lambda (e) (and (equal (plist-get e :dayabs) abs)
                                                    (not (equal (plist-get e :etype) "block"))))
                                   agenda)))
                      (list :type :flex :direction :column :gap 0
                            :layout (list :width colw)
                            :children
                            (or (org-other-agenda--windowed
                                 items
                                 (org-other-agenda--viewport-offset 'week)
                                 'week (+ 5 (length bars))
                                 (lambda (e ordinal)
                                   (list :type 'org-other-agenda-row
                                         :value (let ((v (truncate-string-to-width
                                                          (concat " " (org-other-agenda--state-cell e todo-width)
                                                                  (org-other-agenda--condition-cell e)
                                                                  " " (org-other-agenda--clean-title e))
                                                          colw 0 ?\s)))
                                                  ;; If a day column is too narrow
                                                  ;; for tag text, color by primary tag.
                                                  (add-face-text-property
                                                   0 (length v)
                                                   (org-other-agenda--tag-face e) t v)
                                                  v)
                                         :cool-hd (plist-get e :hd)
                                         :layout (list :focus-id
                                                       (list 'wk (plist-get e :hd)
                                                             abs ordinal)))))
                                (list (list :type 'ooa-text
                                            :value (make-string colw ?\s)))))))))))))

(define-widget 'org-other-agenda-pin 'push-button
  "An overflow entry that can be pinned into the main lane."
  :button-prefix "" :button-suffix "" :format "%[%v%]"
  :value-create (lambda (w) (insert (widget-value w)))
  :action (lambda (w &optional _e)
            (let ((hd (widget-get w :cool-hd)))
              (textui-set-state
               (current-buffer) :pinned
               (lambda (cur) (if (member hd cur)
                                 (delete hd (copy-sequence cur))
                               (cons hd cur)))))))

(defface org-other-agenda-query
  '((((background dark)) (:foreground "#D6A65C")) (t (:foreground "#9C6212")))
  "The query bar.")

(defun org-other-agenda--query-bar (query kind n)
  "Render the query bar for `QUERY' of `KIND' with `N' matches."
  (list :type 'ooa-text
        :value (concat
                "  "
                (if (and (stringp query)
                         (string-match-p "\\S-" query))
                    (concat (propertize (if (eq kind 'tags) "Tags " "Search ")
                                        'face 'org-other-agenda-query)
                            (propertize (if (eq kind 'tags)
                                           (concat "#" query)
                                         query)
                                        'face '(:weight bold))
                            (propertize (format "  → %d items" n)
                                        'face 'org-other-agenda-dim)
                            (propertize "     \\ clear"
                                        'face 'org-other-agenda-dim))
                  (propertize "/ search text or #tag"
                              'face 'org-other-agenda-dim)))))

(defun org-other-agenda--query-page (entries query kind todo-width)
  "Render query `ENTRIES' for `QUERY' of `KIND'.

`TODO-WIDTH' is the shared width of the state column."
  (let ((active (and (stringp query) (string-match-p "\\S-" query)))
        (shown (if (eq kind 'tags) (concat "#" query) query)))
    (if (not active)
        (list (list :type 'ooa-text
                    :value (propertize "  Press / to search text or #tag."
                                       'face 'org-other-agenda-dim)))
      (let ((windowed
             (org-other-agenda--windowed
              entries
              (org-other-agenda--viewport-offset kind)
              kind 2
              (lambda (e ordinal)
                (org-other-agenda--flex-row e kind todo-width ordinal)))))
        (or windowed
            (list (list :type 'ooa-text
                        :value (propertize (format "  No matches for %s" shown)
                                           'face 'org-other-agenda-dim))))))))

;;; ---------------------------------------------------------------- Tabs and frame

(defconst org-other-agenda--fixed-tabs
  '((day    . "Today")
    (week   . "Week")
    (agenda . "Agenda")
    (someday . "Someday")
    (search . "Search")
    (tags   . "Tags"))
  "Tabs that always exist, in display order.")

(defun org-other-agenda--tabs (_all)
  "Return the fixed tabs in their display order."
  org-other-agenda--fixed-tabs)

(define-widget 'org-other-agenda-tab 'push-button
  "A top-level tab button."
  :button-prefix "" :button-suffix "" :format "%[%v%]"
  :value-create (lambda (w) (insert (widget-value w)))
  :action (lambda (w &optional _e)
            (textui-set-state (current-buffer) :tab (widget-get w :cool-tab))))

(defun org-other-agenda--tab-bar (active counts tabs)
  "Render the tab bar for `ACTIVE' with `COUNTS' and `TABS'.

`COUNTS' is an alist of counts, and `TABS' is the tab list."
  (list :type :flex :direction :row :gap 2
        :children
        (mapcar
         (lambda (cell)
           (let* ((id (car cell))
                  (label (format " %s %d " (cdr cell) (or (cdr (assq id counts)) 0)))
                  (on (eq id active)))
             (list :type 'org-other-agenda-tab
                   :value (propertize label 'face
                                      (if on 'org-other-agenda-active-tab
                                        'org-other-agenda-tab))
                   :cool-tab id
                   :layout (list :focus-id (list 'tab id)))))
         tabs)))

(define-widget 'org-other-agenda-toggle 'push-button
  "An expandable group heading."
  :button-prefix "" :button-suffix "" :format "%[%v%]"
  :value-create (lambda (w) (insert (widget-value w)))
  :action (lambda (w &optional _e)
            (let ((key (widget-get w :cool-key)))
              (textui-set-state (current-buffer) key
                                (lambda (v) (not v))))))

(defun org-other-agenda--entry-has-date-p (hd)
  "Return non-nil when the entry at `HD' has any active date.

Check explicit SCHEDULED and DEADLINE properties, then scan the entry body
for active timestamps.  The caller limits this relatively expensive source
buffer lookup to entries emitted by an alltodo block."
  (when (and (markerp hd) (buffer-live-p (marker-buffer hd)))
    (with-current-buffer (marker-buffer hd)
      (org-with-wide-buffer
       (save-excursion
        (goto-char hd)
        (org-back-to-heading t)
        (or (org-entry-get nil "SCHEDULED")
            (org-entry-get nil "DEADLINE")
            (let ((end (save-excursion
                         (outline-next-heading)
                         (point))))
              (forward-line 1)
              (org-end-of-meta-data t)
              (and (< (point) end)
                   (save-restriction
                     (narrow-to-region (point) end)
                     (goto-char (point-min))
                     (re-search-forward org-ts-regexp nil t))))))))))

(defun org-other-agenda--someday-entries (todo)
  "Return undated entries from alltodo `TODO' rows.

Each distinct heading is checked at its source marker for SCHEDULED, DEADLINE,
and active body timestamps.  Restricting the source lookup to `TODO' rows
keeps the cost bounded by the alltodo block rather than every rendered row."
  (let (seen result)
    (dolist (entry todo (nreverse result))
      (let ((hd (plist-get entry :hd)))
        (when (and (markerp hd) (not (member hd seen)))
          (push hd seen)
          (unless (org-other-agenda--entry-has-date-p hd)
            (push entry result)))))))

(defun org-other-agenda--classify (all)
  "Classify `ALL' entries into view tabs and derive the Someday list.

Someday is based only on alltodo rows with no source SCHEDULED, DEADLINE, or
active body timestamp; it is not the complement of the current agenda span."
  (let* ((agenda (cl-remove-if-not (lambda (e) (eq (plist-get e :type) 'agenda)) all))
         (todo   (cl-remove-if-not (lambda (e) (eq (plist-get e :type) 'todo)) all))
         (tags   (cl-remove-if-not (lambda (e) (eq (plist-get e :type) 'tags)) all))
         (search (cl-remove-if-not (lambda (e) (eq (plist-get e :type) 'search)) all))
         (someday (org-other-agenda--someday-entries todo)))
    (list (cons 'week agenda)
          (cons 'day (let ((d (or (plist-get textui-state :date) (org-today))))
                       (cl-remove-if-not
                        (lambda (e) (and (eq (plist-get e :type) 'agenda)
                                         (plist-get e :time)
                                         (equal (plist-get e :dayabs) d)))
                        agenda)))
          (cons 'agenda agenda) (cons 'someday someday)
          (cons 'search search) (cons 'tags tags))))

(defun org-other-agenda--cached-classify (entries cache-key)
  "Return cached classification for `ENTRIES' under `CACHE-KEY'.

Someday classification reads each alltodo marker from its source buffer.  Keep
that work out of repeated full-frame refreshes until generation replaces the
entry list."
  (let ((cache (plist-get textui-state cache-key)))
    (if (and (consp cache) (eq (car cache) entries))
        (cadr cache)
      (let ((groups (org-other-agenda--classify entries)))
        (setq textui-state
              (plist-put (copy-sequence textui-state)
                         cache-key (list entries groups)))
        groups))))

(defun org-other-agenda--viewport-key (scope suffix)
  "Return the TextUI state key for `SUFFIX' in viewport `SCOPE'."
  (intern (format ":%s-%s" scope suffix)))

(defun org-other-agenda--viewport-offset (scope)
  "Return the current scroll offset for viewport `SCOPE'."
  (max 0 (or (plist-get textui-state
                        (org-other-agenda--viewport-key scope "offset"))
             0)))

(defun org-other-agenda--window-height ()
  "Return the narrowest visible body height, or nil when not displayed."
  (let (heights)
    (dolist (window (get-buffer-window-list (current-buffer) nil t))
      (when (window-live-p window)
        (push (window-body-height window) heights)))
    (when heights
      (apply #'min heights))))

(defun org-other-agenda--viewport-fixed-lines (scope)
  "Return the fixed line budget reserved above viewport `SCOPE'."
  (cond ((eq scope 'day) 50)
        ((eq scope 'week) 10)
        ((eq scope 'someday) 3)
        ((memq scope '(search tags)) 2)
        (t 3)))

(defun org-other-agenda--viewport-page-size (scope)
  "Return the current page size for viewport `SCOPE'."
  (or (plist-get textui-state
                 (org-other-agenda--viewport-key scope "page-size"))
      (max 2 (- (or (org-other-agenda--window-height)
                    (plist-get textui-state :viewport-page-size)
                    40)
                (org-other-agenda--viewport-fixed-lines scope)))))

(defun org-other-agenda--windowed (elements offset scope fixed-lines
                                            &optional producer)
  "Return the visible slice of `ELEMENTS' for viewport `SCOPE'.

`OFFSET' is measured in elements, and `FIXED-LINES' reserves space for the
page content that must remain visible above this list.  When `PRODUCER' is
non-nil, call it only for visible elements; this keeps long entry lists from
constructing invisible widgets.  The returned list ends with a position
indicator, and the clamped offset and page size are written back to
`textui-state'.  `PRODUCER', when supplied, receives each visible element and
its absolute index in `ELEMENTS'."
  (when elements
    (let* ((total (length elements))
           (page-size
            (max 2 (- (or (org-other-agenda--window-height)
                          (plist-get textui-state :viewport-page-size)
                          40)
                      fixed-lines)))
           (visible-count (max 1 (1- page-size)))
           (maximum (max 0 (- total visible-count)))
           (start (max 0 (min maximum offset)))
           (end (min total (+ start visible-count)))
           (visible (cl-subseq elements start end))
           (visible
            (if producer
                (cl-loop for item in visible
                         for index from start
                         collect (funcall producer item index))
              visible))
           (indicator
            (list :type 'ooa-text
                  :value
                  (propertize
                   (format "  %d–%d / %d"
                           (1+ start) end total)
                   'face 'org-other-agenda-dim)))
           (state (copy-sequence textui-state)))
      (setq state (plist-put
                   state (org-other-agenda--viewport-key scope "offset") start))
      (setq state (plist-put
                   state (org-other-agenda--viewport-key scope "page-size")
                   page-size))
      (setq textui-state state)
      (append visible (list indicator)))))

(defun org-other-agenda--agenda-items (entries)
  "Return lightweight display descriptors for agenda `ENTRIES'.

Descriptors contain headings and source entry plists, but no TextUI elements.
That distinction lets the generic viewport create widgets only for the visible
slice.  Cache the descriptors for the current entry list and overdue-toggle
state: rebuilding them on every full-frame refresh would scan the entire
agenda even though only the visible slice becomes widgets."
  (let* ((open (plist-get textui-state :overdue-open))
         (cache (plist-get textui-state :agenda-items-cache)))
    (if (and (consp cache)
             (eq (nth 0 cache) entries)
             (eq (nth 1 cache) open))
        (nth 2 cache)
      (let* ((overdue (cl-remove-if-not
                       (lambda (e) (plist-get e :overdue)) entries))
             (fresh (cl-remove-if
                     (lambda (e) (plist-get e :overdue)) entries))
             (cur nil) out)
        (dolist (e fresh)
          (unless (equal cur (plist-get e :day))
            (setq cur (plist-get e :day))
            (push (list :kind 'header :day cur) out))
          (push (list :kind 'entry :entry e :scope 'agenda) out))
        (when overdue
          (push (list :kind 'toggle :open open :count (length overdue)) out)
          (when open
            (dolist (e overdue)
              (push (list :kind 'entry :entry e :scope 'overdue) out))))
        (let ((items (nreverse out)))
          (setq textui-state
                (plist-put (copy-sequence textui-state)
                           :agenda-items-cache (list entries open items)))
          items)))))

(defun org-other-agenda--agenda-item-element (item todo-width &optional ordinal)
  "Render one agenda viewport descriptor `ITEM'.

`TODO-WIDTH' fixes the state-column width, and `ORDINAL' disambiguates
repeated rows in the frame focus map."
  (pcase (plist-get item :kind)
    ('header
     (list :type 'ooa-text
           :value (propertize
                   (format "── %s" (or (plist-get item :day) ""))
                   'face 'org-other-agenda-day)))
    ('entry
     (org-other-agenda--flex-row
      (plist-get item :entry)
      (plist-get item :scope)
      todo-width ordinal))
    ('toggle
     (let ((open (plist-get item :open)))
       (list :type 'org-other-agenda-toggle
             :value (propertize
                     (format "%s %d overdue%s"
                             (if open "▾" "▸")
                             (plist-get item :count)
                             (if open "" "  (click to expand)"))
                     'face 'org-other-agenda-overdue)
             :cool-key :overdue-open
             :layout (list :focus-id '(toggle overdue)))))))

(defun org-other-agenda--agenda-page-size ()
  "Return the current Agenda viewport page size."
  (org-other-agenda--viewport-page-size 'agenda))

(defun org-other-agenda--agenda-body (entries todo-width)
  "Render a bounded viewport for agenda `ENTRIES'.

Group entries by date and collapse overdue entries.  Only the current page of
display descriptors is turned into TextUI elements, so long agendas do not
materialize one widget per source row.  `TODO-WIDTH' sets the state column
width."
  (let ((items (org-other-agenda--agenda-items entries)))
    (when items
      (list (list :type :flex :direction :column :gap 0
                  :children
                  (org-other-agenda--windowed
                   items (org-other-agenda--viewport-offset 'agenda)
                   'agenda 3
                   (lambda (item ordinal)
                     (org-other-agenda--agenda-item-element
                      item todo-width ordinal))))))))

(defun org-other-agenda--someday-page (entries todo-width)
  "Render the undated-task page from `ENTRIES'.

The page explains how to move an item into the dated agenda.  `TODO-WIDTH'
sets the shared state-column width."
  (let ((windowed
         (org-other-agenda--windowed
          entries
          (org-other-agenda--viewport-offset 'someday)
          'someday 3
          (lambda (entry ordinal)
            (org-other-agenda--flex-row entry 'someday todo-width ordinal)))))
    (list
     (list :type 'ooa-text
           :value (propertize
                   "  Tasks without a date. Press s to schedule; press d to set a deadline."
                   'face 'org-other-agenda-dim))
     (if entries
         (list :type :flex :direction :column :gap 0
               :children windowed)
       (list :type 'ooa-text
             :value (propertize "  Nothing without a date."
                                'face 'org-other-agenda-dim))))))

(defun org-other-agenda--frame (width)
  "Build the complete frame for `WIDTH'.

Combine the tab bar, query bar, and selected page in a vertical layout."
  (let* ((tab (or (plist-get textui-state :tab) 'agenda))
         (query-view (memq tab '(search tags)))
         (base (or (plist-get textui-state :base-entries)
                   (plist-get textui-state :entries)))
         (all (if query-view
                  (or (plist-get textui-state :query-entries) nil)
                base))
         (base-groups (org-other-agenda--cached-classify
                       base :base-groups-cache))
         (groups (if query-view
                     (org-other-agenda--cached-classify
                      all :query-groups-cache)
                   base-groups))
         (entries (cdr (assq tab groups)))
         (query (plist-get textui-state :query))
         (query-kind (or (plist-get textui-state :query-kind) 'search))
         (todo-width (org-other-agenda--todo-column-width (or all base)))
         (counts (mapcar (lambda (g) (cons (car g) (length (cdr g))))
                        base-groups))
         (body (cond
                ((eq tab 'day) (org-other-agenda--day-page all width todo-width))
                ((eq tab 'week) (org-other-agenda--week-page all width todo-width))
                ((eq tab 'agenda) (org-other-agenda--agenda-body entries todo-width))
                ((eq tab 'someday)
                 (org-other-agenda--someday-page entries todo-width))
                ((memq tab '(search tags))
                 (org-other-agenda--query-page
                  entries (and (eq tab query-kind) query) tab todo-width))
                (t (cl-loop for e in entries
                            for ordinal from 0
                            collect (org-other-agenda--flex-row
                                     e tab todo-width ordinal))))))
    (when query-view
      (setf (cdr (assq tab counts)) (length entries)))
    ;; Wrap the top level in an explicit column: TextUI flows a top-level
    ;; element list horizontally.  Returning `(tab-bar body)' directly would
    ;; put both on the same row.
    (list
     (list :type :flex :direction :column :gap 0 :children
      (list
       (org-other-agenda--tab-bar tab counts (org-other-agenda--tabs all))
       (org-other-agenda--query-bar
        query query-kind
        (length entries))
       (list :type :flex :direction :column :gap 0
           :layout (list :width width :min-width 40 :grow 1)
           :children
           (or body
               (list (list :type 'ooa-text
                           :value (propertize "(no items)"
                                              'face 'org-other-agenda-dim))))))))))


;;; ---------------------------------------------------------------- Command forwarding

(defun org-other-agenda--button-hd (button)
  "Return the Org heading marker from `BUTTON', or nil for a non-entry widget."
  (when button
    (condition-case nil
        (let ((hd (widget-get button :cool-hd)))
          (and (markerp hd) hd))
      (error nil))))

(defun org-other-agenda--at-point ()
  "Return the `org-hd-marker' of the rendered entry at point.

An entry's clickable widget may start after the fixed priority, status and
time columns.  Prefer the button under point, then inspect the adjacent
character and finally the rest of the current line so a point at the start of
an entry row still selects that row."
  (let* ((position (point))
         (button (or (get-char-property position 'button)
                     (and (> position (point-min))
                          (get-char-property (1- position) 'button))))
         (hd (org-other-agenda--button-hd button)))
    (unless hd
      (save-excursion
        (goto-char (line-beginning-position))
        (let ((end (line-end-position)))
          (while (and (< (point) end) (not hd))
            (setq hd (org-other-agenda--button-hd
                      (get-char-property (point) 'button)))
            (forward-char 1)))))
    (unless (and hd (marker-buffer hd))
      (user-error "No Org agenda entry at point"))
    hd))

(defun org-other-agenda--shadow-position (hd &optional buffer)
  "Return the position of the first row for `HD' in hidden `BUFFER'."
  (with-current-buffer (or buffer org-other-agenda--shadow)
    (save-excursion
      (goto-char (point-min))
      (catch 'found
        (while (< (point) (point-max))
          (when (equal hd (org-get-at-bol 'org-hd-marker))
            (throw 'found (point)))
          (forward-line 1))
        nil))))

(defun org-other-agenda--active-shadow (view)
  "Return the hidden agenda buffer used by the current page in `VIEW'."
  (with-current-buffer view
    (if (and (memq (plist-get textui-state :tab) '(search tags))
             (plist-get textui-state :query))
        (get-buffer org-other-agenda--query-shadow)
      (get-buffer org-other-agenda--shadow))))

(defun org-other-agenda--active-entry-key (view)
  "Return the state key holding the entries rendered by `VIEW'."
  (with-current-buffer view
    (if (and (memq (plist-get textui-state :tab) '(search tags))
             (plist-get textui-state :query))
        :query-entries
      :base-entries)))

(defun org-other-agenda--forward (command)
  "Run native agenda `COMMAND' for the entry at point, then redraw the view.

  `COMMAND' is called interactively in the hidden agenda buffer.  The window
  configuration is restored even when `COMMAND' displays or switches buffers;
  the shadow buffers therefore remain an implementation detail of this view."
  (let* ((view (current-buffer))
         (hd (org-other-agenda--at-point))
         (shadow (org-other-agenda--active-shadow view))
         (entry-key (org-other-agenda--active-entry-key view))
         (prefix current-prefix-arg))
    (unless (buffer-live-p shadow)
      (user-error "Org Other Agenda shadow buffer is not live"))
    (let ((shadow-position
           (org-other-agenda--shadow-position hd shadow)))
      (unless shadow-position
        (user-error "Could not find the entry in the shadow agenda"))
      (unwind-protect
          (save-window-excursion
            (let ((org-agenda-buffer-name (buffer-name shadow))
                  (org-agenda-sticky nil)
                  (org-agenda-window-setup 'current-window)
                  (current-prefix-arg prefix))
              (with-current-buffer shadow
                (goto-char shadow-position)
                (call-interactively command))))
        (when (buffer-live-p view)
          (let ((entries (org-other-agenda--entries shadow)))
            (textui-set-state view entry-key entries)
            (when (eq entry-key :base-entries)
              (textui-set-state view :entries entries))))))))

(defun org-other-agenda-todo ()
  "Forward `org-agenda-todo' to the shadow agenda entry at point."
  (interactive)
  (org-other-agenda--forward #'org-agenda-todo))

(defun org-other-agenda-schedule ()
  "Forward `org-agenda-schedule' to the shadow agenda entry at point."
  (interactive)
  (org-other-agenda--forward #'org-agenda-schedule))

(defun org-other-agenda-deadline ()
  "Forward `org-agenda-deadline' to the shadow agenda entry at point."
  (interactive)
  (org-other-agenda--forward #'org-agenda-deadline))

(defun org-other-agenda-priority ()
  "Forward `org-agenda-priority' to the shadow agenda entry at point."
  (interactive)
  (org-other-agenda--forward #'org-agenda-priority))

(defun org-other-agenda-set-tags ()
  "Forward `org-agenda-set-tags' to the shadow agenda entry at point."
  (interactive)
  (org-other-agenda--forward #'org-agenda-set-tags))


(defcustom org-other-agenda-series
  '((agenda "" ((org-agenda-span 'week)))
    (alltodo ""))
  "Agenda blocks to build the view from.

The value has the shape of the command list in
`org-agenda-custom-commands', so any block type Org accepts works here.
Base blocks and an active query are generated into separate hidden buffers and
re-laid out together."
  :type 'sexp
  :group 'org-other-agenda)

(defcustom org-other-agenda-buffer-name "*Org Other Agenda*"
  "Name of the buffer that displays the view."
  :type 'string
  :group 'org-other-agenda)

(defvar org-other-agenda-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'org-other-agenda-refresh)
    (define-key map (kbd "/") #'org-other-agenda-search)
    (define-key map (kbd "\\") #'org-other-agenda-clear-search)
    (define-key map (kbd "t") #'org-other-agenda-todo)
    (define-key map (kbd "s") #'org-other-agenda-schedule)
    (define-key map (kbd "d") #'org-other-agenda-deadline)
    (define-key map (kbd ",") #'org-other-agenda-priority)
    (define-key map (kbd ":") #'org-other-agenda-set-tags)
    (define-key map (kbd "[") #'org-other-agenda-previous-day)
    (define-key map (kbd "]") #'org-other-agenda-next-day)
    (define-key map (kbd "n") #'org-other-agenda-next-line)
    (define-key map (kbd "p") #'org-other-agenda-previous-line)
    (define-key map (kbd "C-n") #'org-other-agenda-next-line)
    (define-key map (kbd "C-p") #'org-other-agenda-previous-line)
    (define-key map (kbd "C-v") #'org-other-agenda-page-down)
    (define-key map (kbd "M-v") #'org-other-agenda-page-up)
    (define-key map [next] #'org-other-agenda-page-down)
    (define-key map [prior] #'org-other-agenda-page-up)
    (dotimes (i 7)
      (define-key map (kbd (number-to-string (1+ i)))
                  (lambda () (interactive) (org-other-agenda-select-tab i))))
    map)
  "Keys added to the view on top of the TextUI and widget bindings.")

(defun org-other-agenda-select-tab (index)
  "Switch to the tab at `INDEX', counted from zero."
  (interactive "p")
  (let ((cell (nth index (org-other-agenda--tabs
                            (or (plist-get textui-state :base-entries)
                                (plist-get textui-state :entries))))))
    (when cell
      (textui-set-state (current-buffer) :tab (car cell)))))

(defun org-other-agenda--viewport-scope ()
  "Return the scrollable viewport scope for the current tab."
  (let ((tab (plist-get textui-state :tab)))
    (cond ((memq tab '(agenda day week someday search tags)) tab)
          (t nil))))

(defun org-other-agenda--viewport-scroll (amount)
  "Move the current viewport by `AMOUNT' rows."
  (let ((scope (org-other-agenda--viewport-scope)))
    (if (not scope)
        (message "No scrollable list on this page")
      (let* ((key (org-other-agenda--viewport-key scope "offset"))
             (offset (org-other-agenda--viewport-offset scope))
             (next (max 0 (+ offset amount))))
        (unless (= offset next)
          (textui-set-state (current-buffer) key next))))))

(defun org-other-agenda--agenda-scroll (amount)
  "Move the Agenda viewport by `AMOUNT' rows.

Keep this private wrapper for callers from the original Agenda-only viewport
implementation; all tabs now use `org-other-agenda--viewport-scroll'."
  (if (eq (plist-get textui-state :tab) 'agenda)
      (org-other-agenda--viewport-scroll amount)
    (message "Paging is available on the Agenda tab")))

(defun org-other-agenda-next-line ()
  "Show the next row in the current long-list viewport."
  (interactive)
  (org-other-agenda--viewport-scroll 1))

(defun org-other-agenda-previous-line ()
  "Show the previous row in the current long-list viewport."
  (interactive)
  (org-other-agenda--viewport-scroll -1))

(defun org-other-agenda--viewport-page (direction)
  "Move one viewport page in `DIRECTION'."
  (let ((scope (org-other-agenda--viewport-scope)))
    (when scope
      (org-other-agenda--viewport-scroll
       (* direction
          (max 1 (1- (org-other-agenda--viewport-page-size scope))))))))

(defun org-other-agenda-page-down ()
  "Show the next page of rows in the current long-list viewport."
  (interactive)
  (org-other-agenda--viewport-page 1))

(defun org-other-agenda-page-up ()
  "Show the previous page of rows in the current long-list viewport."
  (interactive)
  (org-other-agenda--viewport-page -1))

(defun org-other-agenda--follow-viewport ()
  "Keep a focused row inside the current long-list viewport."
  (when (and (derived-mode-p 'textui-mode)
             (org-other-agenda--viewport-scope))
    (let* ((button (or (get-char-property (point) 'button)
                       (and (> (point) (point-min))
                            (get-char-property (1- (point)) 'button))))
           (hd (and button (org-other-agenda--button-hd button)))
           (window (get-buffer-window (current-buffer) t)))
      (when (and hd (window-live-p window))
        (let* ((line (line-number-at-pos))
               (top (with-current-buffer (window-buffer window)
                      (line-number-at-pos (window-start window))))
               (bottom (+ top (max 1 (1- (window-body-height window)))))
               (delta (cond ((<= line (+ top 1)) -1)
                            ((>= line (1- bottom)) 1))))
          (when delta
            (org-other-agenda--viewport-scroll delta)))))))

(defun org-other-agenda-refresh ()
  "Regenerate the hidden agenda buffer and redraw."
  (interactive)
  (let* ((view (current-buffer))
         (query (plist-get textui-state :query))
         (kind (plist-get textui-state :query-kind)))
    (org-other-agenda--generate)
    (let ((entries (org-other-agenda--entries)))
      (textui-set-state view :base-entries entries)
      (textui-set-state view :entries entries))
    (when (and (stringp query) (string-match-p "\\S-" query))
      (org-other-agenda--generate nil t)
      (textui-set-state view :query-entries
                        (org-other-agenda--entries
                         org-other-agenda--query-shadow)))
    (textui-set-state view :query-kind kind)
    (message "Refreshed")))

(defun org-other-agenda--regenerate-view (view)
  "Regenerate only the active query and update TextUI `VIEW'."
  (with-current-buffer view
    (org-other-agenda--generate nil t)
    (textui-set-state view :query-entries
                      (and (org-other-agenda--query-command)
                           (org-other-agenda--entries
                            org-other-agenda--query-shadow)))))

(defun org-other-agenda-search (query)
  "Search Org entries with `QUERY', or run a tags search for `#TAG'."
  (interactive
   (list (read-string
          "Search (text or #tag): "
          (let ((query (plist-get textui-state :query)))
            (if (eq (plist-get textui-state :query-kind) 'tags)
                (concat "#" (or query ""))
              (or query ""))))))
  (let* ((view (current-buffer))
         (tags-query (and (> (length query) 0)
                          (= (aref query 0) ?#)))
         (kind (if tags-query 'tags 'search))
         (query (if tags-query (substring query 1) query)))
    (textui-set-state view :query query)
    (textui-set-state view :query-kind kind)
    (textui-set-state view (org-other-agenda--viewport-key kind "offset") 0)
    (org-other-agenda--regenerate-view view)
    (textui-set-state view :tab kind)))

(defun org-other-agenda-clear-search ()
  "Clear the active search and restore the cached base agenda."
  (interactive)
  (let ((view (current-buffer)))
    (textui-set-state view :query nil)
    (textui-set-state view :query-entries nil)
    (textui-set-state view :search-offset 0)
    (textui-set-state view :tags-offset 0)))

(defun org-other-agenda-previous-day ()
  "Move the day view one day back."
  (interactive)
  (textui-set-state (current-buffer) :date
                    (lambda (d) (1- (or d (org-today))))))

(defun org-other-agenda-next-day ()
  "Move the day view one day forward."
  (interactive)
  (textui-set-state (current-buffer) :date
                    (lambda (d) (1+ (or d (org-today))))))

;;;###autoload
(defun org-other-agenda ()
  "Display the Org agenda as a board.

Uses `org-agenda-files' and every other Org setting as they are; nothing
is overridden.  The blocks shown come from `org-other-agenda-series'."
  (interactive)
  (org-other-agenda--generate)
  (let ((entries (org-other-agenda--entries)))
    (let ((buffer (textui-open org-other-agenda-buffer-name
                               #'org-other-agenda--frame
                               (list :entries entries
                                     :base-entries entries
                                     :query-entries nil
                                     :query nil
                                     :query-kind 'search
                                     :agenda-offset 0
                                     :agenda-page-size 40
                                     :day-offset 0
                                     :week-offset 0
                                     :someday-offset 0
                                     :search-offset 0
                                     :tags-offset 0
                                     :viewport-page-size 40))))
    (with-current-buffer buffer
      (use-local-map (make-composed-keymap org-other-agenda-mode-map
                                           (current-local-map)))
      (add-hook 'post-command-hook #'org-other-agenda--follow-viewport nil t))
      buffer)))

(provide 'org-other-agenda)
;;; org-other-agenda.el ends here
