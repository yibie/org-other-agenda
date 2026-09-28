;;; org-other-agenda.el --- A different view of your Org agenda  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 chenyibin

;; Author: chenyibin
;; Version: 0.1.1
;; Package-Requires: ((emacs "29.1") (textui "0.5.1") (embark "1.2"))
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
(require 'embark)
(require 'embark-org)
(require 'org-other-agenda-day)

(defgroup org-other-agenda nil
  "Render the Org agenda as a board."
  :group 'org
  :prefix "org-other-agenda-")

(defconst org-other-agenda--shadow " *org-other-agenda-shadow*")
(defconst org-other-agenda--query-shadow " *org-other-agenda-query-shadow*")
(defvar org-other-agenda-series)
(defvar-local org-other-agenda--view nil
  "Non-nil in a TextUI buffer rendered by `org-other-agenda'.")
(defvar-local org-other-agenda--owned-markers nil
  "Org agenda markers owned by this hidden agenda generation.")
(defvar-local org-other-agenda--movement-scrolled nil
  "Non-nil when entry movement already scrolled the current viewport.")
(defvar org-other-agenda-entry-map)
(defvar org-other-agenda-entry-cell-map)
(defvar org-other-agenda-day-map)
(defvar org-other-agenda-slot-map)
(defvar org-other-agenda-view-map)
(defvar org-other-agenda--day-direct-map)
(defvar org-other-agenda--slot-direct-map)
(defvar org-other-agenda--embark-selected-target nil
  "Embark target dynamically bound while a board noun action runs.")

(defmacro org-other-agenda--with-shadow-agenda (buffer prefix &rest body)
  "Run BODY in agenda BUFFER with native hidden-agenda bindings.

PREFIX becomes `current-prefix-arg' while BODY runs.  The caller remains
responsible for `save-window-excursion' when a native command may display a
buffer."
  (declare (indent 2) (debug (form form body)))
  `(with-current-buffer ,buffer
     (let ((org-agenda-buffer-name (buffer-name))
           (org-agenda-sticky nil)
           (org-agenda-window-setup 'current-window)
           (current-prefix-arg ,prefix))
       ,@body)))

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
    (let ((buffer (get-buffer-create buffer-name)))
      (with-current-buffer buffer
        ;; Non-sticky Org agendas share the global marker registry.  Restore
        ;; this shadow's registry dynamically so generating one shadow cannot
        ;; reset another shadow's markers or markers owned by native Org.
        (let ((org-agenda-markers org-other-agenda--owned-markers)
              (org-agenda-buffer buffer))
          (unwind-protect
              (org-other-agenda--with-shadow-agenda buffer nil
                (catch 'exit
                  (org-agenda-run-series
                   "org-other-agenda"
                   (list series org-other-agenda-series))))
            ;; `org-agenda-mode' clears ordinary buffer-local variables while
            ;; preparing the buffer, so restore ownership after Org returns.
            (setq-local org-other-agenda--owned-markers
                        org-agenda-markers)))))))

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
          (let* ((org-marker (org-get-at-bol 'org-marker))
                 (txt (or (org-get-at-bol 'txt) ""))
                 (hd  (org-get-at-bol 'org-hd-marker))
                 (todo (or (org-get-at-bol 'todo-state)
                           (org-other-agenda--todo-state-at-marker hd)))
                 (raw (buffer-substring-no-properties
                       (line-beginning-position) (line-end-position))))
            (push (list :txt (string-trim txt)
                        :hd hd
                        :org-marker org-marker
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

(defalias 'org-other-agenda--object-value #'ooa-day--object-value
  "Add the rendered Org-heading object and optional keymap to `VALUE'.

The implementation lives in `org-other-agenda-day', which this file requires,
so both renderers share one definition of the `org-other-agenda-object' and
`keymap' properties.  A nil keymap uses the entry-cell map for plain text
cells; button widgets pass the entry map explicitly to preserve widget RET.")

(defun org-other-agenda--panel-place (panel)
  "Return the structural place reserved for PANEL's visible title."
  (list panel -1 -2))

(defalias 'org-other-agenda--occurrence #'ooa-day--occurrence
  "Return source provenance for rendered ENTRY.")

(defun org-other-agenda--navigation-value
    (value place context node &optional object keymap occurrence)
  "Annotate VALUE with renderer-owned structural navigation metadata.

PLACE is `(PANEL COL ROW)', CONTEXT lists parent places from inner to outer,
and NODE names a non-action structural node.  OBJECT, KEYMAP and OCCURRENCE
are optional entry semantics."
  (let ((value (ooa-day--navigation-value
                value
                'org-other-agenda-place place
                'org-other-agenda-context context
                'org-other-agenda-node node
                'org-other-agenda-occurrence occurrence)))
    (when object
      (let ((direct-map
             (cond ((eq keymap org-other-agenda-day-map)
                    org-other-agenda--day-direct-map)
                   ((eq keymap org-other-agenda-slot-map)
                    org-other-agenda--slot-direct-map)
                   (t keymap))))
        (setq value (copy-sequence value))
        (add-text-properties
         0 (length value)
         (append (list 'org-other-agenda-object object)
                 (and direct-map (list 'keymap direct-map)))
         value)))
    value))

(defun org-other-agenda--entry-value
    (value entry place context &optional keymap targets)
  "Annotate VALUE as an ENTRY occurrence at PLACE within CONTEXT.

Optional KEYMAP supplies direct entry actions.  TARGETS lists outer nouns."
  (let ((value
         (org-other-agenda--navigation-value
          value place context
          (cons 'heading (org-other-agenda--clean-title entry))
          (cons 'org-heading (plist-get entry :hd))
          (or keymap
              (and (boundp 'org-other-agenda-entry-cell-map)
                   org-other-agenda-entry-cell-map))
          (org-other-agenda--occurrence entry))))
    (when targets
      (add-text-properties
       0 (length value) (list 'org-other-agenda-targets targets) value))
    (when (org-other-agenda--heading-marked-p (plist-get entry :hd))
      (add-face-text-property 0 (length value)
                              'org-other-agenda-marked nil value))
    value))

(defun org-other-agenda--panel-value (value panel &optional label)
  "Annotate VALUE as the visible title of PANEL, named by optional LABEL."
  (let ((value
         (org-other-agenda--navigation-value
          value (org-other-agenda--panel-place panel) nil
          (cons 'panel (or label (symbol-name panel))))))
    (add-text-properties 0 (length value) '(org-other-agenda-parent t) value)
    value))

(defun org-other-agenda--container-value
    (value place panel &optional label object keymap targets)
  "Annotate VALUE as a container at PLACE inside PANEL.

Optional LABEL names the node, OBJECT supplies executable noun data, KEYMAP
provides direct actions, and TARGETS lists outer nouns."
  (let ((value
         (org-other-agenda--navigation-value
          value place (list (org-other-agenda--panel-place panel))
          (cons 'container (or label (symbol-name panel))) object keymap)))
    (when targets
      (add-text-properties
       0 (length value) (list 'org-other-agenda-targets targets) value))
    (add-text-properties 0 (length value) '(org-other-agenda-parent t) value)
    value))

(defun org-other-agenda--parent-value (value)
  "Mark `VALUE' as a date or section header for parent movement."
  (let ((value (copy-sequence (or value ""))))
    (add-text-properties 0 (length value)
                         '(org-other-agenda-parent t)
                         value)
    value))

(define-widget 'org-other-agenda-row 'push-button
  "A clickable agenda entry row.

The entire row responds to mouse clicks and RET."
  :button-face-get (lambda (_widget) nil)
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
        :value (org-other-agenda--entry-value
                (org-other-agenda--row e title-width tag-width todo-width)
                e (list (or scope (plist-get e :type)) 0 (or ordinal 0))
                (list (org-other-agenda--panel-place
                       (or scope (plist-get e :type))))
                org-other-agenda-entry-map)
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
(defface org-other-agenda-current-object
  '((t (:inherit highlight)))
  "Non-metric-changing feedback for the structural noun at point.")
(defface org-other-agenda-marked
  '((((background dark)) (:underline (:color "#ffc777" :style wave)))
    (t (:underline (:color "#9C6212" :style wave))))
  "Non-metric-changing feedback for headings in the marked set.")

(defvar-local org-other-agenda--current-object-overlays nil
  "Overlays highlighting the complete structural noun at point.")

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

(defun org-other-agenda--cell (value width &optional face row)
  "Build a fixed-width cell from `VALUE'.

`WIDTH' sets the cell width and optional `FACE' supplies its face.  ROW is a
property list containing the shared entry, place, context and outer targets."
  (let ((value (if face (propertize value 'face face) value))
        (entry (plist-get row :entry)))
    (list :type 'ooa-text
          :value (if entry
                     (org-other-agenda--entry-value
                      value entry (plist-get row :place)
                      (plist-get row :context)
                      org-other-agenda-entry-cell-map
                      (plist-get row :targets))
                   value)
          :layout (list :width width))))

(defun org-other-agenda--flex-row (e scope todo-width &optional navigation)
  "Render `E' as a flex row with a clickable title cell.

TextUI aligns the columns.  `SCOPE' disambiguates the row's `:focus-id', and
`TODO-WIDTH' fixes the state column width.  NAVIGATION may supply `:ordinal',
`:panel', `:col', `:context' and `:targets'.  The ordinal is the entry's
position in the complete source list and keeps repeated Org rows focusable."
  (let* ((title (org-other-agenda--clean-title e))
         (tags (plist-get e :tags))
         (tagstr (if tags (mapconcat (lambda (tg) (concat "#" tg)) tags " ") ""))
         (ordinal (plist-get navigation :ordinal))
         (panel (or (plist-get navigation :panel) scope))
         (place (list panel (or (plist-get navigation :col) 0)
                      (or ordinal 0)))
         (context (or (plist-get navigation :context)
                      (list (org-other-agenda--panel-place panel))))
         (row (list :entry e :place place :context context
                    :targets (plist-get navigation :targets))))
    (list :type :flex :direction :row :gap 1
          :children
          (list
           (org-other-agenda--cell (org-other-agenda--prio-cell e) 3
                                   nil row)
           (org-other-agenda--cell (org-other-agenda--state-cell e todo-width)
                                   todo-width nil row)
           (org-other-agenda--cell (org-other-agenda--condition-cell e) 1
                                   nil row)
           (org-other-agenda--cell (org-other-agenda--time-cell e) 5
                                   nil row)
           (list :type 'org-other-agenda-row
                 :value (org-other-agenda--entry-value
                         (if (plist-get e :blocked)
                             (propertize title 'face 'org-other-agenda-blocked)
                           title)
                         e place context org-other-agenda-entry-map
                         (plist-get navigation :targets))
                 :cool-hd (plist-get e :hd)
                 :layout (list :grow 1
                               :focus-id (list scope (plist-get e :hd)
                                               (plist-get e :day)
                                               (or ordinal 0))))
           (org-other-agenda--cell tagstr 18 'org-other-agenda-tag
                                   row)))))

(defun org-other-agenda--embark-object-at-point ()
  "Return the rendered Org-heading object at point or on the current line."
  (if-let* ((position (org-other-agenda--navigation-position)))
      (get-text-property position 'org-other-agenda-object)
    (or (get-text-property (point) 'org-other-agenda-object)
        (and (> (point) (point-min))
             (get-text-property (1- (point)) 'org-other-agenda-object))
        (save-excursion
          (goto-char (line-beginning-position))
          (let ((end (line-end-position)))
            (catch 'object
              (while (< (point) end)
                (when-let* ((object (get-text-property
                                     (point) 'org-other-agenda-object)))
                  (throw 'object object))
                (forward-char 1))
              nil))))))

(defun org-other-agenda--embark-title (hd)
  "Return the cleaned rendered title for Org heading marker `HD'."
  (let* ((state (and (boundp 'textui-state) textui-state))
         (entry
          (cl-loop for key in '(:entries :base-entries :query-entries)
                   thereis (cl-find hd (plist-get state key)
                                    :key (lambda (candidate)
                                           (plist-get candidate :hd))
                                    :test #'equal))))
    (or (and entry (org-other-agenda--clean-title entry))
        (org-with-point-at hd (org-get-heading t t t t)))))

(defun org-other-agenda-embark-target ()
  "Return an Embark `org-heading' target for the entry at point.

The target title carries the source marker as its `org-marker' text property,
which is the location contract used by `embark-org-heading-map'."
  (when (and org-other-agenda--view
             (derived-mode-p 'textui-mode))
    (let* ((object (org-other-agenda--embark-object-at-point))
           (hd (and (consp object)
                    (eq (car object) 'org-heading)
                    (markerp (cdr object))
                    (cdr object))))
      (when (and hd (buffer-live-p (marker-buffer hd)))
        (let ((title (org-other-agenda--embark-title hd)))
          (when (stringp title)
            `(org-heading ,(propertize
                            title
                            'org-marker hd
                            'org-other-agenda-origin (current-buffer))
                          ,(line-beginning-position) . ,(line-end-position))))))))

(defun org-other-agenda--noun-object-at-point (type)
  "Return the innermost or outer noun object of TYPE at point."
  (when-let* ((position (org-other-agenda--navigation-position)))
    (let ((object (get-text-property position 'org-other-agenda-object))
          (targets (get-text-property position 'org-other-agenda-targets)))
      (cl-find type (cons object targets) :key #'car-safe))))

(defun org-other-agenda--noun-value (target type)
  "Return TYPE's payload from Embark TARGET or the noun at point."
  (if (stringp target)
      (get-text-property 0 type target)
    (cdr-safe (org-other-agenda--noun-object-at-point type))))

(defun org-other-agenda--embark-noun-target (type)
  "Return an Embark target of TYPE from renderer-owned noun metadata."
  (when (and org-other-agenda--view
             (derived-mode-p 'textui-mode))
    (when-let* ((object (org-other-agenda--noun-object-at-point type)))
      (let ((target
             (pcase type
               ('ooa-day
                (let ((day (cdr object)))
                  (propertize
                   (calendar-date-string
                    (calendar-gregorian-from-absolute day))
                   'ooa-day day)))
               ('ooa-slot
                (pcase-let ((`(,_ ,day ,start ,end) object))
                  (propertize
                   (format "%s %02d:%02d–%02d:%02d"
                           (calendar-date-string
                            (calendar-gregorian-from-absolute day))
                           (/ start 60) (% start 60)
                           (/ end 60) (% end 60))
                   'ooa-slot (cdr object)))))))
        (and target
             (cons type
                   (cons target
                         (cons (line-beginning-position)
                               (line-end-position)))))))))

(defun org-other-agenda-embark-slot-target ()
  "Return the Today time slot target at point, including occupied cells."
  (org-other-agenda--embark-noun-target 'ooa-slot))

(defun org-other-agenda-embark-day-target ()
  "Return the calendar day containing the structural noun at point."
  (org-other-agenda--embark-noun-target 'ooa-day))

(defun org-other-agenda-embark-targets ()
  "Return nested board targets in heading, slot, then day order."
  (delq nil (list (org-other-agenda-embark-target)
                  (org-other-agenda-embark-slot-target)
                  (org-other-agenda-embark-day-target))))

(defconst org-other-agenda--slot 15
  "The number of minutes represented by one time-block row.")
(defcustom org-other-agenda-time-start-hour 8
  "First hour shown in time blocks, inclusive.
Use an integer from 0 through 23, smaller than the end hour.
Changes take effect on the next render."
  :type '(integer :tag "Start hour")
  :group 'org-other-agenda)

(defcustom org-other-agenda-time-end-hour 18
  "Last hour boundary in time blocks, exclusive.
Use an integer from 1 through 24, greater than the start hour.
Changes take effect on the next render."
  :type '(integer :tag "End hour")
  :group 'org-other-agenda)

(defun org-other-agenda--validate-time-range ()
  "Reject invalid time-block hour settings before rendering."
  (unless (and (integerp org-other-agenda-time-start-hour)
               (integerp org-other-agenda-time-end-hour)
               (<= 0 org-other-agenda-time-start-hour)
               (< org-other-agenda-time-start-hour org-other-agenda-time-end-hour)
               (<= org-other-agenda-time-end-hour 24))
    (user-error "Time-block hours must satisfy 0 <= start < end <= 24")))

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
  (org-other-agenda--validate-time-range)
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
         (s0 (/ (* 60 org-other-agenda-time-start-hour) org-other-agenda--slot))
         (s1 (/ (* 60 org-other-agenda-time-end-hour) org-other-agenda--slot))
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
                    :entry e
                    :occurrence (org-other-agenda--occurrence e)
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
            (progn
              (org-other-agenda--validate-time-range)
              (ooa-day--timeblock blocks tbw
                                  org-other-agenda-time-start-hour
                                  org-other-agenda-time-end-hour 30 sel)))
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
               (org-other-agenda--flex-row
                e 'dayrest todo-width
                (list :ordinal ordinal :panel 'day-other
                      :targets (list (cons 'ooa-day sel)))))))
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
                                :value (org-other-agenda--panel-value
                                        (concat
                                          (propertize (format "%s %d " (calendar-month-name (nth 0 g) t) (nth 1 g))
                                                      'face 'ooa-day-title)
                                          (propertize
                                           (format " %d blocks · %d todos"
                                                   (length blocks) (length untimed))
                                                   'face 'ooa-day-meta))
                                        'day-timeblock "time blocks")))
                     timeblock-elements
                     (when over
                       (cons (list :type 'ooa-text
                                   :value (org-other-agenda--panel-value
                                           (propertize
                                            (format "  Overflow (%d)" (length over))
                                            'face 'ooa-day-meta)
                                           'day-overflow "overflow"))
                             (cl-loop for x in over for i from 0 collect
                                      (list :type 'org-other-agenda-pin
                                            :cool-hd (plist-get x :hd)
                                            :layout (list :focus-id
                                                          (list 'pin (plist-get x :hd)))
                                            :value (org-other-agenda--entry-value
                                                    (concat
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
                                                                 'face 'ooa-day-meta))
                                                    (plist-get x :entry)
                                                    (list 'day-overflow 0 i)
                                                    (list (org-other-agenda--panel-place
                                                           'day-overflow))
                                                    org-other-agenda-entry-map)))))))))
       (list :type 'ooa-text :value " ")
       (list :type 'ooa-text
             :value (org-other-agenda--panel-value
                     (propertize
                      (format "  Other items today (%d)    ⊘ blocked · ▶ clocking · ◈ habit"
                              (length untimed))
                      'face 'ooa-day-meta)
                     'day-other "other items"))
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
         (day-items
          (cl-loop for i from 0 below 7
                   for abs = (+ mon i)
                   collect
                   (cl-remove-if-not
                    (lambda (e)
                      (and (equal (plist-get e :dayabs) abs)
                           (not (equal (plist-get e :etype) "block"))))
                    agenda)))
         (fixed-lines (+ 6 (length bars)))
         (week-offset
          (org-other-agenda--clamp-viewport
           'week (org-other-agenda--viewport-offset 'week) fixed-lines
           (apply #'max 0 (mapcar #'length day-items))))
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
        (setq head
              (concat
               head
               (org-other-agenda--container-value
                (propertize txt 'face
                            (if (= abs today) 'ooa-wk-today 'ooa-wk-head))
                (list 'week-days i -1) 'week-days
                (format "day %d" abs)
                (cons 'ooa-day abs) org-other-agenda-day-map)))))
    (append
     (list (list :type 'ooa-text
                 :value (org-other-agenda--panel-value
                         (concat (propertize "  This week " 'face 'ooa-day-title)
                                 (propertize (format "· %d multi-day" (length spans))
                                             'face 'ooa-day-meta))
                         'week-bars "multi-day bars"))
           (list :type 'ooa-text
                 :value (org-other-agenda--parent-value head))
           (list :type 'ooa-text
                 :value (let ((r (concat (make-string labw ?\s)
                                         (make-string (* 7 colw) ?\s))))
                          (add-face-text-property 0 (length r) 'ooa-rule nil r) r)))
     bars
     (list (list :type 'ooa-text :value " "))
     (list (list :type 'ooa-text
                 :value (org-other-agenda--panel-value
                         (propertize "  Daily items" 'face 'ooa-day-meta)
                         'week-days "daily columns")))
     ;; Daily entries are arranged side by side by column.
     (list
      (list :type :flex :direction :row :gap 0
            :children
            (cons (list :type 'ooa-text :value (make-string labw ?\s))
                  (cl-loop for i from 0 below 7 collect
                    (let* ((abs (+ mon i))
                           (items (nth i day-items)))
                      (list :type :flex :direction :column :gap 0
                            :layout (list :width colw)
                            :children
                            (or (org-other-agenda--windowed
                                 items
                                 week-offset
                                 'week fixed-lines
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
                                                  (org-other-agenda--entry-value
                                                   v e
                                                   (list 'week-days i ordinal)
                                                   (list (list 'week-days i -1)
                                                         (org-other-agenda--panel-place
                                                          'week-days))
                                                   org-other-agenda-entry-map
                                                   (list (cons 'ooa-day abs))))
                                         :cool-hd (plist-get e :hd)
                                         :layout (list :focus-id
                                                       (list 'wk (plist-get e :hd)
                                                             abs ordinal))))
                                 t)
                                (list (list :type 'ooa-text
                                            :value (make-string colw ?\s)))))))))))))

(define-widget 'org-other-agenda-pin 'push-button
  "An overflow entry that can be pinned into the main lane."
  :button-prefix "" :button-suffix "" :format "%[%v%]"
  :value-create (lambda (w) (insert (widget-value w)))
  :action (lambda (w &optional _e)
            (org-other-agenda-toggle-pin (widget-get w :cool-hd))))

(defface org-other-agenda-query
  '((((background dark)) (:foreground "#D6A65C")) (t (:foreground "#9C6212")))
  "The query bar.")

(defun org-other-agenda--query-bar (query kind n &optional parent panel)
  "Render the query bar for `QUERY' of `KIND' with `N' matches.

When PARENT is non-nil, mark the bar as the section parent for query rows.
When PANEL is non-nil, make the bar that panel's structural title."
  (let ((value
         (concat
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
    (list :type 'ooa-text
          :value (cond (panel (org-other-agenda--panel-value value panel))
                       (parent (org-other-agenda--parent-value value))
                       (t value)))))

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
                (org-other-agenda--flex-row
                 e kind todo-width (list :ordinal ordinal))))))
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

(defun org-other-agenda--viewport-bounds (offset fixed-lines total)
  "Return viewport bounds for OFFSET, FIXED-LINES and TOTAL rows.

The result is `(START PAGE-SIZE VISIBLE-COUNT)'."
  (let* ((page-size
          (max 2 (- (or (org-other-agenda--window-height)
                        (plist-get textui-state :viewport-page-size)
                        40)
                    fixed-lines)))
         (visible-count (max 1 (1- page-size)))
         (maximum (max 0 (- total visible-count)))
         (start (max 0 (min maximum offset))))
    (list start page-size visible-count)))

(defun org-other-agenda--clamp-viewport (scope offset fixed-lines total)
  "Clamp OFFSET once for a SCOPE panel containing TOTAL structural rows."
  (pcase-let* ((`(,start ,page-size ,_visible-count)
                 (org-other-agenda--viewport-bounds
                  offset fixed-lines total))
                (state (copy-sequence textui-state)))
    (setq state
          (plist-put state
                     (org-other-agenda--viewport-key scope "offset") start))
    (setq state
          (plist-put state
                     (org-other-agenda--viewport-key scope "page-size")
                     page-size))
    (setq textui-state state)
    start))

(defun org-other-agenda--windowed (elements offset scope fixed-lines
                                            &optional producer shared-offset)
  "Return the visible slice of `ELEMENTS' for viewport `SCOPE'.

`OFFSET' is measured in elements, and `FIXED-LINES' reserves space for the
page content that must remain visible above this list.  When `PRODUCER' is
non-nil, call it only for visible elements; this keeps long entry lists from
constructing invisible widgets.  The returned list ends with a position
indicator, and the clamped offset and page size are written back to
`textui-state'.  `PRODUCER', when supplied, receives each visible element and
its absolute index in `ELEMENTS'.  SHARED-OFFSET means OFFSET was clamped once
for a multi-column panel; short columns then render empty instead of rewriting
the shared viewport state."
  (when (and elements
             (or (not shared-offset) (< offset (length elements))))
    (let* ((total (length elements))
           (bounds (org-other-agenda--viewport-bounds
                    offset fixed-lines total))
           (page-size (nth 1 bounds))
           (visible-count (nth 2 bounds))
           (start (if shared-offset offset (car bounds)))
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
      (unless shared-offset
        (setq state (plist-put
                     state (org-other-agenda--viewport-key scope "offset") start))
        (setq state (plist-put
                     state (org-other-agenda--viewport-key scope "page-size")
                     page-size))
        (setq textui-state state))
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
            (push (list :kind 'header :day cur :dayabs (plist-get e :dayabs)) out))
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

(defun org-other-agenda--agenda-item-element
    (item todo-width &optional ordinal container-row)
  "Render one agenda viewport descriptor `ITEM'.

`TODO-WIDTH' fixes the state-column width, and `ORDINAL' disambiguates
repeated rows in the frame focus map.  CONTAINER-ROW identifies its parent."
  (pcase (plist-get item :kind)
    ('header
     (list :type 'ooa-text
           :value (org-other-agenda--container-value
                   (propertize
                    (format "── %s" (or (plist-get item :day) ""))
                   'face 'org-other-agenda-day)
                   (list 'agenda 0 ordinal) 'agenda
                   (or (plist-get item :day) "Agenda")
                   (and (plist-get item :dayabs)
                        (cons 'ooa-day (plist-get item :dayabs)))
                   (and (plist-get item :dayabs)
                        org-other-agenda-day-map))))
    ('entry
     (org-other-agenda--flex-row
     (plist-get item :entry)
     (plist-get item :scope)
      todo-width
      (list
       :ordinal ordinal :panel 'agenda :col 0
       :context
       (delq nil
             (list (and container-row (list 'agenda 0 container-row))
                   (org-other-agenda--panel-place 'agenda)))
       :targets
       (and (plist-get (plist-get item :entry) :dayabs)
            (list (cons 'ooa-day
                        (plist-get (plist-get item :entry) :dayabs)))))))
    ('toggle
     (let ((open (plist-get item :open)))
       (list :type 'org-other-agenda-toggle
             :value (org-other-agenda--container-value
                     (propertize
                      (format "%s %d overdue%s"
                              (if open "▾" "▸")
                              (plist-get item :count)
                              (if open "" "  (click to expand)"))
                      'face 'org-other-agenda-overdue)
                     (list 'agenda 0 ordinal) 'agenda "overdue")
             :cool-key :overdue-open
             :layout (list :focus-id '(toggle overdue)))))))

(defun org-other-agenda--agenda-container-row (items ordinal)
  "Return the nearest container row at or before ORDINAL in ITEMS."
  (let ((index (min ordinal (1- (length items)))) found)
    (while (and (>= index 0) (null found))
      (when (memq (plist-get (nth index items) :kind) '(header toggle))
        (setq found index))
      (setq index (1- index)))
    found))

(defun org-other-agenda--agenda-viewport-parent (items offset)
  "Return a parent header for ITEMS when the viewport starts at OFFSET.

The normal date header is itself a viewport item.  Once the viewport has
scrolled past it, keep a compact section header in the rendered page so `u'
still has a parent to reach without making the off-screen rows part of the
projection again."
  (when (> offset 0)
    (let* ((item (nth offset items))
           (container-row
            (org-other-agenda--agenda-container-row items offset))
           (container (and container-row (nth container-row items)))
           (day (or (plist-get container :day)
                    (and (eq (plist-get container :kind) 'toggle) "Overdue")
                    "Agenda"))
           (dayabs (plist-get container :dayabs)))
      (unless (eq (plist-get item :kind) 'header)
        (list :type 'ooa-text
              :value (org-other-agenda--container-value
                      (format "── %s" day)
                      (list 'agenda 0 container-row) 'agenda day
                      (and dayabs (cons 'ooa-day dayabs))
                      (and dayabs org-other-agenda-day-map)))))))

(defun org-other-agenda--agenda-page-size ()
  "Return the current Agenda viewport page size."
  (org-other-agenda--viewport-page-size 'agenda))

(defun org-other-agenda--agenda-body (entries todo-width)
  "Render a bounded viewport for agenda `ENTRIES'.

Group entries by date and collapse overdue entries.  Only the current page of
display descriptors is turned into TextUI elements, so long agendas do not
materialize one widget per source row.  `TODO-WIDTH' sets the state column
width."
  (let* ((items (org-other-agenda--agenda-items entries))
         (offset (org-other-agenda--viewport-offset 'agenda))
         (parent (org-other-agenda--agenda-viewport-parent items offset)))
    (when items
      (list (list :type :flex :direction :column :gap 0
                  :children
                  (append
                   (list (list :type 'ooa-text
                               :value (org-other-agenda--panel-value
                                       (propertize "  Agenda"
                                                   'face 'ooa-day-title)
                                       'agenda "agenda")))
                   (when parent (list parent))
                   (org-other-agenda--windowed
                    items offset 'agenda (+ 4 (if parent 1 0))
                    (lambda (item ordinal)
                      (org-other-agenda--agenda-item-element
                       item todo-width ordinal
                       (org-other-agenda--agenda-container-row
                        items ordinal))))))))))

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
            (org-other-agenda--flex-row
             entry 'someday todo-width (list :ordinal ordinal))))))
    (list
     (list :type 'ooa-text
           :value (org-other-agenda--panel-value
                   (propertize
                    "  Tasks without a date. Press s to schedule; press d to set a deadline."
                    'face 'org-other-agenda-dim)
                   'someday "someday"))
     (if entries
         (list :type :flex :direction :column :gap 0
               :children windowed)
       (list :type 'ooa-text
             :value (org-other-agenda--parent-value
                     (propertize "  Nothing without a date."
                                 'face 'org-other-agenda-dim)))))))

(defun org-other-agenda--frame (width)
  "Build the complete frame for `WIDTH'.

Combine the tab bar, query bar, and selected page in a vertical layout."
  (setq-local org-other-agenda--view t)
  (add-hook 'eldoc-documentation-functions #'org-other-agenda--eldoc nil t)
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
                                     e tab todo-width
                                     (list :ordinal ordinal)))))))
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
        (length entries)
        query-view (and query-view tab))
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

(defun org-other-agenda--object-hd (position)
  "Return the Org heading marker from the entry object at `POSITION'."
  (let ((object (get-text-property position 'org-other-agenda-object)))
    (and (consp object)
         (eq (car object) 'org-heading)
         (markerp (cdr object))
         (cdr object))))

(defun org-other-agenda--at-point ()
  "Return the `org-hd-marker' of the rendered entry at point.

An entry's clickable widget may start after the fixed priority, status and
time columns.  Prefer the entry object or button under point, then inspect the
adjacent character and finally the rest of the current line so a point at the
start of an entry row still selects that row."
  (let* ((position (point))
         (button (or (get-char-property position 'button)
                     (and (> position (point-min))
                          (get-char-property (1- position) 'button))))
         (hd (or (org-other-agenda--object-hd position)
                 (and (> position (point-min))
                      (org-other-agenda--object-hd (1- position)))
                 (org-other-agenda--button-hd button))))
    (unless hd
      (save-excursion
        (goto-char (line-beginning-position))
        (let ((end (line-end-position)))
          (while (and (< (point) end) (not hd))
            (setq hd (or (org-other-agenda--object-hd (point))
                         (org-other-agenda--button-hd
                          (get-char-property (point) 'button))))
            (forward-char 1)))))
    (unless (and hd (marker-buffer hd))
      (user-error "No Org agenda entry at point"))
    hd))

(defun org-other-agenda-entry-visit ()
  "Visit the Org entry represented by the entry object at point."
  (interactive)
  (org-other-agenda-visit (org-other-agenda--at-point)))

(defun org-other-agenda--shadow-line (buffer predicate)
  "Return `(POSITION . VALUE)' for the first BUFFER line matching PREDICATE.

PREDICATE runs at the beginning of each line.  Its non-nil return value becomes
VALUE."
  (with-current-buffer (or buffer org-other-agenda--shadow)
    (save-excursion
      (goto-char (point-min))
      (catch 'found
        (while (< (point) (point-max))
          (when-let* ((value (funcall predicate)))
            (throw 'found (cons (point) value)))
          (forward-line 1))
        nil))))

(defun org-other-agenda--shadow-position (hd &optional buffer)
  "Return the position of the first row for `HD' in hidden `BUFFER'."
  (car-safe
   (org-other-agenda--shadow-line
    buffer (lambda () (and (equal hd (org-get-at-bol 'org-hd-marker)) t)))))

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
            (org-other-agenda--with-shadow-agenda shadow prefix
              (goto-char shadow-position)
              (call-interactively command)))
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

(defun org-other-agenda--occurrence-at-point ()
  "Return source occurrence provenance at point or on its line."
  (when-let* ((position (org-other-agenda--navigation-position)))
    (get-text-property position 'org-other-agenda-occurrence)))

(defun org-other-agenda--property-positions (property)
  "Return the start of every non-nil PROPERTY run in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (let (positions)
      (while (< (point) (point-max))
        (when (get-text-property (point) property)
          (push (point) positions))
        (goto-char
         (or (next-single-property-change
              (point) property nil (point-max))
             (point-max))))
      (nreverse positions))))

(defun org-other-agenda--preferred-property-position (property predicate)
  "Return the preferred PROPERTY position accepted by PREDICATE.

Prefer the first accepted widget button; otherwise return the first accepted
property run.  PREDICATE receives each run's start position."
  (let (first button)
    (dolist (position (org-other-agenda--property-positions property))
      (when (funcall predicate position)
        (unless first (setq first position))
        (when (and (null button) (get-char-property position 'button))
          (setq button position))))
    (or button first)))

(defun org-other-agenda--same-source-location-p
    (left-buffer left-position right-buffer right-position)
  "Return non-nil when two buffer positions name one source place.

The positions are LEFT-POSITION in LEFT-BUFFER and RIGHT-POSITION in
RIGHT-BUFFER."
  (and (buffer-live-p left-buffer) (buffer-live-p right-buffer)
       (integer-or-marker-p left-position)
       (integer-or-marker-p right-position)
       (= left-position right-position)
       (or (eq left-buffer right-buffer)
           (let ((left-file (buffer-file-name left-buffer))
                 (right-file (buffer-file-name right-buffer)))
             (and left-file right-file
                  (equal (file-truename left-file)
                         (file-truename right-file)))))))

(defun org-other-agenda--same-marker-location-p (left right)
  "Return non-nil when LEFT and RIGHT designate the same source location."
  (and (markerp left) (markerp right)
       (org-other-agenda--same-source-location-p
        (marker-buffer left) (marker-position left)
        (marker-buffer right) (marker-position right))))

(defun org-other-agenda--heading-anchor (hd)
  "Return a normalized heading marker copied from HD, or nil."
  (when (and (markerp hd) (buffer-live-p (marker-buffer hd)))
    (condition-case nil
        (with-current-buffer (marker-buffer hd)
          (save-restriction
            (widen)
            (save-excursion
              (goto-char hd)
              (org-back-to-heading t)
              (copy-marker (point) nil))))
      (error nil))))

(defun org-other-agenda--heading-title (hd)
  "Return the source heading title for HD, or a useful fallback."
  (if-let* ((anchor (org-other-agenda--heading-anchor hd)))
      (prog1
          (org-with-point-at anchor (org-get-heading t t t t))
        (set-marker anchor nil))
    (format "%s" hd)))

(defun org-other-agenda--marked-headings ()
  "Return the view-local set of normalized heading markers."
  (and (boundp 'textui-state) (plist-get textui-state :marked)))

(defun org-other-agenda--marked-member-p (anchor)
  "Return non-nil when normalized heading ANCHOR is marked."
  (and anchor
       (cl-some (lambda (marked)
                  (org-other-agenda--same-marker-location-p anchor marked))
                (org-other-agenda--marked-headings))))

(defun org-other-agenda--heading-marked-p (hd)
  "Return non-nil when heading HD belongs to the marked set."
  (when-let* ((anchor (org-other-agenda--heading-anchor hd)))
    (prog1 (org-other-agenda--marked-member-p anchor)
      (set-marker anchor nil))))

(defun org-other-agenda--add-marked-heading (hd marked)
  "Return MARKED with heading HD added exactly once."
  (if-let* ((anchor (org-other-agenda--heading-anchor hd)))
      (if (cl-some (lambda (candidate)
                     (org-other-agenda--same-marker-location-p
                      anchor candidate))
                   marked)
          (prog1 marked (set-marker anchor nil))
        (cons anchor marked))
    marked))

(defun org-other-agenda--remove-marked-heading (hd marked)
  "Return MARKED without the heading designated by HD."
  (if-let* ((anchor (org-other-agenda--heading-anchor hd)))
      (prog1
          (cl-remove-if
           (lambda (candidate)
             (org-other-agenda--same-marker-location-p anchor candidate))
           marked)
        (set-marker anchor nil))
    marked))

(defun org-other-agenda--occurrence-anchor (occurrence)
  "Return a stable copy of OCCURRENCE's source timestamp anchor."
  (let ((source (plist-get occurrence :org-marker)))
    (when (and (markerp source) (marker-buffer source))
      (with-current-buffer (marker-buffer source)
        (save-excursion
          (save-match-data
            (goto-char source)
            (copy-marker
             (if (org-at-timestamp-p 'lax)
                 (match-beginning 0)
               (marker-position source))
             nil)))))))

(defun org-other-agenda--shadow-occurrence-position (occurrence buffer)
  "Return the exact agenda row for OCCURRENCE in shadow BUFFER."
  (let ((source (plist-get occurrence :org-marker)))
    (car-safe
     (org-other-agenda--shadow-line
      buffer
      (lambda ()
        (and source
             (org-other-agenda--same-marker-location-p
              source (org-get-at-bol 'org-marker))
             t))))))

(defun org-other-agenda--position-for-occurrence (occurrence)
  "Return the preferred rendered position for OCCURRENCE."
  (let ((source (plist-get occurrence :org-marker))
        (source-position (plist-get occurrence :source-position))
        (source-buffer (plist-get occurrence :source-buffer)))
    (org-other-agenda--preferred-property-position
     'org-other-agenda-occurrence
     (lambda (position)
       (let* ((candidate
               (get-text-property position 'org-other-agenda-occurrence))
              (marker (plist-get candidate :org-marker)))
         (or (and source-position source-buffer (markerp marker)
                  (org-other-agenda--same-source-location-p
                   source-buffer source-position
                   (marker-buffer marker) (marker-position marker)))
             (and source
                  (org-other-agenda--same-marker-location-p
                   source marker))))))))

(defun org-other-agenda--position-for-heading (hd)
  "Return the preferred rendered position for heading marker HD."
  (org-other-agenda--preferred-property-position
   'org-other-agenda-object
   (lambda (position)
     (org-other-agenda--same-marker-location-p
      hd (org-other-agenda--object-hd position)))))

(defun org-other-agenda--pin-heading (&optional target)
  "Return the heading marker represented by TARGET or point."
  (let ((target (or target org-other-agenda--embark-selected-target)))
    (cond
     ((markerp target) target)
     ((stringp target) (get-text-property 0 'org-marker target))
     (t (condition-case nil
            (org-other-agenda--at-point)
          (user-error nil))))))

(defun org-other-agenda--pin-state (&optional heading)
  "Return `pin' or `unpin' when HEADING supports that Today action."
  (when (and org-other-agenda--view
             (derived-mode-p 'textui-mode)
             (eq (plist-get textui-state :tab) 'day))
    (when-let* ((heading (org-other-agenda--pin-heading heading)))
      (cond
       ((cl-some
         (lambda (pinned)
           (org-other-agenda--same-marker-location-p heading pinned))
         (plist-get textui-state :pinned))
        'unpin)
       ((cl-some
         (lambda (block)
           (org-other-agenda--same-marker-location-p
            heading (plist-get block :hd)))
         (plist-get textui-state :day-overflow))
        'pin)))))

(defun org-other-agenda--pin-action-filter (binding)
  "Return BINDING only when point supports the Today pin action."
  (and (org-other-agenda--pin-state) binding))

(defun org-other-agenda--pin-action-label ()
  "Return the context-sensitive label for the Today pin action."
  (if (eq (org-other-agenda--pin-state) 'unpin)
      "Unpin from Today main lane"
    "Pin to Today main lane"))

(defun org-other-agenda-toggle-pin (&optional heading)
  "Toggle Today main-lane pinning for rendered HEADING.

HEADING may be a source marker supplied by the overflow widget.  During an
Embark action, the selected `org-heading' target supplies it dynamically."
  (interactive)
  (let* ((view (current-buffer))
         (heading (org-other-agenda--pin-heading heading))
         (state (and heading (org-other-agenda--pin-state heading))))
    (unless state
      (user-error "No pinnable Today heading at point"))
    (let ((pinned (plist-get textui-state :pinned)))
      (setq pinned
            (if (eq state 'unpin)
                (cl-remove-if
                 (lambda (candidate)
                   (org-other-agenda--same-marker-location-p
                    heading candidate))
                 pinned)
              (cons (copy-marker heading nil) pinned)))
      (textui-set-state view :pinned pinned))
    (textui-refresh view)
    (when-let* ((position (org-other-agenda--position-for-heading heading)))
      (goto-char position)
      (org-other-agenda--update-current-feedback))
    (message "%s %s"
             (if (eq state 'unpin) "Unpinned" "Pinned")
             (org-other-agenda--heading-title heading))))

(defun org-other-agenda--forward-occurrence (command)
  "Run native agenda COMMAND on the exact rendered source occurrence."
  (let* ((view (current-buffer))
         (occurrence (org-other-agenda--occurrence-at-point))
         (hd (org-other-agenda--at-point))
         (follow-occurrence
          (and occurrence
               (let ((copy (copy-sequence occurrence))
                     (source (org-other-agenda--occurrence-anchor occurrence)))
                 (when source
                   (setq copy (plist-put copy :org-marker source))
                   (setq copy
                         (plist-put copy :source-position
                                    (marker-position source)))
                   (setq copy
                         (plist-put copy :source-buffer
                                    (marker-buffer source))))
                 copy)))
         (follow-hd (and (markerp hd) (copy-marker hd)))
         (shadow (org-other-agenda--active-shadow view))
         (entry-key (org-other-agenda--active-entry-key view))
         (prefix current-prefix-arg)
         (shadow-position
          (and occurrence shadow
               (org-other-agenda--shadow-occurrence-position
                occurrence shadow)))
         succeeded)
    (unless occurrence
      (user-error "This heading has no source occurrence at point"))
    (unless (buffer-live-p shadow)
      (user-error "Org Other Agenda shadow buffer is not live"))
    (unless shadow-position
      (user-error "Could not find this occurrence in the shadow agenda"))
    (unwind-protect
        (save-window-excursion
          (org-other-agenda--with-shadow-agenda shadow prefix
            (goto-char shadow-position)
            ;; `goto-char' does not run the agenda post-command hook.  Mixed
            ;; series therefore need this explicit component update.
            (org-agenda-update-agenda-type)
            (call-interactively command)
            (setq succeeded t)))
      (when (buffer-live-p view)
        (if succeeded
            (with-current-buffer view
              ;; Date commands can relocate an occurrence.  Re-running the
              ;; series is required; editing the old agenda line is not.
              (let ((current-prefix-arg nil))
                (org-other-agenda-refresh)))
          (let ((entries (org-other-agenda--entries shadow)))
            (textui-set-state view entry-key entries)
            (when (eq entry-key :base-entries)
              (textui-set-state view :entries entries))))
        (textui-refresh view)
        (with-current-buffer view
          (when succeeded
            (when-let* ((position
                         (or (org-other-agenda--position-for-occurrence
                              follow-occurrence)
                             (org-other-agenda--position-for-heading
                              follow-hd))))
              (goto-char position)
              (org-other-agenda--update-current-feedback))))))))

(defun org-other-agenda-date-earlier ()
  "Move the exact Org timestamp at point earlier through native Agenda."
  (interactive)
  (org-other-agenda--forward-occurrence #'org-agenda-do-date-earlier))

(defun org-other-agenda-date-later ()
  "Move the exact Org timestamp at point later through native Agenda."
  (interactive)
  (org-other-agenda--forward-occurrence #'org-agenda-do-date-later))

(defun org-other-agenda-priority-up ()
  "Raise the current heading priority through native Agenda."
  (interactive)
  (org-other-agenda--forward-occurrence #'org-agenda-priority-up))

(defun org-other-agenda-priority-down ()
  "Lower the current heading priority through native Agenda."
  (interactive)
  (org-other-agenda--forward-occurrence #'org-agenda-priority-down))

(defvar org-other-agenda-view-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'org-other-agenda-refresh)
    (define-key map (kbd "/") #'org-other-agenda-search)
    (define-key map (kbd "\\") #'org-other-agenda-clear-search)
    (define-key map (kbd "[") #'org-other-agenda-previous-day)
    (define-key map (kbd "]") #'org-other-agenda-next-day)
    (define-key map (kbd "n") #'org-other-agenda-next-line)
    (define-key map (kbd "p") #'org-other-agenda-previous-line)
    (define-key map (kbd "u") #'org-other-agenda-parent)
    (define-key map (kbd "m") #'org-other-agenda-mark)
    (define-key map (kbd "U") #'org-other-agenda-unmark-all)
    (define-key map (kbd "B") #'org-other-agenda-bulk-action)
    (define-key map (kbd "<left>") #'org-other-agenda-left)
    (define-key map (kbd "<right>") #'org-other-agenda-right)
    (define-key map (kbd "<up>") #'org-other-agenda-up)
    (define-key map (kbd "<down>") #'org-other-agenda-down)
    (define-key map (kbd "M-n") #'org-other-agenda-next-panel)
    (define-key map (kbd "M-p") #'org-other-agenda-previous-panel)
    (define-key map (kbd "C-v") #'org-other-agenda-page-down)
    (define-key map (kbd "M-v") #'org-other-agenda-page-up)
    (define-key map [next] #'org-other-agenda-page-down)
    (define-key map [prior] #'org-other-agenda-page-up)
    (define-key map (kbd "TAB") #'widget-forward)
    (define-key map (kbd "S-TAB") #'widget-backward)
    (define-key map (kbd "<backtab>") #'widget-backward)
    (define-key map (kbd "q") #'quit-window)
    (dotimes (i 6)
      (let ((index i))
        (define-key map (kbd (number-to-string (1+ index)))
                    (lambda ()
                      (interactive)
                      (org-other-agenda-select-tab index)))))
    map)
  "View-wide verbs shared by the mode and renderer-owned object maps.")

(defvar org-other-agenda-day-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "m") #'org-other-agenda-mark-day)
    (define-key map (kbd "s") #'org-other-agenda-schedule-set-here)
    map)
  "Direct and Embark actions for an `ooa-day' noun.")

(defvar org-other-agenda-slot-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "s") #'org-other-agenda-schedule-set-here)
    map)
  "Direct and Embark actions for an `ooa-slot' noun.")

(defvar org-other-agenda--day-direct-map
  (make-composed-keymap
   (list org-other-agenda-day-map org-other-agenda-view-map))
  "Direct day actions combined with view-wide movement verbs.")

(defvar org-other-agenda--slot-direct-map
  (make-composed-keymap
   (list org-other-agenda-slot-map org-other-agenda-view-map))
  "Direct slot actions combined with view-wide movement verbs.")

(defvar org-other-agenda-entry-map
  (let ((map (make-sparse-keymap)))
    ;; Emacs 31 exposes this as `widget-keymap'; use the more specific name
    ;; when a future widget.el provides it.
    (set-keymap-parent
     map
     (make-composed-keymap
      (list org-other-agenda-view-map
            (if (boundp 'widget-button-keymap)
                (symbol-value 'widget-button-keymap)
              (symbol-value 'widget-keymap)))))
    (define-key map (kbd "t") #'org-other-agenda-todo)
    (define-key map (kbd "s") #'org-other-agenda-schedule)
    (define-key map (kbd "d") #'org-other-agenda-deadline)
    (define-key map (kbd ",") #'org-other-agenda-priority)
    (define-key map (kbd ":") #'org-other-agenda-set-tags)
    (define-key map (kbd "C-c C-p") #'org-other-agenda-toggle-pin)
    (define-key map (kbd "S-<left>") #'org-other-agenda-date-earlier)
    (define-key map (kbd "S-<right>") #'org-other-agenda-date-later)
    (define-key map (kbd "S-<up>") #'org-other-agenda-priority-up)
    (define-key map (kbd "S-<down>") #'org-other-agenda-priority-down)
    map)
  "Keymap for direct actions on the rendered Org entry at point.

The parent keeps widget RET, mouse, and entry-navigation behavior available on
button text.  Plain entry cells use `org-other-agenda-entry-cell-map'.")

(defvar org-other-agenda-entry-cell-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map org-other-agenda-entry-map)
    ;; `widget-button-press' falls back to the global RET binding when point
    ;; is on a fixed entry cell instead of the title button.  Shadow that
    ;; fallback so RET remains a row action throughout plain entry text.
    (define-key map (kbd "RET") #'org-other-agenda-entry-visit)
    map)
  "Keymap for non-button text belonging to a rendered Org entry.

It inherits the direct actions and widget navigation from
`org-other-agenda-entry-map', but gives fixed cells a useful RET action.")


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
  (copy-keymap org-other-agenda-view-map)
  "Major-mode fallback copy of `org-other-agenda-view-map'.

The view map is composed into renderer-owned text-property maps, while this
copy is layered over TextUI's local map for text without an object property.
Keeping the two variables distinct makes those two lookup roles explicit.")

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
          (textui-set-state (current-buffer) key next)
          next)))))

(defun org-other-agenda--heading-position-on-line ()
  "Return the preferred Org-heading position on the current line.

  Prefer the title widget, whose `button' property identifies the row's title
  cell.  Fixed cells, time-block text and Week bars have the same heading object
  but no button, so fall back to their first heading-bearing character."
  (save-excursion
    (let ((end (line-end-position)) first title)
      (goto-char (line-beginning-position))
      (while (< (point) end)
        (when (org-other-agenda--object-hd (point))
          (unless first
            (setq first (point)))
          (when (get-char-property (point) 'button)
            (unless title
              (setq title (point)))))
        ;; Heading identity and button membership only change at property
        ;; boundaries; scanning every character repeats the same queries.
        (goto-char (min end (next-char-property-change (point) end))))
      (or title first))))

(defun org-other-agenda--next-heading-position (direction)
  "Return the next heading occurrence position in `DIRECTION', or nil."
  (save-excursion
    (beginning-of-line)
    (catch 'found
      (while (= 0 (forward-line direction))
        (when-let* ((position (org-other-agenda--heading-position-on-line)))
          (throw 'found position)))
      nil)))

(defun org-other-agenda--page-heading-position (direction)
  "Return the first or last heading occurrence for `DIRECTION'."
  (save-excursion
    (goto-char (point-min))
    (let (last)
      (catch 'found
        (while (< (point) (point-max))
          (when-let* ((position (org-other-agenda--heading-position-on-line)))
            (if (> direction 0)
                (setq last position)
              (throw 'found position)))
          (forward-line 1))
        last))))

(defun org-other-agenda--move-to-entry (direction)
  "Move to the next or previous rendered entry in `DIRECTION'."
  (setq org-other-agenda--movement-scrolled nil)
  (let* ((place (org-other-agenda--place-at-point))
         ;; Fixed time blocks precede the list in the buffer, but hidden
         ;; earlier list rows precede its current first row in reading order.
         (previous (and (< direction 0)
                        (eq (car place) 'day-other)
                        (> (org-other-agenda--viewport-offset 'day) 0)
                        (= (nth 2 place)
                           (org-other-agenda--viewport-offset 'day))
                        (list 'day-other (nth 1 place) (1- (nth 2 place)))))
         (position
          (if previous
              (progn
                (org-other-agenda--expose-place previous)
                (setq org-other-agenda--movement-scrolled t)
                (org-other-agenda--position-for-place previous))
            (org-other-agenda--next-heading-position direction))))
    (if position
        (goto-char position)
      (let* ((scope (org-other-agenda--viewport-scope))
             (key (and scope (org-other-agenda--viewport-key scope "offset"))))
        (if (and key (org-other-agenda--viewport-scroll direction))
            (progn
              (setq org-other-agenda--movement-scrolled t)
              ;; `textui-set-state' queues a refresh; movement needs the new
              ;; rows synchronously so point can land on the row just exposed.
              (textui-refresh (current-buffer))
              (let ((position (org-other-agenda--page-heading-position direction)))
                (if position
                    (goto-char position)
                  (setq org-other-agenda--movement-scrolled nil)
                  (message "No %s entry"
                           (if (> direction 0) "next" "previous")))))
          (message "No %s entry"
                   (if (> direction 0) "next" "previous")))))))

(defun org-other-agenda--agenda-scroll (amount)
  "Move the Agenda viewport by `AMOUNT' rows.

Keep this private wrapper for callers from the original Agenda-only viewport
implementation; all tabs now use `org-other-agenda--viewport-scroll'."
  (if (eq (plist-get textui-state :tab) 'agenda)
      (org-other-agenda--viewport-scroll amount)
    (message "Paging is available on the Agenda tab")))

(defun org-other-agenda-next-line ()
  "Move point to the next rendered entry row."
  (interactive)
  (org-other-agenda--move-to-entry 1)
  (org-other-agenda--update-current-feedback))

(defun org-other-agenda-previous-line ()
  "Move point to the previous rendered entry row."
  (interactive)
  (org-other-agenda--move-to-entry -1)
  (org-other-agenda--update-current-feedback))

(defun org-other-agenda--navigation-position (&optional property)
  "Return a position for navigation PROPERTY at point or on its line."
  (let ((property (or property 'org-other-agenda-place)))
    (or (and (get-text-property (point) property) (point))
        (and (> (point) (point-min))
             (get-text-property (1- (point)) property)
             (1- (point)))
        (save-excursion
          (goto-char (line-beginning-position))
          (let ((end (line-end-position)))
            (catch 'found
              (while (< (point) end)
                (when (get-text-property (point) property)
                  (throw 'found (point)))
                (forward-char 1))
              nil))))))

(defun org-other-agenda--place-at-point ()
  "Return the structural place at point or on the current line."
  (when-let* ((position (org-other-agenda--navigation-position)))
    (get-text-property position 'org-other-agenda-place)))

(defun org-other-agenda--position-for-place (place)
  "Return the preferred rendered position for PLACE."
  (org-other-agenda--preferred-property-position
   'org-other-agenda-place
   (lambda (position)
     (equal (get-text-property position 'org-other-agenda-place) place))))

(defun org-other-agenda--rendered-places (panel)
  "Return the distinct structural places currently rendered for PANEL."
  (let (places)
    (dolist (position
             (org-other-agenda--property-positions
              'org-other-agenda-place))
      (let ((place (get-text-property position 'org-other-agenda-place)))
        (when (and (consp place) (eq (car place) panel))
          (cl-pushnew place places :test #'equal))))
    places))

(defun org-other-agenda--page-entries (panel &optional col)
  "Return semantic entries belonging to navigation PANEL and optional COL."
  (let* ((base (or (plist-get textui-state :base-entries)
                   (plist-get textui-state :entries)))
         (query (plist-get textui-state :query-entries))
         (selected (or (plist-get textui-state :date) (org-today)))
         (dow (mod (- (calendar-day-of-week
                       (calendar-gregorian-from-absolute selected)) 1) 7))
         (monday (- selected dow)))
    (pcase panel
      ('agenda (cdr (assq 'agenda (org-other-agenda--classify base))))
      ('someday (cdr (assq 'someday (org-other-agenda--classify base))))
      ((or 'search 'tags)
       (cdr (assq panel (org-other-agenda--classify query))))
      ('day-other
       (cl-remove-if
        (lambda (entry) (plist-get entry :time))
        (cl-remove-if-not
         (lambda (entry)
           (and (eq (plist-get entry :type) 'agenda)
                (equal (plist-get entry :dayabs) selected)))
         base)))
      ('week-days
       (cl-remove-if-not
        (lambda (entry)
          (and (eq (plist-get entry :type) 'agenda)
               (equal (plist-get entry :dayabs) (+ monday col))
               (not (equal (plist-get entry :etype) "block"))))
        base))
      (_ nil))))

(defun org-other-agenda--rendered-heading-places (panel)
  "Return rendered `(PLACE . HEADING)' pairs belonging to PANEL."
  (let (pairs)
    (dolist (position
             (org-other-agenda--property-positions
              'org-other-agenda-place))
      (let ((place (get-text-property position 'org-other-agenda-place))
            (hd (org-other-agenda--object-hd position)))
        (when (and (consp place) (eq (car place) panel) hd)
          (cl-pushnew (cons place hd) pairs
                      :test (lambda (left right)
                              (and (equal (car left) (car right))
                                   (org-other-agenda--same-marker-location-p
                                    (cdr left) (cdr right))))))))
    pairs))

(defun org-other-agenda--rectangle-heading-places (panel)
  "Return all semantic `(PLACE . HEADING)' pairs in PANEL."
  (pcase panel
    ('agenda
     (cl-loop for item in
              (org-other-agenda--agenda-items
               (org-other-agenda--page-entries 'agenda))
              for row from 0
              when (eq (plist-get item :kind) 'entry)
              collect (cons (list 'agenda 0 row)
                            (plist-get (plist-get item :entry) :hd))))
    ((or 'someday 'search 'tags 'day-other)
     (cl-loop for entry in (org-other-agenda--page-entries panel)
              for row from 0
              collect (cons (list panel 0 row) (plist-get entry :hd))))
    ('day-overflow
     (cl-loop for block in (plist-get textui-state :day-overflow)
              for row from 0
              collect (cons (list panel 0 row)
                            (plist-get (plist-get block :entry) :hd))))
    ('week-days
     (cl-loop for col from 0 below 7 append
              (cl-loop for entry in
                       (org-other-agenda--page-entries panel col)
                       for row from 0
                       collect (cons (list panel col row)
                                     (plist-get entry :hd)))))
    ((or 'week-bars 'day-timeblock)
     (org-other-agenda--rendered-heading-places panel))
    (_ nil)))

(defun org-other-agenda--place-in-rectangle-p (place start end)
  "Return non-nil when PLACE is inside the closed START to END rectangle."
  (and (eq (car place) (car start))
       (>= (nth 2 place) 0)
       (<= (min (nth 1 start) (nth 1 end))
           (nth 1 place)
           (max (nth 1 start) (nth 1 end)))
       (<= (min (nth 2 start) (nth 2 end))
           (nth 2 place)
           (max (nth 2 start) (nth 2 end)))))

(defun org-other-agenda--selection-refresh (place)
  "Synchronously render selection state and restore structural PLACE."
  (textui-refresh (current-buffer))
  (when-let* ((position (org-other-agenda--position-for-place place)))
    (goto-char position)
    (org-other-agenda--update-current-feedback)))

(defun org-other-agenda--day-headings (day)
  "Return the heading markers represented on absolute DAY."
  (let ((entries (append (plist-get textui-state :base-entries)
                         (plist-get textui-state :query-entries)))
        headings)
    (dolist (entry entries (nreverse headings))
      (when (equal (plist-get entry :dayabs) day)
        (let ((hd (plist-get entry :hd)))
          (unless (cl-some
                   (lambda (candidate)
                     (org-other-agenda--same-marker-location-p hd candidate))
                   headings)
            (push hd headings)))))))

(defun org-other-agenda-mark-day (&optional target)
  "Toggle all headings on the day at point or Embark TARGET."
  (interactive)
  (let* ((day (org-other-agenda--noun-value target 'ooa-day))
         (place (org-other-agenda--place-at-point))
         (headings (and (integerp day)
                        (org-other-agenda--day-headings day)))
         (marked (org-other-agenda--marked-headings)))
    (unless (integerp day)
      (user-error "No calendar day at point"))
    (if (null headings)
        (progn
          (setq textui-state
                (plist-put textui-state :rectangle-anchor nil))
          (message "No headings on this day"))
      (if (cl-every #'org-other-agenda--heading-marked-p headings)
          (dolist (hd headings)
            (setq marked
                  (org-other-agenda--remove-marked-heading hd marked)))
        (dolist (hd headings)
          (setq marked (org-other-agenda--add-marked-heading hd marked))))
      (setq textui-state (plist-put textui-state :marked marked))
      (setq textui-state
            (plist-put textui-state :rectangle-anchor nil))
      (org-other-agenda--selection-refresh place)
      (message "%d headings marked" (length marked)))))

(defun org-other-agenda--mark-rectangle (start end marked)
  "Return MARKED with every heading in the START to END rectangle added."
  (dolist (pair (org-other-agenda--rectangle-heading-places (car start)) marked)
    (when (org-other-agenda--place-in-rectangle-p (car pair) start end)
      (setq marked
            (org-other-agenda--add-marked-heading (cdr pair) marked)))))

(defun org-other-agenda-mark ()
  "Toggle a heading mark or complete a structural rectangle selection.

The first invocation stores the current structural place as an anchor.  A
second invocation at another place in the same panel adds the closed rectangle
and clears the anchor.  Repeating the command at the anchor only ends the
rectangle gesture, leaving the single-heading toggle unchanged."
  (interactive)
  (let* ((place (org-other-agenda--place-at-point))
         (object (org-other-agenda--embark-object-at-point))
         (hd (and (eq (car-safe object) 'org-heading) (cdr object)))
         (day-object (and (eq (car-safe object) 'ooa-day) object))
         (anchor (plist-get textui-state :rectangle-anchor))
         (marked (org-other-agenda--marked-headings)))
    (cond
     (day-object
      (org-other-agenda-mark-day))
     ((or (null place) (< (nth 2 place) 0))
      (user-error "No selectable structural cell at point"))
     ((and anchor (not (eq (car anchor) (car place))))
      (user-error "Rectangle endpoints must belong to the same panel"))
     (anchor
      (unless (equal anchor place)
        (setq marked
              (org-other-agenda--mark-rectangle anchor place marked)))
      (setq textui-state (plist-put textui-state :marked marked))
      (setq textui-state
            (plist-put textui-state :rectangle-anchor nil))
      (org-other-agenda--selection-refresh place)
      (message "%d headings marked" (length marked)))
     (t
      (when hd
        (setq marked
              (if (org-other-agenda--heading-marked-p hd)
                  (org-other-agenda--remove-marked-heading hd marked)
                (org-other-agenda--add-marked-heading hd marked))))
      (setq textui-state (plist-put textui-state :marked marked))
      (setq textui-state
            (plist-put textui-state :rectangle-anchor place))
      (org-other-agenda--selection-refresh place)
      (message "%d headings marked; rectangle anchor set" (length marked))))))

(defun org-other-agenda-unmark-all ()
  "Clear the marked heading set and any pending rectangle anchor."
  (interactive)
  (let ((place (org-other-agenda--place-at-point)))
    (dolist (marker (org-other-agenda--marked-headings))
      (when (markerp marker) (set-marker marker nil)))
    (setq textui-state (plist-put textui-state :marked nil))
    (setq textui-state
          (plist-put textui-state :rectangle-anchor nil))
    (org-other-agenda--selection-refresh place)
    (message "No headings marked")))

(defun org-other-agenda--shadow-heading (heading shadow)
  "Return `(POSITION . MARKER)' for HEADING in agenda SHADOW, or nil."
  (org-other-agenda--shadow-line
   shadow
   (lambda ()
     (let* ((candidate (org-get-at-bol 'org-hd-marker))
            (anchor (org-other-agenda--heading-anchor candidate)))
       (when anchor
         (prog1 (and (org-other-agenda--same-marker-location-p
                      heading anchor)
                     candidate)
           (set-marker anchor nil)))))))

(defun org-other-agenda--resolve-marked-headings (shadow)
  "Resolve every marked heading to SHADOW's exact agenda marker objects."
  (let (resolved missing)
    (dolist (heading (org-other-agenda--marked-headings))
      (if-let* ((pair (and (buffer-live-p shadow)
                           (org-other-agenda--shadow-heading heading shadow))))
          (push (cdr pair) resolved)
        (push (org-other-agenda--heading-title heading) missing)))
    (when missing
      (user-error "Marked headings are absent from this agenda: %s"
                  (mapconcat #'identity (nreverse missing) ", ")))
    (nreverse resolved)))

(defun org-other-agenda-bulk-action ()
  "Run native `org-agenda-bulk-action' on the marked heading set."
  (interactive)
  (let* ((view (current-buffer))
         (place (org-other-agenda--place-at-point))
         (shadow (org-other-agenda--active-shadow view))
         (prefix current-prefix-arg)
         (marked (org-other-agenda--marked-headings))
         (resolved (and marked
                        (org-other-agenda--resolve-marked-headings shadow)))
         succeeded keep-marks failure)
    (unless marked
      (user-error "No headings marked"))
    (condition-case error-data
        (save-window-excursion
          (org-other-agenda--with-shadow-agenda shadow prefix
            (let ((org-agenda-bulk-marked-entries resolved))
              (goto-char
               (or (text-property-any
                    (point-min) (point-max)
                    'org-hd-marker (car resolved))
                   (point-min)))
              (org-agenda-update-agenda-type)
              (call-interactively #'org-agenda-bulk-action)
              (setq succeeded t
                    keep-marks
                    (and org-agenda-bulk-marked-entries t)))))
      ((error quit) (setq failure error-data)))
    (when (buffer-live-p view)
      (with-current-buffer view
        (when (and succeeded (not keep-marks))
          (setq textui-state (plist-put textui-state :marked nil)))
        (setq textui-state
              (plist-put textui-state :rectangle-anchor nil))
        (org-other-agenda-refresh)
        (textui-refresh view)
        (when (and place
                   (org-other-agenda--position-for-place place))
          (goto-char (org-other-agenda--position-for-place place)))))
    (when failure
      (signal (car failure) (cdr failure)))))

(defun org-other-agenda--schedule-target (target)
  "Return `(DAY START END)' represented by Embark TARGET or point."
  (let ((target (or target org-other-agenda--embark-selected-target)))
    (or (org-other-agenda--noun-value target 'ooa-slot)
        (when-let* ((day (org-other-agenda--noun-value target 'ooa-day)))
          (list day nil nil)))))

(cl-defun org-other-agenda--with-embark-selected-target
    (&rest arguments &key run target &allow-other-keys)
  "Call RUN with Embark TARGET available to the board action.

ARGUMENTS are the around-action hook arguments supplied by Embark."
  (let ((org-other-agenda--embark-selected-target target))
    (apply run arguments)))

(defcustom org-other-agenda-schedule-slot-duration nil
  "How long a task scheduled onto a time slot is booked for.

nil writes the start time alone, <2026-08-20 Thu 14:00>.  `effort' writes a
range as long as the task's effort, <2026-08-20 Thu 14:00-14:45>; a task
without an effort, or one whose range would run past midnight, gets the
start time alone.  Org reads the end of a range as the entry's duration, so
the Today grid draws the task at that length and tools that look for
overlapping appointments can see it."
  :type '(choice (const :tag "Start time only" nil)
                 (const :tag "Start plus the task's effort" effort))
  :group 'org-other-agenda)

(defun org-other-agenda--slot-minutes (heading)
  "Return the minutes to book HEADING for on a time slot, or nil.
See `org-other-agenda-schedule-slot-duration'."
  (when (eq org-other-agenda-schedule-slot-duration 'effort)
    (when-let* ((anchor (org-other-agenda--heading-anchor heading)))
      (prog1
          (let ((effort (org-entry-get anchor org-effort-property)))
            (when (and effort (string-match-p "[0-9]" effort))
              (let ((minutes (ignore-errors (org-duration-to-minutes effort))))
                (and minutes (> minutes 0) (round minutes)))))
        (set-marker anchor nil)))))

(defun org-other-agenda--schedule-time (day start &optional minutes)
  "Return an Org date string for absolute DAY and optional START minutes.
With MINUTES as well, return a time range that long, unless it would run
past midnight."
  (pcase-let ((`(,month ,date ,year)
               (calendar-gregorian-from-absolute day)))
    (format "%04d-%02d-%02d%s" year month date
            (cond ((null start) "")
                  ((and minutes (<= (+ start minutes) (* 24 60)))
                   (let ((end (+ start minutes)))
                     (format " %02d:%02d-%02d:%02d"
                             (/ start 60) (% start 60) (/ end 60) (% end 60))))
                  (t (format " %02d:%02d" (/ start 60) (% start 60)))))))

(defun org-other-agenda-schedule-set-here (&optional target)
  "Schedule every marked heading on the day or slot at point.

TARGET is the string supplied by an Embark day or slot action.  Each heading
is delegated independently to `org-agenda-schedule'.  Successful headings are
removed from the set; failures remain marked and are summarized."
  (interactive)
  (let* ((view (current-buffer))
         (place (org-other-agenda--place-at-point))
         (destination (org-other-agenda--schedule-target target))
         (day (nth 0 destination))
         (start (nth 1 destination))
         (shadow (org-other-agenda--active-shadow view))
         (time (and (integerp day)
                    (org-other-agenda--schedule-time day start)))
         (marked (copy-sequence (org-other-agenda--marked-headings)))
         (remaining (org-other-agenda--marked-headings))
         successes failures)
    (unless destination
      (user-error "No day or time slot at point"))
    (unless marked
      (user-error "No headings marked"))
    (dolist (heading marked)
      (let ((title (org-other-agenda--heading-title heading))
            (pair (and (buffer-live-p shadow)
                       (org-other-agenda--shadow-heading heading shadow))))
        (if (null pair)
            (push (format "%s (absent from this agenda)" title) failures)
          (condition-case error-data
              (save-window-excursion
                (org-other-agenda--with-shadow-agenda shadow nil
                  (goto-char (car pair))
                  (org-agenda-update-agenda-type)
                  (org-agenda-schedule
                   nil
                   (if start
                       (org-other-agenda--schedule-time
                        day start (org-other-agenda--slot-minutes heading))
                     time)))
                (push title successes)
                (setq remaining
                      (org-other-agenda--remove-marked-heading
                       heading remaining)))
            (error
             (push (format "%s (%s)" title
                           (error-message-string error-data))
                   failures))))))
    (setq textui-state (plist-put textui-state :marked remaining))
    (setq textui-state
          (plist-put textui-state :rectangle-anchor nil))
    (org-other-agenda-refresh)
    (textui-refresh view)
    (when (and place (org-other-agenda--position-for-place place))
      (goto-char (org-other-agenda--position-for-place place)))
    (let ((report
           (format "Scheduled %d; failed %d%s"
                   (length successes) (length failures)
                   (if failures
                       (concat ": "
                               (mapconcat #'identity
                                          (nreverse failures) "; "))
                     ""))))
      (message "%s" report)
      report)))

(defun org-other-agenda--panel-row-count (panel col)
  "Return the number of non-header structural rows in PANEL column COL."
  (pcase panel
    ('agenda
     (length (org-other-agenda--agenda-items
              (org-other-agenda--page-entries 'agenda))))
    ((or 'someday 'search 'tags 'day-other 'week-days)
     (length (org-other-agenda--page-entries panel col)))
    ('day-overflow (length (plist-get textui-state :day-overflow)))
    (_
     (let ((rows (delq nil
                       (mapcar (lambda (place)
                                 (and (>= (nth 2 place) 0) (nth 2 place)))
                               (org-other-agenda--rendered-places panel)))))
       (if rows (1+ (apply #'max rows)) 0)))))

(defun org-other-agenda--simple-vertical-panel-p (panel)
  "Return non-nil when PANEL has one vertical structural column."
  (memq panel '(agenda someday search tags day-overflow day-other)))

(defun org-other-agenda--neighbor-place (place direction)
  "Return PLACE's structural neighbor in DIRECTION, or nil."
  (pcase-let ((`(,panel ,col ,row) place))
    (cond
     ((org-other-agenda--simple-vertical-panel-p panel)
      (when (memq direction '(up down))
        (let ((target (+ row (if (eq direction 'up) -1 1)))
              (count (org-other-agenda--panel-row-count panel col)))
          (and (>= target 0) (< target count)
               (list panel col target)))))
     ((eq panel 'day-calendar)
      (let* ((index (+ (* row 7) col))
             (target
              (pcase direction
                ('left (1- index)) ('right (1+ index))
                ('up (- index 7)) ('down (+ index 7)))))
        (and target (<= 0 target) (< target 42)
             (list panel (% target 7) (/ target 7)))))
     ((eq panel 'day-timeblock)
      (let ((target-col (+ col (pcase direction ('left -1) ('right 1) (_ 0))))
            (target-row (+ row (pcase direction ('up -1) ('down 1) (_ 0))))
            (places (org-other-agenda--rendered-places panel)))
        (cl-find (list panel target-col target-row) places :test #'equal)))
     ((eq panel 'week-bars)
      (let ((target (pcase direction
                      ('left (list panel (1- col) row))
                      ('right (list panel (1+ col) row))
                      ('up (list panel col (1- row)))
                      ('down (list panel col (1+ row))))))
        (cl-find target (org-other-agenda--rendered-places panel) :test #'equal)))
     ((eq panel 'week-days)
      (pcase direction
        ((or 'left 'right)
         (let ((target-col (+ col (if (eq direction 'left) -1 1))))
           (when (<= 0 target-col 6)
             (let ((count (org-other-agenda--panel-row-count panel target-col)))
               (list panel target-col
                     (if (< row 0) -1
                       (if (> count 0) (min row (1- count)) -1)))))))
        ('up
         (and (> row -1) (list panel col (1- row))))
        ('down
         (let ((count (org-other-agenda--panel-row-count panel col)))
           (and (< row (1- count)) (list panel col (1+ row)))))))
     (t nil))))

(defun org-other-agenda--place-viewport-scope (place)
  "Return the viewport scope capable of exposing PLACE."
  (pcase (car place)
    ('agenda 'agenda)
    ('day-other 'day)
    ('week-days 'week)
    ('someday 'someday)
    ('search 'search)
    ('tags 'tags)
    (_ nil)))

(defun org-other-agenda--expose-place (place)
  "Adjust one viewport and synchronously render PLACE."
  (when-let* ((scope (org-other-agenda--place-viewport-scope place)))
    (let* ((row (nth 2 place))
           (offset (org-other-agenda--viewport-offset scope))
           (page-size (or (plist-get textui-state
                                     (org-other-agenda--viewport-key
                                      scope "page-size"))
                          (org-other-agenda--viewport-page-size scope)))
           (visible (max 1 (1- page-size)))
           (next (cond ((< row offset) row)
                       ((>= row (+ offset visible))
                        (max 0 (- row visible -1)))
                       (t offset))))
      (unless (= next offset)
        (textui-set-state
         (current-buffer)
         (org-other-agenda--viewport-key scope "offset") next)
        (textui-refresh (current-buffer))))))

(defun org-other-agenda--move-structural (direction)
  "Move point to the structural neighbor in DIRECTION."
  (let* ((place (org-other-agenda--place-at-point))
         (target (and place
                      (org-other-agenda--neighbor-place place direction))))
    (cond
     ((null place) (message "No structural noun at point"))
     ((null target)
      (message "No %s neighbor in %s" direction (car place)))
     (t
      (or (org-other-agenda--position-for-place target)
          (progn
            (org-other-agenda--expose-place target)
            (org-other-agenda--position-for-place target)))
      (if-let* ((position (org-other-agenda--position-for-place target)))
          (progn
            (goto-char position)
            (org-other-agenda--update-current-feedback))
        (message "Could not expose %s in %s" target (car place)))))))

(defun org-other-agenda-left ()
  "Move point to the left structural neighbor."
  (interactive)
  (org-other-agenda--move-structural 'left))

(defun org-other-agenda-right ()
  "Move point to the right structural neighbor."
  (interactive)
  (org-other-agenda--move-structural 'right))

(defun org-other-agenda-up ()
  "Move point to the upper structural neighbor."
  (interactive)
  (org-other-agenda--move-structural 'up))

(defun org-other-agenda-down ()
  "Move point to the lower structural neighbor."
  (interactive)
  (org-other-agenda--move-structural 'down))

(defun org-other-agenda--panel-order ()
  "Return the ordered structural panels for the active tab."
  (pcase (plist-get textui-state :tab)
    ('day (append '(day-calendar day-timeblock)
                  (and (plist-get textui-state :day-overflow)
                       '(day-overflow))
                  '(day-other)))
    ('week
     (append (and (> (org-other-agenda--panel-row-count 'week-bars 0) 0)
                  '(week-bars))
             '(week-days)))
    ('agenda '(agenda))
    ('someday '(someday))
    ('search '(search))
    ('tags '(tags))
    (_ nil)))

(defun org-other-agenda--place-less-p (left right)
  "Return non-nil when LEFT precedes RIGHT inside one panel."
  (or (< (nth 2 left) (nth 2 right))
      (and (= (nth 2 left) (nth 2 right))
           (< (nth 1 left) (nth 1 right)))))

(defun org-other-agenda--first-place-in-panel (panel)
  "Return the first rendered noun place in PANEL, falling back to its title."
  (let* ((places (org-other-agenda--rendered-places panel))
         (body (cl-remove-if (lambda (place) (< (nth 2 place) 0)) places))
         (headers (cl-remove-if-not (lambda (place) (= (nth 2 place) -1)) places)))
    (car (sort (or body headers
                   (and (member (org-other-agenda--panel-place panel) places)
                        (list (org-other-agenda--panel-place panel))))
               #'org-other-agenda--place-less-p))))

(defun org-other-agenda--move-panel (direction)
  "Move to the adjacent panel in DIRECTION without wrapping."
  (let* ((place (org-other-agenda--place-at-point))
         (panel (car-safe place))
         (order (org-other-agenda--panel-order))
         (index (cl-position panel order))
         (target-index (and index (+ index direction)))
         (target-panel (and target-index
                            (<= 0 target-index)
                            (< target-index (length order))
                            (nth target-index order)))
         (target-place (and target-panel
                            (org-other-agenda--first-place-in-panel target-panel))))
    (if-let* ((position (and target-place
                             (org-other-agenda--position-for-place target-place))))
        (progn
          (goto-char position)
          (org-other-agenda--update-current-feedback))
      (message "No %s panel from %s"
               (if (> direction 0) "next" "previous")
               (or panel "this point")))))

(defun org-other-agenda-next-panel ()
  "Move to the first noun of the next panel."
  (interactive)
  (org-other-agenda--move-panel 1))

(defun org-other-agenda-previous-panel ()
  "Move to the first noun of the previous panel."
  (interactive)
  (org-other-agenda--move-panel -1))

(defun org-other-agenda-parent ()
  "Move point to its explicit structural parent."
  (interactive)
  (if-let* ((position (org-other-agenda--navigation-position))
            (context (get-text-property
                      position 'org-other-agenda-context))
            (parent-place (car context))
            (parent-position
             (org-other-agenda--position-for-place parent-place)))
      (progn
        (goto-char parent-position)
        (org-other-agenda--update-current-feedback))
    (message "At top")))

(defun org-other-agenda--clear-current-feedback ()
  "Delete the current-object feedback overlays."
  (mapc #'delete-overlay org-other-agenda--current-object-overlays)
  (setq org-other-agenda--current-object-overlays nil))

(defun org-other-agenda--update-current-feedback ()
  "Highlight every rendered span of the structural noun at point."
  (org-other-agenda--clear-current-feedback)
  (when-let* ((place (org-other-agenda--place-at-point)))
    (save-excursion
      (goto-char (point-min))
      (while (< (point) (point-max))
        (let* ((start (point))
               (value (get-text-property start 'org-other-agenda-place))
               (end (or (next-single-property-change
                         start 'org-other-agenda-place nil (point-max))
                        (point-max))))
          (when (equal value place)
            (let ((overlay (make-overlay start end nil t nil)))
              (overlay-put overlay 'face 'org-other-agenda-current-object)
              (overlay-put overlay 'priority 20)
              (push overlay org-other-agenda--current-object-overlays)))
          (goto-char end))))))

(defun org-other-agenda--eldoc ()
  "Return structural identity text for the noun at point."
  (when-let* ((position (org-other-agenda--navigation-position))
              (place (get-text-property position 'org-other-agenda-place)))
    (let* ((page (or (plist-get textui-state :tab) 'unknown))
           (panel (car place))
           (col (nth 1 place))
           (row (nth 2 place))
           (context (get-text-property
                     position 'org-other-agenda-context))
           (container-place (car context))
           (container-position
            (and container-place
                 (org-other-agenda--position-for-place container-place)))
           (container-node
            (and container-position
                 (get-text-property
                  container-position 'org-other-agenda-node)))
           (container
            (cond ((and (consp container-node) (cdr container-node))
                   (format "%s" (cdr container-node)))
                  ((consp container-place)
                   (format "%s" (car container-place)))
                  (t (format "%s" panel))))
           (node (get-text-property position 'org-other-agenda-node))
           (object (get-text-property position 'org-other-agenda-object))
           (total (and (>= row 0)
                       (org-other-agenda--panel-row-count panel col)))
           (identity
            (cond ((and (consp node) (cdr node)) (format "%s" (cdr node)))
                  ((consp object) (format "%s" (car object)))
                  (t "structure"))))
      (format "%s · %s · %s · %s"
              page container
              (if total (format "%d/%d" (1+ row) total) "header")
              identity))))

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
  (let ((skip org-other-agenda--movement-scrolled))
    (setq org-other-agenda--movement-scrolled nil)
    (unless skip
      (when (and (derived-mode-p 'textui-mode)
                 (org-other-agenda--viewport-scope))
        (let* ((button (or (get-char-property (point) 'button)
                           (and (> (point) (point-min))
                                (get-char-property (1- (point)) 'button))))
               (hd (or (org-other-agenda--object-hd (point))
                       (and (> (point) (point-min))
                            (org-other-agenda--object-hd (1- (point))))
                       (and button (org-other-agenda--button-hd button))))
               (window (get-buffer-window (current-buffer) t)))
          (when (and hd (window-live-p window))
            (let* ((line (line-number-at-pos))
                   (top (with-current-buffer (window-buffer window)
                          (line-number-at-pos (window-start window))))
                   (bottom (+ top (max 1 (1- (window-body-height window)))))
                   (delta (cond ((<= line (+ top 1)) -1)
                                ((>= line (1- bottom)) 1))))
              (when delta
                (org-other-agenda--viewport-scroll delta)))))))))

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

(cl-defun org-other-agenda--embark-refresh
    (&key action type target &allow-other-keys)
  "Refresh the source board after Embark ACTION on TARGET of TYPE."
  (let ((view (and (eq type 'org-heading)
                   (stringp target)
                   (get-text-property
                    0 'org-other-agenda-origin target)))
        (marker (and (stringp target)
                     (get-text-property 0 'org-marker target))))
    (when (and (not (eq action 'org-other-agenda-toggle-pin))
               (buffer-live-p view)
               (buffer-local-value 'org-other-agenda--view view)
               (markerp marker) (buffer-live-p (marker-buffer marker)))
      ;; Embark runs the Org action at the source marker.  The target's
      ;; provenance is the smallest bridge back to the projection that
      ;; created it; external `org-heading' targets carry no such property.
      (with-current-buffer view
        (org-other-agenda-refresh)))))

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
                                     :marked nil
                                     :rectangle-anchor nil
                                     :agenda-offset 0
                                     :agenda-page-size 40
                                     :day-offset 0
                                     :week-offset 0
                                     :someday-offset 0
                                     :search-offset 0
                                     :tags-offset 0
                                     :viewport-page-size 40))))
    (with-current-buffer buffer
      (setq-local buffer-read-only t)
      (use-local-map (make-composed-keymap org-other-agenda-mode-map
                                           (current-local-map)))
      (add-hook 'post-command-hook #'org-other-agenda--follow-viewport nil t)
      (add-hook 'post-command-hook
                #'org-other-agenda--update-current-feedback nil t))
      buffer)))

(setq embark-target-finders
      (delq #'org-other-agenda-embark-target embark-target-finders))
(cl-pushnew #'org-other-agenda-embark-targets embark-target-finders)
(add-to-list 'embark-keymap-alist '(ooa-day org-other-agenda-day-map))
(add-to-list 'embark-keymap-alist '(ooa-slot org-other-agenda-slot-map))
(define-key embark-org-heading-map (kbd "C-c C-p")
            '(menu-item (org-other-agenda--pin-action-label)
                        org-other-agenda-toggle-pin
                        :filter org-other-agenda--pin-action-filter))
(dolist (action '(org-other-agenda-schedule-set-here
                  org-other-agenda-toggle-pin))
  (cl-pushnew #'org-other-agenda--with-embark-selected-target
              (alist-get action embark-around-action-hooks)))
(cl-pushnew #'org-other-agenda--embark-refresh
            (alist-get :always embark-post-action-hooks))

(provide 'org-other-agenda)
;;; org-other-agenda.el ends here
