;;; org-other-agenda-test.el --- ERT coverage for org-other-agenda -*- lexical-binding: t; -*-

;;; Commentary:

;; These tests exercise the semantic boundary between Org's agenda buffer and
;; org-other-agenda's projection.  The fixture date is deliberately pinned in
;; both places: the agenda start day and Org's notion of today.

;;; Code:

(require 'calendar)
(require 'cl-lib)
(require 'ert)
(require 'org-other-agenda)
(require 'org-habit)

(defconst org-other-agenda-test--start-day "2026-08-20"
  "The explicit agenda start day used by every generated fixture view.")

(defconst org-other-agenda-test--today
  (calendar-absolute-from-gregorian '(8 20 2026))
  "The absolute date used as the pinned value of `org-today'.")

(defconst org-other-agenda-test--fixture
  (expand-file-name
   "fixtures/agenda.org"
   (file-name-directory (or load-file-name buffer-file-name)))
  "Absolute path to the synthetic agenda fixture.")

(defconst org-other-agenda-test--view-buffer "*Org Other Agenda Test*"
  "Buffer name used for TextUI views created by the tests.")

(defun org-other-agenda-test--cleanup (&optional extra-file)
  "Remove buffers created while testing the fixture in EXTRA-FILE."
  (dolist (name (list org-other-agenda--shadow
                      org-other-agenda--query-shadow
                      org-other-agenda-test--view-buffer))
    (when-let* ((buffer (get-buffer name)))
      (kill-buffer buffer)))
  (let ((fixtures (mapcar #'file-truename
                         (delq nil (list org-other-agenda-test--fixture
                                         extra-file)))))
    (dolist (buffer (buffer-list))
      (let ((file (buffer-local-value 'buffer-file-name buffer)))
        (when (and file (member (file-truename file) fixtures))
          (kill-buffer buffer))))))

(defmacro org-other-agenda-test--with-agenda-file (fixture &rest body)
  "Run BODY with AGENDA-FILE and a deterministic Org clock.

The package generator receives `org-other-agenda-test--start-day'
explicitly.  The series is bound locally so this test does not depend on a
user's global block selection."
  (declare (indent 0) (debug t))
  `(let* ((org-agenda-files (list ,fixture))
          (org-enforce-todo-dependencies t)
          (org-enforce-todo-checkbox-dependencies t)
          (org-agenda-dim-blocked-tasks t)
          (org-habit-show-habits t)
          (org-blocker-hook
           '(org-block-todo-from-children-or-siblings-or-parent
             org-block-todo-from-checkboxes))
          (org-other-agenda-series
           '((agenda ""
                     ((org-agenda-span 'week)))
             (alltodo "")))
          (org-other-agenda-test--today-advice
           (lambda () org-other-agenda-test--today)))
     (advice-add 'org-today :override org-other-agenda-test--today-advice)
     (unwind-protect
       (progn ,@body)
       (advice-remove 'org-today org-other-agenda-test--today-advice)
       (org-other-agenda-test--cleanup ,fixture))))

(defmacro org-other-agenda-test--with-fixture (&rest body)
  "Run BODY with the synthetic agenda and a deterministic Org clock."
  (declare (indent 0) (debug t))
  `(org-other-agenda-test--with-agenda-file
     org-other-agenda-test--fixture
     ,@body))

(defun org-other-agenda-test--generate ()
  "Generate and return semantic entries for the pinned fixture week."
  (org-other-agenda--generate org-other-agenda-test--start-day)
  (org-other-agenda--entries))

(defun org-other-agenda-test--open-view (&optional name)
  "Open a TextUI view for the explicitly pinned fixture date."
  (let ((name (or name org-other-agenda-test--view-buffer)))
    (when-let* ((old (get-buffer name)))
      (kill-buffer old))
    (org-other-agenda--generate org-other-agenda-test--start-day)
    (let ((entries (org-other-agenda--entries)))
      (let ((view
             (textui-open name
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
        (with-current-buffer view
          (setq-local buffer-read-only t))
        view))))

(defun org-other-agenda-test--select-tab (view tab)
  "Select TAB in VIEW and synchronously render it."
  (textui-set-state view :tab tab)
  (textui-refresh view))

(defun org-other-agenda-test--widget-count (view)
  "Return the number of materialized TextUI widgets in VIEW."
  (with-current-buffer view
    (length textui--widgets)))

(defun org-other-agenda-test--make-size-fixture (count)
  "Create and return a temporary Org file containing COUNT dated TODOs."
  (let ((file (make-temp-file "org-other-agenda-size-" nil ".org")))
    (with-temp-file file
      (insert "#+TITLE: org-other-agenda viewport fixture\n"
              "#+TODO: TODO | DONE\n\n"
              "* Generated viewport tasks\n")
      (dotimes (index count)
        (insert (format "** TODO Generated task %04d                  :generated:\n"
                        (1+ index))
                "   SCHEDULED: <2026-08-20 Thu>\n")))
    file))

(defun org-other-agenda-test--measure-page-widgets (file tab)
  "Return the widget count for TAB generated from temporary Org FILE."
  (org-other-agenda-test--with-agenda-file file
    (let ((view (org-other-agenda-test--open-view)))
      (org-other-agenda-test--select-tab view tab)
      (org-other-agenda-test--widget-count view))))

(defun org-other-agenda-test--title (entry)
  "Return the display title for ENTRY."
  (org-other-agenda--clean-title entry))

(defun org-other-agenda-test--entry-by-title (entries title)
  "Return the first entry in `ENTRIES' whose title is `TITLE'."
  (cl-find-if (lambda (entry)
                (equal (org-other-agenda-test--title entry) title))
              entries))

(defun org-other-agenda-test--assert-rendered-target
    (view tab needle title)
  "Assert that `NEEDLE' in `TAB' resolves to the entry titled `TITLE'."
  (org-other-agenda-test--select-tab view tab)
  (with-current-buffer view
    (goto-char (point-min))
    (should (search-forward needle nil t))
    (goto-char (- (point) (length needle)))
    (let* ((entry (org-other-agenda-test--entry-by-title
                   (plist-get textui-state :entries) title))
           (marker (plist-get entry :hd))
           (object (get-text-property (point) 'org-other-agenda-object))
           (target (org-other-agenda-embark-target))
           (target-title (nth 1 target)))
      (should entry)
      (should (equal object (cons 'org-heading marker)))
      (should (equal (car target) 'org-heading))
      (should (equal (get-text-property 0 'org-marker target-title)
                     marker)))))

(defun org-other-agenda-test--goto-rendered-entry (view title)
  "Move point to a rendered object whose source heading is titled TITLE."
  (with-current-buffer view
    (let* ((entry
            (cl-loop for key in '(:query-entries :entries :base-entries)
                     thereis (org-other-agenda-test--entry-by-title
                              (plist-get textui-state key) title)))
           (marker (and entry (plist-get entry :hd))))
      (when (markerp marker)
        (goto-char (point-min))
        (catch 'found
          (while (< (point) (point-max))
            (when (equal (org-other-agenda--object-hd (point)) marker)
              (throw 'found t))
            (forward-char 1))
          nil)))))

(defun org-other-agenda-test--titles (entries)
  "Return cleaned titles for ENTRIES."
  (mapcar #'org-other-agenda-test--title entries))

(defun org-other-agenda-test--unique-by-hd (entries)
  "Return ENTRIES deduplicated by their `org-hd-marker' value."
  (cl-remove-duplicates entries
                         :test #'equal
                         :key (lambda (entry) (plist-get entry :hd))))

(defun org-other-agenda-test--timed-entry-p (entry titles)
  "Return non-nil when ENTRY is a target timed item on the pinned day."
  (and (eq (plist-get entry :type) 'agenda)
       (equal (plist-get entry :dayabs) org-other-agenda-test--today)
       (plist-get entry :time)
       (member (org-other-agenda-test--title entry) titles)))

(defun org-other-agenda-test--time-order-less-p (a b)
  "Order timed entries by start slot, then by descending duration."
  (let ((ra (org-other-agenda--slots a))
        (rb (org-other-agenda--slots b)))
    (if (= (car ra) (car rb))
        (> (- (cdr ra) (car ra)) (- (cdr rb) (car rb)))
      (< (car ra) (car rb)))))

(ert-deftest org-other-agenda-test-entry-keymap-resolves-actions ()
  "Direct actions resolve from every rendered entry-object span."
  (org-other-agenda-test--with-fixture
    (cl-letf (((symbol-function 'window-body-height)
               (lambda (&optional _window) 80)))
      (let ((view (org-other-agenda-test--open-view)))
        (org-other-agenda-test--select-tab view 'agenda)
        (with-current-buffer view
          (goto-char (point-min))
          (should (search-forward "Monday standup" nil t))
          (goto-char (1- (point)))
          (should (eq (key-binding (kbd "t")) #'org-other-agenda-todo))
          (should (eq (key-binding (kbd "m")) #'org-other-agenda-mark))
          (should (eq (key-binding (kbd "B"))
                      #'org-other-agenda-bulk-action))
          (dolist (binding '(("n" . org-other-agenda-next-line)
                             ("p" . org-other-agenda-previous-line)
                             ("u" . org-other-agenda-parent)
                             ("<left>" . org-other-agenda-left)
                             ("M-n" . org-other-agenda-next-panel)))
            (should (eq (key-binding (kbd (car binding)))
                        (cdr binding))))
          (dolist (binding '(("C-n" . next-line)
                             ("C-p" . previous-line)
                             ("C-f" . forward-char)
                             ("C-b" . backward-char)))
            (should (eq (key-binding (kbd (car binding)))
                        (cdr binding))))
          (should (eq (key-binding (kbd "S-<right>"))
                      #'org-other-agenda-date-later))
          (should (eq (key-binding (kbd "S-<up>"))
                      #'org-other-agenda-priority-up))
          (should (eq (key-binding (kbd "RET"))
                      #'widget-button-press))
          (beginning-of-line)
          (should (get-text-property (point) 'org-other-agenda-object))
          (should (eq (key-binding (kbd "t")) #'org-other-agenda-todo))
          (should (eq (key-binding (kbd "RET"))
                      #'org-other-agenda-entry-visit))
          (should (condition-case nil
                      (progn (call-interactively (key-binding (kbd "RET")))
                             t)
                    (error nil)))
          (should (markerp (org-other-agenda--at-point))))
        (org-other-agenda-test--select-tab view 'day)
        (with-current-buffer view
          (goto-char (point-min))
          (should (search-forward "Design review" nil t))
          (goto-char (1- (point)))
          (should (get-text-property (point) 'org-other-agenda-object))
          (should (eq (key-binding (kbd "t")) #'org-other-agenda-todo))
          (should (eq (key-binding (kbd "RET"))
                      #'org-other-agenda-entry-visit))
          (should (markerp (org-other-agenda--at-point))))
        (org-other-agenda-test--select-tab view 'agenda)
        (with-current-buffer view
          (goto-char (point-min))
          (should (search-forward "──" nil t))
          (beginning-of-line)
          (should-not (eq (key-binding (kbd "t")) #'org-other-agenda-todo))
          (should buffer-read-only))))))

(ert-deftest org-other-agenda-test-mode-map-reserves-movement-keys ()
  "The local map owns movement while actions and native line movement inherit."
  (dolist (key '("t" "s" "d" "," ":" "C-n" "C-p"))
    (should-not (lookup-key org-other-agenda-mode-map (kbd key))))
  (dolist (binding '(("<left>" . org-other-agenda-left)
                     ("<right>" . org-other-agenda-right)
                     ("<up>" . org-other-agenda-up)
                     ("<down>" . org-other-agenda-down)
                     ("M-n" . org-other-agenda-next-panel)
                     ("M-p" . org-other-agenda-previous-panel)
                     ("m" . org-other-agenda-mark)
                     ("U" . org-other-agenda-unmark-all)
                     ("B" . org-other-agenda-bulk-action)))
    (should (eq (lookup-key org-other-agenda-mode-map (kbd (car binding)))
                (cdr binding))))
  (dolist (key '("S-<left>" "S-<right>" "S-<up>" "S-<down>"
                 "M-<left>" "M-<right>" "M-<up>" "M-<down>"))
    (should-not (lookup-key org-other-agenda-mode-map (kbd key))))
  (dotimes (index 6)
    (should (commandp (lookup-key org-other-agenda-mode-map
                                  (kbd (number-to-string (1+ index)))))))
  (should-not (lookup-key org-other-agenda-mode-map (kbd "7"))))

(ert-deftest org-other-agenda-test-entry-movement-crosses-rows ()
  "n and p move between the first two rendered entry rows."
  (org-other-agenda-test--with-fixture
    (cl-letf (((symbol-function 'window-body-height)
               (lambda (&optional _window) 80)))
      (let ((view (org-other-agenda-test--open-view)))
        (org-other-agenda-test--select-tab view 'agenda)
        (with-current-buffer view
          (let* ((first-position (org-other-agenda--page-object-position -1))
                 (first-object (and first-position
                                    (get-text-property first-position
                                                        'org-other-agenda-object))))
            (should first-position)
            (goto-char first-position)
            (let* ((second-position
                    (org-other-agenda--next-object-position 1))
                   (second-object (and second-position
                                       (get-text-property second-position
                                                           'org-other-agenda-object))))
              (should second-position)
              (should second-object)
              (org-other-agenda-next-line)
              (should (equal (get-text-property (point)
                                                 'org-other-agenda-object)
                             second-object))
              (should (= (point) (org-other-agenda--object-position-on-line)))
              (org-other-agenda-previous-line)
              (should (equal (get-text-property (point)
                                                 'org-other-agenda-object)
                             first-object))
              (should (= (point) (org-other-agenda--object-position-on-line))))))))))

(ert-deftest org-other-agenda-test-entry-movement-scrolls-at-page-edge ()
  "n advances the offset and lands on the newly visible row at the edge."
  (org-other-agenda-test--with-fixture
    (cl-letf (((symbol-function 'window-body-height)
               (lambda (&optional _window) 9)))
      (let ((view (org-other-agenda-test--open-view)))
        (org-other-agenda-test--select-tab view 'agenda)
        (with-current-buffer view
          (let* ((last-position (org-other-agenda--page-object-position 1))
                 (old-offset (plist-get textui-state :agenda-offset)))
            (should last-position)
            (goto-char last-position)
            (org-other-agenda-next-line)
            (should (= (plist-get textui-state :agenda-offset)
                       (1+ old-offset)))
            (should (get-text-property (point) 'org-other-agenda-object))
            (should (= (point) (org-other-agenda--object-position-on-line)))
            (org-other-agenda-parent)
            (should (get-text-property (point) 'org-other-agenda-parent))))))))

(ert-deftest org-other-agenda-test-parent-moves-to-date-header ()
  "u moves an agenda entry to its nearest date header."
  (org-other-agenda-test--with-fixture
    (cl-letf (((symbol-function 'window-body-height)
               (lambda (&optional _window) 80)))
      (let ((view (org-other-agenda-test--open-view)))
        (org-other-agenda-test--select-tab view 'agenda)
        (with-current-buffer view
          (goto-char (point-min))
          (should (search-forward "Monday standup" nil t))
          (beginning-of-line)
          (org-other-agenda-parent)
          (should (looking-at "── Monday")))))))

(ert-deftest org-other-agenda-test-parent-covers-every-entry-surface ()
  "u reaches a parent from each page's one-dimensional entry surfaces."
  (org-other-agenda-test--with-fixture
    (cl-letf (((symbol-function 'window-body-height)
               (lambda (&optional _window) 80)))
      (let ((view (org-other-agenda-test--open-view)))
        (dolist (spec '((agenda "Monday standup")
                        (day "Design review")
                        (day "Ship the release notes")
                        (week "Write the quarterly report")
                        (week "Monday standup")
                        (someday "Read the RFC")))
          (org-other-agenda-test--select-tab view (car spec))
          (with-current-buffer view
            (should (org-other-agenda-test--goto-rendered-entry
                     view (cadr spec)))
            (org-other-agenda-parent)
            (should (get-text-property (point)
                                       'org-other-agenda-parent))))
        (dolist (query-spec '((search "invitations" "Prepare the launch checklist")
                              (tags "#build" "Draft the migration plan")))
          (with-current-buffer view
            (org-other-agenda-search (cadr query-spec)))
          (textui-refresh view)
          (org-other-agenda-test--select-tab view (car query-spec))
          (with-current-buffer view
            (should (org-other-agenda-test--goto-rendered-entry
                     view (nth 2 query-spec)))
            (org-other-agenda-parent)
            (should (get-text-property (point)
                                       'org-other-agenda-parent))))))))

(ert-deftest org-other-agenda-test-shadow-buffer-is-hidden ()
  "Generated agenda data lives in a buffer that is shown in no window."
  (org-other-agenda-test--with-fixture
    (org-other-agenda-test--generate)
    (should (buffer-live-p (get-buffer org-other-agenda--shadow)))
    (should-not (get-buffer-window org-other-agenda--shadow t))))

(ert-deftest org-other-agenda-test-sticky-setting-cannot-skip-refresh ()
  "The generator clears `org-agenda-sticky' for every refresh."
  (org-other-agenda-test--with-fixture
    (let (sticky-values)
      (let ((record-sticky
             (lambda (original &rest arguments)
               (push org-agenda-sticky sticky-values)
               (apply original arguments))))
        (advice-add 'org-agenda-run-series :around record-sticky)
        (unwind-protect
            (let ((org-agenda-sticky t))
              (should
               (condition-case nil
                   (progn
                     (org-other-agenda--generate
                      org-other-agenda-test--start-day)
                     t)
                 (error nil)))
              (should
               (condition-case nil
                   (progn
                     (org-other-agenda--generate
                      org-other-agenda-test--start-day)
                     t)
                 (error nil)))
              (should (equal (nreverse sticky-values) '(nil nil))))
          (advice-remove 'org-agenda-run-series record-sticky))))))

(ert-deftest org-other-agenda-test-extracts-blocked-and-habit-semantics ()
  "Extraction preserves three blocked entries and both habit entries."
  (org-other-agenda-test--with-fixture
    (let* ((entries (org-other-agenda-test--generate))
           (blocked (cl-remove-if-not
                     (lambda (entry) (plist-get entry :blocked))
                     entries))
           (blocked-entries (org-other-agenda-test--unique-by-hd blocked))
           (habits (cl-remove-if-not
                    (lambda (entry) (plist-get entry :habit))
                    entries))
           (habit-entries (org-other-agenda-test--unique-by-hd habits)))
      (should (= 3 (length blocked-entries)))
      (should (= 2 (length habit-entries)))
      (dolist (title '("Run the migration dry run"
                       "Cut over production"
                       "Prepare the launch checklist"))
        (should (member title (org-other-agenda-test--titles blocked-entries))))
      (should (member "Daily review" (org-other-agenda-test--titles habit-entries)))
      (should (member "Empty the inbox" (org-other-agenda-test--titles habit-entries))))))

(ert-deftest org-other-agenda-test-fused-buffer-has-mixed-line-types ()
  "A single shadow buffer contains both agenda and todo rows."
  (org-other-agenda-test--with-fixture
    (org-other-agenda-test--generate)
    (let (types)
      (with-current-buffer org-other-agenda--shadow
        (goto-char (point-min))
        (while (< (point) (point-max))
          (when-let* ((type (org-get-at-bol 'org-agenda-type)))
            (cl-pushnew type types))
          (forward-line 1)))
      (should (memq 'agenda types))
      (should (memq 'todo types)))))

(ert-deftest org-other-agenda-test-embark-finder-targets-rendered-entry ()
  "The Embark finder returns an Org-heading target only for entry rows."
  (org-other-agenda-test--with-fixture
    (let ((view (org-other-agenda-test--open-view)))
      (should (memq #'org-other-agenda-embark-targets embark-target-finders))
      (org-other-agenda-test--select-tab view 'agenda)
      (with-current-buffer view
        (goto-char (point-min))
        (should (search-forward "Monday standup" nil t))
        (beginning-of-line)
        (let* ((entry (cl-find-if
                       (lambda (candidate)
                         (equal (org-other-agenda-test--title candidate)
                                "Monday standup"))
                       (plist-get textui-state :entries)))
               (object (get-text-property (point) 'org-other-agenda-object))
               (target (org-other-agenda-embark-target))
               (title (nth 1 target)))
          (should entry)
          (should (equal (car object) 'org-heading))
          (should (markerp (cdr object)))
          (should (equal (cdr object) (plist-get entry :hd)))
          (should (equal (car target) 'org-heading))
          (should (equal (get-text-property 0 'org-marker title)
                         (cdr object))))
        (goto-char (point-min))
        (should (search-forward "──" nil t))
        (beginning-of-line)
        (should-not (org-other-agenda-embark-target))))))

(ert-deftest org-other-agenda-test-embark-action-refreshes-rendered-state ()
  "An Org TODO action followed by the Embark hook refreshes the board row."
  (let ((copy (make-temp-file "org-other-agenda-embark-" nil ".org")))
    (copy-file org-other-agenda-test--fixture copy t)
    (unwind-protect
        (org-other-agenda-test--with-agenda-file copy
          (let ((view (org-other-agenda-test--open-view)))
            (org-other-agenda-test--select-tab view 'agenda)
            (with-current-buffer view
              (goto-char (point-min))
              (should (search-forward "Monday standup" nil t))
              (beginning-of-line)
              (let* ((target (org-other-agenda-embark-target))
                     (title (nth 1 target))
                     (marker (get-text-property 0 'org-marker title))
                     (target-info
                      (list :type 'org-heading
                            :target title
                            :bounds (cddr target))))
                (should (eq (keymap-lookup embark-org-heading-map "t")
                            #'org-todo))
                (org-with-point-at marker
                  (org-todo 'done))
                (embark--run-action-hooks embark-post-action-hooks
                                          'org-todo target-info nil)
                ;; `org-other-agenda--embark-refresh' requests the same state
                ;; refresh used by the live view; force it synchronously for
                ;; this batch assertion.
                (textui-refresh view)
                (goto-char (point-min))
                (should (search-forward "Monday standup" nil t))
                (let ((line (buffer-substring-no-properties
                             (line-beginning-position) (line-end-position))))
                  (should (string-match-p "DONE" line)))))))
      (when (file-exists-p copy)
        (delete-file copy)))))

(ert-deftest org-other-agenda-test-embark-finder-targets-today-timed-row ()
  "A Today time block resolves to its own Org heading marker."
  (org-other-agenda-test--with-fixture
    (cl-letf (((symbol-function 'window-body-height)
               (lambda (&optional _window) 80)))
      (let ((view (org-other-agenda-test--open-view)))
        (org-other-agenda-test--assert-rendered-target
         view 'day "Design review" "Design review")))))

(ert-deftest org-other-agenda-test-embark-finder-targets-week-bar ()
  "A Week multi-day bar resolves to its Org heading marker."
  (org-other-agenda-test--with-fixture
    (cl-letf (((symbol-function 'window-body-height)
               (lambda (&optional _window) 80)))
      (let ((view (org-other-agenda-test--open-view)))
        (org-other-agenda-test--assert-rendered-target
         view 'week "Write the" "Write the quarterly report")))))

(ert-deftest org-other-agenda-test-embark-finder-targets-overflow-row ()
  "A Today overflow row resolves to its own Org heading marker."
  (org-other-agenda-test--with-fixture
    (cl-letf (((symbol-function 'window-body-height)
               (lambda (&optional _window) 80)))
      (let ((view (org-other-agenda-test--open-view)))
        (org-other-agenda-test--assert-rendered-target
         view 'day "1:1 with the designer" "1:1 with the designer")))))

(ert-deftest org-other-agenda-test-restores-multi-day-span-dates ()
  "Block metadata reconstructs the quarterly report's four calendar days."
  (org-other-agenda-test--with-fixture
    (let* ((entries (org-other-agenda-test--generate))
           (span (cl-find-if
                  (lambda (candidate)
                    (equal (plist-get candidate :title)
                           "Write the quarterly report"))
                  (ooa-day--spans entries))))
      (should span)
      (should (= (calendar-absolute-from-gregorian '(8 18 2026))
                 (plist-get span :start)))
      (should (= (calendar-absolute-from-gregorian '(8 21 2026))
                 (plist-get span :end)))
      (should (= 4 (plist-get span :total))))))

(ert-deftest org-other-agenda-test-someday-contains-only-undated-tasks ()
  "Someday contains TODO entries with no planning timestamp anywhere."
  (org-other-agenda-test--with-fixture
    (let* ((entries (org-other-agenda-test--generate))
           (groups (org-other-agenda--classify entries))
           (someday (cdr (assq 'someday groups)))
           (titles (sort (org-other-agenda-test--titles someday) #'string<))
           (expected (sort '("Read the RFC"
                             "Reply to the conference"
                             "Sketch the onboarding flow"
                             "Investigate the zorblatt regression")
                           #'string<)))
      (should (equal titles expected))
      (should-not (member "Renew the domain"
                          (org-other-agenda-test--titles someday)))
      (should-not (member "Write the quarterly report"
                          (org-other-agenda-test--titles someday)))
      (dolist (entry someday)
        (should-not
         (org-other-agenda--entry-has-date-p (plist-get entry :hd)))))))

(ert-deftest org-other-agenda-test-someday-scheduling-closes-loop ()
  "Scheduling a Someday entry removes it from that page after refresh."
  (let ((copy (make-temp-file "org-other-agenda-test-" nil ".org")))
    (copy-file org-other-agenda-test--fixture copy t)
    (unwind-protect
        (org-other-agenda-test--with-agenda-file copy
          (let* ((view (org-other-agenda-test--open-view))
                 (base (with-current-buffer view
                         (plist-get textui-state :base-entries)))
                 (target (cl-find-if
                          (lambda (entry)
                            (equal (org-other-agenda-test--title entry)
                                   "Read the RFC"))
                          (cdr (assq 'someday
                                     (org-other-agenda--classify base))))))
            (should target)
            (org-other-agenda-test--select-tab view 'someday)
            (with-current-buffer view
              (let ((button
                     (cl-find-if
                      (lambda (widget)
                        (equal (widget-get widget :cool-hd)
                               (plist-get target :hd)))
                      textui--widgets)))
                (should button)
                (goto-char (widget-get button :from)))
              (cl-letf (((symbol-function 'org-read-date)
                         (lambda (&optional _with-time to-time &rest _)
                           (if to-time
                               (org-time-string-to-time
                                (concat "<" org-other-agenda-test--start-day ">"))
                             org-other-agenda-test--start-day))))
                (org-other-agenda-schedule))
              (textui-refresh view)
              (let* ((entries (plist-get textui-state :base-entries))
                     (someday (cdr (assq 'someday
                                         (org-other-agenda--classify entries)))))
                (should-not (member "Read the RFC"
                                    (org-other-agenda-test--titles someday)))))))
      (when (file-exists-p copy)
        (delete-file copy)))))

(ert-deftest org-other-agenda-test-search-uses-org-query-and-preserves-base ()
  "Search uses Org's full-text and tag blocks without changing base entries."
  (org-other-agenda-test--with-fixture
    (let* ((view (org-other-agenda-test--open-view))
           (base (with-current-buffer view
                   (plist-get textui-state :entries)))
           (base-titles (org-other-agenda-test--titles base)))
      (with-current-buffer view
        (org-other-agenda-search "invitations"))
      (textui-refresh view)
      (with-current-buffer view
        (let ((query-entries (plist-get textui-state :query-entries)))
          (should (eq (plist-get textui-state :query-kind) 'search))
          (should (equal (org-other-agenda-test--titles query-entries)
                         '("Prepare the launch checklist")))
          (should (cl-every (lambda (entry)
                              (eq (plist-get entry :type) 'search))
                            query-entries))
          (should (equal (org-other-agenda-test--titles
                          (plist-get textui-state :entries))
                         base-titles))))
      (with-current-buffer view
        (org-other-agenda-search "#build"))
      (textui-refresh view)
      (with-current-buffer view
        (let ((query-entries (plist-get textui-state :query-entries)))
          (should (eq (plist-get textui-state :query-kind) 'tags))
          (should (equal (sort (org-other-agenda-test--titles query-entries)
                               #'string<)
                         (sort '("Draft the migration plan"
                                 "Run the migration dry run"
                                 "Cut over production")
                               #'string<)))
          (should (cl-every (lambda (entry)
                              (eq (plist-get entry :type) 'tags))
                            query-entries))
          (should (equal (org-other-agenda-test--titles
                          (plist-get textui-state :entries))
                         base-titles))))
      (with-current-buffer view
        (org-other-agenda-clear-search))
      (with-current-buffer view
        (should-not (plist-get textui-state :query))
        (should-not (plist-get textui-state :query-entries))
        (should (equal (org-other-agenda-test--titles
                        (plist-get textui-state :entries))
                       base-titles))))))

(ert-deftest org-other-agenda-test-viewport-scales-with-window-not-entry-count ()
  "Agenda and Today materialize similar widget counts for large and small lists."
  (let ((large (org-other-agenda-test--make-size-fixture 2000))
        (small (org-other-agenda-test--make-size-fixture 200)))
    (unwind-protect
        (cl-letf (((symbol-function 'window-body-height)
                   (lambda (&optional _window) 80)))
          (dolist (tab '(agenda day))
            (let ((large-count
                   (org-other-agenda-test--measure-page-widgets large tab))
                  (small-count
                   (org-other-agenda-test--measure-page-widgets small tab)))
              (should (<= (abs (- large-count small-count)) 2)))))
      (when (file-exists-p large)
        (delete-file large))
      (when (file-exists-p small)
        (delete-file small)))))

(ert-deftest org-other-agenda-test-untagged-entry-renders ()
  "An entry without tags renders instead of aborting the whole board.

TextUI refuses a native widget that renders zero characters, and an empty
tag column used to hit that check.  The bundled fixture tags every heading,
so this regression needs its own file."
  (let ((file (make-temp-file "org-other-agenda-untagged-" nil ".org")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "#+TODO: TODO | DONE\n\n"
                    "* Inbox\n"
                    "** TODO An untagged task\n"
                    "   SCHEDULED: <2026-08-20 Thu>\n"
                    "** TODO Another untagged task\n"))
          (org-other-agenda-test--with-agenda-file file
            (let ((view (org-other-agenda-test--open-view)))
              ;; Week truncates titles to its column width, so match a
              ;; prefix that survives every layout.
              (dolist (case '((agenda . "An untagged task")
                              (day . "An untagged task")
                              (week . "An un")
                              (someday . "Another untagged task")))
                (org-other-agenda-test--select-tab view (car case))
                (should (with-current-buffer view
                          (save-excursion
                            (goto-char (point-min))
                            (search-forward (cdr case) nil t))))))))
      (when (file-exists-p file)
        (delete-file file)))))

(ert-deftest org-other-agenda-test-overlapping-time-blocks-have-stable-overflow ()
  "Four-way overlap keeps stable lanes and identifies the overflow item."
  (org-other-agenda-test--with-fixture
    (let* ((wanted '("All-day workshop" "Standup" "Design review"
                     "Quick sync" "1:1 with the designer"))
           (entries (org-other-agenda-test--generate))
           (timed (sort
                   (org-other-agenda-test--unique-by-hd
                    (cl-remove-if-not
                     (lambda (entry)
                       (org-other-agenda-test--timed-entry-p entry wanted))
                     entries))
                   #'org-other-agenda-test--time-order-less-p))
           (first (org-other-agenda--assign-lanes timed 3))
           (second (org-other-agenda--assign-lanes timed 3)))
      (should (= 5 (length timed)))
      (should (equal
               (list (org-other-agenda-test--titles (nth 0 (car first)))
                     (org-other-agenda-test--titles (nth 1 (car first)))
                     (org-other-agenda-test--titles (nth 2 (car first))))
               '(("All-day workshop")
                 ("Standup" "Design review")
                 ("Quick sync"))))
      (should (equal (org-other-agenda-test--titles (cdr first))
                     '("1:1 with the designer")))
      (should (equal
               (list (org-other-agenda-test--titles (nth 0 (car second)))
                     (org-other-agenda-test--titles (nth 1 (car second)))
                     (org-other-agenda-test--titles (nth 2 (car second))))
               (list (org-other-agenda-test--titles (nth 0 (car first)))
                     (org-other-agenda-test--titles (nth 1 (car first)))
                     (org-other-agenda-test--titles (nth 2 (car first))))))
      (should (equal (org-other-agenda-test--titles (cdr second))
                     (org-other-agenda-test--titles (cdr first)))))))

(ert-deftest org-other-agenda-test-structural-place-survives-page-size ()
  "An occurrence keeps its absolute place when the viewport size changes."
  (org-other-agenda-test--with-fixture
    (let ((view (org-other-agenda-test--open-view)))
      (org-other-agenda-test--select-tab view 'agenda)
      (with-current-buffer view
        (should (org-other-agenda-test--goto-rendered-entry
                 view "Monday standup"))
        (let ((place (org-other-agenda--place-at-point)))
          (should (equal (car place) 'agenda))
          (setq textui-state
                (plist-put textui-state :viewport-page-size 18))
          (textui-refresh view)
          (should (org-other-agenda-test--goto-rendered-entry
                   view "Monday standup"))
          (should (equal (org-other-agenda--place-at-point) place)))))))

(ert-deftest org-other-agenda-test-week-uses-one-panel-offset ()
  "Unequal Week columns share one clamp and expose a clamped row once."
  (let ((file (make-temp-file "org-other-agenda-week-motion-" nil ".org")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "#+TODO: TODO | DONE\n\n* Week motion\n")
            (dotimes (index 20)
              (insert (format "** TODO Thursday %02d\n   SCHEDULED: <2026-08-20 Thu>\n"
                              index)))
            (dotimes (index 2)
              (insert (format "** TODO Friday %02d\n   SCHEDULED: <2026-08-21 Fri>\n"
                              index))))
          (org-other-agenda-test--with-agenda-file file
            (let ((view (org-other-agenda-test--open-view)))
              (with-current-buffer view
                (setq textui-state (plist-put textui-state :tab 'week))
                (setq textui-state
                      (plist-put textui-state :viewport-page-size 15))
                (setq textui-state (plist-put textui-state :week-offset 10))
                (textui-refresh view)
                (should (= 10 (plist-get textui-state :week-offset)))
                (goto-char (org-other-agenda--position-for-place
                            '(week-days 3 10)))
                (let ((original (symbol-function 'textui-refresh))
                      (refreshes 0))
                  (cl-letf (((symbol-function 'textui-refresh)
                             (lambda (&rest arguments)
                               (setq refreshes (1+ refreshes))
                               (apply original arguments))))
                    (org-other-agenda-right))
                  (should (= refreshes 1)))
                (should (equal (org-other-agenda--place-at-point)
                               '(week-days 4 1)))
                (should (= 1 (plist-get textui-state :week-offset)))
                ;; Wednesday is genuinely empty, so horizontal movement lands
                ;; on its visible day header rather than inventing row zero.
                (goto-char (org-other-agenda--position-for-place
                            '(week-days 3 1)))
                (org-other-agenda-left)
                (should (equal (org-other-agenda--place-at-point)
                               '(week-days 2 -1)))))))
      (when (file-exists-p file)
        (delete-file file)))))

(ert-deftest org-other-agenda-test-arrows-obey-panel-boundaries ()
  "Bare arrows move point structurally and preserve state at a boundary."
  (org-other-agenda-test--with-fixture
    (let ((view (org-other-agenda-test--open-view)))
      (org-other-agenda-test--select-tab view 'day)
      (with-current-buffer view
        (goto-char (org-other-agenda--position-for-place
                    '(day-calendar 3 0)))
        (org-other-agenda-left)
        (should (equal (org-other-agenda--place-at-point)
                       '(day-calendar 2 0)))
        (org-other-agenda-down)
        (should (equal (org-other-agenda--place-at-point)
                       '(day-calendar 2 1))))
      (org-other-agenda-test--select-tab view 'agenda)
      (with-current-buffer view
        (should (org-other-agenda-test--goto-rendered-entry
                 view "Monday standup"))
        (let ((position (point))
              (state (copy-tree textui-state)))
          (org-other-agenda-left)
          (should (= position (point)))
          (should (equal state textui-state)))))))

(ert-deftest org-other-agenda-test-parent-follows-explicit-context-chain ()
  "Entries reach their container and panel titles through explicit context."
  (org-other-agenda-test--with-fixture
    (cl-letf (((symbol-function 'window-body-height)
               (lambda (&optional _window) 80)))
      (let ((view (org-other-agenda-test--open-view)))
        (org-other-agenda-test--select-tab view 'agenda)
        (with-current-buffer view
          (should (org-other-agenda-test--goto-rendered-entry
                   view "Monday standup"))
          (org-other-agenda-parent)
          (should (eq (car (org-other-agenda--place-at-point)) 'agenda))
          (should (= (nth 2 (org-other-agenda--place-at-point))
                     (org-other-agenda--agenda-container-row
                      (org-other-agenda--agenda-items
                       (org-other-agenda--page-entries 'agenda))
                      (1+ (nth 2 (org-other-agenda--place-at-point))))))
          (org-other-agenda-parent)
          (should (equal (org-other-agenda--place-at-point)
                         '(agenda -1 -2))))
        (org-other-agenda-test--select-tab view 'week)
        (with-current-buffer view
          (should (org-other-agenda-test--goto-rendered-entry
                   view "Monday standup"))
          (org-other-agenda-parent)
          (should (= -1 (nth 2 (org-other-agenda--place-at-point))))
          (org-other-agenda-parent)
          (should (equal (org-other-agenda--place-at-point)
                         '(week-days -1 -2))))
        (org-other-agenda-test--select-tab view 'day)
        (with-current-buffer view
          (should (org-other-agenda-test--goto-rendered-entry
                   view "1:1 with the designer"))
          (org-other-agenda-parent)
          (should (equal (org-other-agenda--place-at-point)
                         '(day-overflow -1 -2))))))))

(ert-deftest org-other-agenda-test-panel-motion-is-ordered-and-nonwrapping ()
  "Panel movement follows the confirmed Today order without wrapping."
  (org-other-agenda-test--with-fixture
    (cl-letf (((symbol-function 'window-body-height)
               (lambda (&optional _window) 80)))
      (let ((view (org-other-agenda-test--open-view)))
        (org-other-agenda-test--select-tab view 'day)
        (with-current-buffer view
          (goto-char (org-other-agenda--position-for-place
                      '(day-calendar 0 0)))
          (org-other-agenda-next-panel)
          (should (eq (car (org-other-agenda--place-at-point))
                      'day-timeblock))
          (org-other-agenda-next-panel)
          (should (eq (car (org-other-agenda--place-at-point))
                      'day-overflow))
          (org-other-agenda-next-panel)
          (should (eq (car (org-other-agenda--place-at-point))
                      'day-other))
          (let ((position (point))
                (state (copy-tree textui-state)))
            (org-other-agenda-next-panel)
            (should (= position (point)))
            (should (equal state textui-state))))))))

(ert-deftest org-other-agenda-test-structural-feedback-matches-place ()
  "Overlay feedback and Eldoc describe the same structural place."
  (org-other-agenda-test--with-fixture
    (let ((view (org-other-agenda-test--open-view)))
      (org-other-agenda-test--select-tab view 'agenda)
      (with-current-buffer view
        (should (org-other-agenda-test--goto-rendered-entry
                 view "Monday standup"))
        (let ((place (org-other-agenda--place-at-point)))
          (org-other-agenda--update-current-feedback)
          (should org-other-agenda--current-object-overlays)
          (dolist (overlay org-other-agenda--current-object-overlays)
            (should (eq (overlay-get overlay 'face)
                        'org-other-agenda-current-object))
            (should (equal
                     (get-text-property
                      (overlay-start overlay) 'org-other-agenda-place)
                     place)))
          (should (string-match-p "agenda.*Monday standup"
                                  (org-other-agenda--eldoc))))))))

(ert-deftest org-other-agenda-test-nested-targets-use-verified-order ()
  "Occupied and empty Today cells expose the confirmed Embark target order."
  (org-other-agenda-test--with-fixture
    (cl-letf (((symbol-function 'window-body-height)
               (lambda (&optional _window) 80)))
      (let ((view (org-other-agenda-test--open-view)))
        (org-other-agenda-test--select-tab view 'day)
        (with-current-buffer view
          (should (org-other-agenda-test--goto-rendered-entry
                   view "Design review"))
          (should (equal (mapcar #'car
                                 (org-other-agenda-embark-targets))
                         '(org-heading ooa-slot ooa-day)))
          (should (equal
                   (mapcar (lambda (target) (plist-get target :type))
                           (cl-subseq (embark--targets) 0 3))
                   '(org-heading ooa-slot ooa-day)))
          (goto-char (point-min))
          (let (empty-position)
            (while (and (< (point) (point-max)) (null empty-position))
              (when (eq (car-safe
                         (get-text-property
                          (point) 'org-other-agenda-object))
                        'ooa-slot)
                (setq empty-position (point)))
              (goto-char
               (or (next-single-property-change
                    (point) 'org-other-agenda-object nil (point-max))
                   (point-max))))
            (should empty-position)
            (goto-char empty-position)
            (should (equal (mapcar #'car
                                   (org-other-agenda-embark-targets))
                           '(ooa-slot ooa-day)))
            (should (eq (key-binding (kbd "s"))
                        #'org-other-agenda-schedule-set-here))
            (should (eq (key-binding (kbd "m"))
                        #'org-other-agenda-mark))
            (should (eq (key-binding (kbd "u"))
                        #'org-other-agenda-parent))
            (should (eq (key-binding (kbd "q")) #'quit-window))
            (let ((place (org-other-agenda--place-at-point)))
              (org-other-agenda-mark)
              (should (equal (plist-get textui-state :rectangle-anchor)
                             place))
              (should-not (org-other-agenda--marked-headings))
              (org-other-agenda-unmark-all))
            (should (eq (keymap-lookup org-other-agenda-day-map "m")
                        #'org-other-agenda-mark-day))
            (should (eq (keymap-lookup org-other-agenda-day-map "s")
                        #'org-other-agenda-schedule-set-here))
            (should-not (keymap-lookup org-other-agenda-day-map "g"))))))))

(ert-deftest org-other-agenda-test-day-targets-cover-three-surfaces ()
  "Today calendar, Week headings and Agenda headers expose `ooa-day'."
  (org-other-agenda-test--with-fixture
    (cl-letf (((symbol-function 'window-body-height)
               (lambda (&optional _window) 80)))
      (let ((view (org-other-agenda-test--open-view)))
        (org-other-agenda-test--select-tab view 'day)
        (with-current-buffer view
          (goto-char (org-other-agenda--position-for-place
                      '(day-calendar 3 2)))
          (should (equal (mapcar #'car
                                 (org-other-agenda-embark-targets))
                         '(ooa-day)))
          (should (eq (key-binding (kbd "RET")) #'widget-button-press)))
        (org-other-agenda-test--select-tab view 'week)
        (with-current-buffer view
          (should (org-other-agenda-test--goto-rendered-entry
                   view "Monday standup"))
          (should (equal (mapcar #'car
                                 (org-other-agenda-embark-targets))
                         '(org-heading ooa-day))))
        (org-other-agenda-test--select-tab view 'agenda)
        (with-current-buffer view
          (goto-char (point-min))
          (should (search-forward "──" nil t))
          (beginning-of-line)
          (should (equal (mapcar #'car
                                 (org-other-agenda-embark-targets))
                         '(ooa-day))))))))

(ert-deftest org-other-agenda-test-shift-action-refreshes-agenda-type ()
  "A Shift action preserves prefix and refreshes mixed-series component type."
  (org-other-agenda-test--with-fixture
    (let ((view (org-other-agenda-test--open-view)) seen-prefix seen-type)
      (org-other-agenda-test--select-tab view 'someday)
      (with-current-buffer view
        (should (org-other-agenda-test--goto-rendered-entry view "Read the RFC"))
        (cl-letf (((symbol-function 'org-agenda-priority-up)
                   (lambda ()
                     (interactive)
                     (setq seen-prefix current-prefix-arg
                           seen-type org-agenda-type))))
          (let ((current-prefix-arg '(4)))
            (org-other-agenda-priority-up)))
        (should (equal seen-prefix '(4)))
        (should (eq seen-type 'todo))
        (should-error (org-other-agenda-date-later)
                      :type 'error)))))

(ert-deftest org-other-agenda-test-date-shift-uses-exact-org-marker ()
  "Same-type timestamp occurrences are moved independently and point follows."
  (let ((file (make-temp-file "org-other-agenda-occurrence-" nil ".org")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "#+TODO: TODO | DONE\n\n"
                    "* TODO Collision\n"
                    "  SCHEDULED: <2026-08-20 Thu 09:00>\n"
                    "  <2026-08-20 Thu 12:00>\n"
                    "  <2026-08-20 Thu 15:00>\n"))
          (org-other-agenda-test--with-agenda-file file
            (let ((view (org-other-agenda-test--open-view)))
              (org-other-agenda-test--select-tab view 'agenda)
              (with-current-buffer view
                (goto-char (point-min))
                (let (chosen chosen-position)
                  (while (< (point) (point-max))
                    (let ((occurrence
                           (get-text-property
                            (point) 'org-other-agenda-occurrence)))
                      (when (and (equal (plist-get occurrence :type)
                                        "timestamp")
                                 (or (null chosen)
                                     (> (marker-position
                                         (plist-get occurrence :org-marker))
                                        (marker-position
                                         (plist-get chosen :org-marker)))))
                        (setq chosen occurrence
                              chosen-position (point))))
                    (goto-char
                     (or (next-single-property-change
                          (point) 'org-other-agenda-occurrence nil (point-max))
                         (point-max))))
                  (should chosen-position)
                  (goto-char chosen-position)
                  (let ((source-buffer (marker-buffer (plist-get chosen :hd)))
                        (source-position
                         (marker-position
                          (org-other-agenda--occurrence-anchor chosen))))
                    (let ((current-prefix-arg 1))
                      (org-other-agenda-date-later))
                    (with-current-buffer source-buffer
                      (save-excursion
                        (goto-char (point-min))
                        (should (search-forward "<2026-08-20 Thu 12:00>" nil t))
                        (should (search-forward "<2026-08-21 Fri 15:00>" nil t))))
                    (should
                     (let ((actual
                            (plist-get
                             (org-other-agenda--occurrence-at-point)
                             :org-marker)))
                       (and (eq source-buffer (marker-buffer actual))
                            (= source-position
                               (marker-position actual)))))))))))
      (when (file-exists-p file)
        (delete-file file)))))

(ert-deftest org-other-agenda-test-week-shift-right-follows-heading ()
  "The final Week scenario moves one occurrence right and follows it."
  (let ((file (make-temp-file "org-other-agenda-week-shift-" nil ".org")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "#+TODO: TODO | DONE\n\n"
                    "* TODO Move me\n"
                    "  SCHEDULED: <2026-08-20 Thu>\n"))
          (org-other-agenda-test--with-agenda-file file
            (let ((view (org-other-agenda-test--open-view)))
              (org-other-agenda-test--select-tab view 'week)
              (with-current-buffer view
                (should (org-other-agenda-test--goto-rendered-entry
                         view "Move me"))
                (should (= 3 (nth 1 (org-other-agenda--place-at-point))))
                (org-other-agenda-date-later)
                (should (= 4 (nth 1 (org-other-agenda--place-at-point))))
                (with-current-buffer
                    (marker-buffer (org-other-agenda--at-point))
                  (save-excursion
                    (goto-char (point-min))
                    (should (search-forward "<2026-08-21 Fri>" nil t))))))))
      (when (file-exists-p file)
        (delete-file file)))))

(ert-deftest org-other-agenda-test-week-range-shift-preserves-native-semantics ()
  "A Week bar delegates range movement to Org and follows the shifted bar."
  (let ((file (make-temp-file "org-other-agenda-range-shift-" nil ".org")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "#+TODO: TODO | DONE\n\n"
                    "* TODO Range task\n"
                    "  <2026-08-18 Tue>--<2026-08-21 Fri>\n"))
          (org-other-agenda-test--with-agenda-file file
            (let ((view (org-other-agenda-test--open-view)))
              (org-other-agenda-test--select-tab view 'week)
              (with-current-buffer view
                (goto-char (point-min))
                (should (search-forward "Range task" nil t))
                (goto-char (- (point) (length "Range task")))
                (should (= 1 (nth 1 (org-other-agenda--place-at-point))))
                (org-other-agenda-date-later)
                (should (= 2 (nth 1 (org-other-agenda--place-at-point))))
                (with-current-buffer
                    (marker-buffer (org-other-agenda--at-point))
                  (save-excursion
                    (goto-char (point-min))
                    (should
                     (search-forward
                      "<2026-08-19 Wed>--<2026-08-22 Sat>" nil t))))))))
      (when (file-exists-p file)
        (delete-file file)))))

(ert-deftest org-other-agenda-test-mark-deduplicates-and-highlights-occurrences ()
  "One heading mark highlights every independently rendered occurrence."
  (let ((file (make-temp-file "org-other-agenda-marks-" nil ".org")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "#+TODO: TODO | DONE\n\n"
                    "* TODO Repeated heading\n"
                    "  <2026-08-20 Thu 09:00>\n"
                    "  <2026-08-20 Thu 12:00>\n"))
          (org-other-agenda-test--with-agenda-file file
            (let ((view (org-other-agenda-test--open-view)))
              (org-other-agenda-test--select-tab view 'agenda)
              (with-current-buffer view
                (should (org-other-agenda-test--goto-rendered-entry
                         view "Repeated heading"))
                (org-other-agenda-mark)
                (should (= 1 (length (org-other-agenda--marked-headings))))
                (let ((marked (car (org-other-agenda--marked-headings)))
                      places)
                  (goto-char (point-min))
                  (while (< (point) (point-max))
                    (let ((hd (org-other-agenda--object-hd (point))))
                      (when (and hd
                                 (org-other-agenda--heading-marked-p hd))
                        (cl-pushnew
                         (get-text-property
                          (point) 'org-other-agenda-place)
                         places :test #'equal)
                        (let ((face (get-text-property (point) 'face)))
                          (should
                           (or (eq face 'org-other-agenda-marked)
                               (and (listp face)
                                    (memq 'org-other-agenda-marked face)))))))
                    (goto-char
                     (or (next-single-property-change
                          (point) 'org-other-agenda-object nil (point-max))
                         (point-max))))
                  (should (>= (length places) 2))
                  (should (buffer-live-p (marker-buffer marked))))))))
      (when (file-exists-p file)
        (delete-file file)))))

(ert-deftest org-other-agenda-test-week-rectangle-crosses-viewport ()
  "A structural rectangle survives refresh and marks off-screen Week rows."
  (let ((file (make-temp-file "org-other-agenda-rectangle-" nil ".org")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "#+TODO: TODO | DONE\n\n* Rectangle\n")
            (dolist (day '("20 Thu" "21 Fri"))
              (dotimes (index 12)
                (insert (format "** TODO %s task %02d\n   SCHEDULED: <2026-08-%s>\n"
                                day index day)))))
          (org-other-agenda-test--with-agenda-file file
            (let ((view (org-other-agenda-test--open-view)))
              (with-current-buffer view
                (setq textui-state (plist-put textui-state :tab 'week))
                (setq textui-state
                      (plist-put textui-state :viewport-page-size 15))
                (textui-refresh view)
                (goto-char (org-other-agenda--position-for-place
                            '(week-days 3 0)))
                (org-other-agenda-mark)
                (should (equal (plist-get textui-state :rectangle-anchor)
                               '(week-days 3 0)))
                (org-other-agenda--expose-place '(week-days 4 9))
                (goto-char (org-other-agenda--position-for-place
                            '(week-days 4 9)))
                (org-other-agenda-mark)
                (should-not (plist-get textui-state :rectangle-anchor))
                (should (> (plist-get textui-state :week-offset) 0))
                (should (= 20 (length (org-other-agenda--marked-headings))))))))
      (when (file-exists-p file)
        (delete-file file)))))

(ert-deftest org-other-agenda-test-day-mark-and-unmark-use-one-set ()
  "Day marking feeds the same heading set and U clears it."
  (org-other-agenda-test--with-fixture
    (let ((view (org-other-agenda-test--open-view)))
      (org-other-agenda-test--select-tab view 'day)
      (with-current-buffer view
        (goto-char (point-min))
        (let (position)
          (while (and (< (point) (point-max)) (null position))
            (when (equal (get-text-property
                          (point) 'org-other-agenda-object)
                         (cons 'ooa-day org-other-agenda-test--today))
              (setq position (point)))
            (forward-char 1))
          (should position)
          (goto-char position)
          (org-other-agenda-mark-day)
          (should (> (length (org-other-agenda--marked-headings)) 1))
          (dolist (hd (org-other-agenda--day-headings
                       org-other-agenda-test--today))
            (should (org-other-agenda--heading-marked-p hd)))
          (org-other-agenda-unmark-all)
          (should-not (org-other-agenda--marked-headings)))))))

(ert-deftest org-other-agenda-test-week-rectangle-runs-native-bulk-todo ()
  "The final rectangle scenario delegates B t once per unique heading."
  (let ((file (make-temp-file "org-other-agenda-bulk-" nil ".org")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "#+TODO: TODO | DONE\n\n* Bulk\n"
                    "** TODO Thursday one\n   SCHEDULED: <2026-08-20 Thu>\n"
                    "** TODO Thursday two\n   SCHEDULED: <2026-08-20 Thu>\n"
                    "** TODO Friday one\n   SCHEDULED: <2026-08-21 Fri>\n"
                    "** TODO Friday two\n   SCHEDULED: <2026-08-21 Fri>\n"))
          (org-other-agenda-test--with-agenda-file file
            (let ((view (org-other-agenda-test--open-view)))
              (org-other-agenda-test--select-tab view 'week)
              (with-current-buffer view
                (goto-char (org-other-agenda--position-for-place
                            '(week-days 3 0)))
                (org-other-agenda-mark)
                (goto-char (org-other-agenda--position-for-place
                            '(week-days 4 1)))
                (org-other-agenda-mark)
                (should (= 4 (length (org-other-agenda--marked-headings))))
                (cl-letf (((symbol-function 'read-char-exclusive)
                           (lambda (&rest _arguments) ?t))
                          ((symbol-function 'completing-read)
                           (lambda (&rest _arguments) "DONE")))
                  (org-other-agenda-bulk-action))
                (should-not (org-other-agenda--marked-headings)))
              (with-current-buffer (find-file-noselect file)
                (goto-char (point-min))
                (should (= 4 (how-many "^\\*\\* DONE "
                                       (point-min) (point-max))))))))
      (when (file-exists-p file)
        (delete-file file)))))

(ert-deftest org-other-agenda-test-bulk-error-preserves-marks-and-effects ()
  "A native bulk error preserves package marks and does not roll back effects."
  (let ((file (make-temp-file "org-other-agenda-bulk-error-" nil ".org")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "#+TODO: TODO | DONE\n\n"
                    "* TODO First\n  SCHEDULED: <2026-08-20 Thu>\n"
                    "* TODO Second\n  SCHEDULED: <2026-08-20 Thu>\n"))
          (org-other-agenda-test--with-agenda-file file
            (let ((view (org-other-agenda-test--open-view)))
              (org-other-agenda-test--select-tab view 'week)
              (with-current-buffer view
                (goto-char (org-other-agenda--position-for-place
                            '(week-days 3 0)))
                (org-other-agenda-mark)
                (goto-char (org-other-agenda--position-for-place
                            '(week-days 3 1)))
                (org-other-agenda-mark)
                (cl-letf (((symbol-function 'org-agenda-bulk-action)
                           (lambda (&optional _arg)
                             (interactive "P")
                             (goto-char
                              (text-property-any
                               (point-min) (point-max) 'org-hd-marker
                               (car org-agenda-bulk-marked-entries)))
                             (org-agenda-todo "DONE")
                             (error "Synthetic native failure"))))
                  (should-error (org-other-agenda-bulk-action)
                                :type 'error))
                (should (= 2 (length (org-other-agenda--marked-headings))))
                (with-current-buffer (find-file-noselect file)
                  (goto-char (point-min))
                  (should (= 1 (how-many "^\\* DONE "
                                         (point-min) (point-max)))))))))
      (when (file-exists-p file)
        (delete-file file)))))

(ert-deftest org-other-agenda-test-empty-slot-schedules-marked-heading ()
  "The final Today scenario schedules a marked heading at an empty slot."
  (let ((file (make-temp-file "org-other-agenda-slot-" nil ".org")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "#+TODO: TODO | DONE\n\n* TODO Put me here\n"))
          (org-other-agenda-test--with-agenda-file file
            (let ((view (org-other-agenda-test--open-view)))
              (org-other-agenda-test--select-tab view 'someday)
              (with-current-buffer view
                (should (org-other-agenda-test--goto-rendered-entry
                         view "Put me here"))
                (org-other-agenda-mark))
              (org-other-agenda-test--select-tab view 'day)
              (with-current-buffer view
                (goto-char (org-other-agenda--position-for-place
                            '(day-timeblock 0 3)))
                (should (eq (key-binding (kbd "s"))
                            #'org-other-agenda-schedule-set-here))
                (org-other-agenda-schedule-set-here)
                (should-not (org-other-agenda--marked-headings)))
              (with-current-buffer (find-file-noselect file)
                (goto-char (point-min))
                (should (search-forward
                         "SCHEDULED: <2026-08-20 Thu 09:30>" nil t)))))
      (when (file-exists-p file)
        (delete-file file))))))

(ert-deftest org-other-agenda-test-schedule-set-continues-after-failure ()
  "Schedule-set removes successes, retains failures, and reports both."
  (let ((file (make-temp-file "org-other-agenda-schedule-set-" nil ".org")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "#+TODO: TODO | DONE\n\n* TODO First\n\n* TODO Second\n"))
          (org-other-agenda-test--with-agenda-file file
            (let ((view (org-other-agenda-test--open-view))
                  (original (symbol-function 'org-agenda-schedule))
                  (calls 0))
              (org-other-agenda-test--select-tab view 'someday)
              (with-current-buffer view
                (dolist (title '("First" "Second"))
                  (should (org-other-agenda-test--goto-rendered-entry
                           view title))
                  (setq textui-state
                        (plist-put
                         textui-state :marked
                         (org-other-agenda--add-marked-heading
                          (org-other-agenda--at-point)
                          (org-other-agenda--marked-headings)))))
                (org-other-agenda-test--select-tab view 'day)
                (goto-char (org-other-agenda--position-for-place
                            '(day-timeblock 0 4)))
                (let ((report
                       (cl-letf (((symbol-function 'org-agenda-schedule)
                                  (lambda (arg &optional time)
                                    (setq calls (1+ calls))
                                    (if (= calls 2)
                                        (error "Synthetic schedule failure")
                                      (funcall original arg time)))))
                         (org-other-agenda-schedule-set-here))))
                  (should (string-match-p "Scheduled 1; failed 1" report)))
                (should (= 2 calls))
                (should (= 1 (length (org-other-agenda--marked-headings))))))))
      (when (file-exists-p file)
        (delete-file file)))))

(ert-deftest org-other-agenda-test-invalid-mark-blocks-bulk-and-survives ()
  "An invalid source is rejected before native bulk and remains marked."
  (let ((file (make-temp-file "org-other-agenda-invalid-mark-" nil ".org")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "#+TODO: TODO | DONE\n\n"
                    "* TODO Vanishing heading\n"
                    "  SCHEDULED: <2026-08-20 Thu>\n"))
          (org-other-agenda-test--with-agenda-file file
            (let ((view (org-other-agenda-test--open-view)))
              (org-other-agenda-test--select-tab view 'week)
              (with-current-buffer view
                (should (org-other-agenda-test--goto-rendered-entry
                         view "Vanishing heading"))
                (org-other-agenda-mark)
                (let* ((marked (org-other-agenda--marked-headings))
                       (source (marker-buffer (car marked))))
                  (kill-buffer source)
                  (should-error (org-other-agenda-bulk-action)
                                :type 'user-error)
                  (should (eq marked
                              (org-other-agenda--marked-headings))))))))
      (when (file-exists-p file)
        (delete-file file)))))

(ert-deftest org-other-agenda-test-cross-panel-rectangle-is-rejected ()
  "A rectangle cannot cross panels and rejection preserves its anchor."
  (org-other-agenda-test--with-fixture
    (let ((view (org-other-agenda-test--open-view)))
      (org-other-agenda-test--select-tab view 'week)
      (with-current-buffer view
        (should (org-other-agenda-test--goto-rendered-entry
                 view "Monday standup"))
        (org-other-agenda-mark)
        (let ((anchor (plist-get textui-state :rectangle-anchor)))
          (org-other-agenda-test--select-tab view 'day)
          (goto-char (point-min))
          (let (slot-position)
            (while (and (< (point) (point-max)) (null slot-position))
              (when (eq (car-safe
                         (get-text-property
                          (point) 'org-other-agenda-object))
                        'ooa-slot)
                (setq slot-position (point)))
              (forward-char 1))
            (should slot-position)
            (goto-char slot-position)
            (should-error (org-other-agenda-mark) :type 'user-error)
            (should (equal anchor
                           (plist-get textui-state :rectangle-anchor)))
            (should (= 1 (length
                          (org-other-agenda--marked-headings))))))))))

(ert-deftest org-other-agenda-test-native-persistent-bulk-keeps-marks ()
  "Successful native bulk retains package marks when persistence is enabled."
  (let ((file (make-temp-file "org-other-agenda-persistent-" nil ".org")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "#+TODO: TODO | DONE\n\n"
                    "* TODO Keep the mark\n"
                    "  SCHEDULED: <2026-08-20 Thu>\n"))
          (org-other-agenda-test--with-agenda-file file
            (let ((view (org-other-agenda-test--open-view))
                  (org-agenda-persistent-marks t))
              (org-other-agenda-test--select-tab view 'week)
              (with-current-buffer view
                (should (org-other-agenda-test--goto-rendered-entry
                         view "Keep the mark"))
                (org-other-agenda-mark)
                (cl-letf (((symbol-function 'read-char-exclusive)
                           (lambda (&rest _arguments) ?t))
                          ((symbol-function 'completing-read)
                           (lambda (&rest _arguments) "DONE")))
                  (org-other-agenda-bulk-action))
                (should (= 1 (length
                              (org-other-agenda--marked-headings))))))))
      (when (file-exists-p file)
        (delete-file file)))))

;;; org-other-agenda-test.el ends here
