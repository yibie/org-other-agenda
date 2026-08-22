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
                         :viewport-page-size 40)))))

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
      (should (memq #'org-other-agenda-embark-target embark-target-finders))
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

;;; org-other-agenda-test.el ends here
