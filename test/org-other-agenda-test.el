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

(defconst org-other-agenda-test--view-bindings
  '(("g" . org-other-agenda-refresh)
    ("/" . org-other-agenda-search)
    ("\\" . org-other-agenda-clear-search)
    ("[" . org-other-agenda-previous-day)
    ("]" . org-other-agenda-next-day)
    ("n" . org-other-agenda-next-line)
    ("p" . org-other-agenda-previous-line)
    ("u" . org-other-agenda-parent)
    ("m" . org-other-agenda-mark)
    ("U" . org-other-agenda-unmark-all)
    ("B" . org-other-agenda-bulk-action)
    ("<left>" . org-other-agenda-left)
    ("<right>" . org-other-agenda-right)
    ("<up>" . org-other-agenda-up)
    ("<down>" . org-other-agenda-down)
    ("M-n" . org-other-agenda-next-panel)
    ("M-p" . org-other-agenda-previous-panel)
    ("C-v" . org-other-agenda-page-down)
    ("M-v" . org-other-agenda-page-up)
    ("<next>" . org-other-agenda-page-down)
    ("<prior>" . org-other-agenda-page-up)
    ("TAB" . widget-forward)
    ("S-TAB" . widget-backward)
    ("<backtab>" . widget-backward)
    ("q" . quit-window))
  "Every view-wide key and its command, including event aliases.")

(defconst org-other-agenda-test--entry-bindings
  '(("t" . org-other-agenda-todo)
    ("s" . org-other-agenda-schedule)
    ("d" . org-other-agenda-deadline)
    ("," . org-other-agenda-priority)
    (":" . org-other-agenda-set-tags)
    ("C-c C-p" . org-other-agenda-toggle-pin)
    ("S-<left>" . org-other-agenda-date-earlier)
    ("S-<right>" . org-other-agenda-date-later)
    ("S-<up>" . org-other-agenda-priority-up)
    ("S-<down>" . org-other-agenda-priority-down))
  "Every direct key and command available on an Org heading.")

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
          (setq-local buffer-read-only t)
          (use-local-map
           (make-composed-keymap org-other-agenda-mode-map
                                 (current-local-map))))
        view))))

(defun org-other-agenda-test--select-tab (view tab)
  "Select TAB in VIEW and synchronously render it."
  (textui-set-state view :tab tab)
  (textui-refresh view))

(defun org-other-agenda-test--key-command (key &optional position)
  "Return the command resolved by KEY at optional POSITION."
  (key-binding (kbd key) nil t position))

(defun org-other-agenda-test--call-key (key &optional position)
  "Interactively call the command resolved by KEY at optional POSITION."
  (let ((command (org-other-agenda-test--key-command key position)))
    (should (commandp command))
    (call-interactively command)))

(defun org-other-agenda-test--assert-entry-bindings (view title)
  "Assert every entry binding on TITLE rendered in VIEW."
  (with-current-buffer view
    (should (org-other-agenda-test--goto-rendered-entry view title))
    (dolist (binding org-other-agenda-test--entry-bindings)
      (should (eq (org-other-agenda-test--key-command (car binding))
                  (cdr binding))))
    (should (memq (org-other-agenda-test--key-command "RET")
                  '(org-other-agenda-entry-visit widget-button-press)))))

(defun org-other-agenda-test--embark-action-entry (type command)
  "Return TYPE's formatted Embark action entry for COMMAND."
  (cl-find command
           (car (embark--formatted-bindings
                 (embark--raw-action-keymap type)))
           :key (lambda (entry) (nth 2 entry))))

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

(defun org-other-agenda-test--entry-markers-live-p (entry)
  "Return non-nil when ENTRY's heading and occurrence markers are live."
  (cl-every (lambda (key)
              (let ((marker (plist-get entry key)))
                (and (markerp marker) (marker-buffer marker))))
            '(:hd :org-marker)))

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

(ert-deftest org-other-agenda-test-complete-key-contract-resolves ()
  "Every supported key resolves in its declared scope."
  (dolist (binding org-other-agenda-test--view-bindings)
    (should (eq (keymap-lookup org-other-agenda-view-map (car binding))
                (cdr binding)))
    (should (eq (keymap-lookup org-other-agenda-mode-map (car binding))
                (cdr binding))))
  (dolist (binding org-other-agenda-test--entry-bindings)
    (should (eq (keymap-lookup org-other-agenda-entry-map (car binding))
                (cdr binding))))
  (should (eq (keymap-lookup org-other-agenda-entry-map "RET")
              #'widget-button-press))
  (should (eq (keymap-lookup org-other-agenda-entry-map "<down-mouse-1>")
              #'widget-button-click))
  (should (eq (keymap-lookup org-other-agenda-entry-cell-map "RET")
              #'org-other-agenda-entry-visit))
  (dolist (binding '(("m" . org-other-agenda-mark-day)
                     ("s" . org-other-agenda-schedule-set-here)))
    (should (eq (keymap-lookup org-other-agenda-day-map (car binding))
                (cdr binding))))
  (should (eq (keymap-lookup org-other-agenda-slot-map "s")
              #'org-other-agenda-schedule-set-here))
  (dolist (key '("t" "s" "d" "," ":" "C-n" "C-p" "C-f" "C-b"))
    (should-not (keymap-lookup org-other-agenda-mode-map key)))
  (dolist (key '("S-<left>" "S-<right>" "S-<up>" "S-<down>"
                 "M-<left>" "M-<right>" "M-<up>" "M-<down>"))
    (should-not (keymap-lookup org-other-agenda-mode-map key)))
  (dotimes (index 6)
    (should (commandp (keymap-lookup org-other-agenda-mode-map
                                     (number-to-string (1+ index))))))
  (should-not (keymap-lookup org-other-agenda-mode-map "7")))

(ert-deftest org-other-agenda-test-entry-keys-resolve-on-every-page ()
  "All entry keys resolve on Today, Week, and every list page."
  (org-other-agenda-test--with-fixture
    (cl-letf (((symbol-function 'window-body-height)
               (lambda (&optional _window) 80)))
      (let ((view (org-other-agenda-test--open-view)))
        (dolist (surface '((agenda . "Monday standup")
                           (day . "Design review")
                           (week . "Monday standup")
                           (someday . "Read the RFC")))
          (org-other-agenda-test--select-tab view (car surface))
          (org-other-agenda-test--assert-entry-bindings view (cdr surface)))
        (with-current-buffer view
          (org-other-agenda-search "invitations")
          (textui-refresh view))
        (org-other-agenda-test--assert-entry-bindings
         view "Prepare the launch checklist")
        (with-current-buffer view
          (org-other-agenda-search "#build")
          (textui-refresh view))
        (org-other-agenda-test--assert-entry-bindings
         view "Draft the migration plan")))))

(ert-deftest org-other-agenda-test-all-keys-dispatch-in-rendered-contexts ()
  "Every key invokes its command from a real rendered noun."
  (org-other-agenda-test--with-fixture
    (cl-letf (((symbol-function 'window-body-height)
               (lambda (&optional _window) 80)))
      (let ((view (org-other-agenda-test--open-view)))
        (org-other-agenda-test--select-tab view 'agenda)
        (with-current-buffer view
          (should (org-other-agenda-test--goto-rendered-entry
                   view "Monday standup"))
          (dolist (binding (append org-other-agenda-test--view-bindings
                                   org-other-agenda-test--entry-bindings))
            (let ((key (car binding))
                  (command (cdr binding))
                  called)
              (should (eq (org-other-agenda-test--key-command key) command))
              (cl-letf (((symbol-function command)
                         (lambda ()
                           (interactive)
                           (setq called t))))
                (org-other-agenda-test--call-key key))
              (should called)))
          (goto-char (point-min))
          (should (search-forward "Monday standup" nil t))
          (goto-char (1- (point)))
          (let ((button-position (point))
                visited)
            (should (eq (org-other-agenda-test--key-command
                         "<down-mouse-1>" button-position)
                        #'widget-button-click))
            (should (eq (org-other-agenda-test--key-command
                         "RET" button-position)
                        #'widget-button-press))
            (cl-letf (((symbol-function 'org-other-agenda-visit)
                       (lambda (heading &optional _event)
                         (setq visited heading))))
              (org-other-agenda-test--call-key "RET" button-position))
            (should (markerp visited)))
          (beginning-of-line)
          (should (eq (org-other-agenda-test--key-command "RET")
                      #'org-other-agenda-entry-visit))
          (let (visited)
            (cl-letf (((symbol-function 'org-other-agenda-entry-visit)
                       (lambda ()
                         (interactive)
                         (setq visited t))))
              (org-other-agenda-test--call-key "RET"))
            (should visited)))
        (org-other-agenda-test--select-tab view 'day)
        (with-current-buffer view
          (goto-char (org-other-agenda--position-for-place
                      '(day-calendar 3 2)))
          (dolist (binding '(("m" . org-other-agenda-mark-day)
                             ("s" . org-other-agenda-schedule-set-here)))
            (let ((command (cdr binding)) called)
              (should (eq (org-other-agenda-test--key-command (car binding))
                          command))
              (cl-letf (((symbol-function command)
                         (lambda ()
                           (interactive)
                           (setq called t))))
                (org-other-agenda-test--call-key (car binding)))
              (should called)))
          (goto-char (org-other-agenda--position-for-place
                      '(day-timeblock 0 3)))
          (dolist (binding '(("s" . org-other-agenda-schedule-set-here)
                             ("m" . org-other-agenda-mark)
                             ("u" . org-other-agenda-parent)))
            (let ((command (cdr binding)) called)
              (should (eq (org-other-agenda-test--key-command (car binding))
                          command))
              (cl-letf (((symbol-function command)
                         (lambda ()
                           (interactive)
                           (setq called t))))
                (org-other-agenda-test--call-key (car binding)))
              (should called)))
          (cl-loop for key from 1 to 6
                   for tab in '(day week agenda someday search tags)
                   do (org-other-agenda-test--call-key
                       (number-to-string key))
                   do (should (eq (plist-get textui-state :tab) tab))))))))

(ert-deftest org-other-agenda-test-all-entry-action-keys-forward-to-org ()
  "Every entry action key reaches its corresponding native Org command."
  (org-other-agenda-test--with-fixture
    (cl-letf (((symbol-function 'window-body-height)
               (lambda (&optional _window) 80)))
      (let ((view (org-other-agenda-test--open-view)))
        (dolist (spec '(("t" org-other-agenda-todo org-agenda-todo)
                        ("s" org-other-agenda-schedule org-agenda-schedule)
                        ("d" org-other-agenda-deadline org-agenda-deadline)
                        ("," org-other-agenda-priority org-agenda-priority)
                        (":" org-other-agenda-set-tags org-agenda-set-tags)
                        ("S-<left>" org-other-agenda-date-earlier
                         org-agenda-do-date-earlier)
                        ("S-<right>" org-other-agenda-date-later
                         org-agenda-do-date-later)
                        ("S-<up>" org-other-agenda-priority-up
                         org-agenda-priority-up)
                        ("S-<down>" org-other-agenda-priority-down
                         org-agenda-priority-down)))
          (org-other-agenda-test--select-tab view 'agenda)
          (with-current-buffer view
            (should (org-other-agenda-test--goto-rendered-entry
                     view "Monday standup"))
            (let ((key (nth 0 spec))
                  (command (nth 1 spec))
                  (native (nth 2 spec))
                  called)
              (should (eq (org-other-agenda-test--key-command key) command))
              (cl-letf (((symbol-function native)
                         (lambda ()
                           (interactive)
                           (setq called t))))
                (org-other-agenda-test--call-key key))
              (should called))))))))

(ert-deftest org-other-agenda-test-view-state-and-widget-keys-take-effect ()
  "Date, paging, tab, and widget keys change their intended view state."
  (org-other-agenda-test--with-fixture
    (cl-letf (((symbol-function 'window-body-height)
               (lambda (&optional _window) 80)))
      (let ((view (org-other-agenda-test--open-view)))
        (org-other-agenda-test--select-tab view 'day)
        (with-current-buffer view
          (setq textui-state
                (plist-put textui-state :date org-other-agenda-test--today))
          (org-other-agenda-test--call-key "]")
          (should (= (1+ org-other-agenda-test--today)
                     (plist-get textui-state :date)))
          (org-other-agenda-test--call-key "[")
          (should (= org-other-agenda-test--today
                     (plist-get textui-state :date))))
        (org-other-agenda-test--select-tab view 'agenda)
        (with-current-buffer view
          (setq textui-state (plist-put textui-state :agenda-offset 0))
          (textui-refresh view)
          (let ((step (max 1 (1- (org-other-agenda--viewport-page-size
                                  'agenda)))))
            (org-other-agenda-test--call-key "C-v")
            (should (= step (plist-get textui-state :agenda-offset)))
            (org-other-agenda-test--call-key "M-v")
            (should (= 0 (plist-get textui-state :agenda-offset)))
            (org-other-agenda-test--call-key "<next>")
            (should (= step (plist-get textui-state :agenda-offset)))
            (org-other-agenda-test--call-key "<prior>")
            (should (= 0 (plist-get textui-state :agenda-offset))))
          (goto-char (point-min))
          (org-other-agenda-test--call-key "TAB")
          (let ((first (point)))
            (should (widget-at first))
            (org-other-agenda-test--call-key "TAB")
            (let ((second (point)))
              (should (widget-at second))
              (should (/= first second))
              (org-other-agenda-test--call-key "S-TAB")
              (should (= first (point)))
              (org-other-agenda-test--call-key "TAB")
              (should (= second (point)))
              (org-other-agenda-test--call-key "<backtab>")
              (should (= first (point))))))))))

(ert-deftest org-other-agenda-test-entry-movement-crosses-rows ()
  "n and p move between the first two rendered entry rows."
  (org-other-agenda-test--with-fixture
    (cl-letf (((symbol-function 'window-body-height)
               (lambda (&optional _window) 80)))
      (let ((view (org-other-agenda-test--open-view)))
        (org-other-agenda-test--select-tab view 'agenda)
        (with-current-buffer view
          (let* ((first-position (org-other-agenda--page-heading-position -1))
                 (first-object (and first-position
                                    (get-text-property first-position
                                                        'org-other-agenda-object))))
            (should first-position)
            (goto-char first-position)
            (let* ((second-position
                    (org-other-agenda--next-heading-position 1))
                   (second-object (and second-position
                                       (get-text-property second-position
                                                           'org-other-agenda-object))))
              (should second-position)
              (should second-object)
              (org-other-agenda-next-line)
              (should (equal (get-text-property (point)
                                                 'org-other-agenda-object)
                             second-object))
              (should (= (point) (org-other-agenda--heading-position-on-line)))
              (org-other-agenda-previous-line)
              (should (equal (get-text-property (point)
                                                 'org-other-agenda-object)
                             first-object))
              (should (= (point) (org-other-agenda--heading-position-on-line))))))))))

(ert-deftest org-other-agenda-test-entry-movement-skips-non-heading-nouns ()
  "n and p skip day and slot nouns on every page that renders them."
  (org-other-agenda-test--with-fixture
    (cl-letf (((symbol-function 'window-body-height)
               (lambda (&optional _window) 80)))
      (let ((view (org-other-agenda-test--open-view)))
        (dolist (tab '(agenda day week))
          (org-other-agenda-test--select-tab view tab)
          (with-current-buffer view
            (goto-char (point-min))
            (org-other-agenda-next-line)
            (should (eq (car-safe
                         (get-text-property
                          (point) 'org-other-agenda-object))
                        'org-heading))
            (goto-char (point-max))
            (org-other-agenda-previous-line)
            (should (eq (car-safe
                         (get-text-property
                          (point) 'org-other-agenda-object))
                        'org-heading))))))))

(ert-deftest org-other-agenda-test-entry-movement-scrolls-at-page-edge ()
  "n advances the offset and lands on the newly visible row at the edge."
  (org-other-agenda-test--with-fixture
    (cl-letf (((symbol-function 'window-body-height)
               (lambda (&optional _window) 9)))
      (let ((view (org-other-agenda-test--open-view)))
        (org-other-agenda-test--select-tab view 'agenda)
        (with-current-buffer view
          (let* ((last-position (org-other-agenda--page-heading-position 1))
                 (old-offset (plist-get textui-state :agenda-offset)))
            (should last-position)
            (goto-char last-position)
            (org-other-agenda-next-line)
            (should (= (plist-get textui-state :agenda-offset)
                       (1+ old-offset)))
            (should (get-text-property (point) 'org-other-agenda-object))
            (should (= (point) (org-other-agenda--heading-position-on-line)))
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
              (let* ((target-info (car (embark--targets)))
                     (title (plist-get target-info :target))
                     (marker (get-text-property 0 'org-marker title))
                     (bounds (plist-get target-info :bounds)))
                (should (eq (plist-get target-info :type) 'org-heading))
                (should (consp bounds))
                (should (eq (keymap-lookup embark-org-heading-map "t")
                            #'org-todo))
                (should (eq (get-text-property
                             0 'org-other-agenda-origin title)
                            view))
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

(ert-deftest org-other-agenda-test-embark-refresh-requires-live-board-origin ()
  "Embark heading actions refresh only targets created by a live board."
  (org-other-agenda-test--with-fixture
    (let* ((view (org-other-agenda-test--open-view))
           (marker
            (with-current-buffer view
              (plist-get (car (plist-get textui-state :base-entries)) :hd)))
           (external-target (propertize "External heading" 'org-marker marker))
           (dead-view (generate-new-buffer " *org-other-agenda-dead-origin*"))
           (dead-target
            (propertize "Dead board heading"
                        'org-marker marker
                        'org-other-agenda-origin dead-view))
           refreshed)
      (kill-buffer dead-view)
      (cl-letf (((symbol-function 'org-other-agenda-refresh)
                 (lambda ()
                   (push (current-buffer) refreshed))))
        (dolist (target (list external-target dead-target))
          (embark--run-action-hooks
           embark-post-action-hooks 'org-todo
           (list :type 'org-heading :target target :bounds nil)
           nil)))
      (should-not refreshed))))

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

(ert-deftest org-other-agenda-test-pin-is-a-conditional-heading-action ()
  "Direct, Embark, and widget paths share the conditional pin action."
  (org-other-agenda-test--with-fixture
    (cl-letf (((symbol-function 'window-body-height)
               (lambda (&optional _window) 80)))
      (let ((view (org-other-agenda-test--open-view)) heading)
        (set-window-buffer (selected-window) view)
        (org-other-agenda-test--select-tab view 'day)
        (with-current-buffer view
          (should (org-other-agenda-test--goto-rendered-entry
                   view "1:1 with the designer"))
          (setq heading (org-other-agenda--at-point))
          (should (eq (org-other-agenda--pin-state heading) 'pin))
          (should (equal (org-other-agenda--pin-action-label)
                         "Pin to Today main lane"))
          (should (eq (keymap-lookup embark-org-heading-map "C-c C-p")
                      #'org-other-agenda-toggle-pin))
          (should (eq (keymap-lookup
                       (embark--raw-action-keymap 'org-heading) "C-c C-p")
                      #'org-other-agenda-toggle-pin))
          (let ((action
                 (org-other-agenda-test--embark-action-entry
                  'org-heading #'org-other-agenda-toggle-pin)))
            (should action)
            (should (equal (nth 1 action) "Pin to Today main lane")))
          (should (eq (org-other-agenda-test--key-command "C-c C-p")
                      #'org-other-agenda-toggle-pin))
          (org-other-agenda-test--call-key "C-c C-p")
          (should (eq (org-other-agenda--pin-state heading) 'unpin))
          (should (org-other-agenda--same-marker-location-p
                   heading (org-other-agenda--at-point)))
          (should (equal (org-other-agenda--pin-action-label)
                         "Unpin from Today main lane"))
          (should (equal
                   (nth 1 (org-other-agenda-test--embark-action-entry
                           'org-heading #'org-other-agenda-toggle-pin))
                   "Unpin from Today main lane"))
          (let ((target (car (embark--targets))))
            (should (eq (plist-get target :type) 'org-heading))
            (embark--act #'org-other-agenda-toggle-pin target nil))
          (should (eq (org-other-agenda--pin-state heading) 'pin))
          (should (org-other-agenda-test--goto-rendered-entry
                   view "1:1 with the designer"))
          (should (widget-at (point)))
          (should (eq (org-other-agenda-test--key-command "RET")
                      #'widget-button-press))
          (org-other-agenda-test--call-key "RET")
          (should (eq (org-other-agenda--pin-state heading) 'unpin))
          (org-other-agenda-refresh)
          (textui-refresh view)
          (should (org-other-agenda-test--goto-rendered-entry
                   view "1:1 with the designer"))
          (setq heading (org-other-agenda--at-point))
          (should (eq (org-other-agenda--pin-state heading) 'unpin)))
        (org-other-agenda-test--select-tab view 'agenda)
        (with-current-buffer view
          (should (org-other-agenda-test--goto-rendered-entry
                   view "Monday standup"))
          (let ((before (plist-get textui-state :pinned)))
            (should-not (org-other-agenda--pin-state))
            (should-not (keymap-lookup embark-org-heading-map "C-c C-p"))
            (should-not (keymap-lookup
                         (embark--raw-action-keymap 'org-heading)
                         "C-c C-p"))
            (should-not (org-other-agenda-test--embark-action-entry
                         'org-heading #'org-other-agenda-toggle-pin))
            (should-error (org-other-agenda-toggle-pin) :type 'user-error)
            (should (eq before (plist-get textui-state :pinned)))))
        (with-current-buffer (marker-buffer heading)
          (goto-char heading)
          (should-not (keymap-lookup embark-org-heading-map
                                     "C-c C-p"))
          (should-not (keymap-lookup
                       (embark--raw-action-keymap 'org-heading) "C-c C-p"))
          (should-not (org-other-agenda-test--embark-action-entry
                       'org-heading #'org-other-agenda-toggle-pin))
          (should-error (org-other-agenda-toggle-pin)
                        :type 'user-error))))))

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
                (org-other-agenda-test--call-key "s"))
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

(ert-deftest org-other-agenda-test-query-generations-own-their-markers ()
  "Query generations preserve live base markers and retire only their own."
  (org-other-agenda-test--with-fixture
    (let* ((view (org-other-agenda-test--open-view))
           (base (with-current-buffer view
                   (plist-get textui-state :base-entries)))
           (base-entry (org-other-agenda-test--entry-by-title
                        base "Monday standup")))
      (should (cl-every #'org-other-agenda-test--entry-markers-live-p base))
      (with-current-buffer view
        (org-other-agenda-search "invitations")
        (textui-refresh view))
      (let ((first-query
             (with-current-buffer view
               (plist-get textui-state :query-entries))))
        (should first-query)
        (should (cl-every #'org-other-agenda-test--entry-markers-live-p base))
        (should (cl-every #'org-other-agenda-test--entry-markers-live-p
                          first-query))
        (org-other-agenda-test--select-tab view 'agenda)
        (with-current-buffer view
          (should (org-other-agenda-test--goto-rendered-entry
                   view "Monday standup"))
          (let (forwarded-marker)
            (cl-letf (((symbol-function 'org-agenda-todo)
                       (lambda ()
                         (interactive)
                         (setq forwarded-marker
                               (org-get-at-bol 'org-hd-marker)))))
              (org-other-agenda-todo))
            (should (equal forwarded-marker (plist-get base-entry :hd)))))
        (with-current-buffer view
          (org-other-agenda-search "#build")
          (textui-refresh view))
        (let ((second-query
               (with-current-buffer view
                 (plist-get textui-state :query-entries))))
          (should second-query)
          (should (cl-every #'org-other-agenda-test--entry-markers-live-p base))
          (should-not (cl-some #'org-other-agenda-test--entry-markers-live-p
                               first-query))
          (should (cl-every #'org-other-agenda-test--entry-markers-live-p
                            second-query))
          (with-current-buffer view
            (org-other-agenda-clear-search))
          (should (cl-every #'org-other-agenda-test--entry-markers-live-p base))
          (with-current-buffer view
            (org-other-agenda-search "invitations")
            (textui-refresh view))
          (let ((query-count
                 (with-current-buffer org-other-agenda--query-shadow
                   (length org-other-agenda--owned-markers))))
            (with-current-buffer view
              (org-other-agenda-search "invitations")
              (textui-refresh view))
            (should (= query-count
                       (with-current-buffer org-other-agenda--query-shadow
                         (length org-other-agenda--owned-markers)))))
          (let ((query-before-refresh
                 (with-current-buffer view
                   (plist-get textui-state :query-entries))))
            (should query-before-refresh)
            (with-current-buffer view
              (org-other-agenda-refresh)
              (textui-refresh view))
            (with-current-buffer view
              (should (plist-get textui-state :base-entries))
              (should (plist-get textui-state :query-entries))
              (should-not
               (cl-some #'org-other-agenda-test--entry-markers-live-p base))
              (should-not
               (cl-some #'org-other-agenda-test--entry-markers-live-p
                        query-before-refresh))
              (should
               (cl-every #'org-other-agenda-test--entry-markers-live-p
                         (plist-get textui-state :base-entries)))
              (should
               (cl-every #'org-other-agenda-test--entry-markers-live-p
                         (plist-get textui-state :query-entries))))))))))

(ert-deftest org-other-agenda-test-shadow-generation-preserves-external-markers ()
  "Shadow generations do not reset markers owned by external Org agendas."
  (org-other-agenda-test--with-fixture
    (let ((buffer (generate-new-buffer " *org-other-agenda-external-marker*"))
          marker)
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (insert "external")
              (goto-char (point-min))
              (let ((org-agenda-buffer nil))
                (setq marker (org-agenda-new-marker))))
            (let ((view (org-other-agenda-test--open-view)))
              (should (marker-buffer marker))
              (with-current-buffer view
                (org-other-agenda-search "invitations")
                (org-other-agenda-refresh))
              (should (marker-buffer marker))))
        (setq org-agenda-markers (delq marker org-agenda-markers))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

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
          (let ((eldoc (org-other-agenda--eldoc)))
            (should
             (string-match-p
              "\\`agenda · [^·]+ · [0-9]+/[0-9]+ · Monday standup\\'"
              eldoc))
            (should-not (string-match-p " · col " eldoc))))))))

(ert-deftest org-other-agenda-test-feedback-counts-rendered-structure ()
  "Calendar and time-block totals come from their rendered places."
  (org-other-agenda-test--with-fixture
    (cl-letf (((symbol-function 'window-body-height)
               (lambda (&optional _window) 80)))
      (let ((view (org-other-agenda-test--open-view)))
        (org-other-agenda-test--select-tab view 'day)
        (with-current-buffer view
          (should (= 6 (org-other-agenda--panel-row-count
                        'day-calendar 0)))
          (should (= 20 (org-other-agenda--panel-row-count
                         'day-timeblock 0)))
          (goto-char (org-other-agenda--position-for-place
                      '(day-timeblock 0 0)))
          (should
           (string-match-p
            "\\`day · time blocks · 1/20 · "
            (org-other-agenda--eldoc))))))))

(ert-deftest org-other-agenda-test-single-mark-gesture-can-end-in-place ()
  "A documented in-place m ends the rectangle gesture but keeps its mark."
  (org-other-agenda-test--with-fixture
    (let ((view (org-other-agenda-test--open-view)))
      (org-other-agenda-test--select-tab view 'week)
      (with-current-buffer view
        (should (org-other-agenda-test--goto-rendered-entry
                 view "Monday standup"))
        (org-other-agenda-mark)
        (should (plist-get textui-state :rectangle-anchor))
        (org-other-agenda-mark)
        (should-not (plist-get textui-state :rectangle-anchor))
        (should (= 1 (length (org-other-agenda--marked-headings)))))
      (org-other-agenda-test--select-tab view 'day)
      (with-current-buffer view
        (should (org-other-agenda-test--goto-rendered-entry
                 view "Design review"))
        (org-other-agenda-mark)
        (should (= 2 (length (org-other-agenda--marked-headings))))))))

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

(ert-deftest org-other-agenda-test-embark-selected-noun-reaches-action ()
  "Embark passes the selected nested noun to its action and unwinds it."
  (org-other-agenda-test--with-fixture
    (cl-letf (((symbol-function 'window-body-height)
               (lambda (&optional _window) 80)))
      (let ((view (org-other-agenda-test--open-view)))
        (set-window-buffer (selected-window) view)
        (cl-labels
            ((destination
              (target)
              (let (seen)
                (cl-letf
                    (((symbol-function 'org-other-agenda-schedule-set-here)
                      (lambda ()
                        (interactive)
                        (setq seen (org-other-agenda--schedule-target nil)))))
                  (embark--act #'org-other-agenda-schedule-set-here
                               target nil))
                (should-not org-other-agenda--embark-selected-target)
                seen)))
          (org-other-agenda-test--select-tab view 'day)
          (with-current-buffer view
            (should (org-other-agenda-test--goto-rendered-entry
                     view "Design review"))
            (let* ((targets (embark--targets))
                   (slot (cl-find 'ooa-slot targets
                                  :key (lambda (candidate)
                                         (plist-get candidate :type))))
                   (day (cl-find 'ooa-day targets
                                 :key (lambda (candidate)
                                        (plist-get candidate :type))))
                   (slot-value (get-text-property
                                0 'ooa-slot (plist-get slot :target)))
                   (day-value (get-text-property
                               0 'ooa-day (plist-get day :target))))
              (should (equal (destination slot) slot-value))
              (should (equal (destination day)
                             (list day-value nil nil)))
              (should (equal (org-other-agenda--schedule-target nil)
                             slot-value)))
            (goto-char (org-other-agenda--position-for-place
                        '(day-timeblock 0 3)))
            (let* ((targets (embark--targets))
                   (slot (cl-find 'ooa-slot targets
                                  :key (lambda (candidate)
                                         (plist-get candidate :type))))
                   (slot-value (get-text-property
                                0 'ooa-slot (plist-get slot :target))))
              (should (equal (destination slot) slot-value))))
          (org-other-agenda-test--select-tab view 'week)
          (with-current-buffer view
            (should (org-other-agenda-test--goto-rendered-entry
                     view "Monday standup"))
            (let* ((day (cl-find 'ooa-day (embark--targets)
                                 :key (lambda (candidate)
                                        (plist-get candidate :type))))
                   (day-value (get-text-property
                               0 'ooa-day (plist-get day :target))))
              (should (equal (destination day)
                             (list day-value nil nil))))))))))

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
                (org-other-agenda-test--call-key "S-<right>")
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
                (org-other-agenda-test--call-key "m")
                (goto-char (org-other-agenda--position-for-place
                            '(week-days 4 1)))
                (org-other-agenda-test--call-key "m")
                (should (= 4 (length (org-other-agenda--marked-headings))))
                (cl-letf (((symbol-function 'read-char-exclusive)
                           (lambda (&rest _arguments) ?t))
                          ((symbol-function 'completing-read)
                           (lambda (&rest _arguments) "DONE")))
                  (org-other-agenda-test--call-key "B"))
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
                (org-other-agenda-test--call-key "m"))
              (org-other-agenda-test--select-tab view 'day)
              (with-current-buffer view
                (goto-char (org-other-agenda--position-for-place
                            '(day-timeblock 0 3)))
                (should (eq (key-binding (kbd "s"))
                            #'org-other-agenda-schedule-set-here))
                (org-other-agenda-test--call-key "s")
                (should-not (org-other-agenda--marked-headings)))
              (with-current-buffer (find-file-noselect file)
                (goto-char (point-min))
                (should (search-forward
                         "SCHEDULED: <2026-08-20 Thu 09:30>" nil t)))))
      (when (file-exists-p file)
        (delete-file file))))))

(ert-deftest org-other-agenda-test-embark-selected-day-overrides-point-slot ()
  "An Embark day action on an occupied slot schedules without a time."
  (let ((file (make-temp-file "org-other-agenda-embark-day-" nil ".org")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "#+TODO: TODO | DONE\n\n"
                    "* TODO Occupied slot\n"
                    "  SCHEDULED: <2026-08-20 Thu 09:00-10:00>\n\n"
                    "* TODO Put me on the selected day\n"))
          (org-other-agenda-test--with-agenda-file file
            (let ((view (org-other-agenda-test--open-view)))
              (set-window-buffer (selected-window) view)
              (org-other-agenda-test--select-tab view 'someday)
              (with-current-buffer view
                (should (org-other-agenda-test--goto-rendered-entry
                         view "Put me on the selected day"))
                (org-other-agenda-mark))
              (org-other-agenda-test--select-tab view 'day)
              (with-current-buffer view
                (should (org-other-agenda-test--goto-rendered-entry
                         view "Occupied slot"))
                (let ((target
                       (cl-find 'ooa-day (embark--targets)
                                :key (lambda (candidate)
                                       (plist-get candidate :type)))))
                  (should target)
                  (should (equal
                           (org-other-agenda--schedule-target
                            (plist-get target :target))
                           (list org-other-agenda-test--today nil nil)))
                  (should (equal (org-other-agenda--schedule-target nil)
                                 (list org-other-agenda-test--today
                                       (* 9 60) (+ (* 9 60) 30))))
                  (embark--act #'org-other-agenda-schedule-set-here
                               target nil)))
              (with-current-buffer (find-file-noselect file)
                (goto-char (point-min))
                (should (search-forward
                         "SCHEDULED: <2026-08-20 Thu>" nil t))))))
      (when (file-exists-p file)
        (delete-file file)))))

(ert-deftest org-other-agenda-test-day-key-schedules-marked-heading ()
  "The day-local s key schedules a marked heading without a time."
  (let ((file (make-temp-file "org-other-agenda-day-key-" nil ".org")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "#+TODO: TODO | DONE\n\n* TODO Put me on this day\n"))
          (org-other-agenda-test--with-agenda-file file
            (let ((view (org-other-agenda-test--open-view)))
              (org-other-agenda-test--select-tab view 'someday)
              (with-current-buffer view
                (should (org-other-agenda-test--goto-rendered-entry
                         view "Put me on this day"))
                (org-other-agenda-test--call-key "m"))
              (org-other-agenda-test--select-tab view 'day)
              (with-current-buffer view
                (goto-char (point-min))
                (let (day-position)
                  (while (and (< (point) (point-max)) (null day-position))
                    (when (equal
                           (get-text-property
                            (point) 'org-other-agenda-object)
                           (cons 'ooa-day org-other-agenda-test--today))
                      (setq day-position (point)))
                    (goto-char
                     (or (next-single-property-change
                          (point) 'org-other-agenda-object nil (point-max))
                         (point-max))))
                  (should day-position)
                  (goto-char day-position))
                (should (eq (org-other-agenda-test--key-command "s")
                            #'org-other-agenda-schedule-set-here))
                (org-other-agenda-test--call-key "s")
                (should-not (org-other-agenda--marked-headings)))
              (with-current-buffer (find-file-noselect file)
                (goto-char (point-min))
                (should (search-forward
                         "SCHEDULED: <2026-08-20 Thu>" nil t))))))
      (when (file-exists-p file)
        (delete-file file)))))

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
