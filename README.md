# org-other-agenda

Org's agenda is a list. This renders the same data as a board.

- a month calendar you can click, with density dots for scheduled days and deadlines
- a day view with time blocks whose height follows their duration, and lanes for overlaps
- a week view where multi-day tasks are drawn as bars spanning the days they cover
- a Someday list of everything with no date, so you can schedule it from there
- search that reaches entry bodies, not just headings

Nothing about Org changes. A hidden agenda buffer is produced by Org itself,
its per-line text properties are read back, and the result is re-laid out with
[TextUI](https://github.com/yibie/textui). Sorting, filtering and every
`org-agenda-*` command keep their own semantics; entry identity stays
`org-hd-marker`.

## Screenshots

![Today](images/today.png)

Today combines the month calendar with proportional time blocks, four-way
overlap lanes, and numbered overflow entries.

![Week](images/week.png)

Week draws multi-day tasks as bars spanning the dates they cover.

![List](images/list.png)

List shows agenda rows with separate TODO state and condition columns.

## Status

Early, but usable. Rendering, the calendar, time blocks, week bars, search and
command forwarding all work against real Org data.

## Requirements

Emacs 29.1 or newer, Org 9.6 or newer, and TextUI.

## Install

```elisp
(add-to-list 'load-path "/path/to/textui")
(add-to-list 'load-path "/path/to/org-other-agenda")
(require 'org-other-agenda)
```

Then `M-x org-other-agenda`.

Your `org-agenda-files` and every other Org setting are used as they are.
The package overrides nothing.

## Tabs

| Tab | Shows |
|---|---|
| Today | a month calendar and the selected day's time blocks |
| Week | the week, with multi-day tasks as spanning bars |
| Agenda | the daily/weekly agenda as a list |
| Someday | entries with no date at all — schedule them from here |
| Search | results of the last text search |
| Tags | results of the last tag search |

Someday and Agenda are complementary and do not overlap: everything with a date
is in Agenda, everything without one is in Someday.

## Keys

| Key | Action |
|---|---|
| `1`–`6` | switch tab |
| `TAB` / `S-TAB` | move between entries |
| `RET` or mouse-1 | visit the Org entry |
| `t` `s` `d` `,` `:` | TODO state, schedule, deadline, priority, tags |
| `/` | search |
| `\` | clear the search |
| `[` `]` | previous / next day |
| `C-v` `M-v` | scroll a long list |
| `g` | refresh |
| `q` | quit |

The editing keys forward to the native `org-agenda-*` command for the entry at
point, so your TODO sequences, logging and hooks all apply. The month calendar,
the tab bar, the overflow markers and the overdue fold are clickable.

## Searching

`/` takes a query. A query starting with `#` matches tags, anything else is a
full-text search:

```
review            full text, including entry bodies
#build            tagged build
#work+urgent      Org's own tag matching syntax
```

Both run Org's own `search` and `tags` agenda blocks, so Org's boolean syntax,
regexp support and tag inheritance all work. `\` clears the query and returns
to the cached agenda without regenerating it.

## Which blocks the agenda is built from

`org-other-agenda-series` has the shape of a command list in
`org-agenda-custom-commands`:

```elisp
(setq org-other-agenda-series
      '((agenda "" ((org-agenda-span 'week)))
        (alltodo "")))
```

Any block type Org accepts works here.

## Performance

Measured on a synthetic 2000-entry file:

| | before | after |
|---|---:|---:|
| a search | 3802 ms | 8 ms |
| clearing a search | full regeneration | 0 ms |
| first render | 1025 ms | 323 ms |
| re-rendering the Agenda tab | 1155 ms | 17 ms |
| re-rendering the Today tab | 532 ms | 17 ms |

Long lists render only the rows that fit the window, so render cost tracks
window height rather than entry count. Switching tabs never regenerates the
agenda; only `g` and a new search do.

What remains is Org's own cost: building the hidden agenda buffer takes about
3.7 s for 2000 entries, the same as a native agenda over the same files.
Org's [own tuning options](https://orgmode.org/manual/Speeding-Up-Your-Agendas.html)
buy about 12% together, and the largest of them,
`org-agenda-dim-blocked-tasks`, is worth only 8% while disabling blocked-task
detection entirely — which this package uses for the blocked marker and for
deciding what belongs in Someday. Those settings are left to you rather than
forced in code.

## Column alignment needs a CJK font at exactly 2:1

If your Org files contain CJK text, columns can only align when a wide glyph is
drawn exactly twice as wide as a space. Emacs counts it as two columns
regardless, so a font that draws it narrower shifts every column after it. On
macOS, measure the current frame and solve for the CJK size instead of using a
fixed value:

```elisp
(let ((space (string-pixel-width " "))
      (family "PingFang SC")
      best)
  (dolist (size (number-sequence 8 30))
    (set-fontset-font t 'han (font-spec :family family :size size))
    (set-fontset-font t 'cjk-misc (font-spec :family family :size size))
    (when (= (string-pixel-width "中") (* 2 space))
      (setq best size)))
  (when best
    (set-fontset-font t 'han (font-spec :family family :size best))
    (set-fontset-font t 'cjk-misc (font-spec :family family :size best))))
```

Run this after the GUI frame exists, and run it again if the default font
height changes: a hard-coded size stops being 2:1 after a theme, DPI, or text
scale change. In a terminal the problem does not arise; the terminal enforces
the cell grid itself.

## Overlapping time blocks

When several tasks overlap, lanes are allocated from the available width, at
most three and never narrower than 20 columns. Whatever does not fit is listed
below the grid with a number; clicking that number pins the entry into the main
lane and pushes the current occupant out. Lane geometry is fixed for the whole
day, so bars never shift horizontally between rows.

## Try it without installing

For your own Org files, load the package and run `M-x org-other-agenda`. It uses
your existing `org-agenda-files` and Org settings without overriding them.

For a repeatable checkout demo, evaluate this small snippet in a throwaway
`emacs -Q` session after loading the package:

```elisp
(require 'calendar)
(require 'org-other-agenda)
(setq org-agenda-files
      (list (expand-file-name
             "test/fixtures/agenda.org" "/path/to/org-other-agenda"))
      org-enforce-todo-dependencies t
      org-enforce-todo-checkbox-dependencies t
      org-agenda-dim-blocked-tasks t
      org-habit-show-habits t)
(advice-add 'org-today :override
            (lambda () (calendar-absolute-from-gregorian '(8 20 2026))))
(org-other-agenda)
```

Loads the package against a synthetic fixture with a pinned date, so the view is
reproducible. The fixture is in `test/fixtures/` and covers blocked tasks,
habits, a running clock, multi-day spans and a four-way overlap.

## Development

```
make check                 compile, checkdoc and the test suite
make ORG=/path/to/org/lisp check    against a newer Org checkout
```

## License

GPL-3.0-or-later.
