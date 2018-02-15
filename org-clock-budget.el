;;; org-clock-budget.el --- Budget your time with org! -*- lexical-binding: t -*-

;; Copyright (C) 2015-2017 Matúš Goljer

;; Author: Matúš Goljer <matus.goljer@gmail.com>
;; Maintainer: Matúš Goljer <matus.goljer@gmail.com>
;; Version: 0.0.1
;; Created: 23rd October 2015
;; Package-requires: ((dash "2.10.0") (s "1.0") (cl-lib "0.6.0"))
;; Keywords: calendar, convenience

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'dash)
(require 's)

(require 'calendar)
(require 'org)
(require 'org-table)
(require 'button)

(defgroup org-clock-budget ()
  "Report to budget time."
  :group 'org
  :prefix "org-clock-budget-")

(defcustom org-clock-budget-intervals
  '(
    ("BUDGET_YEAR" org-clock-budget-interval-this-year)
    ("BUDGET_QUARTER" org-clock-budget-interval-this-quarter)
    ("BUDGET_MONTH" org-clock-budget-interval-this-month)
    ("BUDGET_WEEK" org-clock-budget-interval-this-week)
    )
  "Intervals recognized by `org-clock-budget-report'.

A list of lists (NAME INTERVAL-FN) where:

- NAME: name of property for this interval, must start with prefix BUDGET_
- INTERVAL-FN: a function of no arguments returning a cons (FROM . TO)
               representing an interval on which clocking for this
               interval takes place."
  :type '(alist :key-type (string :tag "Name")
                :value-type (group (function :tag "Interval function")))
  :options '("BUDGET_YEAR" "BUDGET_MONTH" "BUDGET_WEEK")
  :group 'org-clock-budget)

(defcustom org-clock-budget-default-sort-column nil
  "Column on which to sort by default.

If nil, rows are inserted in order they are scanned in the agenda
files, as returned by `org-agenda-files'.

Otherwise the value is a list (BUDGET VALUE DIRECTION) where
BUDGET is one of `org-clock-budget-interval', VALUE is a symbol,
one of 'clock, 'budget, 'ratio determining the column associated
with the selected BUDGET and DIRECTION is a symbol 'asc or
'desc."
  :type '(choice
          (const :tag "No ordering" nil)
          (list (string :tag "Name")
                (choice
                 (const :tag "Ratio" ratio)
                 (const :tag "Clock" clock)
                 (const :tag "Budget" budget))
                (choice
                 (const :tag "Descending" desc)
                 (const :tag "Ascending" asc))))
  :group 'org-clock-budget)

(defcustom org-clock-budget-ratio-faces '((1.0 font-lock-warning-face))
  "An alist determining formatted ratio colors.

The `car' is a float determining ratio of clock vs budget.  If
the ratio is over this value, the `cadr' (face) is used to
colorize it in the report.

The highest smaller possible ratio is used to determine the
face."
  :type '(alist :key-type (float :tag "Ratio")
                :value-type (group (face :tag "Face")))
  :options '(1.0)
  :group 'org-clock-budget)

(defun org-clock-budget-interval-this-week ()
  "Return the interval representing this week."
  (let ((case-fold-search t))
    (cons
     (org-read-date nil nil "++Mon" nil (org-time-string-to-time (org-read-date nil nil "-7d")))
     (concat (org-read-date nil nil "--Sun" nil (org-time-string-to-time (org-read-date nil nil "+7d"))) " 23:59:59"))))

(defun org-clock-budget-interval-this-month ()
  "Return the interval representing this month."
  (cons
   (format-time-string "%Y-%m-01")
   (format-time-string (format
                        "%%Y-%%m-%02d 23:59:59"
                        (calendar-last-day-of-month
                         (string-to-number (format-time-string "%m"))
                         (string-to-number (format-time-string "%Y")))))))

(defun org-clock-budget-interval-this-quarter ()
  "Return the interval representing this quarter (3-month period)."
  (let* ((current-quarter
          ;; [0, 3]
          (/ (- (string-to-number (format-time-string "%m")) 1) 3))
         (first-month (+ (* 3 current-quarter) 1))
         (last-month (+ first-month 2)))
    (cons
     (format-time-string (format "%%Y-%02d-01" first-month))
     (format-time-string (format
                          "%%Y-%02d-%02d 23:59:59"
                          last-month
                          (calendar-last-day-of-month
                           last-month
                           (string-to-number (format-time-string "%Y"))))))))

(defun org-clock-budget-interval-this-year ()
  "Return the interval representing this year."
  (cons
   (format-time-string "%Y-01-01")
   (format-time-string "%Y-12-31 23:59:59")))

(defun org-clock-budget--get-budget-symbol (prop-name)
  "Return PROP-NAME as budget symbol."
  (intern (concat ":" prop-name)))

(defun org-clock-budget--get-clock-symbol (prop-name)
  "Return PROP-NAME as clock symbol."
  (intern (concat ":"(replace-regexp-in-string "budget" "clock" prop-name))))

(defun org-clock-budget--get-entry-clocked (from to)
  "Return time clocked from FROM to TO of the entry at point."
  (save-excursion
    (save-restriction
      (widen)
      (org-narrow-to-subtree)
      (org-clock-sum from to)
      (org-get-at-bol :org-clock-minutes))))

(defun org-clock-budget--get-entries-with-budget (from to budget)
  "Get all tasks with a budget.

FROM and TO specify the time range and should be YYYY-MM-DD
strings.

BUDGET is type of the budget property.

CLOCK is a string derived from BUDGET by replacing the string
\"BUDGET\" with \"CLOCK\".

Return a list (headline CLOCK clocked-time BUDGET budget :marker
marker-to-headline)"
  (save-excursion
    (goto-char (point-min))
    (let ((result nil)
          (re (concat ":" budget ":")))
      (while (re-search-forward re nil t)
        (-when-let (current-budget (--when-let (org-entry-get (point) budget)
                                     (org-hh:mm-string-to-minutes it)))
          (save-excursion
            (org-back-to-heading t)
            (let* ((clock (org-clock-budget--get-entry-clocked from to)))
              (push (list (org-get-heading t t)
                          (org-clock-budget--get-clock-symbol budget) (or clock 0)
                          (org-clock-budget--get-budget-symbol budget) current-budget
                          :marker (point-marker)) result)))))
      (nreverse result))))

(defun org-clock-budget ()
  "Retrieve headlines with clock budget in current buffer.

Each headline with at least one clock budget specified is
retrieved with clocked time for the specific time range.  It is
enough for a headline to have one budget specified.

You can add or remove intervals by customizing
`org-clock-budget-intervals'."
  (let ((budgets (-mapcat
                  (-lambda ((name int-fn))
                    (-let [(from . to) (funcall int-fn)]
                      (org-clock-budget--get-entries-with-budget
                       from to name)))
                  org-clock-budget-intervals)))
    (-map (lambda (x)
            (let ((header (car x)))
              (cons header (apply '-concat (-map 'cdr (cdr x))))))
          (-group-by 'car budgets))))

(defun org-clock-budget--with-column-header (function)
  "Run FUNCTION with point at header of current column."
  (save-excursion
    (let ((cc (org-table-current-column)))
      (goto-char (point-min))
      (org-table-goto-column cc)
      (skip-chars-forward " ")
      (funcall function))))

(defun org-clock-budget--get-column-property (key)
  "Get column property KEY."
  (org-clock-budget--with-column-header
   (lambda () (get-text-property (point) key))))

(defun org-clock-budget--set-column-property (key value)
  "Set column property KEY to VALUE."
  (org-clock-budget--with-column-header
   (lambda () (put-text-property (point) (1+ (point)) key value))))

(defun org-clock-budget-report-sort ()
  "Sort the column under point."
  (interactive)
  (let* ((sort-type (org-clock-budget--get-column-property
                     :org-clock-budget-report-sort))
         (inhibit-read-only t)
         (new-sort-type (if (= (downcase sort-type) sort-type)
                            (upcase sort-type)
                          (downcase sort-type))))
    (org-table-sort-lines nil sort-type)
    (org-clock-budget--set-column-property
     :org-clock-budget-report-sort new-sort-type)
    new-sort-type))

(defun org-clock-budget-remove-budget ()
  "Remove budgets from the task under point."
  (interactive)
  (org-with-point-at (save-excursion
                       (beginning-of-line)
                       (forward-char 2)
                       (get-text-property (point) 'marker))
      (--each (-map 'car org-clock-budget-intervals)
     (org-entry-delete (point) it)))
  (let ((inhibit-read-only t)
        (column (current-column)))
    (delete-region (point-at-bol) (progn (forward-line 1) (point)))
    (move-to-column column)))

(define-button-type 'org-clock-budget-report-button
  'action 'org-clock-budget-report-button-action)

(defun org-clock-budget-report-button-action (button)
  "Follow `org-clock-budget-report' BUTTON to the corresponding entry."
  (org-goto-marker-or-bmk (button-get button 'marker)))

(defun org-clock-budget-report-row-format ()
  "Return format string for a row of `org-clock-budget-report'."
  (concat
   "| %s |"
   (s-repeat (* 3 (length org-clock-budget-intervals)) " %s |")
   "\n"))

(defun org-clock-budget-report-initial-sort ()
  "Sort the columns of `org-clock-budget-report'."
  (save-excursion
    (-when-let* (((name value direction) org-clock-budget-default-sort-column)
                 (index (--find-index (equal name it) (-map 'car org-clock-budget-intervals)))
                 (offset (cond
                          ((eq value 'budget) 0)
                          ((eq value 'clock) 1)
                          ((eq value 'ratio) 2))))
      (goto-char (point-min))
      (forward-line 2)
      (org-table-goto-column (+ 2 (* 3 index) offset))
      (let* ((dir (org-clock-budget-report-sort))
             (dir (if (s-uppercase? (string dir)) 'asc 'desc)))
        (unless (eq direction dir)
          (org-clock-budget-report-sort))))))

(defun org-clock-budget-report-format-ratio (ratio)
  "Format RATIO.

Ratio is clock / budget."
  (let* ((ratio-formatted (format "%2.1f%%" (* 100 ratio)))
         (sorted-ratios (--sort (> (car it) (car other)) org-clock-budget-ratio-faces))
         (face (cadr (--first (> ratio (car it)) sorted-ratios))))
    (if face
        (propertize ratio-formatted 'font-lock-face face)
      ratio-formatted)))

(defun org-clock-budget-report ()
  "Produce a clock budget report.

A clock budget report lists for each time range three columns,
the budget for this range, the already clocked time and a % of
used-up time.  A headline can have one or multiple budgets set.
Only headlines with at least one budget are shown."
  (interactive)
  (let ((output (get-buffer-create "*Org clock budget report*"))
        (stats (--mapcat (with-current-buffer (org-get-agenda-file-buffer it)
                           (org-clock-budget))
                         (org-agenda-files)))
        (sums (--map (list (car it) 0 0) org-clock-budget-intervals)))
    (with-current-buffer output
      (read-only-mode -1)
      (erase-buffer)
      (insert (apply
               'format (org-clock-budget-report-row-format)
               (propertize "Task" :org-clock-budget-report-sort ?a)
               (--mapcat
                (let* ((name (cadr (s-match "BUDGET_\\(.*\\)" (car it))))
                       (name-cap (s-capitalize name)))
                  (list
                   (propertize (concat name-cap " budget") :org-clock-budget-report-sort ?T)
                   (propertize (concat name-cap " clocked") :org-clock-budget-report-sort ?T)
                   (propertize (concat (substring name-cap 0 1) " C/G") :org-clock-budget-report-sort ?N)))
                org-clock-budget-intervals)))
      (insert "|-\n")
      (-each stats
        (lambda (row-data)
          (-let* (((header &keys :marker marker) row-data)
                  (row (list (concat
                              (replace-regexp-in-string
                               "|" "{pipe}"
                               (truncate-string-to-width header 40))))))
            (--each org-clock-budget-intervals
              (-let* ((name (car it))
                      (clock (org-clock-budget--get-clock-symbol name))
                      (budget (org-clock-budget--get-budget-symbol name))
                      ((_ &keys clock clock budget budget) row-data))
                (cl-incf (cadr (assoc name sums)) (or budget 0))
                (cl-incf (cl-caddr (assoc name sums)) (or clock 0))
                (push (if (and budget (> budget 0)) (org-minutes-to-clocksum-string budget) "") row)
                (push (if (and clock (> clock 0)) (org-minutes-to-clocksum-string clock) "") row)
                (push (if (and budget (> budget 0))
                          (org-clock-budget-report-format-ratio (/ clock (float budget)))
                        "") row)))
            (insert (apply 'format (org-clock-budget-report-row-format) (nreverse row)))
            (make-text-button
             (save-excursion (forward-line -1) (point))
             (save-excursion (forward-line -1) (re-search-forward "|" nil nil 2) (- (point) 2))
             'marker marker
             'type 'org-clock-budget-report-button))))
      (insert "|-\n")
      (insert (apply
               'format (org-clock-budget-report-row-format) ""
               (--mapcat
                (let* ((name (car it))
                       (budget (cadr (assoc name sums)))
                       (clock (cl-caddr (assoc name sums))))
                  (list
                   (org-minutes-to-clocksum-string budget)
                   (org-minutes-to-clocksum-string clock)
                   (format "%2.1f%%" (* 100 (/ clock (float budget))))))
                org-clock-budget-intervals)))
      (org-clock-budget-report-mode)
      (setq-local font-lock-keywords nil)
      (font-lock-mode -1)
      (font-lock-mode 1)
      (variable-pitch-mode -1)
      (org-table-align)
      (goto-char (point-min))
      (org-clock-budget-report-initial-sort)
      (read-only-mode 1))
    (pop-to-buffer output)))

(defun org-clock-budget-insert-into-agenda ()
  "Insert budget/clock information into agenda."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((habit (get-text-property (point) 'org-agenda-type)))
          (when (eq habit 'agenda)
            (-when-let (regexp (org-get-at-bol 'org-todo-regexp))
              (when (re-search-forward regexp (line-end-position) t)
                (-when-let (info (org-clock-budget-agenda-prefix-week))
                  (insert-before-markers info)
                  (when (re-search-forward "  " (line-end-position) t)
                    (delete-char (length info))))))))
        (forward-line)))))

(defun org-clock-budget-agenda-prefix-week ()
  "Return agenda prefix for weekly budget."
  (org-with-point-at (org-get-at-bol 'org-hd-marker)
    (-when-let (b (org-entry-get (point) "BUDGET_WEEK"))
      (format
       " [%s%s]"
       (-let (((from . to) (org-clock-budget-interval-this-week)))
         (--if-let (org-clock-budget--get-entry-clocked from to)
             (concat (org-minutes-to-clocksum-string it) "/")
           ""))
       b))))

(defvar org-clock-budget-report-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map org-mode-map)
    (define-key map "s" 'org-clock-budget-report-sort)
    (define-key map "g" 'org-clock-budget-report)
    (define-key map "D" 'org-clock-budget-remove-budget)
    (define-key map "q" 'quit-window)
    map)
  "Keymap for `org-clock-budget-report-mode'.")

(define-derived-mode org-clock-budget-report-mode org-mode "Clock budget"
  "Mode for reporting time budgets."
  (use-local-map org-clock-budget-report-mode-map))

(provide 'org-clock-budget)
;;; org-clock-budget.el ends here
