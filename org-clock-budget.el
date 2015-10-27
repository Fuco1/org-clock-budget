;;; org-clock-budget.el --- Budget your time with org! -*- lexical-binding: t -*-

;; Copyright (C) 2015 Matúš Goljer

;; Author: Matúš Goljer <matus.goljer@gmail.com>
;; Maintainer: Matúš Goljer <matus.goljer@gmail.com>
;; Version: 0.0.1
;; Created: 23rd October 2015
;; Package-requires: ((dash "2.10.0") (s "1.0"))
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

(defun org-clock-budget-interval-this-week ()
  "Return the interval representing this week."
  (cons
   (org-read-date nil nil "++Mon" nil (org-time-string-to-time (org-read-date nil nil "-7d")))
   (org-read-date nil nil "--Sun" nil (org-time-string-to-time (org-read-date nil nil "+7d")))))

(defun org-clock-budget-interval-this-month ()
  "Return the interval representing this month."
  (cons
   (format-time-string "%Y-%m-01")
   (format-time-string (format
                        "%%Y-%%m-%2d"
                        (calendar-last-day-of-month
                         (string-to-number (format-time-string "%m"))
                         (string-to-number (format-time-string "%Y")))))))

(defun org-clock-budget-interval-this-year ()
  "Return the interval representing this year."
  (cons
   (format-time-string "%Y-01-01")
   (format-time-string "%Y-12-31")))

(defun org-clock-budget--get-budget-symbol (prop-name)
  "Return PROP-NAME as budget symbol."
  (intern (concat ":" prop-name)))

(defun org-clock-budget--get-clock-symbol (prop-name)
  "Return PROP-NAME as clock symbol."
  (intern (concat ":"(replace-regexp-in-string "budget" "clock" prop-name))))

(defun org-clock-budget--get-entries-with-budget (from to budget)
  "Get all tasks with a budget.

FROM and TO specify the time range and should be YYYY-MM-DD
strings.

BUDGET is type of the budget property.

CLOCK is a string derived from BUDGET by replacing the string
\"BUDGET\" with \"CLOCK\".

Return a list (headline CLOCK clocked-time BUDGET budget :marker
marker-to-headline)"
  (org-clock-sum from (concat to " 23:59:59"))
  (let ((result nil))
    (org-map-entries
     (lambda ()
       (let* ((clock (get-text-property (point) :org-clock-minutes))
              (current-budget (--when-let (org-entry-get (point) budget)
                                (org-hh:mm-string-to-minutes it))))
         (when current-budget
           (push (list (org-get-heading t t)
                       (org-clock-budget--get-clock-symbol budget) (or clock 0)
                       (org-clock-budget--get-budget-symbol budget) current-budget
                       :marker (point-marker)) result)))))
    (nreverse result)))

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
  (let ((sort-type (org-clock-budget--get-column-property
                    :org-clock-budget-report-sort))
        (inhibit-read-only t))
    (org-table-sort-lines nil sort-type)
    (org-clock-budget--set-column-property
     :org-clock-budget-report-sort
      (if (= (downcase sort-type) sort-type)
          (upcase sort-type)
        (downcase sort-type)))))

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
                (incf (cadr (assoc name sums)) (or budget 0))
                (incf (caddr (assoc name sums)) (or clock 0))
                (push (if budget (org-minutes-to-clocksum-string budget) "") row)
                (push (if clock (org-minutes-to-clocksum-string clock) "") row)
                (push (if budget (format "%2.1f%%" (* 100 (/ clock (float budget)))) "") row)))
            (insert (apply 'format (org-clock-budget-report-row-format) (nreverse row)))
            (make-text-button (save-excursion (forward-line -1) (point)) (1- (point))
                              'marker marker
                              'type 'org-clock-budget-report-button))))
      (insert "|-\n")
      (insert (apply
               'format (org-clock-budget-report-row-format) ""
               (--mapcat
                (let* ((name (car it))
                       (budget (cadr (assoc name sums)))
                       (clock (caddr (assoc name sums))))
                  (list
                   (org-minutes-to-clocksum-string budget)
                   (org-minutes-to-clocksum-string clock)
                   (format "%2.1f%%" (* 100 (/ clock (float budget))))))
                org-clock-budget-intervals)))
      (org-clock-budget-report-mode)
      (variable-pitch-mode -1)
      (org-table-align)
      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer output)))

(defvar org-clock-budget-report-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map org-mode-map)
    (define-key map "s" 'org-clock-budget-report-sort)
    (define-key map "g" 'org-clock-budget-report)
    map)
  "Keymap for `org-clock-budget-report-mode'.")

(define-derived-mode org-clock-budget-report-mode org-mode "Clock budget"
  "Mode for reporting time budgets."
  (use-local-map org-clock-budget-report-mode-map))

(provide 'org-clock-budget)
;;; org-clock-budget.el ends here
