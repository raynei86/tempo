;;;; Tests for the DATE type, exercised through the public TEMPO
;;;; package (re-exported from TEMPO.DATE).
;;;;
;;;; Conventions:
;;;; - Type errors and calendar errors are separate tiers: non-integers
;;;;   are TYPE-ERRORs, impossible integer triples are
;;;;   INVALID-DATE-ERRORs.
;;;; - VALID-DATE-P is a total predicate: anything that is not a date
;;;;   returns NIL rather than signalling.

(defpackage tempo/tests/date
  (:use #:cl #:tempo #:rove))

(in-package #:tempo/tests/date)

;;; A float that exists only as runtime data. A literal float in a call
;;; would conflict with the declared INTEGER argument types at compile
;;; time; indirecting it keeps the check a runtime concern.
(defparameter *runtime-float* 1.5)

;;; Runtime values whose types are deliberately unknown to the compiler,
;;; so the checks below are runtime checks rather than compile-time
;;; constant diagnostics.
(defparameter *runtime-weekday* 8)

(defparameter *bad-month* 13)

(deftest test-construction
  (testing "positional and keyword constructors agree"
    (ok (equalp (date 2026 10 30) (make-date :year 2026 :month 10 :day 30)))
    (ok (equalp (make-date :year 2026) (date 2026 1 1))))
  (testing "YEAR is required"
    (ok (signals (make-date) 'error)))
  (testing "impossible dates signal INVALID-DATE-ERROR"
    (ok (signals (date 2026 2 30) 'invalid-date-error))
    (ok (signals (date 2025 2 29) 'invalid-date-error))
    (ok (signals (make-date :year 2026 :month 0) 'invalid-date-error))
    (ok (signals (make-date :year 2026 :month 13) 'invalid-date-error))
    (ok (signals (make-date :year 2026 :day 0) 'invalid-date-error)))
  (testing "non-integers are TYPE-ERRORs, not calendar errors"
    (ok (signals (make-date :year *runtime-float*) 'type-error))
    (ok (signals (make-date :year 2026 :month *runtime-float*) 'type-error))
    (ok (signals (make-date :year 2026 :day *runtime-float*) 'type-error))))

(deftest test-valid-date-p
  (testing "real dates"
    (ok (valid-date-p 2026 10 30))
    (ok (valid-date-p 2024 2 29))
    (ok (valid-date-p 2026 1 1)))
  (testing "impossible dates"
    (ok (not (valid-date-p 2025 2 29)))
    (ok (not (valid-date-p 2026 2 30)))
    (ok (not (valid-date-p 2026 0 1)))
    (ok (not (valid-date-p 2026 13 1)))
    (ok (not (valid-date-p 2026 1 0)))
    (ok (not (valid-date-p 2026 1 32))))
  (testing "non-integers are simply not dates"
    (ok (not (valid-date-p nil nil nil)))
    (ok (not (valid-date-p "2026" 1 1)))
    (ok (not (valid-date-p 2026 1 *runtime-float*)))
    (ok (not (valid-date-p *runtime-float* 1 1)))))

(deftest test-day-numbers
  (testing "Rata Die anchors"
    (ok (= 1 (rata-die (date 1 1 1))))
    (ok (= 0 (rata-die (date 0 12 31))))
    (ok (= 719163 (rata-die (date 1970 1 1))))
    (ok (= 730120 (rata-die (date 2000 1 1)))))
  (testing "day-number round trips, including year zero"
    (dolist (rd '(1 365 366 0 -365 719162 719163 730120))
      (ok (= rd (rata-die (date-from-rata-die rd))))))
  (testing "Julian day numbers"
    (ok (= 2451545 (jdn (date 2000 1 1))))
    (ok (equalp (date 2000 1 1) (date-from-jdn 2451545)))
    (dolist (n '(2451545 2440588 1721426))
      (ok (= n (jdn (date-from-jdn n)))))))

(deftest test-calendar-queries
  (testing "leap years"
    (ok (leap-year-p 2024))
    (ok (not (leap-year-p 2025)))
    (ok (leap-year-p 2000))
    (ok (not (leap-year-p 1900)))
    (ok (leap-year-p (date 2024 2 1))))
  (testing "month and year lengths"
    (ok (= 29 (days-in-month (date 2024 2 1))))
    (ok (= 28 (days-in-month (date 2025 2 1))))
    (ok (= 29 (days-in-month 2024 2)))
    (ok (= 28 (days-in-month 2025 2)))
    (ok (= 31 (days-in-month 2026 7)))
    (ok (= 366 (days-in-year 2024)))
    (ok (= 365 (days-in-year 2025))))
  (testing "days-in-month argument errors"
    (ok (signals (days-in-month *bad-month*) 'error))
    (ok (signals (days-in-month 2026 *bad-month*) 'type-error)))
  (testing "day of week"
    (ok (= 1 (day-of-week (date 2026 11 23))))
    (ok (= 5 (day-of-week (date 2026 10 30))))
    (ok (= 7 (day-of-week (date 2026 11 22)))))
  (testing "day of year"
    (ok (= 1 (day-of-year (date 2026 1 1))))
    (ok (= 366 (day-of-year (date 2024 12 31))))
    (ok (= 304 (day-of-year (date 2026 10 31))))))

(deftest test-iso-calendar
  (testing "known ISO week boundaries"
    (ok (equal '(1970 1 4) (multiple-value-list (iso-calendar (date 1970 1 1)))))
    (ok (equal '(2026 1 4) (multiple-value-list (iso-calendar (date 2026 1 1)))))
    (ok (equal '(2026 44 5) (multiple-value-list (iso-calendar (date 2026 10 30)))))
    (ok (equal '(2026 53 5) (multiple-value-list (iso-calendar (date 2027 1 1)))))
    (ok (equal '(2022 52 7) (multiple-value-list (iso-calendar (date 2023 1 1)))))
    (ok (equal '(2020 53 5) (multiple-value-list (iso-calendar (date 2021 1 1)))))
    (ok (equal '(2025 1 1) (multiple-value-list (iso-calendar (date 2024 12 30)))))
    (ok (equal '(1999 52 6) (multiple-value-list (iso-calendar (date 2000 1 1)))))))

(deftest test-replace-date
  (testing "replacing fields"
    (ok (equalp (replace-date (date 2026 10 30) :day 1) (date 2026 10 1)))
    (ok (equalp (replace-date (date 2026 10 30) :month 2 :day 1) (date 2026 2 1)))
    (ok (equalp (replace-date (date 2026 10 30)) (date 2026 10 30)))
    (ok (equalp (replace-date (date 2024 2 29) :year 2028) (date 2028 2 29))))
  (testing "impossible combinations still signal"
    (ok (signals (replace-date (date 2024 2 29) :year 2025) 'invalid-date-error))
    (ok (signals (replace-date (date 2026 1 31) :month 2) 'invalid-date-error))
    (ok (signals (replace-date (date 2026 1 31) :month 13) 'invalid-date-error)))
  (testing "the clamp restart is offered only when the day overflows"
    (ok (equalp (handler-bind ((invalid-date-error
                                (lambda (c)
                                  (declare (ignore c))
                                  (invoke-restart 'clamp-to-end-of-month))))
                  (replace-date (date 2024 1 31) :month 2))
                (date 2024 2 29)))
    (ok (equalp (handler-bind ((invalid-date-error
                                (lambda (c)
                                  (declare (ignore c))
                                  (invoke-restart 'clamp-to-end-of-month))))
                  (replace-date (date 2026 1 1) :day 32))
                (date 2026 1 31))))
  (testing "no clamp restart for a bad month or a day underflow"
    (flet ((restart-offered-p (thunk)
             (let ((seen nil))
               (catch 'signalled
                 (handler-bind ((invalid-date-error
                                 (lambda (c)
                                   (declare (ignore c))
                                   (setf seen
                                         (and (find-restart 'clamp-to-end-of-month) t))
                                   (throw 'signalled nil))))
                   (funcall thunk)))
               seen)))
      (ok (null (restart-offered-p
                 (lambda () (replace-date (date 2026 1 31) :month 13)))))
      (ok (null (restart-offered-p
                 (lambda () (replace-date (date 2026 1 31) :day 0)))))
      (ok (restart-offered-p
           (lambda () (replace-date (date 2026 1 31) :day 32)))))))

(deftest test-start-and-end
  (testing "month and year boundaries"
    (ok (equalp (start-of-month (date 2026 10 30)) (date 2026 10 1)))
    (ok (equalp (end-of-month (date 2026 10 30)) (date 2026 10 31)))
    (ok (equalp (end-of-month (date 2024 2 5)) (date 2024 2 29)))
    (ok (equalp (start-of-year (date 2026 10 30)) (date 2026 1 1)))
    (ok (equalp (end-of-year (date 2026 10 30)) (date 2026 12 31))))
  (testing "week boundaries (ISO week, Monday through Sunday)"
    (ok (equalp (start-of-week (date 2026 10 30)) (date 2026 10 26)))
    (ok (equalp (end-of-week (date 2026 10 30)) (date 2026 11 1)))
    (ok (equalp (start-of-week (date 2026 10 26)) (date 2026 10 26)))
    (ok (equalp (end-of-week (date 2026 11 1)) (date 2026 11 1)))))

(deftest test-comparisons
  (testing "ordering"
    (ok (date< (date 2026 1 1) (date 2026 1 2)))
    (ok (date< (date 2025 12 31) (date 2026 1 1)))
    (ok (date<= (date 2026 1 1) (date 2026 1 1)))
    (ok (date> (date 2026 2 1) (date 2026 1 31)))
    (ok (date>= (date 2026 1 1) (date 2026 1 1)))
    (ok (date= (date 2026 1 1) (date 2026 1 1))))
  (testing "variadic chains"
    (ok (date=))
    (ok (date= (date 2026 1 1)))
    (ok (date= (date 2026 1 1) (date 2026 1 1) (date 2026 1 1)))
    (ok (date< (date 2026 1 1) (date 2026 1 2) (date 2026 1 3)))
    (ok (not (date< (date 2026 1 1) (date 2026 1 3) (date 2026 1 2)))))
  (testing "inequality"
    (ok (date/= (date 2026 1 1) (date 2026 1 2) (date 2026 1 3)))
    (ok (not (date/= (date 2026 1 1) (date 2026 1 2) (date 2026 1 1))))))

(deftest test-weekdays
  (testing "next weekday is strictly after"
    (ok (equalp (next-weekday (date 2026 10 30) 1) (date 2026 11 2)))
    (ok (equalp (next-weekday (date 2026 10 30) 5) (date 2026 11 6)))
    (ok (equalp (next-weekday (date 2026 10 26) 1) (date 2026 11 2))))
  (testing "previous weekday is strictly before"
    (ok (equalp (previous-weekday (date 2026 10 30) 5) (date 2026 10 23)))
    (ok (equalp (previous-weekday (date 2026 10 30) 1) (date 2026 10 26)))
    (ok (equalp (previous-weekday (date 2026 11 2) 1) (date 2026 10 26))))
  (testing "weekday numbers outside 1-7 are rejected"
    (ok (signals (next-weekday (date 2026 10 30) *runtime-weekday*) 'type-error))
    (ok (signals (previous-weekday (date 2026 10 30) *runtime-weekday*) 'type-error))))

(deftest test-printing
  (ok (string= "#<DATE 2026-10-30>" (princ-to-string (date 2026 10 30))))
  (ok (string= "#<DATE 0001-01-01>" (princ-to-string (date 1 1 1)))))

(deftest test-today
  (let ((now (today)))
    (ok (date-p now))
    (ok (= 1 (- (rata-die (tomorrow)) (rata-die now))))
    (ok (= 1 (- (rata-die now) (rata-die (yesterday)))))))
