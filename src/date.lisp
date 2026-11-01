(uiop:define-package #:tempo.date
  (:use #:cl)
  (:import-from :tempo.conditions :invalid-date-error)
  (:export
   ;; Type, constructors, accessors
   #:date
   #:date-p
   #:make-date
   #:date-year
   #:date-month
   #:date-day
   #:destructure-date

   ;; Day numbers
   #:rata-die
   #:date-from-rata-die
   #:jdn
   #:date-from-jdn

   ;; Calendar queries
   #:valid-date-p
   #:leap-year-p
   #:days-in-month
   #:days-in-year
   #:day-of-week
   #:day-of-year
   #:iso-calendar

   ;; Comparisons
   #:date=
   #:date/=
   #:date<
   #:date<=
   #:date>
   #:date>=

   ;; Adjusters
   #:clamp-to-end-of-month
   #:replace-date
   #:start-of-month
   #:end-of-month
   #:start-of-year
   #:end-of-year
   #:start-of-week
   #:end-of-week
   #:next-weekday
   #:previous-weekday

   ;; Clock
   #:today
   #:yesterday
   #:tomorrow))

(in-package #:tempo.date)

(defstruct (date (:constructor %make-date (year month day)))
  (year 0 :type integer :read-only t)
  (month 1 :type (integer 1 12) :read-only t)
  (day  1 :type (integer 1 31) :read-only t))

(declaim (ftype (function (integer integer integer) (values date &optional))
                date)
         (ftype (function (&key (:year integer) (:month integer) (:day integer))
                          (values date &optional))
                make-date)
         (ftype (function (date &key (:year integer) (:month integer) (:day integer))
                          (values date &optional))
                replace-date)
         (ftype (function (t t t) (values boolean &optional))
                valid-date-p)
         (ftype (function (date)
                          (values integer (integer 1 12) (integer 1 31) &optional))
                destructure-date)
         (ftype (function (date) (values integer &optional))
                rata-die jdn)
         (ftype (function (integer) (values date &optional))
                date-from-rata-die date-from-jdn)
         (ftype (function ((or date integer)) (values boolean &optional))
                leap-year-p)
         (ftype (function ((or date integer) &optional (integer 1 12))
                          (values (integer 28 31) &optional))
                days-in-month)
         (ftype (function ((or date integer)) (values (member 365 366) &optional))
                days-in-year)
         (ftype (function (date) (values (integer 1 7) &optional))
                day-of-week)
         (ftype (function (date) (values (integer 1 366) &optional))
                day-of-year)
         (ftype (function (date)
                          (values integer (integer 1 53) (integer 1 7) &optional))
                iso-calendar)
         (ftype (function * (values boolean &optional))
                date= date/= date< date<= date> date>=)
         (ftype (function (date) (values date &optional))
                start-of-month end-of-month start-of-year end-of-year
                start-of-week end-of-week)
         (ftype (function (date (integer 1 7)) (values date &optional))
                next-weekday previous-weekday)
         (ftype (function () (values date &optional))
                today yesterday tomorrow))

(defun %leap-year-p (year)
  (and (zerop (mod year 4))
       (or (not (zerop (mod year 100)))
           (zerop (mod year 400)))))

(defun %days-in-month (year month)
  (if (and (= month 2) (%leap-year-p year))
      29
      (svref #(31 28 31 30 31 30 31 31 30 31 30 31) (1- month))))

(defun %month-name (month)
  (svref #("January" "February" "March" "April" "May" "June"
           "July" "August" "September" "October" "November" "December")
         (1- month)))

(defun valid-date-p (year month day)
  "True if YEAR, MONTH, and DAY name a real calendar date.

This predicate is total: anything that is not an integer simply yields
NIL instead of signalling."
  (and (integerp year)
       (integerp month)
       (integerp day)
       (<= 1 month 12)
       (<= 1 day (%days-in-month year month))))

(defun %invalid-date-reason (year month day)
  (cond ((< month 1) "month must be at least 1")
        ((> month 12) "month must be at most 12")
        ((< day 1)    "day must be at least 1")
        (t (format nil "~a ~d has ~d days, not ~d days"
                   (%month-name month) year (%days-in-month year month) day))))

(defun make-date (&key (year 0 yearp) (month 1) (day 1))
  "Create a date from YEAR, MONTH, and DAY.

YEAR is required; MONTH and DAY default to 1. Signals
INVALID-DATE-ERROR when the fields do not name a real calendar date,
and a TYPE-ERROR when any field is not an integer."
  (unless yearp
    (error "MAKE-DATE requires :YEAR."))
  (check-type year integer)
  (check-type month integer)
  (check-type day integer)
  (unless (valid-date-p year month day)
    (error 'invalid-date-error
           :year year :month month :day day
           :reason (%invalid-date-reason year month day)))
  (%make-date year month day))

(defun date (year month day)
  "Create a DATE from YEAR, MONTH, and DAY."
  (make-date :year year :month month :day day))

(defun destructure-date (date)
  "Return (values YEAR MONTH DAY) for DATE."
  (values (date-year date) (date-month date) (date-day date)))

;;; Standard representations

;; https://howardhinnant.github.io/date_algorithms.html#civil_from_days
(defun %days-from-unix-epoch (date)
  (multiple-value-bind (year month day) (destructure-date date)
    (let* ((y   (if (<= month 2) (1- year) year))
           (era (floor y 400))	      ; floor division — no -399 trick
           (yoe (- y (* era 400)))    ; [0, 399]
           (doy (+ (floor (+ (* 153 (if (> month 2)
                                        (- month 3)
                                        (+ month 9)))
                             2)
                          5)
                   (1- day)))		; [0, 365]
           (doe (+ (* yoe 365) (floor yoe 4) (- (floor yoe 100)) doy))) ; [0, 146096]
      (- (+ (* era 146097) doe) 719468))))

(defun %days-to-date (days)
  "Inverse of %DAYS-FROM-UNIX-EPOCH (inputs are days since 1970-01-01)."
  (let* ((z   (+ days 719468))
         (era (floor z 146097))                        ; floor division — no -146096 trick
         (doe (- z (* era 146097)))                    ; [0, 146096]
         (yoe (floor (+ doe (- (floor doe 1460))
                             (floor doe 36524)
                             (- (floor doe 146096)))
                     365))                             ; [0, 399]
         (y   (+ yoe (* era 400)))
         (doy (- doe (+ (* 365 yoe) (floor yoe 4) (- (floor yoe 100)))))  ; [0, 365]
         (mp  (floor (+ (* 5 doy) 2) 153))             ; [0, 11]
         (day (1+ (- doy (floor (+ (* 153 mp) 2) 5)))) ; [1, 31]
         (month (if (< mp 10) (+ mp 3) (- mp 9)))      ; [1, 12]
         (year  (+ y (if (<= month 2) 1 0))))          ; the Jan/Feb adjustment
    (make-date :year year :month month :day day)))

(defun rata-die (date)
  "The Rata Die day number of DATE, where 0001-01-01 is day 1."
  (+ (%days-from-unix-epoch date) 719163))

(defun date-from-rata-die (rata-die-days)
  (%days-to-date (- rata-die-days 719163)))

(defun jdn (date)
  "The Julian day number of DATE.

The Julian day begins at noon, so this is the JDN of DATE at noon."
  (+ (rata-die date) 1721425))

(defun date-from-jdn (n)
  "The civil date with Julian day number N."
  (date-from-rata-die (- n 1721425)))

;;; Query functions

(defun leap-year-p (date-or-year)
  "True if DATE-OR-YEAR is a leap year.

Accepts either a DATE or an integer year."
  (etypecase date-or-year
    (date (%leap-year-p (date-year date-or-year)))
    (integer (%leap-year-p date-or-year))))

(defun days-in-month (date-or-year &optional month)
  "Number of days in DATE's month, or in MONTH of the integer YEAR.

Examples:
  (days-in-month (date 2024 2 1))  ; => 29
  (days-in-month 2024 2)           ; => 29"
  (if month
      (progn
        (check-type date-or-year integer)
        (check-type month (integer 1 12))
        (%days-in-month date-or-year month))
      (etypecase date-or-year
        (date
         (%days-in-month (date-year date-or-year)
                         (date-month date-or-year)))
        (integer
         (error "DAYS-IN-MONTH requires MONTH when DATE-OR-YEAR is a year.")))))

(defun days-in-year (date-or-year)
  "Number of days in DATE-OR-YEAR.

Accepts either a DATE or an integer year."
  (if (leap-year-p date-or-year) 366 365))

(defun day-of-week (date)
  "The weekday of DATE as a number from 1 (Monday) through 7 (Sunday)."
  (1+ (mod (1- (rata-die date)) 7)))

(defun day-of-year (date)
  "The ordinal day of DATE within its year, from 1 to 365 or 366."
  (loop :for m from 1 upto (1- (date-month date))
	:sum (%days-in-month (date-year date) m) into total
	:finally (return (+ total (date-day date)))))

(defun iso-calendar (date)
  "Return (values ISO-YEAR ISO-WEEK ISO-WEEKDAY) for DATE."
  (let* ((weekday  (day-of-week date))            ; 1..7, Monday=1
         (thursday (+ (rata-die date) (- 4 weekday)))    ; Thursday of DATE's ISO week
         (iso-year (date-year (date-from-rata-die thursday)))
         (jan-1    (rata-die (date iso-year 1 1)))
         (week     (1+ (floor (- thursday jan-1) 7))))
    (values iso-year week weekday)))

;;; Comparisons

(defmacro define-date-comparison (name operator docstring)
  `(defun ,name (&rest dates)
     ,docstring
     (declare (dynamic-extent dates))
     (loop :for a :in dates
           :for b :in (cdr dates)
           :always (,operator (rata-die a) (rata-die b)))))

(define-date-comparison date= = "True if all DATES name the same day.")

(define-date-comparison date< < "True if DATES are in strictly increasing day order.")

(define-date-comparison date<= <= "True if DATES are in nondecreasing day order.")

(define-date-comparison date> > "True if DATES are in strictly decreasing day order.")

(define-date-comparison date>= >= "True if DATES are in nonincreasing day order.")

(defun date/= (&rest dates)
  "True if no two of DATES name the same day."
  (declare (dynamic-extent dates))
  (loop :for (d . rest) :on dates
        :always (loop :for other :in rest
                      :never (= (rata-die d) (rata-die other)))))

;;; Adjusters

(defun replace-date (date &key (year  (date-year date))
                            (month (date-month date))
                            (day   (date-day date)))
  "Return a copy of DATE with any of YEAR, MONTH, or DAY replaced.

Signals INVALID-DATE-ERROR if the result is not a real date. When DAY
overflows the target month, the CLAMP-TO-END-OF-MONTH restart is
available."
  (check-type year integer)
  (check-type month integer)
  (check-type day integer)
  (flet ((fail ()
           (error 'invalid-date-error
                  :year year :month month :day day
                  :reason (%invalid-date-reason year month day))))
    (unless (valid-date-p year month day)
      (if (and (<= 1 month 12) (> day (%days-in-month year month)))
          (restart-case (fail)
            (clamp-to-end-of-month ()
              :report "Use the last day of the target month."
              (setf day (%days-in-month year month))))
          (fail))))
  (%make-date year month day))

(defun start-of-month (date)
  (replace-date date :day 1))

(defun end-of-month (date)
  (replace-date date :day (days-in-month date)))

(defun start-of-year (date)
  (replace-date date :month 1 :day 1))

(defun end-of-year (date)
  (replace-date date :month 12 :day 31))

(defun start-of-week (date)
  (date-from-rata-die (- (rata-die date) (1- (day-of-week date)))))

(defun end-of-week (date)
  (date-from-rata-die (+ (rata-die date) (- 7 (day-of-week date)))))

(defun next-weekday (date weekday)
  "The first date strictly after DATE whose weekday is WEEKDAY.

WEEKDAY is a number from 1 (Monday) through 7 (Sunday)."
  (check-type weekday (integer 1 7))
  (let ((delta (mod (- weekday (day-of-week date)) 7)))
    (date-from-rata-die (+ (rata-die date) (if (zerop delta) 7 delta)))))

(defun previous-weekday (date weekday)
  "The last date strictly before DATE whose weekday is WEEKDAY.

WEEKDAY is a number from 1 (Monday) through 7 (Sunday)."
  (check-type weekday (integer 1 7))
  (let ((delta (mod (- (day-of-week date) weekday) 7)))
    (date-from-rata-die (- (rata-die date) (if (zerop delta) 7 delta)))))

;;; Clock
(defun today ()
  (multiple-value-bind (second minute hour day month year)
      (get-decoded-time)
    (declare (ignore second minute hour))
    (date year month day)))

(defun yesterday ()
  (date-from-rata-die (1- (rata-die (today)))))

(defun tomorrow ()
  (date-from-rata-die (1+ (rata-die (today)))))

;;; Printing
(defmethod print-object ((object date) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~4,'0d-~2,'0d-~2,'0d"
            (date-year object)
            (date-month object)
            (date-day object))))
