;;;; datetime.lisp --- naive local date and time
;;;;
;;;; A DATETIME composes a DATE with a WALL-TIME: a naive local date
;;;; and time with no time zone. The components are stored directly,
;;;; so DATETIME-DATE and DATETIME-WALL-TIME return the shared
;;;; components themselves and DATETIME+/- / DATETIME-DIFFERENCE work
;;;; on exact rational seconds.

(uiop:define-package #:tempo.datetime
  (:use #:cl)
  (:import-from #:tempo.conditions
                #:inexact-duration-error)
  (:import-from #:tempo.date
                #:date
                #:make-date
                #:valid-date-p
                #:date-year
                #:date-month
                #:date-day
                #:destructure-date
                #:replace-date
                #:rata-die
                #:date-from-rata-die
                #:write-date-to-iso
                #:%compare-date)
  (:import-from #:tempo.wall-time
                #:wall-time
                #:make-wall-time
                #:valid-wall-time-p
                #:wall-time-hour
                #:wall-time-minute
                #:wall-time-second
                #:destructure-wall-time
                #:write-wall-time-to-iso
                #:%seconds-since-midnight
                #:%make-wall-time)
  (:import-from #:tempo.duration
                #:duration
                #:make-duration
                #:%exact-seconds)
  (:export
   ;; Type, constructors, accessors
   #:datetime
   #:datetime-p
   #:make-datetime
   #:datetime-from-parts
   #:datetime-date
   #:datetime-wall-time
   #:datetime-year
   #:datetime-month
   #:datetime-day
   #:datetime-hour
   #:datetime-minute
   #:datetime-second
   #:valid-datetime-p
   #:destructure-datetime

   ;; Comparisons
   #:datetime=
   #:datetime/=
   #:datetime<
   #:datetime<=
   #:datetime>
   #:datetime>=

   ;; Difference & arithmetic
   #:datetime-difference
   #:datetime+
   #:datetime-

   ;; Adjusters
   #:replace-datetime
   #:start-of-day

   ;; Formatting
   #:write-datetime-to-iso
   #:format-datetime-iso
   #:format-datetime))

(in-package #:tempo.datetime)

(defstruct (datetime (:constructor %make-datetime (date wall-time)))
  (date      nil :type date      :read-only t)
  (wall-time nil :type wall-time :read-only t))

(defmethod make-load-form ((self datetime) &optional environment)
  (make-load-form-saving-slots self :environment environment))

(declaim (inline datetime-year datetime-month datetime-day
                 datetime-hour datetime-minute datetime-second))

(declaim (ftype (function (&key (:year integer) (:month integer) (:day integer)
                                (:hour integer) (:minute integer) (:second rational))
                          (values datetime &optional))
                make-datetime)
         (ftype (function (integer integer integer &optional real real real)
                          (values datetime &optional))
                datetime)
         (ftype (function (date wall-time) (values datetime &optional))
                datetime-from-parts)
         (ftype (function (t t t t t t) (values boolean &optional))
                valid-datetime-p)
         (ftype (function (datetime)
                          (values integer (integer 1 12) (integer 1 31)
                                  (integer 0 23) (integer 0 59) (rational 0 (60))
                                  &optional))
                destructure-datetime)
         (ftype (function (datetime datetime) (values (integer -1 1) &optional))
                %compare-datetime)
         (ftype (function (datetime) (values rational &optional))
                %datetime-total-seconds)
         (ftype (function (datetime datetime) (values duration &optional))
                datetime-difference)
         (ftype (function (datetime rational) (values datetime &optional))
                %datetime-advance)
         (ftype (function (datetime duration) (values datetime &optional))
                datetime+ datetime-)
         (ftype (function (datetime
                           &key (:year integer) (:month integer) (:day integer)
                                (:hour integer) (:minute integer) (:second real))
                          (values datetime &optional))
                replace-datetime)
         (ftype (function (datetime) (values datetime &optional))
                start-of-day))

;;; Validation

(defun valid-datetime-p (year month day hour minute second)
  "True if the six fields name a real naive date and time.

This predicate is total: anything that is not a valid calendar or wall
clock field value simply yields NIL instead of signalling."
  (and (valid-date-p year month day)
       (valid-wall-time-p hour minute second)))

;;; Constructors

(defun make-datetime (&key (year 0 yearp) (month 1) (day 1)
                           (hour 0) (minute 0) (second 0))
  "Create a datetime from YEAR (required), MONTH, DAY, HOUR, MINUTE,
and SECOND.

MONTH and DAY default to 1; HOUR, MINUTE, and SECOND default to 0.
SECOND must be a rational. Errors from the component constructors
propagate: INVALID-DATE-ERROR and INVALID-TIME-ERROR for impossible
field values, TYPE-ERROR for non-numeric ones."
  (unless yearp
    (error "MAKE-DATETIME requires :YEAR."))
  (%make-datetime (make-date :year year :month month :day day)
                  (make-wall-time :hour hour :minute minute :second second)))

(defun datetime (year month day &optional (hour 0) (minute 0) (second 0))
  "Create a DATETIME from its components.

YEAR, MONTH, and DAY are required; HOUR and MINUTE default to 0, and
SECOND may be any real; it is rationalized before conversion."
  (make-datetime :year year :month month :day day :hour hour :minute minute
                 :second (rationalize second)))

(defun datetime-from-parts (date wall-time)
  "Compose a DATETIME from an existing DATE and WALL-TIME.

The components are stored as-is: values are immutable, so sharing them
is safe, and this constructor performs a single allocation."
  (check-type date date)
  (check-type wall-time wall-time)
  (%make-datetime date wall-time))

(defparameter *midnight* (make-wall-time)
  "Shared WALL-TIME of 00:00:00; values are immutable, so sharing is
safe between datetimes.")

;;; Accessors

(defun datetime-year (datetime)
  "The YEAR component of DATETIME."
  (date-year (datetime-date datetime)))

(defun datetime-month (datetime)
  "The MONTH component of DATETIME."
  (date-month (datetime-date datetime)))

(defun datetime-day (datetime)
  "The DAY component of DATETIME."
  (date-day (datetime-date datetime)))

(defun datetime-hour (datetime)
  "The HOUR component of DATETIME."
  (wall-time-hour (datetime-wall-time datetime)))

(defun datetime-minute (datetime)
  "The MINUTE component of DATETIME."
  (wall-time-minute (datetime-wall-time datetime)))

(defun datetime-second (datetime)
  "The SECOND component of DATETIME, an exact rational below 60."
  (wall-time-second (datetime-wall-time datetime)))

(defun destructure-datetime (datetime)
  "Return (values YEAR MONTH DAY HOUR MINUTE SECOND) for DATETIME."
  (multiple-value-bind (year month day)
      (destructure-date (datetime-date datetime))
    (multiple-value-bind (hour minute second)
        (destructure-wall-time (datetime-wall-time datetime))
      (values year month day hour minute second))))

;;; Comparisons

(defun %compare-datetime (datetime-a datetime-b)
  "Lexicographically compare DATETIME-A and DATETIME-B: date fields
first, then wall clock fields. Returns -1 if DATETIME-A is earlier, 1
if it is later, and 0 if they are the same."
  (let ((r (%compare-date (datetime-date datetime-a)
                          (datetime-date datetime-b))))
    (if (/= r 0)
        r
        (let ((r (- (%seconds-since-midnight (datetime-wall-time datetime-a))
                    (%seconds-since-midnight (datetime-wall-time datetime-b)))))
          (cond ((< r 0) -1)
                ((> r 0) 1)
                (t 0))))))

(defmacro define-datetime-comparison (name result-comparison docstring)
  "Define a variadic DATETIME comparison function NAME.

RESULT-COMPARISON is an expression over the variable CMP, bound to the
value of %COMPARE-DATETIME for each adjacent pair of arguments."
  `(progn
     (declaim (ftype (function * (values boolean &optional)) ,name))
     (defun ,name (datetime &rest datetimes)
       ,docstring
       (declare (dynamic-extent datetimes))
       (loop :for a = datetime :then b
             :for b :in datetimes
             :for cmp = (%compare-datetime a b)
             :always ,result-comparison))))

(define-datetime-comparison datetime= (= cmp 0)
  "True if all DATETIMES are the same date and time.")

(define-datetime-comparison datetime< (< cmp 0)
  "True if DATETIMES are in strictly increasing order.")

(define-datetime-comparison datetime<= (<= cmp 0)
  "True if DATETIMES are in nondecreasing order.")

(define-datetime-comparison datetime> (> cmp 0)
  "True if DATETIMES are in strictly decreasing order.")

(define-datetime-comparison datetime>= (>= cmp 0)
  "True if DATETIMES are in nonincreasing order.")

(defun datetime/= (datetime &rest datetimes)
  "True if no two of DATETIME and DATETIMES are the same."
  (declare (dynamic-extent datetimes))
  (let ((all (cons datetime datetimes)))
    (declare (dynamic-extent all))
    (loop :for (dt . rest) :on all
          :always (loop :for other :in rest
                        :never (zerop (%compare-datetime dt other))))))

;;; Difference and arithmetic

(defun %datetime-total-seconds (datetime)
  "DATETIME as an exact number of seconds in the lexicographic epoch:
an exact integral day count multiplied by 86400 plus the wall clock
position of the day."
  (+ (* (rata-die (datetime-date datetime)) 86400)
     (%seconds-since-midnight (datetime-wall-time datetime))))

(defun datetime-difference (a b)
  "Return the exact DURATION from B to A: the value that, added to B,
yields A.

The result is exact elapsed time; its months component is always zero.
A calendar decomposition (months, periods) needs a policy and arrives
with the Phase 3 month arithmetic."
  (make-duration :seconds (- (%datetime-total-seconds a)
                              (%datetime-total-seconds b))))

(defun %datetime-advance (datetime seconds)
  "DATETIME advanced by an exact (possibly fractional or negative)
number of SECONDS."
  (let ((total (+ (%datetime-total-seconds datetime) seconds)))
    (multiple-value-bind (day seconds-of-day) (floor total 86400)
      (multiple-value-bind (hour remainder) (truncate seconds-of-day 3600)
        (multiple-value-bind (minute second) (truncate remainder 60)
          (%make-datetime (date-from-rata-die day)
                          (%make-wall-time hour minute second)))))))

(defun datetime+ (datetime duration)
  "Return DATETIME advanced by DURATION.

Only the exact seconds component is used: a duration with non-zero
months has no fixed offset in elapsed time and signals
INEXACT-DURATION-ERROR. Calendar month arithmetic arrives with the
Phase 3 adders."
  (%datetime-advance datetime (%exact-seconds duration)))

(defun datetime- (datetime duration)
  "Return DATETIME adjusted backward by DURATION.

Like DATETIME+, only the exact seconds component is used; months
signal INEXACT-DURATION-ERROR."
  (%datetime-advance datetime (- (%exact-seconds duration))))

;;; Adjusters

(defun replace-datetime (datetime
                         &key (year    (datetime-year datetime))
                              (month   (datetime-month datetime))
                              (day     (datetime-day datetime))
                              (hour    (datetime-hour datetime))
                              (minute  (datetime-minute datetime))
                              (second  (datetime-second datetime)))
  "Return a copy of DATETIME with any of its six fields replaced.

The date component is rebuilt by REPLACE-DATE, so the
CLAMP-TO-END-OF-MONTH restart is available when the day would overflow
the target month; the wall clock component by MAKE-WALL-TIME. SECOND
is rationalized, so floats are accepted."
  (%make-datetime (replace-date (datetime-date datetime)
                                :year year :month month :day day)
                  (make-wall-time :hour hour :minute minute
                                  :second (rationalize second))))

(defun start-of-day (datetime)
  "Return midnight of DATETIME's date."
  (%make-datetime (datetime-date datetime) *midnight*))

;;; Formatting

(defun write-datetime-to-iso (datetime stream)
  "Write DATETIME to STREAM as an ISO-8601 datetime, for example
2026-10-30T09:30:00; a fractional second is written as a decimal
fraction following the seconds, e.g. 2026-10-30T09:30:00.5."
  (write-date-to-iso (datetime-date datetime) stream)
  (write-char #\T stream)
  (write-wall-time-to-iso (datetime-wall-time datetime) stream))

(defun format-datetime-iso (datetime)
  "Return DATETIME formatted as an ISO-8601 datetime string."
  (with-output-to-string (s)
    (write-datetime-to-iso datetime s)))

(defun format-datetime (stream datetime &optional colon-p at-sign-p)
  "Write DATETIME to STREAM in ISO-8601 format.

This is the CL:FORMAT directive function ~/tempo:format-datetime/.
COLON-P and AT-SIGN-P are accepted for CL:FORMAT and ignored."
  (declare (ignore colon-p at-sign-p))
  (write-datetime-to-iso datetime stream))

(defmethod print-object ((object datetime) stream)
  (print-unreadable-object (object stream :type t)
    (write-datetime-to-iso object stream)))