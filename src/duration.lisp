;;;; duration.lisp --- Simple durations for arithmetic
;;;;
;;;; Commentary:
;;;;
;;;; A DURATION is a span of time represented in an exact rational
;;;; number of seconds. All input to the constructors are
;;;; rationalized, so any number is accepted. Since a DURATION is
;;;; immutable, all the functions here return new values. DURATION is
;;;; independent of any timeline; for durations of time represented by
;;;; start and end of two points in time, see TIME-INTERVAL.
;;;;
;;;; Code:

(uiop:define-package #:tempo.duration
  (:use #:cl)
  (:import-from #:tempo.conditions #:inexact-duration-error)
  (:export
   ;; Type, constructors, and accessors
   #:duration
   #:duration-p
   #:make-duration
   #:duration-months
   #:duration-seconds

   ;; Unit constructors
   #:years
   #:months
   #:weeks
   #:days
   #:hours
   #:minutes
   #:seconds
   #:milliseconds
   #:microseconds
   #:nanoseconds

   ;; Conversions & Extraction
   #:in-years
   #:in-months
   #:in-days
   #:in-hours
   #:in-minutes
   #:in-seconds
   #:in-milliseconds
   #:in-microseconds
   #:in-nanoseconds
   #:destructure-duration

   ;; Arithmetic
   #:duration+
   #:duration-
   #:duration*
   #:duration/
   #:duration-divide
   #:duration-floor
   #:duration-ceiling
   #:duration-round
   #:duration-abs

   ;; Predicates & Comparison
   #:duration-exact-p
   #:duration-zerop
   #:duration-plusp
   #:duration-minusp
   #:duration=
   #:duration/=
   #:duration<
   #:duration<=
   #:duration>
   #:duration>=

   ;; Formatting
   #:format-duration-iso
   #:write-duration-to-iso
   #:format-duration))

(in-package #:tempo.duration)

(defstruct duration
  (months  0 :type integer  :read-only t)
  (seconds 0 :type rational :read-only t))

(defmethod make-load-form ((self duration) &optional environment)
  (make-load-form-saving-slots self :environment environment))

(declaim (inline seconds minutes hours days weeks months
                 milliseconds microseconds nanoseconds
                 duration-exact-p duration-zerop))

(declaim (ftype (function (real) (values duration &optional))
                years weeks days hours minutes seconds
                milliseconds microseconds nanoseconds)
         (ftype (function (integer) (values duration &optional))
                months)
         (ftype (function (&key (:years real) (:months integer)
                                (:weeks real) (:days real)
                                (:hours real) (:minutes real) (:seconds real)
                                (:milliseconds real) (:microseconds real)
                                (:nanoseconds real))
                          (values duration &optional))
                duration)
         (ftype (function (duration) (values rational &optional))
                in-years in-days in-hours in-minutes in-seconds
                in-milliseconds in-microseconds in-nanoseconds)
         (ftype (function (duration) (values integer &optional))
                in-months)
         (ftype (function (duration)
                          (values integer integer integer integer integer rational &optional))
                destructure-duration)
         (ftype (function * (values duration &optional))
                duration+ duration-)
         (ftype (function ((or duration real) (or duration real))
                          (values duration &optional))
                duration*)
         (ftype (function (duration (or real duration))
                          (values (or duration rational) &optional))
                duration/)
         (ftype (function (duration &optional duration)
                          (values duration &optional))
                duration-floor duration-ceiling duration-round)
         (ftype (function (duration (or real duration))
                          (values integer duration &optional))
                duration-divide)
         (ftype (function (duration) (values duration &optional))
                duration-abs)
         (ftype (function (duration) (values boolean &optional))
                duration-exact-p duration-zerop duration-plusp duration-minusp)
         (ftype (function * (values boolean &optional))
                duration= duration/= duration< duration<= duration> duration>=))

;;; Unit constructors

(defun years (n)
  "Return a duration of N years (12 * N months).

N may be any real number whose rationalized product with 12 is an
integer (for example, 1, 1/2, or 1.5). Signals INEXACT-DURATION-ERROR
if 12 * N is not a whole number of months."
  (check-type n real)
  (let ((total-months (* (rationalize n) 12)))
    (unless (integerp total-months)
      (error 'inexact-duration-error
             :duration nil
             :reason "fractional years must resolve to a whole number of months"))
    (make-duration :months total-months)))

(defun months (n)
  "Return a duration of N calendar months.

N must be an integer."
  (check-type n integer)
  (make-duration :months n))

(defun days (n)
  "Return a duration of N days.

N may be any real number; it is rationalized before conversion."
  (make-duration :seconds (rationalize (* n 86400))))

(defun weeks (n)
  "Return a duration of N weeks.

N may be any real number; it is rationalized before conversion."
  (days (* n 7)))

(defun hours (n)
  "Return a duration of N hours.

N may be any real number; it is rationalized before conversion."
  (make-duration :seconds (rationalize (* n 3600))))

(defun minutes (n)
  "Return a duration of N minutes.

N may be any real number; it is rationalized before conversion."
  (make-duration :seconds (rationalize (* n 60))))

(defun seconds (n)
  "Return a duration of N seconds.

N may be any real number; it is rationalized before conversion."
  (make-duration :seconds (rationalize n)))

(defun milliseconds (n)
  "Return a duration of N milliseconds.

N may be any real number; it is rationalized before conversion."
  (make-duration :seconds (rationalize (/ n 1000))))

(defun microseconds (n)
  "Return a duration of N microseconds.

N may be any real number; it is rationalized before conversion."
  (make-duration :seconds (rationalize (/ n 1000000))))

(defun nanoseconds (n)
  "Return a duration of N nanoseconds.

N may be any real number; it is rationalized before conversion."
  (make-duration :seconds (rationalize (/ n 1000000000))))

(defun duration (&key (years 0) (months 0) (weeks 0) (days 0)
                      (hours 0) (minutes 0) (seconds 0)
                      (milliseconds 0) (microseconds 0) (nanoseconds 0))
  "Return a DURATION combining any of the supplied calendar and time units."
  (check-type months integer)
  (let ((year-months (duration-months (years years))))
    (make-duration
     :months (+ year-months months)
     :seconds (+ (* (rationalize weeks) 604800)
                 (* (rationalize days) 86400)
                 (* (rationalize hours) 3600)
                 (* (rationalize minutes) 60)
                 (rationalize seconds)
                 (rationalize (/ milliseconds 1000))
                 (rationalize (/ microseconds 1000000))
                 (rationalize (/ nanoseconds 1000000000))))))

;; Converting to other units

(defun %exact-seconds (d)
  (if (zerop (duration-months d))
      (duration-seconds d)
      (error 'inexact-duration-error
             :duration d
             :reason "calendar months do not have a fixed number of seconds")))

(defun in-years (d)
  "Return the length of D expressed in years as an exact rational.

Signals INEXACT-DURATION-ERROR if D has a non-zero seconds component."
  (unless (zerop (duration-seconds d))
    (error 'inexact-duration-error
           :duration d
           :reason "exact seconds cannot be converted to calendar years"))
  (/ (duration-months d) 12))

(defun in-months (d)
  "Return the length of D expressed in calendar months as an integer.

Signals INEXACT-DURATION-ERROR if D has a non-zero seconds component."
  (unless (zerop (duration-seconds d))
    (error 'inexact-duration-error
           :duration d
           :reason "exact seconds cannot be converted to calendar months"))
  (duration-months d))

(defun in-days (d)
  "Return the length of D expressed in days as an exact rational.

Signals INEXACT-DURATION-ERROR if D has a non-zero months component."
  (/ (%exact-seconds d) 86400))

(defun in-hours (d)
  "Return the length of D expressed in hours as an exact rational.

Signals INEXACT-DURATION-ERROR if D has a non-zero months component."
  (/ (%exact-seconds d) 3600))

(defun in-minutes (d)
  "Return the length of D expressed in minutes as an exact rational.

Signals INEXACT-DURATION-ERROR if D has a non-zero months component."
  (/ (%exact-seconds d) 60))

(defun in-seconds (d)
  "Return the length of D in seconds as an exact rational.

Signals INEXACT-DURATION-ERROR if D has a non-zero months component."
  (%exact-seconds d))

(defun in-milliseconds (d)
  "Return the length of D expressed in milliseconds as an exact rational.

Signals INEXACT-DURATION-ERROR if D has a non-zero months component."
  (* (%exact-seconds d) 1000))

(defun in-microseconds (d)
  "Return the length of D expressed in microseconds as an exact rational.

Signals INEXACT-DURATION-ERROR if D has a non-zero months component."
  (* (%exact-seconds d) 1000000))

(defun in-nanoseconds (d)
  "Return the length of D expressed in nanoseconds as an exact rational.

Signals INEXACT-DURATION-ERROR if D has a non-zero months component."
  (* (%exact-seconds d) 1000000000))

(defun destructure-duration (duration)
  "Return (values YEARS MONTHS DAYS HOURS MINUTES SECONDS) for DURATION.

YEARS, MONTHS, DAYS, HOURS, and MINUTES are integers truncated toward
zero and SECONDS is the exact rational remainder. Components of a
negative duration carry the sign of their underlying slot."
  (multiple-value-bind (years months)
      (truncate (duration-months duration) 12)
    (multiple-value-bind (days rem-days)
        (truncate (duration-seconds duration) 86400)
      (multiple-value-bind (hours rem-hours)
          (truncate rem-days 3600)
        (multiple-value-bind (minutes seconds)
            (truncate rem-hours 60)
          (values years months days hours minutes seconds))))))

;;; Arithmetic

(defun duration+ (&rest durations)
  "Return the sum of DURATIONS.

With no arguments, return a zero duration."
  (declare (dynamic-extent durations))
  (if (null durations)
      (seconds 0)
      (loop :for d :in durations
            :sum (duration-months d) :into total-months
            :sum (duration-seconds d) :into total-seconds
            :finally (return (make-duration :months total-months
                                            :seconds total-seconds)))))

(defun duration- (duration &rest durations)
  "Return DURATION minus the remaining DURATIONS.

With no remaining arguments, return the negation of DURATION."
  (declare (dynamic-extent durations))
  (if (null durations)
      (make-duration :months (- (duration-months duration))
                     :seconds (- (duration-seconds duration)))
      (loop :for d :in durations
            :sum (- (duration-months d)) :into diff-months
            :sum (- (duration-seconds d)) :into diff-seconds
            :finally (return (make-duration :months (+ (duration-months duration)
                                                       diff-months)
                                            :seconds (+ (duration-seconds duration)
                                                        diff-seconds))))))

(defun %scale-duration (duration factor)
  (let* ((r (rationalize factor))
         (scaled-months (* (duration-months duration) r)))
    (unless (integerp scaled-months)
      (error 'inexact-duration-error
             :duration duration
             :reason "scaling calendar months did not produce a whole number of months"))
    (make-duration :months scaled-months
                   :seconds (* (duration-seconds duration) r))))

(defun duration* (a b)
  "Return the product of A and B.

One of A and B must be a duration and the other a real number."
  (etypecase a
    (duration (etypecase b
                (real (%scale-duration a b))))
    (real (etypecase b
            (duration (%scale-duration b a))))))

(defun duration/ (duration divisor)
  "Return DURATION divided by DIVISOR.

DIVISOR is either a real scalar, in which case the result is a
duration, or another duration, in which case the result is an exact
ratio. Signals INEXACT-DURATION-ERROR when calendar months cannot be
divided exactly. See also DURATION-DIVIDE."
  (etypecase divisor
    (real
     (%scale-duration duration (/ 1 (rationalize divisor))))
    (duration
     (cond ((and (zerop (duration-months duration))
                 (zerop (duration-months divisor)))
            (/ (duration-seconds duration) (duration-seconds divisor)))
           ((and (zerop (duration-seconds duration))
                 (zerop (duration-seconds divisor)))
            (/ (duration-months duration) (duration-months divisor)))
           (t
            (error 'inexact-duration-error
                   :duration duration
                   :reason "cannot compute a ratio between incompatible calendar and exact durations"))))))

(defun duration-floor (duration &optional (resolution (seconds 1)))
  "Return DURATION snapped down to the nearest multiple of RESOLUTION.

RESOLUTION defaults to one second. Signals INEXACT-DURATION-ERROR if
either DURATION or RESOLUTION has calendar months."
  (let ((sec (%exact-seconds duration))
        (res (%exact-seconds resolution)))
    (make-duration :seconds (* (floor sec res) res))))

(defun duration-ceiling (duration &optional (resolution (seconds 1)))
  "Return DURATION snapped up to the nearest multiple of RESOLUTION.

RESOLUTION defaults to one second. Signals INEXACT-DURATION-ERROR if
either DURATION or RESOLUTION has calendar months."
  (let ((sec (%exact-seconds duration))
        (res (%exact-seconds resolution)))
    (make-duration :seconds (* (ceiling sec res) res))))

(defun duration-round (duration &optional (resolution (seconds 1)))
  "Return DURATION snapped to the nearest multiple of RESOLUTION.

Ties are rounded to even. RESOLUTION defaults to one second. Signals
INEXACT-DURATION-ERROR if either DURATION or RESOLUTION has calendar
months."
  (let ((sec (%exact-seconds duration))
        (res (%exact-seconds resolution)))
    (make-duration :seconds (* (round sec res) res))))

(defun duration-divide (duration divisor)
  "Return (values QUOTIENT REMAINDER) dividing DURATION by DIVISOR.

DIVISOR is a real number or a duration. The division truncates toward
zero; QUOTIENT is an integer and REMAINDER is a duration. See also
DURATION/ for scaling or representing a ratio."
  (if (and (duration-p divisor)
           (zerop (duration-seconds duration))
           (zerop (duration-seconds divisor))
           (not (zerop (duration-months divisor))))
      (multiple-value-bind (quotient remainder)
          (truncate (duration-months duration) (duration-months divisor))
        (values quotient (make-duration :months remainder)))
      (let ((sec (%exact-seconds duration))
            (div (if (duration-p divisor)
                     (%exact-seconds divisor)
                     (rationalize divisor))))
        (multiple-value-bind (quotient remainder)
            (truncate sec div)
          (values quotient (make-duration :seconds remainder))))))

(defun %duration-equal-p (a b)
  (and (= (duration-months a) (duration-months b))
       (= (duration-seconds a) (duration-seconds b))))

(defun %compare-durations (operator a b)
  (let ((m1 (duration-months a))
        (s1 (duration-seconds a))
        (m2 (duration-months b))
        (s2 (duration-seconds b)))
    (cond ((= m1 m2)
           (funcall operator s1 s2))
          ((and (zerop s1) (zerop s2))
           (funcall operator m1 m2))
          (t
           (error 'inexact-duration-error
                  :duration a
                  :reason "cannot order durations that mix different calendar months and exact seconds")))))

(defun duration= (duration &rest durations)
  "Return true if all DURATIONS have equal months and seconds components.

A single argument is always equal to itself."
  (declare (dynamic-extent durations))
  (if (null durations)
      t
      (loop :for d :in durations
            :always (%duration-equal-p duration d))))

(defmacro define-comparison-operator (name operator docstring)
  "Define a variadic duration ordering function NAME using OPERATOR.

DOCSTRING becomes the generated function's documentation string."
  `(progn
     (declaim (ftype (function * (values boolean &optional)) ,name))
     (defun ,name (duration &rest durations)
       ,docstring
       (declare (dynamic-extent durations))
       (if (null durations)
           t
           (loop :for i :in durations
                 :and prev-i = duration :then i
                 :always (%compare-durations #',operator prev-i i))))))

(define-comparison-operator duration< <
  "Return true if DURATIONS are in strictly increasing order.")

(define-comparison-operator duration<= <=
  "Return true if DURATIONS are in nondecreasing order.")

(define-comparison-operator duration> >
  "Return true if DURATIONS are in strictly decreasing order.")

(define-comparison-operator duration>= >=
  "Return true if DURATIONS are in nonincreasing order.")

(defun duration/= (duration &rest durations)
  "Return true if no two of DURATION and DURATIONS are equal."
  (declare (dynamic-extent durations))
  (let ((all (cons duration durations)))
    (declare (dynamic-extent all))
    (loop :for (d . rest) :on all
          :always (loop :for other :in rest
                        :never (%duration-equal-p d other)))))

(defun duration-exact-p (duration)
  "Return true if DURATION has no calendar month component."
  (zerop (duration-months duration)))

(defun duration-zerop (duration)
  "Return true if DURATION has zero months and zero seconds."
  (and (zerop (duration-months duration))
       (zerop (duration-seconds duration))))

(defun duration-abs (duration)
  "Return the absolute value of DURATION.

Signals INEXACT-DURATION-ERROR if DURATION has months and seconds of
opposite signs."
  (cond ((duration-zerop duration) duration)
        ((duration-minusp duration) (duration- duration))
        (t duration)))

(defun duration-plusp (duration)
  "Return true if DURATION is positive.

Signals INEXACT-DURATION-ERROR if DURATION has months and seconds of
opposite signs."
  (let ((m (duration-months duration))
        (s (duration-seconds duration)))
    (cond ((zerop m) (plusp s))
          ((zerop s) (plusp m))
          ((and (plusp m) (plusp s)) t)
          ((and (minusp m) (minusp s)) nil)
          (t (error 'inexact-duration-error
                    :duration duration
                    :reason "months and seconds have opposite signs")))))

(defun duration-minusp (duration)
  "Return true if DURATION is negative.

Signals INEXACT-DURATION-ERROR if DURATION has months and seconds of
opposite signs."
  (let ((m (duration-months duration))
        (s (duration-seconds duration)))
    (cond ((zerop m) (minusp s))
          ((zerop s) (minusp m))
          ((and (minusp m) (minusp s)) t)
          ((and (plusp m) (plusp s)) nil)
          (t (error 'inexact-duration-error
                    :duration duration
                    :reason "months and seconds have opposite signs")))))

;;; Formatting utils

(defun write-duration-to-iso (duration stream)
  "Write DURATION to STREAM in ISO-8601 format.

A zero duration is written as \"PT0S\"; negative durations are
prefixed with a minus sign."
  (cond ((duration-zerop duration)
         (write-string "PT0S" stream))
        ((and (<= (duration-months duration) 0)
              (<= (duration-seconds duration) 0))
         (write-char #\- stream)
         (write-duration-to-iso (duration- duration) stream))
        (t
         (multiple-value-bind (years months days hours minutes seconds)
             (destructure-duration duration)
           (format stream "P~@[~dY~]~@[~dM~]~@[~dD~]~:[~;T~]~@[~dH~]~@[~dM~]"
                   (and (not (zerop years)) years)
                   (and (not (zerop months)) months)
                   (and (not (zerop days)) days)
                   (or (not (zerop hours))
                       (not (zerop minutes))
                       (not (zerop seconds)))
                   (and (not (zerop hours)) hours)
                   (and (not (zerop minutes)) minutes))
           (unless (zerop seconds)
             (if (integerp seconds)
                 (format stream "~dS" seconds)
                 (format stream "~FS" (float seconds 1.0d0))))))))

(defun format-duration-iso (duration)
  "Return DURATION formatted as an ISO-8601 string."
  (with-output-to-string (s)
    (write-duration-to-iso duration s)))

(defun format-duration (stream duration &optional colon-p at-sign-p)
  "Write DURATION to STREAM in ISO-8601 format.

This is the CL:FORMAT directive function ~/tempo:format-duration/.
COLON-P and AT-SIGN-P are accepted for CL:FORMAT and ignored."
  (declare (ignore colon-p at-sign-p))
  (write-duration-to-iso duration stream))

(defmethod print-object ((obj duration) stream)
  (print-unreadable-object (obj stream :type t)
    (write-duration-to-iso obj stream)))
