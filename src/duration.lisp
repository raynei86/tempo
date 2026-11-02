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
  (:export
   ;; Type and accessors
   #:duration
   #:duration-p
   #:make-duration
   #:duration-seconds

   ;; Unit constructors
   #:days
   #:hours
   #:minutes
   #:seconds
   #:milliseconds
   #:microseconds
   #:nanoseconds

   ;; Conversions & Extraction
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
  (seconds 0 :type rational :read-only t))

(declaim (inline seconds minutes hours days
                 milliseconds microseconds nanoseconds
                 in-seconds in-minutes in-hours in-days
                 in-milliseconds in-microseconds in-nanoseconds
                 duration-zerop duration-plusp duration-minusp
                 duration-abs))

(declaim (ftype (function (real) (values duration &optional))
                days hours minutes seconds
                milliseconds microseconds nanoseconds)
         (ftype (function (duration) (values rational &optional))
                in-days in-hours in-minutes in-seconds
                in-milliseconds in-microseconds in-nanoseconds)
         (ftype (function (duration)
                          (values integer integer integer rational &optional))
                destructure-duration)
         (ftype (function * (values duration &optional))
                duration+ duration-)
         (ftype (function ((or duration rational) (or duration rational))
                          (values duration &optional))
                duration*)
         (ftype (function (duration (or rational duration))
                          (values (or duration rational) &optional))
                duration/)
         (ftype (function (duration &optional duration)
                          (values duration &optional))
                duration-floor duration-ceiling duration-round)
         (ftype (function (duration (or rational duration))
                          (values integer duration &optional))
                duration-divide)
         (ftype (function (duration) (values duration &optional))
                duration-abs)
         (ftype (function (duration) (values boolean &optional))
                duration-zerop duration-plusp duration-minusp)
         (ftype (function * (values boolean &optional))
                duration= duration/= duration< duration<= duration> duration>=))

;;; Unit constructors
(defun days (n)
  "Return a duration of N days.

N may be any real number; it is rationalized before conversion."
  (make-duration :seconds (rationalize (* n 86400))))

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

;; Converting to other units
(defun in-days (d)
  "Return the length of D expressed in days, as an exact rational."
  (/ (duration-seconds d) 86400))

(defun in-hours (d)
  "Return the length of D expressed in hours, as an exact rational."
  (/ (duration-seconds d) 3600))

(defun in-minutes (d)
  "Return the length of D expressed in minutes, as an exact rational."
  (/ (duration-seconds d) 60))

(defun in-seconds (d)
  "Return the length of D in seconds, as an exact rational."
  (duration-seconds d))

(defun in-milliseconds (d)
  "Return the length of D expressed in milliseconds, as an exact rational."
  (* (duration-seconds d) 1000))

(defun in-microseconds (d)
  "Return the length of D expressed in microseconds, as an exact rational."
  (* (duration-seconds d) 1000000))

(defun in-nanoseconds (d)
  "Return the length of D expressed in nanoseconds, as an exact rational."
  (* (duration-seconds d) 1000000000))

(defun destructure-duration (duration)
  "Return (values DAYS HOURS MINUTES SECONDS) for DURATION.

DAYS, HOURS, and MINUTES are whole numbers and SECONDS is the exact
rational remainder. For a negative duration, DAYS is negative and
the remaining components are nonnegative."
  (let* ((total-seconds (in-seconds duration))
         (days (floor total-seconds 86400))
         (hours (floor (mod total-seconds 86400) 3600))
         (minutes (floor (mod total-seconds 3600) 60))
         (seconds (mod total-seconds 60)))
    (values days hours minutes seconds)))

;;; Arithmetic

(defun duration+ (&rest durations)
  "Return the sum of DURATIONS.

With no arguments, return a zero duration."
  (declare (dynamic-extent durations))
  (if (null durations)
      (seconds 0)
      (loop :for i in durations
            :sum (in-seconds i) into total
            :finally (return (make-duration :seconds total)))))

(defun duration- (duration &rest durations)
  "Return DURATION minus the remaining DURATIONS.

With no remaining arguments, return the negation of DURATION."
  (declare (dynamic-extent durations))
  (if (null durations)
      (make-duration :seconds (- (in-seconds duration)))
      (loop :for i in durations
            :sum (- (in-seconds i)) into difference
            :finally (return (make-duration :seconds
                                            (+ (in-seconds duration)
                                               difference))))))

(defun duration* (a b)
  "Return the product of A and B.

One of A and B must be a duration and the other a rational number."
  (etypecase a
    (duration (etypecase b
                (rational (make-duration :seconds (* (in-seconds a) b)))))
    (rational (etypecase b
                (duration (make-duration :seconds (* a (in-seconds b))))))))

(defun duration/ (duration divisor)
  "Return DURATION divided by DIVISOR.

DIVISOR is either a rational scalar, in which case the result is a
duration, or another duration, in which case the result is a ratio.
See also DURATION-DIVIDE."
  (etypecase divisor
    (rational (make-duration :seconds (/ (in-seconds duration) divisor)))
    (duration (/ (in-seconds duration) (in-seconds divisor)))))

(defun duration-floor (duration &optional (resolution (seconds 1)))
  "Return DURATION snapped down to the nearest multiple of RESOLUTION.

RESOLUTION defaults to one second."
  (let ((res (duration-seconds resolution)))
    (make-duration :seconds (* (floor (duration-seconds duration) res) res))))

(defun duration-ceiling (duration &optional (resolution (seconds 1)))
  "Return DURATION snapped up to the nearest multiple of RESOLUTION.

RESOLUTION defaults to one second."
  (let ((res (duration-seconds resolution)))
    (make-duration :seconds (* (ceiling (duration-seconds duration) res) res))))

(defun duration-round (duration &optional (resolution (seconds 1)))
  "Return DURATION snapped to the nearest multiple of RESOLUTION.

Ties are rounded to even. RESOLUTION defaults to one second."
  (let ((res (duration-seconds resolution)))
    (make-duration :seconds (* (round (duration-seconds duration) res) res))))

(defun duration-divide (duration divisor)
  "Return (values QUOTIENT REMAINDER) dividing DURATION by DIVISOR.

DIVISOR is a rational number or a duration. The division truncates
toward zero; QUOTIENT is an integer and REMAINDER is a duration. See
also DURATION/ for scaling or representing a ratio."
  (let ((div (if (duration-p divisor) (in-seconds divisor) divisor)))
    (multiple-value-bind (quotient remainder)
        (truncate (in-seconds duration) div)
      (values quotient (make-duration :seconds remainder)))))

(defmacro define-comparison-operator (name operator docstring)
  "Define a variadic duration comparison function NAME using OPERATOR.

DOCSTRING becomes the generated function's documentation string."
  `(progn
     (declaim (ftype (function * (values boolean &optional)) ,name))
     (defun ,name (duration &rest durations)
       ,docstring
       (declare (dynamic-extent durations))
       (if (null durations)
           t
           (loop :for i in durations
                 :and prev-i = duration then i
                 :always (,operator (in-seconds prev-i) (in-seconds i)))))))

(define-comparison-operator duration= =
  "Return true if all DURATIONS are equal.

A single argument is always equal to itself.")

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
                        :never (= (in-seconds d) (in-seconds other))))))

(defun duration-abs (duration)
  "Return the absolute value of DURATION."
  (make-duration :seconds (abs (in-seconds duration))))

(defun duration-zerop (duration)
  "Return true if DURATION is zero."
  (zerop (in-seconds duration)))

(defun duration-plusp (duration)
  "Return true if DURATION is positive."
  (plusp (in-seconds duration)))

(defun duration-minusp (duration)
  "Return true if DURATION is negative."
  (minusp (in-seconds duration)))

;;; Formatting utils

(defun write-duration-to-iso (duration stream)
  "Write DURATION to STREAM in ISO-8601 format.

A zero duration is written as \"PT0S\"; negative durations are
prefixed with a minus sign."
  (let ((total (in-seconds duration)))
    (if (zerop total)
        (write-string "PT0S" stream)
        (progn
          (when (minusp total)
            (write-char #\- stream)
            (setf total (abs total)))

          (bind:bind (((:values days rd)         (truncate total 86400))
                      ((:values hours rh)        (truncate rd 3600))
                      ((:values minutes seconds) (truncate rh 60)))

            (format stream "P~@[~dD~]~:[~;T~]~@[~dH~]~@[~dM~]"
                    (and (plusp days) days)
                    (plusp (+ hours minutes seconds))
                    (and (plusp hours) hours)
                    (and (plusp minutes) minutes))

            (when (plusp seconds)
              (if (integerp seconds)
                  (format stream "~dS" seconds)
                  (format stream "~FS" (float seconds 1.0d0)))))))))

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
