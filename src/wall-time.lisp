(uiop:define-package #:tempo.wall-time
  (:use #:cl)
  (:import-from #:tempo.conditions
                #:invalid-time-error
                #:inexact-duration-error)
  (:import-from #:tempo.duration
                #:make-duration
                #:duration
                #:duration-months
                #:duration-seconds)
  (:export
   ;; Type, constructors, accessors
   #:wall-time
   #:wall-time-p
   #:make-wall-time
   #:wall-time-hour
   #:wall-time-minute
   #:wall-time-second
   #:valid-wall-time-p
   #:destructure-wall-time

   ;; Conversions
   #:wall-time->duration
   #:duration->wall-time

   ;; Comparisons
   #:wall-time=
   #:wall-time/=
   #:wall-time<
   #:wall-time<=
   #:wall-time>
   #:wall-time>=

   ;; Formatting
   #:write-wall-time-to-iso
   #:format-wall-time-iso
   #:format-wall-time))

(in-package #:tempo.wall-time)

(defstruct (wall-time (:constructor %make-wall-time (hour minute second)))
  (hour   0 :type (integer 0 23)  :read-only t)
  (minute 0 :type (integer 0 59)  :read-only t)
  (second 0 :type (rational 0 (60)) :read-only t))

(defmethod make-load-form ((self wall-time) &optional environment)
  (make-load-form-saving-slots self :environment environment))

(declaim (ftype (function (&key (:hour integer) (:minute integer) (:second rational))
                          (values wall-time &optional))
                make-wall-time)
         (ftype (function (integer integer &optional real)
                          (values wall-time &optional))
                wall-time)
         (ftype (function (t t t) (values boolean &optional))
                valid-wall-time-p)
         (ftype (function (wall-time)
                          (values (integer 0 23) (integer 0 59) (rational 0 (60)) &optional))
                destructure-wall-time)
         (ftype (function (wall-time) (values duration &optional))
                wall-time->duration)
         (ftype (function (duration) (values wall-time &optional))
                duration->wall-time)
         (ftype (function * (values boolean &optional))
                wall-time= wall-time/= wall-time< wall-time<=
                wall-time> wall-time>=)
         (ftype (function (wall-time) (values string &optional))
                format-wall-time-iso))

(defun %invalid-time-reason (hour minute second)
  (cond ((< hour 0)     "hour must be at least 0")
        ((> hour 23)    "hour must be at most 23")
        ((< minute 0)   "minute must be at least 0")
        ((> minute 59)  "minute must be at most 59")
        ((< second 0)   "second must be at least 0")
        ((>= second 60) "second must be less than 60")
        (t (format nil "~D:~2,'0D:~A does not name a time of day"
                   hour minute second))))

(defun valid-wall-time-p (hour minute second)
  "True if HOUR, MINUTE, and SECOND name a real time of day.

This predicate is total: anything that is not an integer HOUR,
nonnegative integer MINUTE, or rational SECOND between 0 (inclusive)
and 60 (exclusive) simply yields NIL instead of signalling."
  (and (integerp hour)
       (integerp minute)
       (rationalp second)
       (<= 0 hour 23)
       (<= 0 minute 59)
       (<= 0 second)
       (< second 60)))

(defun make-wall-time (&key (hour 0) (minute 0) (second 0))
  "Create a time of day from HOUR, MINUTE, and SECOND.

All fields default to 0 (midnight). Signals INVALID-TIME-ERROR when
the fields do not name a real time of day, and a TYPE-ERROR when HOUR
or MINUTE is not an integer or SECOND is not a rational."
  (check-type hour integer)
  (check-type minute integer)
  (check-type second rational)
  (unless (valid-wall-time-p hour minute second)
    (error 'invalid-time-error
           :hour hour :minute minute :second second
           :reason (%invalid-time-reason hour minute second)))
  (%make-wall-time hour minute second))

(defun wall-time (hour minute &optional (second 0))
  "Create a WALL-TIME from HOUR, MINUTE, and SECOND.

SECOND may be any real; it is rationalized before conversion."
  (make-wall-time :hour hour :minute minute :second (rationalize second)))

(defun destructure-wall-time (wall-time)
  "Return (values HOUR MINUTE SECOND) for WALL-TIME."
  (values (wall-time-hour wall-time)
          (wall-time-minute wall-time)
          (wall-time-second wall-time)))

;;; Conversions
(defun %seconds-since-midnight (wall-time)
  (+ (* (wall-time-hour wall-time) 3600)
     (* (wall-time-minute wall-time) 60)
     (wall-time-second wall-time)))

(defun wall-time->duration (wall-time)
  "The exact DURATION from midnight to WALL-TIME."
  (make-duration :seconds (%seconds-since-midnight wall-time)))

(defun duration->wall-time (duration)
  "The WALL-TIME that DURATION falls on since midnight.

The dial wraps modulo 24 hours: (HOURS 25) is 01:00:00, and a negative
duration such as (SECONDS -10) wraps to 23:59:50. A duration with
calendar months has no exact position on the dial and signals
INEXACT-DURATION-ERROR."
  (unless (zerop (duration-months duration))
    (error 'inexact-duration-error
           :duration duration
           :reason "calendar months have no exact position on a wall-time dial"))
  (let ((seconds (mod (duration-seconds duration) 86400)))
    (multiple-value-bind (hour rem) (truncate seconds 3600)
      (multiple-value-bind (minute second) (truncate rem 60)
        (%make-wall-time hour minute second)))))

;;; Comparisons
(defmacro define-wall-time-comparison (name operator docstring)
  "Define a variadic WALL-TIME comparison function NAME using OPERATOR.

DOCSTRING becomes the generated function's documentation string."
  `(progn
     (declaim (ftype (function * (values boolean &optional)) ,name))
     (defun ,name (wall-time &rest times-of-day)
       ,docstring
       (declare (dynamic-extent times-of-day))
       (loop :for a = wall-time :then b
             :for b :in times-of-day
             :always (,operator (%seconds-since-midnight a)
                                (%seconds-since-midnight b))))))

(define-wall-time-comparison wall-time= =
  "True if all TIMES-OF-DAY are the same time of day.

A single argument is the same as itself.")

(define-wall-time-comparison wall-time< <
  "True if TIMES-OF-DAY are in strictly increasing order.")

(define-wall-time-comparison wall-time<= <=
  "True if TIMES-OF-DAY are in nondecreasing order.")

(define-wall-time-comparison wall-time> >
  "True if TIMES-OF-DAY are in strictly decreasing order.")

(define-wall-time-comparison wall-time>= >=
  "True if TIMES-OF-DAY are in nonincreasing order.")

(defun wall-time/= (wall-time &rest times-of-day)
  "True if no two of WALL-TIME and TIMES-OF-DAY are the same."
  (declare (dynamic-extent times-of-day))
  (let ((all (cons wall-time times-of-day)))
    (declare (dynamic-extent all))
    (loop :for (tod . rest) :on all
          :always (loop :for other :in rest
                        :never (= (%seconds-since-midnight tod)
                                  (%seconds-since-midnight other))))))

;;; Printing
(defun %fraction-string (fraction)
  "The exact rational FRACTION between 0 and 1 as a decimal string
beginning with a decimal point, e.g. \".5\"."
  (subseq (format nil "~f" (float fraction 1.0d0)) 1))

(defun %write-second (second stream)
  (multiple-value-bind (whole fraction) (truncate second)
    (format stream "~2,'0d" whole)
    (unless (zerop fraction)
      (write-string (%fraction-string fraction) stream))))

(defun write-wall-time-to-iso (wall-time stream)
  "Write WALL-TIME to STREAM as an ISO-8601 time, for example
\"09:30:00\"; a fractional second is written as a decimal fraction
following the seconds, for example \"09:30:00.5\"."
  (format stream "~2,'0d:~2,'0d:"
          (wall-time-hour wall-time)
          (wall-time-minute wall-time))
  (%write-second (wall-time-second wall-time) stream))

(defun format-wall-time-iso (wall-time)
  "Return WALL-TIME formatted as an ISO-8601 time string."
  (with-output-to-string (s)
    (write-wall-time-to-iso wall-time s)))

(defun format-wall-time (stream wall-time &optional colon-p at-sign-p)
  "Write WALL-TIME to STREAM as an ISO-8601 time.

This is the CL:FORMAT directive function ~/tempo:format-wall-time/.
COLON-P and AT-SIGN-P are accepted for CL:FORMAT and ignored."
  (declare (ignore colon-p at-sign-p))
  (write-wall-time-to-iso wall-time stream))

(defmethod print-object ((object wall-time) stream)
  (print-unreadable-object (object stream :type t)
    (write-wall-time-to-iso object stream)))
