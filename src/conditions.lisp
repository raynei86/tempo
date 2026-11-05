;;;; The tempo condition hierarchy.
;;;;
;;;; Every domain error signalled by tempo is a subtype of TEMPO-ERROR, so
;;;; user code can establish a single handler for all of it.

(uiop:define-package #:tempo.conditions
  (:use #:cl)
  (:export
   #:tempo-error
   #:invalid-date-error
   #:invalid-date-year
   #:invalid-date-month
   #:invalid-date-day
   #:invalid-date-reason
   #:invalid-time-error
   #:invalid-time-hour
   #:invalid-time-minute
   #:invalid-time-second
   #:invalid-time-reason
   #:inexact-duration-error
   #:inexact-duration
   #:inexact-duration-reason))

(in-package #:tempo.conditions)

(define-condition tempo-error (error) ()
  (:documentation
   "Root of every error condition signalled by tempo."))

(define-condition invalid-time-error (tempo-error)
  ((hour   :initarg :hour   :type integer :reader invalid-time-hour)
   (minute :initarg :minute :type integer :reader invalid-time-minute)
   (second :initarg :second :reader invalid-time-second)
   (reason :initarg :reason :initform nil :type (or null string)
           :reader invalid-time-reason))
  (:report
   (lambda (condition stream)
     (format stream "Invalid time: ~d:~2,'0d:~a~@[ (~a)~]"
             (invalid-time-hour condition)
             (invalid-time-minute condition)
             (invalid-time-second condition)
             (invalid-time-reason condition))))
  (:documentation
   "Signalled when hour/minute/second fields do not name a real time of
day, e.g. 24:00:00 or 09:30:60. The invalid fields are available via
INVALID-TIME-HOUR, INVALID-TIME-MINUTE, INVALID-TIME-SECOND, and a
human-readable INVALID-TIME-REASON."))

(define-condition inexact-duration-error (tempo-error)
  ((duration :initarg :duration :reader inexact-duration)
   (reason :initarg :reason :initform nil :type (or null string)
           :reader inexact-duration-reason))
  (:report
   (lambda (condition stream)
     (format stream "Inexact duration: ~a~@[ (~a)~]"
             (inexact-duration condition)
             (inexact-duration-reason condition))))
  (:documentation
   "Signalled when an operation requires an exact duration, or when
calendar and exact time components cannot be collapsed without a
reference date. The offending duration is available via
INEXACT-DURATION, and a human-readable explanation via
INEXACT-DURATION-REASON."))

(define-condition invalid-date-error (tempo-error)
  ((year :initarg :year :type integer :reader invalid-date-year)
   (month :initarg :month :type integer :reader invalid-date-month)
   (day :initarg :day :type integer :reader invalid-date-day)
   (reason :initarg :reason :initform nil :type (or null string)
           :reader invalid-date-reason))
  (:report
   (lambda (condition stream)
     (format stream "Invalid date: ~d-~2,'0d-~2,'0d~@[ (~a)~]"
             (invalid-date-year condition)
             (invalid-date-month condition)
             (invalid-date-day condition)
             (invalid-date-reason condition))))
  (:documentation
   "Signalled when year/month/day fields do not name a real calendar
date, e.g. February 30th, or February 29th of a non-leap year. The
invalid fields are available via INVALID-DATE-YEAR,
INVALID-DATE-MONTH, INVALID-DATE-DAY, and a human-readable
INVALID-DATE-REASON."))
