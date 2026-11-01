;;;; The tempo condition hierarchy.
;;;;
;;;; Every error signalled by tempo is a subtype of TEMPO-ERROR, so
;;;; user code can establish a single handler for all of it.

(uiop:define-package #:tempo.conditions
  (:use #:cl)
  (:export
   #:tempo-error
   #:invalid-date-error
   #:invalid-date-year
   #:invalid-date-month
   #:invalid-date-day
   #:invalid-date-reason))

(in-package #:tempo.conditions)

(define-condition tempo-error (error) ()
  (:documentation
   "Root of every error condition signalled by tempo."))

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
date, e.g. February 30th, or February 29th of a non-leap year.  The
invalid fields are available via INVALID-DATE-YEAR,
INVALID-DATE-MONTH, INVALID-DATE-DAY, and a human-readable
INVALID-DATE-REASON."))
