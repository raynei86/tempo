(uiop:define-package tempo.duration
  (:use #:cl))

(in-package #:tempo.duration)

;;; A DURATION is simply just a span of seconds. There is no calendar
;;; specific functionality to a DURATION; that is delegated to an
;;; INTERVAL. This is the same terminology found in ISO 8601.

(defstruct duration
  (seconds 0 :type rational :read-only t))

(defmethod print-object ((obj duration) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "P")))

;; Unit constructors
(defun weeks (n)
  (days (* n 7)))

(defun days (n)
  (hours (* n 24)))

(defun hours (n)
  (minutes (* n 60)))

(defun minutes (n)
  (make-duration :seconds (* n 60)))

(defun seconds (n)
  (make-duration :seconds n))

(defun miliseconds (n)
  (make-duration :seconds (/ n 10)))

(defun microseconds (n)
  (make-duration :seconds (/ n 100)))

(defun nanoseconds (n)
  (make-duration :seconds (/ n 1000)))

;; Converting to other units
(defun as-seconds (duration)
  (duration-seconds duration))

(defun as-miliseconds (duration)
  (/ (as-seconds duration) 10))

(defun destructure-duration (duration)
  (let* ((total-seconds (as-seconds duration))
	 (days (floor total-seconds 86400))
	 (hours (floor (mod total-seconds 86400) 3600))
	 (minutes (floor (mod total-seconds 3600) 60))
	 (seconds (mod total-seconds 60)))
    (values days hours minutes seconds)))

;; Arithmetic

(defun duration+ (&rest durations)
  (declare (dynamic-extent durations))
  (if (null durations)
      (seconds 0)
      (loop :for i in durations
	    :sum (as-seconds i) into total
	    :finally (return (make-duration :seconds total)))))

(defun duration- (duration &rest durations)
  (declare (dynamic-extent durations))
  (if (null (cdr durations))
      (make-duration :seconds (- (as-seconds duration)))
      (loop :for i in (cdr durations)
	    :sum (- (as-seconds i)) into difference
	    :with first-duration = (as-seconds (first durations))
	    :finally (return (make-duration :seconds (+ first-duration difference))))))

(defun duration* (duration scalar)
  (make-duration :seconds (* (as-seconds duration) scalar)))

(defun duration/ (duration scalar)
  (make-duration :seconds (/ (as-seconds duration) scalar)))

(defmacro define-comparison-operator (name operator)
  `(defun ,name (duration &rest durations)
     (declare (dynamic-extent durations))
     (if (null durations)
	 t
	 (loop :for i in durations
	       :always (,operator (as-seconds duration) (as-seconds i))))))

(define-comparison-operator duration= =)
(define-comparison-operator duration/= /=)
(define-comparison-operator duration< <)
(define-comparison-operator duration<= <=)
(define-comparison-operator duration> >)
(define-comparison-operator duration>= >=)

(defun duration-abs (duration)
  (make-duration :seconds (abs (as-seconds duration))))

(defun duration-zerop (duration)
  (zerop (as-seconds duration)))

(defun duration-plusp (duration)
  (plusp (as-seconds duration)))

(defun duration-minusp (duration)
  (minusp (as-seconds duration)))

