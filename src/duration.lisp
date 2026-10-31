(uiop:define-package tempo.duration
  (:use #:cl))

(in-package #:tempo.duration)

;;; A DURATION is simply just a span of seconds. There is no calendar
;;; specific functionality to a DURATION; that is delegated to an
;;; INTERVAL. This is the same terminology found in ISO 8601.

(defstruct duration
  (seconds 0 :type rational :read-only t))

;; Unit constructors
(defun days (n)         (make-duration :seconds (* n 86400)))
(defun hours (n)        (make-duration :seconds (* n 3600)))
(defun minutes (n)      (make-duration :seconds (* n 60)))
(defun seconds (n)      (make-duration :seconds n))
(defun milliseconds (n) (make-duration :seconds (/ n 1000)))
(defun microseconds (n) (make-duration :seconds (/ n 1000000)))
(defun nanoseconds (n)  (make-duration :seconds (/ n 1000000000)))

;; Converting to other units
(defun in-days (d)         (/ (duration-seconds d) 86400))
(defun in-hours (d)        (/ (duration-seconds d) 3600))
(defun in-minutes (d)      (/ (duration-seconds d) 60))
(defun in-seconds (d)      (duration-seconds d))
(defun in-milliseconds (d) (* (duration-seconds d) 1000))
(defun in-microseconds (d) (* (duration-seconds d) 1000000))
(defun in-nanoseconds (d)  (* (duration-seconds d) 1000000000))

(defun destructure-duration (duration)
  (let* ((total-seconds (in-seconds duration))
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
	    :sum (in-seconds i) into total
	    :finally (return (make-duration :seconds total)))))

(defun duration- (duration &rest durations)
  (declare (dynamic-extent durations))
  (if (null (cdr durations))
      (make-duration :seconds (- (in-seconds duration)))
      (loop :for i in (cdr durations)
	    :sum (- (in-seconds i)) into difference
	    :with first-duration = (in-seconds (first durations))
	    :finally (return (make-duration :seconds (+ first-duration difference))))))

(defun duration* (a b)
  (etypecase a
    (duration (etypecase b
                (rational (make-duration :seconds (* (in-seconds a) b)))))
    (rational (etypecase b
                (duration (make-duration :seconds (* a (in-seconds b))))))))

(defun duration/ (duration divisor)
  "DIVISOR can be either a scalar or another DURATION.

Depending on the type of DIVISOR, this function either returns a
scaled down duration or a ratio between two durations."
  (etypecase divisor
    (rational (make-duration :seconds (/ (in-seconds duration) divisor)))
    (duration (/ (in-seconds duration) (in-seconds divisor)))))

(defun duration-floor (duration &optional (resolution (seconds 1)))
  "Snap DURATION down to the nearest multiple of RESOLUTION."
  (let ((res (duration-seconds resolution)))
    (make-duration :seconds (* (floor (duration-seconds duration) res) res))))

(defun duration-ceiling (duration &optional (resolution (seconds 1)))
  "Snap DURATION up to the nearest multiple of RESOLUTION."
  (let ((res (duration-seconds resolution)))
    (make-duration :seconds (* (ceiling (duration-seconds duration) res) res))))

(defun duration-round (duration &optional (resolution (seconds 1)))
  "Snap DURATION to the nearest multiple of RESOLUTION."
  (let ((res (duration-seconds resolution)))
    (make-duration :seconds (* (round (duration-seconds duration) res) res))))

(defun duration-divide (duration divisor)
  (multiple-value-bind (quotient remainder) (truncate (in-seconds duration) divisor)
    (values quotient (make-duration :seconds remainder))))

(defmacro define-comparison-operator (name operator)
  `(defun ,name (duration &rest durations)
     (declare (dynamic-extent durations))
     (if (null durations)
	 t
	 (loop :for i in durations
	       :and prev-i = duration then i
	       :always (,operator (in-seconds prev-i) (in-seconds i))))))

(define-comparison-operator duration= =)
(define-comparison-operator duration/= /=)
(define-comparison-operator duration< <)
(define-comparison-operator duration<= <=)
(define-comparison-operator duration> >)
(define-comparison-operator duration>= >=)

(defun duration-abs (duration)
  (make-duration :seconds (abs (in-seconds duration))))
(defun duration-zerop (duration)  (zerop (in-seconds duration)))
(defun duration-plusp (duration)  (plusp (in-seconds duration)))
(defun duration-minusp (duration) (minusp (in-seconds duration)))

(defun write-duration-to-iso (duration stream)
  (let ((total (in-seconds duration)))
    (when (zerop total) (write-string "PT0S" stream))
    (when (minusp total)
      (write-char #\- stream)
      (setf total (abs total)))

    (bind:bind (((:values days rd)        (truncate total 86400))
	   ((:values hours rh)       (truncate rd 3600))
	   ((:values minutes seconds) (truncate rh 60)))

      (format stream "P~@[~dD~]~:[~;T~]~@[~dH~]~@[~dM~]"
	      (and (plusp days) days)
	      (plusp (+ hours minutes seconds))
	      (and (plusp hours) hours)
	      (and (plusp minutes) minutes))

      (when (plusp seconds)
	(if (integerp seconds)
	    (format stream "~dS" seconds)
	    (format stream "~FS" (float seconds 1.0d0)))))))

(defun format-duration-iso (duration)
  "Return DURATION formatted as an ISO-8601 string."
  (with-output-to-string (s)
    (write-duration-to-iso duration s)))

(defmethod print-object ((obj duration) stream)
  (print-unreadable-object (obj stream :type t)
    (write-duration-to-iso obj stream)))
