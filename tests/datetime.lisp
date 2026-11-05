;;;; Tests for the DATETIME type, exercised through the public TEMPO
;;;; package (re-exported from TEMPO.DATETIME).
;;;;
;;;; Conventions mirror the DATE and WALL-TIME tests:
;;;; - Type errors and calendar/time-of-day errors are separate tiers;
;;;;   the components' own conditions propagate through DATETIME.
;;;; - VALID-DATETIME-P is a total predicate.
;;;; - Golden-string tests pin the printed form.
;;;; - Runtime values are indirected through DEFPARAMETER.

(defpackage tempo/tests/datetime
  (:use #:cl #:tempo #:rove))

(in-package #:tempo/tests/datetime)

(defparameter *runtime-float* 1.5)

(defparameter *runtime-month* 13)
(defparameter *runtime-day* 32)
(defparameter *runtime-hour* 24)
(defparameter *runtime-minute* 61)
(defparameter *runtime-second* 60)

(deftest test-construction
  (testing "positional and keyword constructors agree"
    (ok (equalp (datetime 2026 10 30 9 30)
                (make-datetime :year 2026 :month 10 :day 30 :hour 9 :minute 30)))
    (ok (equalp (datetime 2026 1 1)
                (make-datetime :year 2026)))
    (ok (datetime-p (make-datetime :year 2026))))
  (testing "datetime-from-parts reuses existing components"
    (let ((dt (datetime-from-parts (date 2026 10 30) (wall-time 9 30))))
      (ok (datetime-p dt))
      (ok (equalp (datetime-date dt) (date 2026 10 30)))
      (ok (equalp (datetime-wall-time dt) (wall-time 9 30)))))
  (testing "fractional seconds are rationalized at the positional boundary"
    (ok (= 61/2 (datetime-second (datetime 2026 10 30 9 30 30.5)))))
  (testing "YEAR is required"
    (ok (signals (make-datetime) 'error)))
  (testing "impossible dates and times signal the component conditions"
    (ok (signals (datetime 2026 *runtime-month* 1) 'invalid-date-error))
    (ok (signals (datetime 2026 2 30) 'invalid-date-error))
    (ok (signals (datetime 2026 10 *runtime-day*) 'invalid-date-error))
    (ok (signals (datetime 2026 10 30 *runtime-hour* 0) 'invalid-time-error))
    (ok (signals (datetime 2026 10 30 9 *runtime-minute*) 'invalid-time-error))
    (ok (signals (datetime 2026 10 30 9 30 *runtime-second*) 'invalid-time-error)))
  (testing "non-numeric fields signal TYPE-ERROR"
    (ok (signals (make-datetime :year *runtime-float*) 'type-error))
    (ok (signals (make-datetime :month *runtime-float*) 'type-error))
    (ok (signals (make-datetime :day *runtime-float*) 'type-error))
    (ok (signals (make-datetime :hour *runtime-float*) 'type-error))
    (ok (signals (make-datetime :minute *runtime-float*) 'type-error))
    (ok (signals (make-datetime :second *runtime-float*) 'type-error))))

(deftest test-valid-datetime-p
  (testing "real datetimes"
    (ok (valid-datetime-p 2026 10 30 9 30 0))
    (ok (valid-datetime-p 2024 2 29 23 59 119/2))
    (ok (valid-datetime-p 0 12 31 0 0 0))
    (ok (valid-datetime-p 1 1 1 0 0 1/2)))
  (testing "impossible components yield NIL"
    (ok (not (valid-datetime-p 2026 2 30 9 30 0)))
    (ok (not (valid-datetime-p 2026 10 30 24 0 0)))
    (ok (not (valid-datetime-p 2026 10 30 9 60 0)))
    (ok (not (valid-datetime-p 2026 10 30 9 30 60)))
    (ok (not (valid-datetime-p 2026 10 30 9 30 -1/2))))
  (testing "wrong kinds are simply not datetimes"
    (ok (not (valid-datetime-p nil nil nil nil nil nil)))
    (ok (not (valid-datetime-p *runtime-float* 10 30 9 30 0)))
    (ok (not (valid-datetime-p 2026 10 30 9 30 *runtime-float*)))))

(deftest test-accessors
  (let ((dt (datetime 2026 10 30 9 30 119/2)))
    (ok (= 2026 (datetime-year dt)))
    (ok (= 10 (datetime-month dt)))
    (ok (= 30 (datetime-day dt)))
    (ok (= 9 (datetime-hour dt)))
    (ok (= 30 (datetime-minute dt)))
    (ok (= 119/2 (datetime-second dt))))
  (let ((dt (make-datetime :year 2026)))
    (ok (equalp (datetime-date dt) (date 2026 1 1)))
    (ok (equalp (datetime-wall-time dt) (wall-time 0 0)))))

(deftest test-destructure-datetime
  (multiple-value-bind (y mo d h m s)
      (destructure-datetime (datetime 2026 10 30 9 30 119/2))
    (ok (= 2026 y)) (ok (= 10 mo)) (ok (= 30 d))
    (ok (= 9 h)) (ok (= 30 m)) (ok (= 119/2 s)))
  (multiple-value-bind (y mo d h m s)
      (destructure-datetime (make-datetime :year 2026))
    (ok (= 2026 y)) (ok (= 1 mo)) (ok (= 1 d))
    (ok (= 0 h)) (ok (= 0 m)) (ok (= 0 s))))

(deftest test-comparisons
  (testing "ordering across date and time fields"
    (ok (datetime< (datetime 2026 10 30 9 0) (datetime 2026 10 30 9 30)))
    (ok (datetime< (datetime 2026 10 30 23 59 59) (datetime 2026 10 31 0 0)))
    (ok (datetime<= (datetime 2026 10 30 9 30 1/2) (datetime 2026 10 30 9 30 1/2)))
    (ok (datetime> (datetime 2027 1 1 0 0) (datetime 2026 12 31 23 59)))
    (ok (datetime>= (datetime 2026 10 30 9 30) (datetime 2026 10 30 9 30)))
    (ok (datetime= (datetime 2026 10 30 9 30) (datetime 2026 10 30 9 30 0)))
    (ng (datetime= (datetime 2026 10 30 9 30) (datetime 2026 10 30 9 31))))
  (testing "variadic chains"
    (ok (datetime= (datetime 2026 1 1 0 0)))
    (ok (datetime< (datetime 2026 1 1 0 0)
                   (datetime 2026 1 2 0 0)
                   (datetime 2026 1 2 1 0)))
    (ok (not (datetime< (datetime 2026 1 1 0 0)
                        (datetime 2026 1 2 1 0)
                        (datetime 2026 1 2 0 0)))))
  (testing "at least one argument is required, like CL = and CL <"
    (ok (signals (apply #'datetime= '()) 'error))
    (ok (signals (apply #'datetime< '()) 'error))
    (ok (signals (apply #'datetime/= '()) 'error))))

(deftest test-datetime-difference
  (testing "elapsed durations are exact"
    (ok (= 90 (duration-seconds (datetime-difference
                                 (datetime 2026 10 30 9 30)
                                 (datetime 2026 10 30 9 28 30)))))
    (ok (= -3600 (duration-seconds (datetime-difference
                                    (datetime 2026 10 30 9 0)
                                    (datetime 2026 10 30 10 0)))))
    (ok (= 1/2 (duration-seconds (datetime-difference
                                  (datetime 2026 10 30 9 0 1/2)
                                  (datetime 2026 10 30 9 0)))))
    (ok (= 86400 (duration-seconds (datetime-difference
                                    (datetime 2026 10 31 0 0)
                                    (datetime 2026 10 30 0 0)))))
    (ok (zerop (duration-months (datetime-difference
                                 (datetime 2026 10 31 0 0)
                                 (datetime 2026 10 30 0 0))))))
  (testing "difference round-trips through datetime+ and datetime-"
    (let ((a (datetime 2026 10 30 9 30 119/2))
          (b (datetime 2026 10 29 0 0)))
      (ok (equalp (datetime+ b (datetime-difference a b)) a))
      (ok (equalp (datetime- a (datetime-difference a b)) b)))))

(deftest test-datetime-arithmetic
  (testing "exact seconds carry across days"
    (ok (equalp (datetime+ (datetime 2026 10 30 23 59 59) (seconds 2))
                (datetime 2026 10 31 0 0 1)))
    (ok (equalp (datetime+ (datetime 2026 10 30 9 30)
                           (duration :hours 1 :minutes 30 :seconds 1/2))
                (datetime 2026 10 30 11 0 1/2)))
    (ok (equalp (datetime+ (datetime 2026 10 30 9 30) (milliseconds 500))
                (datetime 2026 10 30 9 30 1/2))))
  (testing "negative durations wrap to earlier days"
    (ok (equalp (datetime- (datetime 2026 10 31 0 0) (seconds 1))
                (datetime 2026 10 30 23 59 59)))
    (ok (equalp (datetime- (datetime 1 1 1 0 0) (seconds 1))
                (datetime 0 12 31 23 59 59))))
  (testing "calendar months have no fixed second count"
    (ok (signals (datetime+ (datetime 2026 10 30 9 30) (months 1))
                 'inexact-duration-error))
    (ok (signals (datetime+ (datetime 2026 10 30 9 30)
                            (duration :months 1 :hours 2))
                 'inexact-duration-error))))

(deftest test-replace-datetime
  (testing "field replacement"
    (ok (equalp (replace-datetime (datetime 2026 10 30 9 30) :hour 5)
                (datetime 2026 10 30 5 30)))
    (ok (equalp (replace-datetime (datetime 2026 10 30 9 30) :year 2027 :day 1)
                (datetime 2027 10 1 9 30)))
    (ok (equalp (replace-datetime (datetime 2026 10 30 9 30))
                (datetime 2026 10 30 9 30)))
    (ok (equalp (replace-datetime (datetime 2026 10 30 9 30) :second 30.5)
                (datetime 2026 10 30 9 30 61/2))))
  (testing "component conditions propagate"
    (ok (signals (replace-datetime (datetime 2026 10 30 9 30) :month *runtime-month*)
                 'invalid-date-error))
    (ok (signals (replace-datetime (datetime 2026 10 30 9 30) :hour *runtime-hour*)
                 'invalid-time-error)))
  (testing "the clamp restart propagates from the date component"
    (ok (equalp (handler-bind ((invalid-date-error
                                (lambda (c)
                                  (declare (ignore c))
                                  (invoke-restart 'clamp-to-end-of-month))))
                  (replace-datetime (datetime 2024 1 31 9 30) :month 2))
                (datetime 2024 2 29 9 30)))))

(deftest test-start-of-day
  (ok (equalp (start-of-day (datetime 2026 10 30 9 30 119/2))
              (datetime 2026 10 30 0 0)))
  (ok (equalp (start-of-day (datetime 2026 10 30 9 30))
              (start-of-day (datetime 2026 10 30 23 59)))))

(deftest test-printing
  (testing "ISO-8601 golden strings"
    (ok (string= "2026-10-30T09:30:00" (format-datetime-iso (datetime 2026 10 30 9 30))))
    (ok (string= "2026-01-01T00:00:00" (format-datetime-iso (make-datetime :year 2026))))
    (ok (string= "2026-10-30T09:30:00.5" (format-datetime-iso (datetime 2026 10 30 9 30 1/2))))
    (ok (string= "2026-10-30T23:59:59.5" (format-datetime-iso (datetime 2026 10 30 23 59 119/2)))))
  (testing "print-object at the REPL"
    (ok (string= "#<DATETIME 2026-10-30T09:30:00>" (format nil "~a" (datetime 2026 10 30 9 30))))
    (ok (string= "#<DATETIME 0001-01-01T00:00:00>" (format nil "~a" (make-datetime :year 1)))))
  (testing "cl:format directive ~/tempo:format-datetime/"
    (ok (string= "at 2026-10-30T09:30:00"
                 (format nil "at ~/tempo:format-datetime/" (datetime 2026 10 30 9 30)))))
  (testing "known rough edge: non-terminating fractions print as decimal expansion"
    ;; Same rough edge family as DURATION (PT0.33...) and WALL-TIME.
    (ok (search "2026-10-30T09:30:00.33"
                (format-datetime-iso (datetime 2026 10 30 9 30 1/3))))))

(deftest test-equality-and-hashing
  (testing "defstruct gives slot-wise EQUALP through the nested components"
    (ok (equalp (datetime 2026 10 30 9 30) (datetime 2026 10 30 9 30)))
    (ng (equalp (datetime 2026 10 30 9 30) (datetime 2026 10 30 9 31))))
  (testing "datetimes work as EQUALP hash-table keys"
    (let ((table (make-hash-table :test 'equalp)))
      (setf (gethash (datetime 2026 10 30 9 30) table) 'tower-hall)
      (ok (eq 'tower-hall
              (gethash (make-datetime :year 2026 :month 10 :day 30
                                      :hour 9 :minute 30)
                       table)))
      (ng (gethash (datetime 2026 10 30 9 31) table)))))