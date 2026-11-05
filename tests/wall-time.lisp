;;;; Tests for the WALL-TIME type, exercised through the public TEMPO
;;;; package (re-exported from TEMPO.WALL-TIME).
;;;;
;;;; Conventions mirror the DATE tests:
;;;; - Type errors and wall-time errors are separate tiers.
;;;; - VALID-WALL-TIME-P is a total predicate: nonsense yields NIL.
;;;; - Golden-string tests pin the printed form.
;;;; - Runtime values are indirected through DEFPARAMETER so type
;;;;   checks stay runtime checks rather than compile-time constant
;;;;   diagnostics.

(defpackage tempo/tests/wall-time
  (:use #:cl #:tempo #:rove))

(in-package #:tempo/tests/wall-time)

(defparameter *runtime-float* 1.5)

(defparameter *runtime-hour* 24)
(defparameter *runtime-hour-low* -1)
(defparameter *runtime-minute* 61)
(defparameter *runtime-second* 60)
(defparameter *runtime-second-low* -1/2)

(deftest test-construction
  (testing "positional and keyword constructors agree"
    (ok (equalp (wall-time 9 30) (make-wall-time :hour 9 :minute 30)))
    (ok (equalp (make-wall-time) (wall-time 0 0))))
  (testing "fractional seconds are rationalized at the positional boundary"
    (ok (= 61/2 (wall-time-second (wall-time 9 30 30.5)))))
  (testing "impossible times signal INVALID-TIME-ERROR"
    (ok (signals (wall-time *runtime-hour* 0) 'invalid-time-error))
    (ok (signals (wall-time *runtime-hour-low* 0) 'invalid-time-error))
    (ok (signals (wall-time 0 *runtime-minute*) 'invalid-time-error))
    (ok (signals (wall-time 9 30 *runtime-second*) 'invalid-time-error))
    (ok (signals (make-wall-time :hour 9 :second *runtime-second-low*)
                 'invalid-time-error)))
  (testing "non-integers are TYPE-ERRORs, not time errors"
    (ok (signals (make-wall-time :hour *runtime-float*) 'type-error))
    (ok (signals (make-wall-time :minute *runtime-float*) 'type-error))
    (ok (signals (make-wall-time :second *runtime-float*) 'type-error))))

(deftest test-valid-wall-time-p
  (testing "real times"
    (ok (valid-wall-time-p 0 0 0))
    (ok (valid-wall-time-p 23 59 59))
    (ok (valid-wall-time-p 0 0 1/2))
    (ok (valid-wall-time-p 23 59 119/2)))
  (testing "impossible times"
    (ok (not (valid-wall-time-p 24 0 0)))
    (ok (not (valid-wall-time-p -1 0 0)))
    (ok (not (valid-wall-time-p 0 60 0)))
    (ok (not (valid-wall-time-p 0 0 60)))
    (ok (not (valid-wall-time-p 0 0 -1/2))))
  (testing "wrong kinds are simply not times"
    (ok (not (valid-wall-time-p nil nil nil)))
    (ok (not (valid-wall-time-p "9" 30 0)))
    (ok (not (valid-wall-time-p *runtime-float* 30 0)))
    (ok (not (valid-wall-time-p 9 30 *runtime-float*)))))

(deftest test-destructure-wall-time
  (multiple-value-bind (h m s) (destructure-wall-time (wall-time 9 30 119/2))
    (ok (= 9 h)) (ok (= 30 m)) (ok (= 119/2 s)))
  (multiple-value-bind (h m s) (destructure-wall-time (wall-time 0 0))
    (ok (= 0 h)) (ok (= 0 m)) (ok (= 0 s))))

(deftest test-conversions
  (testing "wall-time->duration measures exact seconds since midnight"
    (ok (duration-zerop (wall-time->duration (wall-time 0 0))))
    (ok (duration= (wall-time->duration (wall-time 9 30))
                   (duration+ (hours 9) (minutes 30))))
    (ok (duration= (wall-time->duration (wall-time 23 59 119/2))
                   (duration- (days 1) (seconds 1/2)))))
  (testing "round trips are exact"
    (loop for (h m s) in '((0 0 0) (9 30 0) (23 59 59) (23 59 119/2) (12 0 1/3))
          do (let ((tod (wall-time h m s)))
               (ok (equalp (duration->wall-time (wall-time->duration tod))
                           tod)))))
  (testing "clock arithmetic wraps modulo 24 hours"
    (ok (equalp (duration->wall-time (hours 25)) (wall-time 1 0)))
    (ok (equalp (duration->wall-time (minutes 1445)) (wall-time 0 5)))
    (ok (equalp (duration->wall-time (seconds -10)) (wall-time 23 59 50)))
    (ok (equalp (duration->wall-time (days 2)) (wall-time 0 0))))
  (testing "calendar months have no position on the dial"
    (ok (signals (duration->wall-time (months 1)) 'inexact-duration-error))
    (ok (signals (duration->wall-time (duration :months 1 :hours 2))
                 'inexact-duration-error))))

(deftest test-comparisons
  (testing "ordering"
    (ok (wall-time< (wall-time 9 0) (wall-time 9 30)))
    (ok (wall-time< (wall-time 0 0) (wall-time 0 0 1)))
    (ok (wall-time<= (wall-time 5 0 1/2) (wall-time 5 0 1/2)))
    (ok (wall-time> (wall-time 18 0) (wall-time 6 0)))
    (ok (wall-time>= (wall-time 23 59 59) (wall-time 23 59 59)))
    (ok (wall-time= (wall-time 9 30) (wall-time 9 30 0))))
  (testing "variadic chains"
    (ok (wall-time= (wall-time 9 0)))
    (ok (wall-time= (wall-time 9 0) (wall-time 9 0) (wall-time 9 0)))
    (ok (wall-time< (wall-time 9 0) (wall-time 10 0) (wall-time 11 0)))
    (ok (not (wall-time< (wall-time 9 0) (wall-time 11 0) (wall-time 10 0)))))
  (testing "at least one argument is required, like CL = and CL <"
    (ok (signals (apply #'wall-time= '()) 'error))
    (ok (signals (apply #'wall-time/= '()) 'error))))

(deftest test-equality-and-hashing
  (testing "defstruct gives slot-wise EQUALP by the standard"
    (ok (equalp (wall-time 9 30) (make-wall-time :hour 9 :minute 30)))
    (ng (equalp (wall-time 9 30) (wall-time 9 31))))
  (testing "times-of-day work as EQUALP hash-table keys"
    (let ((table (make-hash-table :test 'equalp)))
      (setf (gethash (wall-time 9 30) table) 'half-past-nine)
      (ok (eq 'half-past-nine
              (gethash (make-wall-time :hour 9 :minute 30 :second 0) table)))
      (ng (gethash (wall-time 9 31) table)))))

(deftest test-printing
  (testing "ISO-8601 golden strings"
    (ok (string= "00:00:00" (format-wall-time-iso (wall-time 0 0))))
    (ok (string= "09:30:00" (format-wall-time-iso (wall-time 9 30))))
    (ok (string= "23:59:59" (format-wall-time-iso (wall-time 23 59 59))))
    (ok (string= "09:30:00.5" (format-wall-time-iso (wall-time 9 30 1/2))))
    (ok (string= "23:59:59.5" (format-wall-time-iso (wall-time 23 59 119/2)))))
  (testing "print-object at the REPL"
    (ok (string= "#<WALL-TIME 09:30:00>" (format nil "~a" (wall-time 9 30))))
    (ok (string= "#<WALL-TIME 00:00:00>" (format nil "~a" (wall-time 0 0)))))
  (testing "cl:format directive ~/tempo:format-wall-time/"
    (ok (string= "was 09:30:00"
                 (format nil "was ~/tempo:format-wall-time/" (wall-time 9 30)))))
  (testing "known rough edge: non-terminating fractions print as decimal expansion"
    ;; Same family of rough edge as DURATION's PT0.33...; keep a loose
    ;; prefix check until printing switches to exact fractions.
    (ok (search "09:30:00.33" (format-wall-time-iso (wall-time 9 30 1/3))))))