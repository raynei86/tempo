;;;; Tests for the DURATION type, exercised through the public TEMPO
;;;; package (re-exported from TEMPO.DURATION).
;;;;
;;;; Conventions used in this file:
;;;; - Golden-string tests pin the printed form: the printed form is
;;;;   API, so a formatting change is a deliberate breaking change.
;;;; - Property-style loops use only RATIONAL inputs, so exactness
;;;;   assertions are meaningful. Float inputs have a dedicated
;;;;   RATIONALIZE block of their own.
;;;; - Comments flag places where current semantics are deliberate but
;;;;   subtle (round-to-even, floor decomposition of negatives).
;;;; - Test names carry a TEST- prefix so DEFTEST can never collide
;;;;   with, and accidentally redefine, an imported library function.

(defpackage tempo/tests/duration
  (:use #:cl #:tempo #:rove))

(in-package #:tempo/tests/duration)

;;; A float that exists only as runtime data. It tests that the raw
;;; constructor rejects floats: a LITERAL float in the call would
;;; conflict with the SECONDS slot's RATIONAL type at compile time,
;;; which (correctly!) aborts compilation under ASDF's default
;;; compile-failure policy. Keep this indirected.
(defparameter *runtime-float* 1.5)

(deftest test-constructors
  (testing "constructors store exact rationals and calendar months"
    (ok (duration-p (seconds 0)))
    (ok (= 5400 (duration-seconds (minutes 90))))
    (ok (= 1/1000 (duration-seconds (milliseconds 1))))
    (ok (= 1/1000000 (duration-seconds (microseconds 1))))
    (ok (= 1/1000000000 (duration-seconds (nanoseconds 1))))
    (ok (= 12 (duration-months (years 1))))
    (ok (= 6 (duration-months (years 1/2))))
    (ok (= 18 (duration-months (years 1.5))))
    (ok (= 3 (duration-months (months 3))))
    (ok (duration= (weeks 2) (days 14))))
  (testing "composite keyword constructor"
    (let ((d (duration :years 1 :months 2 :days 3 :hours 4 :minutes 5 :seconds 6)))
      (ok (= 14 (duration-months d)))
      (ok (= (+ (* 3 86400) (* 4 3600) (* 5 60) 6) (duration-seconds d)))))
  (testing "floats are RATIONALIZEd at the boundary"
    (ok (duration= (seconds 2.5) (seconds 5/2)))
    (ok (duration= (hours 1.5) (minutes 90)))
    ;; RATIONALIZE recovers the intended fraction, not the binary error:
    (ok (duration= (seconds 0.1) (milliseconds 100))))
  (testing "fractional years that do not resolve to whole months signal INEXACT-DURATION-ERROR"
    (ok (signals (years 1/5) 'inexact-duration-error)))
  (testing "the raw constructor stays strict; unit constructors are the forgiving boundary"
    (ok (signals (make-duration :seconds *runtime-float*) 'type-error))
    (ok (signals (make-duration :months *runtime-float*) 'type-error))))

(deftest test-conversions
  (testing "in-* returns exact rationals — never floats, never truncated"
    (ok (= 3/2 (in-hours (minutes 90))))
    (ok (= 3/4 (in-hours (minutes 45))))
    (ok (= 3/2 (in-days (hours 36))))
    (ok (= 1000 (in-milliseconds (seconds 1))))
    (ok (= 1/3 (in-minutes (seconds 20))))
    (ok (= 18 (in-months (years 3/2))))
    (ok (= 3/2 (in-years (months 18)))))
  (testing "conversions round-trip losslessly through every unit"
    (dolist (s '(0 1 59 60 61 5400 86400 90000 5/2 1/3 -45))
      (let ((d (seconds s)))
        (ok (= s (in-seconds d)))
        (ok (duration= d (minutes (in-minutes d))))
        (ok (duration= d (hours (in-hours d))))
        (ok (duration= d (days (in-days d))))
        (ok (duration= d (milliseconds (in-milliseconds d)))))))
  (testing "inexact conversions signal INEXACT-DURATION-ERROR"
    (ok (signals (in-seconds (months 1)) 'inexact-duration-error))
    (ok (signals (in-days (years 1)) 'inexact-duration-error))
    (ok (signals (in-months (days 30)) 'inexact-duration-error))
    (ok (signals (in-years (seconds 1)) 'inexact-duration-error))))

(deftest test-destructure-duration
  (testing "canonical 6-value decomposition"
    (multiple-value-bind (y mo d h m s) (destructure-duration (minutes 90))
      (ok (= 0 y)) (ok (= 0 mo)) (ok (= 0 d))
      (ok (= 1 h)) (ok (= 30 m)) (ok (= 0 s)))
    (multiple-value-bind (y mo d h m s) (destructure-duration (hours 25))
      (ok (= 0 y)) (ok (= 0 mo)) (ok (= 1 d))
      (ok (= 1 h)) (ok (= 0 m)) (ok (= 0 s)))
    (multiple-value-bind (y mo d h m s)
        (destructure-duration (duration :years 2 :months 3 :days 4 :hours 5 :minutes 6 :seconds 7))
      (ok (= 2 y)) (ok (= 3 mo)) (ok (= 4 d))
      (ok (= 5 h)) (ok (= 6 m)) (ok (= 7 s))))
  (testing "fractional seconds stay rational"
    (multiple-value-bind (y mo d h m s) (destructure-duration (seconds 61.5))
      (ok (= 0 y)) (ok (= 0 mo)) (ok (= 0 d))
      (ok (= 0 h)) (ok (= 1 m)) (ok (= 3/2 s))))
  (testing "negatives decompose with TRUNCATE semantics (signed components)"
    (multiple-value-bind (y mo d h m s) (destructure-duration (minutes -90))
      (ok (= 0 y)) (ok (= 0 mo)) (ok (= 0 d))
      (ok (= -1 h)) (ok (= -30 m)) (ok (= 0 s)))
    (multiple-value-bind (y mo d h m s) (destructure-duration (months -14))
      (ok (= -1 y)) (ok (= -2 mo)) (ok (= 0 d))
      (ok (= 0 h)) (ok (= 0 m)) (ok (= 0 s)))))

(deftest test-arithmetic
  (testing "duration+ follows the identities of CL +"
    (ok (duration-zerop (duration+)))
    (ok (duration= (seconds 30) (duration+ (seconds 30))))
    (ok (duration= (seconds 60)
                   (duration+ (seconds 10) (seconds 20) (seconds 30))))
    (ok (duration= (minutes 2) (apply #'duration+ (list (seconds 30)
                                                        (seconds 90)))))
    ;; Fractions stay exact:
    (ok (duration= (seconds 1) (duration+ (seconds 1/3) (seconds 2/3))))
    ;; Calendar and exact components sum independently:
    (ok (duration= (duration :years 1 :months 2 :hours 3)
                   (duration+ (years 1) (months 2) (hours 3)))))
  (testing "duration- follows the identities of CL -"
    (ok (duration= (seconds -10) (duration- (seconds 10))))
    (ok (duration= (months -3) (duration- (months 3))))
    (ok (duration= (seconds 7) (duration- (seconds 10) (seconds 3))))
    (ok (duration= (seconds 5)
                   (duration- (seconds 10) (seconds 3) (seconds 2))))
    (ok (duration= (seconds 2/3) (duration- (seconds 1) (seconds 1/3)))))
  (testing "algebra holds over exact rationals and calendar months"
    (let ((a (duration :months 2 :seconds 61))
          (b (minutes 3/2))
          (c (duration :years 1 :milliseconds 250)))
      (ok (duration= (duration+ a b) (duration+ b a)))
      (ok (duration= (duration+ (duration+ a b) c)
                     (duration+ a (duration+ b c))))
      (ok (duration= (duration- (duration+ a b) b) a))
      (ok (duration= (duration+ a (seconds 0)) a))))
  (testing "duration* scales and commutes, including reals and whole-month results"
    (ok (duration= (seconds 10) (duration* (seconds 5) 2)))
    (ok (duration= (seconds 10) (duration* 2 (seconds 5))))
    (ok (duration= (seconds 5/2) (duration* (seconds 5) 0.5)))
    (ok (duration= (months 6) (duration* (years 1) 1/2)))
    (ok (signals (duration* (months 1) 1/2) 'inexact-duration-error)))
  (testing "duration/ has scalar and ratio branches"
    (ok (duration= (minutes 30) (duration/ (hours 1) 2)))
    (ok (duration= (minutes 40) (duration/ (hours 1) 1.5)))
    (ok (duration= (months 6) (duration/ (years 1) 2)))
    (ok (= 4 (duration/ (hours 1) (minutes 15))))
    (ok (= 3/2 (duration/ (minutes 90) (hours 1))))
    (ok (= 4 (duration/ (years 1) (months 3))))
    (ok (signals (duration/ (months 1) 2) 'inexact-duration-error))
    (ok (signals (duration/ (months 1) (days 30)) 'inexact-duration-error))))

(deftest test-snapping
  (testing "default resolution is one second"
    (ok (duration= (seconds 2) (duration-floor (seconds 5/2))))
    (ok (duration= (seconds 3) (duration-ceiling (seconds 5/2))))
    ;; CL ROUND is round-half-to-even: 2.5s -> 2s, 3.5s -> 4s.
    (ok (duration= (seconds 2) (duration-round (seconds 5/2))))
    (ok (duration= (seconds 4) (duration-round (seconds 7/2)))))
  (testing "custom resolutions"
    (ok (duration= (minutes 45) (duration-floor (minutes 47) (minutes 15))))
    (ok (duration= (minutes 45) (duration-ceiling (minutes 45) (minutes 15)))) ; exact stays put
    (ok (duration= (minutes 90) (duration-ceiling (minutes 80) (minutes 30))))
    (ok (duration= (milliseconds 1400)
                   (duration-floor (milliseconds 1450) (milliseconds 200)))))
  (testing "FLOOR semantics on negatives: toward minus infinity"
    (ok (duration= (seconds -3) (duration-floor (seconds -5/2))))))

(deftest test-divide
  (testing "duration divisor"
    (multiple-value-bind (q r) (duration-divide (seconds 100) (seconds 30))
      (ok (= 3 q))
      (ok (duration= (seconds 10) r))))
  (testing "scalar divisor"
    (multiple-value-bind (q r) (duration-divide (seconds 100) 30)
      (ok (= 3 q))
      (ok (duration= (seconds 10) r)))))

(deftest test-comparison
  (testing "chaining compares adjacent pairs, like CL <"
    (ok (duration< (seconds 1) (seconds 10) (seconds 20)))
    (ng (duration< (seconds 1) (seconds 10) (seconds 2))) ; non-monotonic rejected
    (ok (duration<= (seconds 5) (seconds 5) (seconds 6)))
    (ok (duration> (seconds 20) (seconds 10) (seconds 1)))
    (ok (duration>= (seconds 5) (seconds 5))))
  (testing "calendar month ordering works when seconds are zero or months match"
    (ok (duration< (months 6) (years 1) (months 18)))
    (ok (duration< (duration :months 2 :hours 1)
                   (duration :months 2 :hours 2)))
    (ok (signals (duration< (months 1) (days 30)) 'inexact-duration-error)))
  (testing "single-argument calls return T, like CL"
    (ok (duration< (seconds 1)))
    (ok (duration= (seconds 1)))
    (ok (duration/= (seconds 1))))
  (testing "duration= requires all equal"
    (ok (duration= (minutes 90) (hours 3/2) (seconds 5400)))
    (ok (duration= (years 1) (months 12)))
    (ng (duration= (months 1) (days 30)))
    (ng (duration= (seconds 1) (seconds 1) (seconds 2))))
  (testing "duration/= requires pairwise distinctness"
    (ok (duration/= (seconds 1) (seconds 2) (seconds 3)))
    (ok (duration/= (months 1) (days 30)))
    (ng (duration/= (seconds 1) (seconds 2) (seconds 1))) ; first equals last
    (ng (duration/= (seconds 1) (seconds 1))))
  (testing "negatives compare by signed value"
    (ok (duration< (seconds -5) (seconds 5)))
    (ok (duration< (seconds -10) (seconds -5)))))

(deftest test-predicates
  (ok (duration-exact-p (seconds 30)))
  (ng (duration-exact-p (months 1)))
  (ok (duration-zerop (seconds 0)))
  (ok (duration-zerop (months 0)))
  (ng (duration-zerop (milliseconds 1)))
  (ng (duration-zerop (months 1)))
  (ok (duration-plusp (seconds 1)))
  (ok (duration-plusp (months 1)))
  (ok (duration-minusp (seconds -1)))
  (ok (duration-minusp (months -1)))
  (ng (duration-minusp (seconds 0)))
  (ok (signals (duration-plusp (duration :months 1 :seconds -1))
               'inexact-duration-error))
  (ok (duration= (seconds 5) (duration-abs (seconds -5))))
  (ok (duration= (months 5) (duration-abs (months -5))))
  (ok (duration= (seconds 5) (duration-abs (seconds 5)))))

(deftest test-equality-and-hashing
  (testing "defstruct gives slot-wise EQUALP by the standard"
    (ok (equalp (minutes 90) (hours 3/2)))
    (ng (equalp (minutes 90) (hours 2))))
  (testing "durations work as EQUALP hash-table keys"
    (let ((table (make-hash-table :test 'equalp)))
      (setf (gethash (minutes 90) table) 'ninety)
      (ok (eq 'ninety (gethash (hours 3/2) table)))
      (ng (gethash (seconds 90) table)))))

(deftest test-printing
  (testing "ISO-8601 golden strings"
    (ok (string= "PT0S" (format-duration-iso (seconds 0))))
    (ok (string= "PT30S" (format-duration-iso (seconds 30))))
    (ok (string= "PT1H30M" (format-duration-iso (minutes 90))))
    (ok (string= "P1DT1H" (format-duration-iso (hours 25))))
    (ok (string= "P3D" (format-duration-iso (days 3))))
    (ok (string= "P1Y" (format-duration-iso (years 1))))
    (ok (string= "P2M" (format-duration-iso (months 2))))
    (ok (string= "P1Y2M3DT4H5M"
                 (format-duration-iso
                  (duration :years 1 :months 2 :days 3 :hours 4 :minutes 5))))
    (ok (string= "PT0.5S" (format-duration-iso (milliseconds 500))))
    (ok (string= "PT2.5S" (format-duration-iso (seconds 5/2))))
    (ok (string= "-PT45S" (format-duration-iso (seconds -45))))
    (ok (string= "-PT1H30M" (format-duration-iso (minutes -90))))
    (ok (string= "-P1Y6M" (format-duration-iso (months -18))))
    (ok (string= "-PT0.5S" (format-duration-iso (seconds -1/2)))))
  (testing "print-object at the REPL"
    (ok (string= "#<DURATION PT1H30M>" (format nil "~a" (minutes 90)))))
  (testing "cl:format directive ~/tempo:format-duration/"
    (ok (string= "took PT2H30M"
                 (format nil "took ~/tempo:format-duration/" (hours 2.5)))))
  (testing "known rough edge: non-terminating fractions print as decimal expansion"
    ;; 1/3 s currently prints as a full double expansion. Recorded with a
    ;; loose prefix check; if printing switches to exact fractions (PT1/3S),
    ;; tighten this into a golden string.
    (ok (search "PT0.33" (format-duration-iso (seconds 1/3))))))
