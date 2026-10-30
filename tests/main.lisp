(defpackage tempo/tests/main
  (:use :cl
        :tempo
        :rove))
(in-package :tempo/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :tempo)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
