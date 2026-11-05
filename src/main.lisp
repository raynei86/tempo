(uiop:define-package #:tempo
  (:use #:cl)
  (:use-reexport #:tempo.conditions
                 #:tempo.duration
                 #:tempo.date
                 #:tempo.wall-time))

(in-package #:tempo)
