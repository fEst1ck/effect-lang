#lang s-exp "effect.rkt"

(def reverse
  (handler [(print s k) (do (continue k #f) (print s))]))

(def abc
  (lambda (_)
    (do
      (perform print "A\n")
      (perform print "B\n")
      (perform print "C\n"))))

(with reverse (abc #f))