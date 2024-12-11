#lang s-exp "mini-effect.rkt"

(def always-read
  (lambda (s)
    (handler [(read _ k) (continue k s)])))

(def always-bob
  (always-read "Bob"))

(def print-full-name
  (lambda (_)
    (do
      (print "What is your forename?\n")
      (let (forename (perform read #f))
        (do
          (print "What is your surname?\n")
          (let (surname (perform read #f))
            (do
              (print forename)
              (print " ")
              (print surname))))))))

(with always-bob (print-full-name #f))