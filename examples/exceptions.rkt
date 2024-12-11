#lang s-exp "mini-effect.rkt"

(def default
  (lambda (x)
    (handler [(raise _ _) x])))

(with (default 42)
  (+ 1 (* (perform raise #f) 3)))