#lang s-exp "../mini-effect.rkt"

(def collect
  (handler
   [(return x k) (continue k (cons x ()))]
   [(print s k)
    (let (res (continue k #f))
      (let (x (car res))
        (let (acc (cdr res))
          (cons x (cons s acc)))))]))

(def abc
  (lambda (_)
    (do
      (perform print "A")
      (perform print "B")
      (perform print "C"))))

(with collect (abc #f))

;; the result is
;; '(#f "A" "B" "C")
;; which is a pair of #f and a list ("A" "B" "C")