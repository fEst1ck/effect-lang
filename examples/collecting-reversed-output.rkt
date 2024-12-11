#lang s-exp "../mini-effect.rkt"

(def reverse
  (handler [(print s k) (do (continue k #f) (perform print s))]))

(def collect
  (handler
   [(return x k) (continue k (cons x ()))]
   [(print s k)
    (let (res (continue k #f))
      (let (x (car res))
        (let (acc (cdr res))
          (cons x (cons s acc)))))]))

(def abc1
  (lambda (_)
    (do
      (perform print "A")
      (perform print "B")
      (perform print "C"))))

(def abc
  (lambda (_)
   (do
      (perform print "A"))))

(with collect (with reverse (abc1 #f)))