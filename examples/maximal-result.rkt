#lang s-exp "../mini-effect.rkt"

(def choose
  (lambda (x)
    (lambda (y)
      (let (b (perform decide #f))
        (if b x y)))))

(def choose-sum
  (lambda (_) (let (choose choose)
                (let (x ((choose 15) 30))
                  (let (y ((choose 5) 10))
                    (+ x y))))))

(def pick-true
  (handler
   [(decide _ k) (let [t (continue k #t)]
                   (let [f (continue k #f)]
                     (if (< t f) f t)))]))

(with pick-true (choose-sum #f))
;; 40