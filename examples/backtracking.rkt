#lang s-exp "../mini-effect.rkt"

(def choose-int
  (lambda (m)
    (lambda (n)
      (letrec [(helper m)
               (if (< n m)
                   (perform fail ())
                   (let [b (perform decide ())]
                     (if b m (helper (+ m 1)))))]
        (helper m)))))

(def is-sqr?
  (lambda (n)
    (letrec [(helper k) (if (< n k)
                            #f
                            (if (= (* k k) n)
                                #t
                                (helper (+ k 1))))]
      (helper 0))))

(def pythagorean
  (lambda (m)
    (lambda (n)
      (let [a ((choose-int m) (+ n -1))]
        (let [b ((choose-int (+ a 1)) n)]
          (if (is-sqr? (+ (* a a) (* b b)))
              (cons a b)
              (perform fail ())))))))

(def backtrack
  (handler [(decide _ k)
            (with (handler [(fail _ _) (continue k #f)])
                  (continue k #t))]))

(with backtrack ((pythagorean 4) 15))
;; '(5 . 12)