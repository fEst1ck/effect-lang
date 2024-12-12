#lang s-exp "../mini-effect.rkt"

(def state
  (handler
    [(get _ k) (lambda (s) ((continue k s) s))]
    [(set s k) (lambda (_) ((continue k #f) s))]
	[(return x _) (lambda (_) x)]))

((with state
  (let [x (perform get #f)]
    (let [_ (perform set 42)]
	  (+ x (perform get #f)))))
  1)
;; 43