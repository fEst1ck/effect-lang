#lang racket

(require rackunit)

(define debug #f)

;; apply-env : Env * Name -> Value
(define (apply-env env x)
  (let ([res? (assoc x env)])
    (if res? (cdr res?)
        (error 'appy-env "~a doesn't exist in the environment" x))))

(check-equal? (apply-env '((x . 1)) 'x) 1)

(check-exn exn:fail? (lambda () (apply-env '((x . 1)) 'y)))

;; in-env? : Name * Env -> Bool
(define (in-env? x env)
  (assoc x env))

(check-not-false (in-env? 'x '((x . 1))))

(check-false (in-env? 'x '((y . 0))))

;; function closure
(struct closure
  (arg ; arg name : symbol?
   body ; body of the function : Term
   saved-env ; env
   ) #:transparent)

;; closure * value * continuation -> value
(define (apply-closure cls v cont)
  (match-define (closure arg body saved-env^) cls)
  (define saved-env (force saved-env^))
  (eval body
        (extend-env arg v saved-env)
        cont))

;; handler closure
(struct handler-closure
  (
   ;x ; x name : symbol?
   ;k ; k name : symbol?
   xks ; Listof (Pair x k)
   body ; comp : computation
   env ; env
   ) #:transparent)

;; Result of a computation
(struct return (val) #:transparent)

(struct op
  (op-name ; symbol
   op-val ; value
   cont-name ; symbol
   cont-body ; computation
   env ; env
   ) #:transparent)

;; empty-env : -> Env
(define (empty-env) '())

;; extend-env : Var * Value * Env -> Value
(define (extend-env x v env)
  (cons (cons x v) env))

;; extend-letrec-env : Name * Name * Term * Env -> Env
;; (apply (extend-letrec-env f x e env) f) = (extend-letrec-env f x e env)
(define (extend-letrec-env f x e env)
  (extend-env f (closure x e (delay (extend-letrec-env f x e env)))
              env))
  

;; □
(define (empty-stack) '())

(define stack-empty? empty?)

(define stack-top car)

(define stack-rest cdr)

(struct handler-clause (x k body env) #:transparent)

(struct handler (
                 clauses ; list of pair of op-name to handler-clause
                 ) #:transparent)

;; handler * symbol -> handler-clause | #f
;; h: a handler value
;; op-name: name of an operation
;; returns the handler-clause named op-name in h or #f if not present
(define (handles? h op-name)
  (define (iter-clauses clauses)
    (if (empty? clauses) #f
        (let* ([clause (car clauses)]
               [clause-name (car clause)])
          (if (eq? clause-name op-name)
              (cdr clause)
              (iter-clauses (cdr clauses))))))
  (iter-clauses (handler-clauses h)))

(define handler-1 (handler `((op . ,(handler-clause 'x 'k 42 (empty-env))))))

(check-equal? (handles? handler-1 'op)
              (handler-clause 'x 'k 42 (empty-env)))

(check-equal? (handles? handler-1 'oops)
              #f)

;; continuation builders

;; □
(struct end-cont () #:transparent)

;; (if □ e1 e2)
(struct if-cont (e1 e2 saved-env saved-cont) #:transparent)

;; (let (x □) e)
(struct let-cont (x e saved-env saved-cont) #:transparent)

;; (< □ e)
(struct lt-cont1 (e saved-env saved-cont) #:transparent)

;; (< v □)
(struct lt-cont2 (v saved-cont) #:transparent)

;; (+ □ e)
(struct add-cont1 (e saved-env saved-cont) #:transparent)

;; (+ v □)
(struct add-cont2 (v saved-cont) #:transparent)

;; (□ e)
(struct app-cont1 (e saved-env saved-cont) #:transparent)

;; (v □)
(struct app-cont2 (v saved-cont) #:transparent)

;; (perform op □)
(struct perform-cont (op saved-cont) #:transparent)

;; (with handler □)
(struct handler-cont
  (
   handler ; handler val
   saved-cont ; continuation
   ) #:transparent)

;; (cotinue k □)
(struct continue-cont
  (
   k ; continuation
   saved-cont)
  #:transparent)

;; symbol * continuation -> continuation * continuation * handler-caluse
;; op-name: an operation name
;; cont: a continuation
;; If cont = E1[(with-handler h [E2])]
;; where h maps op-name to clause
;; returns values: E1, (with-handler h E2), clause
(define (capture-handler op-name cont)
  (match cont
    [(end-cont) (error (format "unhandled op: ~a" op-name))]
    [(add-cont1 e saved-env saved-cont)
     (define-values (E1 E2 clause) (capture-handler op-name saved-cont))
     (values E1 (add-cont1 e saved-env E2) clause)]
    [(add-cont2 v saved-cont)
     (define-values (E1 E2 clause) (capture-handler op-name saved-cont))
     (values E1 (add-cont2 v E2) clause)]
    [(lt-cont1 e saved-env saved-cont)
     (define-values (E1 E2 clause) (capture-handler op-name saved-cont))
     (values E1 (lt-cont1 e saved-env E2) clause)]
    [(lt-cont2 v saved-cont)
     (define-values (E1 E2 clause) (capture-handler op-name saved-cont))
     (values E1 (lt-cont2 v E2) clause)]
    [(if-cont e1 e2 saved-env saved-cont)
     (define-values (E1 E2 clause) (capture-handler op-name saved-cont))
     (values E1 (if-cont e1 e2 saved-env E2) clause)]
    [(let-cont x e saved-env saved-cont)
     (define-values (E1 E2 clause) (capture-handler op-name saved-cont))
     (values E1 (let-cont x e saved-env E2) clause)]
    [(app-cont1 e2 saved-env saved-cont)
     (define-values (E1 E2 clause) (capture-handler op-name saved-cont))
     (values E1 (app-cont1 e2 saved-env E2) clause)]
    [(app-cont2 v saved-cont)
     (define-values (E1 E2 clause) (capture-handler op-name saved-cont))
     (values E1 (app-cont2 v E2) clause)]
    [(continue-cont k saved-cont)
     (define-values (E1 E2 clause) (capture-handler op-name saved-cont))
     (values E1 (continue-cont k E2) clause)]
    [(perform-cont op saved-cont)
     (define-values (E1 E2 clause) (capture-handler op-name saved-cont))
     (values E1 (perform-cont op E2) clause)]
    [(handler-cont handler saved-cont)
     (define clause? (handles? handler op-name))
     (if clause?
         (values saved-cont (handler-cont handler (end-cont)) clause?)
         (let-values ([(E1 E2 clause) (capture-handler op-name saved-cont)])
           (values E1 (handler-cont handler E2) clause)))]))

;; continuation * continuation -> continuation
;; returns cont1[cont2]
(define (compose-cont cont1 cont2)
  (match cont2
    [(end-cont) cont1]
    [(add-cont1 e saved-env saved-cont)
     (add-cont1 e saved-env (compose-cont cont1 saved-cont))]
    [(add-cont2 v saved-cont)
     (add-cont2 v (compose-cont cont1 saved-cont))]
    [(lt-cont1 e saved-env saved-cont)
     (lt-cont1 e saved-env (compose-cont cont1 saved-cont))]
    [(lt-cont2 v saved-cont)
     (lt-cont2 v (compose-cont cont1 saved-cont))]
    [(if-cont e1 e2 saved-env saved-cont)
     (if-cont e1 e2 saved-env (compose-cont cont1 saved-cont))]
    [(let-cont x e saved-env saved-cont)
     (let-cont x e saved-env (compose-cont cont1 saved-cont))]
    [(app-cont1 e2 saved-env saved-cont)
     (app-cont1 e2 saved-env (compose-cont cont1 saved-cont))]
    [(app-cont2 v saved-cont)
     (app-cont2 v (compose-cont cont1 saved-cont))]
    [(continue-cont k saved-cont)
     (continue-cont k (compose-cont cont1 saved-cont))]
    [(perform-cont op saved-cont)
     (perform-cont op (compose-cont cont1 saved-cont))]
    [(handler-cont handler saved-cont)
     (handler-cont handler (compose-cont cont1 saved-cont))]))

(define-values (E1 E2 cls)
  (capture-handler
   'op
   ;; (if (handle handler-1 (if [] 0 1)) 2 3)
   (if-cont 0 1 (empty-env)
            (handler-cont handler-1
                          (if-cont 2 3 (empty-env)
                                   (end-cont))))))

(check-equal?
 E1
 ;; (if □ 2 3)
 (if-cont 2 3 (empty-env) (end-cont)))

(check-equal?
 E2
 ;; (handle handler-1 (if [] 0 1))
 (if-cont 0 1 (empty-env) (handler-cont handler-1 (end-cont))))

(check-equal? cls (handler-clause 'x 'k 42 (empty-env)))

;; handler-clause * value * continuation * continuation -> value
(define (apply-handler-clause cls v k cont)
  (when debug (displayln (format "apply-handler-clause\n\t~a\n\t~a\n\t~a\n\t~a" cls v k cont)))
  (match-define (handler-clause arg-name k-name body saved-env) cls)
  (eval body
        (extend-env k-name k
                    (extend-env arg-name v saved-env))
        cont))

;; continuation * value -> value
(define (apply-cont cont v)
  (when debug (displayln (format "apply-cont\n\t~a\n\t~a" cont v)))
  (match cont
    [(end-cont) v]
    [(if-cont e1 e2 saved-env saved-cont)
     (if v
         (eval e1 saved-env saved-cont)
         (eval e2 saved-env saved-cont))]
    [(let-cont x e saved-env saved-cont)
     (eval e (extend-env x v saved-env) saved-cont)]
    [(add-cont1 e saved-env saved-cont)
     (eval e saved-env (add-cont2 v saved-cont))]
    [(add-cont2 v1 saved-cont)
     (apply-cont saved-cont (+ v1 v))]
    [(lt-cont1 e saved-env saved-cont)
     (eval e saved-env (lt-cont2 v saved-cont))]
    [(lt-cont2 v1 saved-cont)
     (apply-cont saved-cont (< v1 v))]
    [(continue-cont k saved-cont)
     ;(apply-cont saved-cont (apply-cont k v))]
     (apply-cont (compose-cont saved-cont k) v)]
    [(perform-cont op saved-cont)
     (define-values (E1 E2 clause) (capture-handler op saved-cont))
     (apply-handler-clause clause v E2 E1)]
    [(app-cont1 e saved-env saved-cont)
     (eval e saved-env (app-cont2 v saved-cont))]
    [(app-cont2 v1 saved-cont)
     (apply-closure v1 v saved-cont)]
    [(handler-cont handler saved-cont)
     (apply-cont saved-cont v)]
    [_ (error 'todo (format "~a" cont))]))

(define (eval e env cont)
  (when debug (displayln (format "eval\n\t~a\n\ta~a\n\t~a" e env cont)))
  (match e
    ['() (apply-cont cont '())]
    [(? boolean? v) v]
    [(? number? n) (apply-cont cont n)]
    [(? symbol? x) (apply-cont cont (apply-env env x))]
    [`(lambda (,(? symbol? x)) ,body)
     (apply-cont cont (closure x body env))]
    [`(< ,e1 ,e2)
     (eval e1 env (lt-cont1 e2 env cont))]
    [`(+ ,e1 ,e2)
     (eval e1 env (add-cont1 e2 env cont))]
    [`(if ,e1 ,e2 ,e3)
     (eval e1 env (if-cont e2 e3 env cont))]
    [`(perform ,op ,arg)
     (eval arg env (perform-cont op cont))]
    [`(let (,x ,e) ,body)
     (eval e env (let-cont x body env cont))]
    [`(letrec ((,(? symbol? f) ,(? symbol? x)) ,e) ,body)
     (eval body (extend-letrec-env f x e env) cont)]
    [`(continue ,(? symbol? k) ,e)
     (define k-val (apply-env env k))
     (eval e env (continue-cont k-val cont))]
    [`(,f ,arg)
     (eval f env (app-cont1 arg env cont))]
    [`(handle ([(,ops ,xs ,ks) ,es] ...) ,body)
     (define clauses (for/list ([op ops] [x xs] [k ks] [e es])
                       (cons op (handler-clause x k e env))))
     (define new-cont
       (handler-cont (handler clauses) cont))
     (eval body env new-cont)]
    [_ (error 'bad-syntax (format "~a" e))]))

(define (eval-closed e)
  (eval e (empty-env) (end-cont)))

(check-equal? (eval-closed 1) 1)

(check-equal? (eval-closed '(let (x 42) x)) 42)

(define t1 '(handle ([(get _ k) (continue k 1)]) (perform get ())))
(check-equal? (eval-closed t1) 1)

(define t2 '(handle ([(get _ k) (continue k 1)]) (+ (perform get ()) (perform get ()))))
(check-equal? (eval-closed t2) 2)

(define t3
  '(handle ([(get1 _ k) (continue k 1)])
           (handle ([(get2 _ k) (continue k 2)])
                   (+ (perform get1 ()) (perform get2 ())))))
(check-equal? (eval-closed t3) 3)

(define t4
  '(handle ([(get1 _ k) (continue k 1)])
           (handle ([(get2 _ k) (continue k 2)])
                   (+ (perform get2 ()) (perform get1 ())))))
(check-equal? (eval-closed t4) 3)

(define t5
  '(handle ([(throw _ k) -1])
           (+ 42 (perform throw ()))))
(check-equal? (eval-closed t5) -1)

(define test-rec
  '(letrec ((f n) (if (< 0 n) (+ n (f (+ n -1))) 0)) (f 3)))
(check-equal? (eval-closed test-rec) 6)
