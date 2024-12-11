#lang typed/racket

(provide (all-defined-out))

(module+ test
  (require typed/rackunit))

(define debug #f)

(define-type Name Symbol)

;; Environment

(define-type Env (Listof (Pair Name Any)))

;; empty-env : -> Env
;; creates an empty environment
(define (empty-env) '())

;; extend-env : Name * Value * Env -> Value
;; extends an environment with a name and value
(: extend-env (-> Name Any Env Env))
(define (extend-env x v env)
  (cons (cons x v) env))

;; extend-letrec-env : Name * Name * Term * Env -> Env
;; (apply (extend-letrec-env f x e env) f) = (extend-letrec-env f x e env)
(: extend-letrec-env (-> Name Name Term Env Env))
(define (extend-letrec-env f x e env)
  (extend-env f (closure x e (delay (extend-letrec-env f x e env)))
              env))

;; apply-env : Env * Name -> Value
(: apply-env (-> Env Name Value))
(define (apply-env env x)
  (let ([res? (assoc x env)])
    (if res? (cdr res?)
        (error 'appy-env "~a doesn't exist in the environment" x))))

(module+ test
  (check-equal? (apply-env '((x . 1)) 'x) 1)
  (check-exn exn:fail? (lambda () (apply-env '((x . 1)) 'y))))

;; in-env? : Name * Env -> Bool
(: in-env? (-> Name Env Boolean))
(define (in-env? x env)
  (if (assoc x env) #t #f))

(module+ test
  (check-not-false (in-env? 'x '((x . 1))))
  (check-false (in-env? 'x '((y . 0)))))

;; Closure

;; function closure
(struct closure
  ([arg : Name]
   [body : Term]
   [saved-env : (U Env (Promise Env))])
  #:transparent)

;; closure * value * continuation -> value
(: apply-closure (-> closure Value Cont Value))
(define (apply-closure cls v cont)
  (match-define (closure arg body saved-env^) cls)
  (define saved-env (if (promise? saved-env^) (force saved-env^) saved-env^))
  (eval body
        (extend-env arg v saved-env)
        cont))

;; [(x k) body]
(struct handler-clause
  ([x : Name]
   [k : Name]
   [body : Term]
   [env : Env])
  #:transparent)

;; ([(x k) body] ...)
(struct handler
  ([clauses : (Listof (Pair Name handler-clause))])
  #:transparent)

;; handler * symbol -> handler-clause | #f
;; h: a handler value
;; op-name: name of an operation
;; returns the handler-clause named op-name in h or #f if not present
(: handles? (-> handler Name (Option handler-clause)))
(define (handles? h op-name)
  (let iter-clauses ([clauses (handler-clauses h)])
    (if (empty? clauses) #f
        (let* ([clause (car clauses)]
               [clause-name (car clause)])
          (if (eq? clause-name op-name)
              (cdr clause)
              (iter-clauses (cdr clauses)))))))

(define handler-1 (handler `((op . ,(handler-clause 'x 'k 42 (empty-env))))))

(module+ test
  (check-equal? (handles? handler-1 'op)
                (handler-clause 'x 'k 42 (empty-env)))

  (check-equal? (handles? handler-1 'oops)
                #f))

(define-type Term
  (U Number
     Boolean
     String
     Symbol
     (Listof Term)))

(define-type Value Any)

;; Continuation Builder

(define-type Cont
  (U end-cont
     if-cont
     let-cont
     print-cont
     car-cont
     cdr-cont
     lt-cont1
     lt-cont2
     eq-cont1
     eq-cont2
     add-cont1
     add-cont2
     mul-cont1
     mul-cont2
     cons-cont1
     cons-cont2
     app-cont1
     app-cont2
     perform-cont
     with-cont1
     with-cont2
     handler-cont
     continue-cont))

;; □
(struct end-cont () #:transparent)

;; (if □ e1 e2)
(struct if-cont ([e1 : Term] [e2 : Term] [saved-env : Env] [saved-cont : Cont]) #:transparent)

;; (let (x □) e)
(struct let-cont ([x : Name] [e : Term] [saved-env : Env] [saved-cont : Cont]) #:transparent)

;; (print □)
(struct print-cont ([saved-cont : Cont]) #:transparent)

;; (car □)
(struct car-cont ([saved-cont : Cont]) #:transparent)

;; (cdr □)
(struct cdr-cont ([saved-cont : Cont]) #:transparent)

;; (< □ e)
(struct lt-cont1 ([e : Term] [saved-env : Env] [saved-cont : Cont]) #:transparent)

;; (< v □)
(struct lt-cont2 ([v : Value] [saved-cont : Cont]) #:transparent)

;; (= □ e)
(struct eq-cont1 ([e : Term] [saved-env : Env] [saved-cont : Cont]) #:transparent)

;; (= v □)
(struct eq-cont2 ([v : Value] [saved-cont : Cont]) #:transparent)

;; (+ □ e)
(struct add-cont1 ([e : Term] [saved-env : Env] [saved-cont : Cont]) #:transparent)

;; (+ v □)
(struct add-cont2 ([v : Value] [saved-cont : Cont]) #:transparent)

;; (* □ e)
(struct mul-cont1 ([e : Term] [saved-env : Env] [saved-cont : Cont]) #:transparent)

;; (* v □)
(struct mul-cont2 ([v : Value] [saved-cont : Cont]) #:transparent)

;; (cons □ e)
(struct cons-cont1 ([e : Term] [saved-env : Env] [saved-cont : Cont]) #:transparent)

;; (cons v □)
(struct cons-cont2 ([v : Value] [saved-cont : Cont]) #:transparent)

;; (□ e)
(struct app-cont1 ([e : Term] [saved-env : Env] [saved-cont : Cont]) #:transparent)

;; (v □)
(struct app-cont2 ([v : Value] [saved-cont : Cont]) #:transparent)

;; (perform op □)
(struct perform-cont ([op : Name] [saved-cont : Cont]) #:transparent)

;; (with □ e)
(struct with-cont1
  ([e : Term]
   [saved-env : Env]
   [saved-cont : Cont])
  #:transparent)

;; (with v □)
(struct with-cont2
  ([v : Value]
   [saved-cont : Cont])
  #:transparent)

;; (handle handler □)
(struct handler-cont
  ([handler : handler]
   [saved-cont : Cont])
  #:transparent)

;; (cotinue k □)
(struct continue-cont
  (
   [k : Cont]
   [saved-cont : Cont])
  #:transparent)

;; symbol * continuation -> continuation * continuation * handler-caluse
;; op-name: an operation name
;; cont: a continuation
;; If cont = E1[(with-handler h [E2])]
;; where h maps op-name to clause
;; returns values: E1, (with-handler h E2), clause
(: capture-handler (-> Name Cont (Values Cont Cont handler-clause)))
(define (capture-handler op-name cont)
  (match cont
    [(end-cont) (error (format "unhandled op: ~a" op-name))]
    [(print-cont saved-cont)
     (define-values (E1 E2 clause) (capture-handler op-name saved-cont))
     (values E1 (print-cont E2) clause)]
    [(car-cont saved-cont)
     (define-values (E1 E2 clause) (capture-handler op-name saved-cont))
     (values E1 (car-cont E2) clause)]
    [(cdr-cont saved-cont)
     (define-values (E1 E2 clause) (capture-handler op-name saved-cont))
     (values E1 (cdr-cont E2) clause)]
    [(add-cont1 e saved-env saved-cont)
     (define-values (E1 E2 clause) (capture-handler op-name saved-cont))
     (values E1 (add-cont1 e saved-env E2) clause)]
    [(add-cont2 v saved-cont)
     (define-values (E1 E2 clause) (capture-handler op-name saved-cont))
     (values E1 (add-cont2 v E2) clause)]
    [(mul-cont1 e saved-env saved-cont)
     (define-values (E1 E2 clause) (capture-handler op-name saved-cont))
     (values E1 (mul-cont1 e saved-env E2) clause)]
    [(mul-cont2 v saved-cont)
     (define-values (E1 E2 clause) (capture-handler op-name saved-cont))
     (values E1 (mul-cont2 v E2) clause)]
    [(lt-cont1 e saved-env saved-cont)
     (define-values (E1 E2 clause) (capture-handler op-name saved-cont))
     (values E1 (lt-cont1 e saved-env E2) clause)]
    [(lt-cont2 v saved-cont)
     (define-values (E1 E2 clause) (capture-handler op-name saved-cont))
     (values E1 (lt-cont2 v E2) clause)]
    [(eq-cont1 e saved-env saved-cont)
     (define-values (E1 E2 clause) (capture-handler op-name saved-cont))
     (values E1 (eq-cont1 e saved-env E2) clause)]
    [(eq-cont2 v saved-cont)
     (define-values (E1 E2 clause) (capture-handler op-name saved-cont))
     (values E1 (eq-cont2 v E2) clause)]
    [(cons-cont1 e saved-env saved-cont)
     (define-values (E1 E2 clause) (capture-handler op-name saved-cont))
     (values E1 (cons-cont1 e saved-env E2) clause)]
    [(cons-cont2 v saved-cont)
     (define-values (E1 E2 clause) (capture-handler op-name saved-cont))
     (values E1 (cons-cont2 v E2) clause)]
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
    [(with-cont1 e saved-env saved-cont)
     (define-values (E1 E2 clause) (capture-handler op-name saved-cont))
     (values E1 (with-cont1 e saved-env E2) clause)]
    [(with-cont2 v saved-cont)
     (define h (cast v handler))
     (define clause? (handles? h op-name))
     (if clause?
         (values saved-cont (handler-cont h (end-cont)) clause?)
         (let-values ([(E1 E2 clause) (capture-handler op-name saved-cont)])
           (values E1 (handler-cont h E2) clause)))]
    [(handler-cont handler saved-cont)
     (define clause? (handles? handler op-name))
     (if clause?
         (values saved-cont (handler-cont handler (end-cont)) clause?)
         (let-values ([(E1 E2 clause) (capture-handler op-name saved-cont)])
           (values E1 (handler-cont handler E2) clause)))]))

;; continuation * continuation -> continuation
;; returns cont1[cont2]
(: compose-cont (-> Cont Cont Cont))
(define (compose-cont cont1 cont2)
  (match cont2
    [(end-cont) cont1]
    [(print-cont saved-cont)
     (print-cont (compose-cont cont1 saved-cont))]
    [(car-cont saved-cont)
     (car-cont (compose-cont cont1 saved-cont))]
    [(cdr-cont saved-cont)
     (cdr-cont (compose-cont cont1 saved-cont))]
    [(add-cont1 e saved-env saved-cont)
     (add-cont1 e saved-env (compose-cont cont1 saved-cont))]
    [(add-cont2 v saved-cont)
     (add-cont2 v (compose-cont cont1 saved-cont))]
    [(mul-cont1 e saved-env saved-cont)
     (mul-cont1 e saved-env (compose-cont cont1 saved-cont))]
    [(mul-cont2 v saved-cont)
     (mul-cont2 v (compose-cont cont1 saved-cont))]
    [(lt-cont1 e saved-env saved-cont)
     (lt-cont1 e saved-env (compose-cont cont1 saved-cont))]
    [(lt-cont2 v saved-cont)
     (lt-cont2 v (compose-cont cont1 saved-cont))]
    [(eq-cont1 e saved-env saved-cont)
     (eq-cont1 e saved-env (compose-cont cont1 saved-cont))]
    [(eq-cont2 v saved-cont)
     (eq-cont2 v (compose-cont cont1 saved-cont))]
    [(cons-cont1 e saved-env saved-cont)
     (cons-cont1 e saved-env (compose-cont cont1 saved-cont))]
    [(cons-cont2 v saved-cont)
     (cons-cont2 v (compose-cont cont1 saved-cont))]
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
    [(with-cont1 e saved-env saved-cont)
     (with-cont1 e saved-env (compose-cont cont1 saved-cont))]
    [(with-cont2 v saved-cont)
     (with-cont2 v (compose-cont cont1 saved-cont))]
    [(handler-cont handler saved-cont)
     (handler-cont handler (compose-cont cont1 saved-cont))]))

(module+ test
  (test-begin
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

   (check-equal? cls (handler-clause 'x 'k 42 (empty-env)))))



;; handler-clause * value * continuation * continuation -> value
(: apply-handler-clause (-> handler-clause Value Cont Cont Value))
(define (apply-handler-clause cls v k cont)
  (when debug (displayln (format "apply-handler-clause\n\t~a\n\t~a\n\t~a\n\t~a" cls v k cont)))
  (match-define (handler-clause arg-name k-name body saved-env) cls)
  (eval body
        (extend-env k-name k
                    (extend-env arg-name v saved-env))
        cont))

;; continuation * value -> value
(: apply-cont (-> Cont Value Value))
(define (apply-cont cont v)
  (when debug (displayln (format "apply-cont\n\t~a\n\t~a" cont v)))
  (match cont
    [(end-cont) v]
    [(print-cont saved-cont)
     (display v)
     (apply-cont saved-cont #f)]
    [(car-cont saved-cont) (apply-cont saved-cont (if (pair? v) (car v) (error 'type-error (format "car: expected list, got ~a" v))))]
    [(cdr-cont saved-cont) (apply-cont saved-cont (if (pair? v) (cdr v) (error 'type-error (format "cdr: expected list, got ~a" v))))]
    [(if-cont e1 e2 saved-env saved-cont)
     (if v
         (eval e1 saved-env saved-cont)
         (eval e2 saved-env saved-cont))]
    [(let-cont x e saved-env saved-cont)
     (eval e (extend-env x v saved-env) saved-cont)]
    [(add-cont1 e saved-env saved-cont)
     (eval e saved-env (add-cont2 v saved-cont))]
    [(add-cont2 v1 saved-cont)
     (if (and (number? v1) (number? v))
         (apply-cont saved-cont (+ v1 v))
         (error 'type-error (format "~a + ~a: expected numbers" v1 v)))]
    [(mul-cont1 e saved-env saved-cont)
     (eval e saved-env (mul-cont2 v saved-cont))]
    [(mul-cont2 v1 saved-cont)
     (if (and (number? v1) (number? v))
         (apply-cont saved-cont (* v1 v))
         (error 'type-error (format "~a * ~a: expected numbers" v1 v)))]
    [(lt-cont1 e saved-env saved-cont)
     (eval e saved-env (lt-cont2 v saved-cont))]
    [(lt-cont2 v1 saved-cont)
     (if (and (integer? v1) (integer? v))
         (apply-cont saved-cont (< v1 v))
         (error 'type-error (format "~a < ~a: expected integers" v1 v)))]
    [(eq-cont1 e saved-env saved-cont)
     (eval e saved-env (eq-cont2 v saved-cont))]
    [(eq-cont2 v1 saved-cont)
     (if (and (integer? v1) (integer? v))
         (apply-cont saved-cont (= v1 v))
         (error 'type-error (format "~a = ~a: expected integers" v1 v)))]
    [(cons-cont1 e saved-env saved-cont)
     (eval e saved-env (cons-cont2 v saved-cont))]
    [(cons-cont2 v1 saved-cont)
     (apply-cont saved-cont (cons v1 v))]
    [(continue-cont k saved-cont)
     ;(apply-cont saved-cont (apply-cont k v))]
     (apply-cont (compose-cont saved-cont k) v)]
    [(perform-cont op saved-cont)
     (define-values (E1 E2 clause) (capture-handler op saved-cont))
     (apply-handler-clause clause v E2 E1)]
    [(app-cont1 e saved-env saved-cont)
     (eval e saved-env (app-cont2 v saved-cont))]
    [(app-cont2 v1 saved-cont)
     (apply-closure (cast v1 closure) v saved-cont)]
    [(with-cont1 e saved-env saved-cont)
     (eval e saved-env (with-cont2 v saved-cont))]
    [(with-cont2 _handler saved-cont)
     (apply-cont saved-cont v)]
    [(handler-cont _handler saved-cont)
     (apply-cont saved-cont v)]
    [_ (error 'todo (format "~a" cont))]))

;; core interpreter logic
(: eval (-> Term Env Cont Value))
(define (eval e env cont)
  (when debug (displayln (format "eval\n\t~a\n\t~a\n\t~a" e env cont)))
  (match e
    ['() (apply-cont cont '())]
    [(? boolean? v) (apply-cont cont v)]
    [(? number? n) (apply-cont cont n)]
    [(? string? s) (apply-cont cont s)]
    [(? symbol? x) (apply-cont cont (apply-env env x))]
    [`(lambda (,(? symbol? x)) ,body)
     (apply-cont cont (closure x body env))]
    [`(print ,e)
     (eval e env (print-cont cont))]
    [`(car ,e)
     (eval e env (car-cont cont))]
    [`(cdr ,e)
     (eval e env (cdr-cont cont))]
    [`(< ,e1 ,e2)
     (eval e1 env (lt-cont1 e2 env cont))]
    [`(= ,e1 ,e2)
     (eval e1 env (eq-cont1 e2 env cont))]
    [`(+ ,e1 ,e2)
     (eval e1 env (add-cont1 e2 env cont))]
    [`(* ,e1 ,e2)
     (eval e1 env (mul-cont1 e2 env cont))]
    [`(cons ,e1 ,e2)
     (eval e1 env (cons-cont1 e2 env cont))]
    [`(if ,e1 ,e2 ,e3)
     (eval e1 env (if-cont e2 e3 env cont))]
    [`(perform ,(? symbol? op) ,arg)
     (eval arg env (perform-cont op cont))]
    [`(let (,(? symbol? x) ,e) ,body)
     (eval e env (let-cont x body env cont))]
    [`(letrec ((,(? symbol? f) ,(? symbol? x)) ,e) ,body)
     (eval body (extend-letrec-env f x e env) cont)]
    [`(continue ,(? symbol? k) ,e)
     (define k-val (apply-env env k))
     (eval e env (continue-cont (cast k-val Cont) cont))]
    [`(handler [(,(? symbol? ops) ,(? symbol? xs) ,(? symbol? ks)) ,es] ...)
     (: clauses (Listof (Pair Symbol handler-clause)))
     (define clauses (for/list ([op (cast ops (Listof Symbol))] [x (cast xs (Listof Symbol))] [k (cast ks (Listof Symbol))] [e (cast es (Listof Term))])
                       (cons op (handler-clause x k e env))))
     (apply-cont cont (handler clauses))]
    [`(,f ,arg)
     (eval f env (app-cont1 arg env cont))]
    [`(with ,h ,e) (eval h env (with-cont1 e env cont))]
    [`(handle ([(,(? symbol? ops) ,(? symbol? xs) ,(? symbol? ks)) ,es] ...) ,body)
     (: clauses (Listof (Pair Symbol handler-clause)))
     (define clauses (for/list ([op (cast ops (Listof Symbol))] [x (cast xs (Listof Symbol))] [k (cast ks (Listof Symbol))] [e (cast es (Listof Term))])
                       (cons op (handler-clause x k e env))))
     (define new-cont
       (handler-cont (handler clauses) cont))
     (eval body env new-cont)]
    [_ (error 'bad-syntax (format "~a" e))]))

(: eval-closed (-> Term Value))
(define (eval-closed e)
  (eval e (empty-env) (end-cont)))

;; sanity checks

(module+ test
  (check-equal? (eval-closed 1) 1)

  (check-equal? (eval-closed '(+ 1 1)) 2)

  (check-equal? (eval-closed '(* 2 3)) 6)

  (check-equal? (eval-closed '(< 1 2)) #t)

  (check-equal? (eval-closed '(< 2 1)) #f)

  (check-equal? (eval-closed '(= 1 1)) #t)

  (check-equal? (eval-closed '(= 2 1)) #f)

  (check-equal? (eval-closed #t) #t)

  (check-equal? (eval-closed '(let (x 42) x)) 42)

  (check-equal? (eval-closed '(cons 1 ())) (cons 1 '()))

  (check-equal? (eval-closed '(car (cons #t #f))) #t)

  (test-begin
   (define test-rec
     '(letrec ((f n) (if (< 0 n) (+ n (f (+ n -1))) 0)) (f 3)))
   (check-equal? (eval-closed test-rec) 6))

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

  ;; examples from An Introduction to Algebriac Effect Handlers by Matija Pretnar
  (define print-full-name
    `(let (_ (peform print "What is your forename?"))
       (let (forename (perform read ()))
         (let (_ (perform (print "What is your surname?")))
           (let (surname (perform read ()))
             (let (_ (perform print forename))
               (let (_ (perform print surname))
                 (perform return ()))))))))

  (define choose
    '(lambda (x)
       (lambda (y)
         (let (b (perform decide #f))
           (if b x y)))))

  (define choose-sum
    `(let (choose ,choose)
       (let (x ((choose 15) 30))
         (let (y ((choose 5) 10))
           (+ x y)))))

  (define pick-true
    '([(decide _ k) (continue k #t)]))

  (check-equal? (eval-closed `(handle ,pick-true ((,choose 1) 2))) 1)

  (check-equal? (eval-closed `(handle ,pick-true ,choose-sum)) 20)

  (define pick-max
    '([(decide _ k)
       (let [t (continue k #t)]
         (let [f (continue k #f)]
           (if (< t f) f t)))]))

  (check-equal? (eval-closed `(handle ,pick-max ,choose-sum)) 40)

  (define choose-int
    '(lambda (m)
       (lambda (n)
         (letrec [(helper m)
                  (if (< n m)
                      (perform fail ())
                      (let [b (perform decide ())]
                        (if b m (helper (+ m 1)))))]
           (helper m)))))

  (define is-sqr?
    '(lambda (n)
       (letrec ([helper k] (if (< n k)
                               #f
                               (if (= (* k k) n)
                                   #t
                                   (helper (+ k 1)))))
         (helper 0))))

  (check-equal? (eval-closed `(,is-sqr? 3)) #f)

  (check-equal? (eval-closed `(,is-sqr? 4)) #t)

  (define pythagorean
    `(lambda (m)
       (lambda (n)
         (let [a ((,choose-int m) (+ n -1))]
           (let [b ((,choose-int (+ a 1)) n)]
             (if (,is-sqr? (+ (* a a) (* b b)))
                 (cons a b)
                 (perform fail ())))))))

  (define backtrack
    '([(decide _ k)
       (handle
        ([(fail _ _) (continue k #f)])
        (continue k #t))]))

  (check-equal? (eval-closed `(handle ,backtrack ((,pythagorean 4) 15))) (cons 5 12))

  (define state
    '([(get _ k) (lambda (s) ((continue k s) s))]
      [(set s k) (lambda (_) ((continue k ()) s))]
      [(return x _) (lambda (_) x)]))

  (define push-42
    `((handle ,state
              (perform return
                       (let [s (perform get ())]
                         (let [_ (perform set (cons 42 s))]
                           (perform get ()))))) ()))

  (check-equal? (eval-closed push-42) (cons 42 '())))