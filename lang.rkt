#lang typed/racket

(provide eval-closed type-check-closed)

(module+ test
  (require typed/rackunit))

(define debug #f)

(define-type Name Symbol)

;; Environment
;;
;; An environment is a representation of a map from names to values.

(define-type Env (Listof (Pair Name Any)))

;; Creates an empty environment.
(define (empty-env) '())

;; Extends an environment with a name and value.
(: extend-env (-> Name Any Env Env))
(define (extend-env x v env)
  (cons (cons x v) env))

;; Extends an environment with a recursive function.
;; (apply (extend-letrec-env f x e env) f) = (extend-letrec-env f x e env)
(: extend-letrec-env (-> Name Name Term Env Env))
(define (extend-letrec-env f x e env)
  (extend-env f (closure x e (delay (extend-letrec-env f x e env)))
              env))

;; Returns the value of a key.
(: apply-env (-> Env Name Value))
(define (apply-env env x)
  (let ([res? (assoc x env)])
    (if res? (cdr res?)
        (error 'appy-env "~a doesn't exist in the environment" x))))

(module+ test
  (check-equal? (apply-env '((x . 1)) 'x) 1)
  (check-exn exn:fail? (lambda () (apply-env '((x . 1)) 'y))))

;; Tests the existence of a key.
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

(module+ test
  (define handler-1 (handler `((op . ,(handler-clause 'x 'k 42 (empty-env))))))
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
  ([k : Cont]
   [saved-cont : Cont])
  #:transparent)

;; op-name: an operation name
;; cont: a continuation
;; If cont = E1[(with-handler h [E2])]
;; where h is the innermost handler that maps op-name to clause
;; returns values: E1, (with-handler h E2), clause
;; note that (with-handler h E2) is the captured continuation
;; which later binds to the `k` in the handler clause
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

;; Given cont1, cont2, returns cont1[cont2]
;; (apply-cont (compose-cont cont1 cont2) value) = (apply-cont cont1 (apply-cont cont2 value))
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

(: apply-handler-clause (-> handler-clause Value Cont Cont Value))
(define (apply-handler-clause cls v k cont)
  (when debug (displayln (format "apply-handler-clause\n\t~a\n\t~a\n\t~a\n\t~a" cls v k cont)))
  (match-define (handler-clause arg-name k-name body saved-env) cls)
  (eval body
        (extend-env k-name k
                    (extend-env arg-name v saved-env))
        cont))

;; Given a continuation and a value, completes the computation.
(: apply-cont (-> Cont Value Value))
(define (apply-cont cont v)
  (when debug (displayln (format "apply-cont\n\tcont: ~a\n\tvalue: ~a" cont v)))
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
    [(with-cont2 h saved-cont)
      (define clause? (handles? (cast h handler) 'return))
      (if clause? (apply-handler-clause clause? v (end-cont) saved-cont)
        (apply-cont saved-cont v))] 
    [(handler-cont handler saved-cont)
      (define clause? (handles? handler 'return))
      (if clause? (apply-handler-clause clause? v (end-cont) saved-cont)
        (apply-cont saved-cont v))] 
    [_ (error 'todo (format "~a" cont))]))

;; Core interpreter logic
;; Evaluates a term in the given environment and continuation.
(: eval (-> Term Env Cont Value))
(define (eval e env cont)
  (when debug (displayln (format "eval\n\te: ~a\n\tenv: ~a\n\tcont: ~a" e env cont)))
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
    [`(do) (apply-cont cont #f)]
    [`(do ,e1 ,e2 ...)
      (eval `(let (_ ,e1) (do . ,e2)) env cont)]
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

;; Evaluates a closed term in the empty environment and continuation.
(: eval-closed (-> Term Value))
(define (eval-closed e)
  (eval e (empty-env) (end-cont)))

;; Testing the interpreter
(module+ test
  ;; sanity checks for simple things
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

(define-type Type
  (U Symbol
     (Listof Type)))

;; Checks that in context `ctx`, term `e` has type `type`
;; returns false or raise exceptions if type check fails
(: type-check (-> Env Term Type Boolean))
(define (type-check ctx e type)
  (match e
    ['()
     (match type
       ;; actually need to check _ is a valid type
       [`(Listof ,_) true]
       [#t false])]
    [(? boolean? v)
     (or
      (eq? type 'Boolean)
      ;; assuming well-formedness of types
      (match type
        [`(! Boolean ,s) #t]
        [_ #f]))]
    [(? number? n)
     (or
      (eq? type 'Number)
      (match type
        [`(! Number ,s) #t]
        [_ #f]))]
    [(? string? s)
     (or
      (eq? type 'String)
      (match type
        [`(! String ,s) #t]
        [_ #f]))]
    [(? symbol? x) (equal? (apply-env ctx x) type)]
    [`(lambda (,(? symbol? x)) ,body)
     (match type
       [`(-> ,A ,B) (type-check (extend-env x A ctx) body B)]
       [_ #f])]
    [`(print ,e)
     (and
      (eq? type 'Boolean)
      (type-check ctx e 'String))]
;;     [`(car ,e)
;;      (eval e env (car-cont cont))]
;;     [`(cdr ,e)
;;      (eval e env (cdr-cont cont))]
    [`(< ,e1 ,e2)
     (and
      (type-check ctx e1 'Number)
      (type-check ctx e2 'Number))]
    [`(= ,e1 ,e2)
     (and
      (type-check ctx e1 'Number)
      (type-check ctx e2 'Number))]
    [`(+ ,e1 ,e2)
     (if (equal? type 'Number)
     (and
      (type-check ctx e1 'Number)
      (type-check ctx e2 'Number))
     (match type
       [`(! Number ,sigs)
        ;; TODO: we hve to do this fix to other operators
        (and
         (type-check ctx e1 type)
         (type-check ctx e2 type))]))]
    [`(* ,e1 ,e2)
     (and
      (type-check ctx e1 'Number)
      (type-check ctx e2 'Number))]
;;     [`(cons ,e1 ,e2)
;;      (eval e1 env (cons-cont1 e2 env cont))]
    [`(if ,e1 ,e2 ,e3)
     (and
      (type-check ctx e1 'Boolean)
      (type-check ctx e2 type)
      (type-check ctx e3 type))]
    [`(perform ,(? symbol? op) ,arg)
     (match type
       [`(! ,t ,sigs)
        (match (cast (apply-env (cast sigs Env) op) Type)
          [`(-> ,A ,B)
           (and
            (equal? t B)
            (type-check ctx arg A))])])]
    [`(let ([: ,(? symbol? x) ,A] ,e) ,body)
     (and
      (type-check ctx e (cast A Type))
      (type-check (extend-env x A ctx) body type))]
    [`(do) (eq? type 'Boolean)]
    [`(do ,e1 ,e2 ...)
      (type-check ctx `(let ([: _ Boolean] ,e1) (do . ,e2)) type)]
;;     [`(letrec ((,(? symbol? f) ,(? symbol? x)) ,e) ,body)
;;      (eval body (extend-letrec-env f x e env) cont)]
    [`(continue ,(? symbol? k) ,e)
      (match (apply-env ctx k)
        [`(Continuation ,A ,B)
         (and
          (equal? B type)
          (type-check ctx e (cast A Type)))])]
    [`(handler [(,(? symbol? ops) ,(? symbol? xs) ,(? symbol? ks)) ,es] ...)
     (match type
       [`(=> (! ,A ,sigs) ,D)
         (define clauses (for/list : (Listof (Pair Symbol (List Name Name Term)))
                           ([op : Symbol (cast ops (Listof Symbol))] [x : Symbol (cast xs (Listof Symbol))] [k (cast ks (Listof Symbol))] [e (cast es (Listof Term))])
                           (list op x k e)))
         (for/fold ([res #t])
                   ([op-clause clauses])
           (and res
                (let* ([op (car op-clause)]
                      [clause (cdr op-clause)]
                      [op-ty (apply-env (cast sigs Env) (cast op Symbol))])
                  (match op-ty
                    [`(-> ,X ,Y)
                     (match clause
                       [(list x k e)
                        (type-check (extend-env (cast x Symbol) X
                          (extend-env (cast k Symbol) `(Continuation ,Y ,D) ctx))
                                    e D)])]))))])]
    [`([: ,f ,A] ,arg)
     (and
      (type-check ctx f (cast A Type))
      (match A
        [`(-> ,X ,Y)
         (and
          (equal? Y type)
          (type-check ctx arg (cast X Type)))]))]
    [`(with [: ,h ,ct] ,e)
     (and
      (type-check ctx h (cast ct Type))
      (match ct
        [`(=> ,C ,D)
         (and
          (equal? D type)
          (type-check ctx e (cast C Type)))]))]
         
;;     [`(handle ([(,(? symbol? ops) ,(? symbol? xs) ,(? symbol? ks)) ,es] ...) ,body)
;;      (: clauses (Listof (Pair Symbol handler-clause)))
;;      (define clauses (for/list ([op (cast ops (Listof Symbol))] [x (cast xs (Listof Symbol))] [k (cast ks (Listof Symbol))] [e (cast es (Listof Term))])
;;                        (cons op (handler-clause x k e env))))
;;      (define new-cont
;;        (handler-cont (handler clauses) cont))
;;      (eval body env new-cont)]
    [_ (error 'bad-syntax (format "~a" e))]))

;; Type checks a closed term
(: type-check-closed (-> Term Type Boolean))
(define (type-check-closed expr type)
  (type-check (empty-env) expr type))

;; Testing the type checker
(module+ test
  (check-true (type-check-closed #t 'Boolean))
  (check-true (type-check-closed #t '(! Boolean ((op1 . (-> String Boolean)) (op2 . (-> Number Number))))))
  (check-false (type-check-closed #f 'Number))
  (check-true (type-check-closed '(perform foo 1) '(! Boolean ((foo . (-> Number Boolean))))))
  (check-exn exn:fail? (lambda () (type-check-closed '(perform faa 1) '(! Boolean ((foo . (-> Number Boolean)))))))
  (check-true (type-check-closed
   '(handler [(raise _ _) 42])
   '(=> (! String ((raise . (-> Boolean Number))))
        Number)))
  (check-true (type-check-closed
   '(handler [(raise _ _) 42])
   '(=> (! String ((raise . (-> Boolean Number))))
        ;; types don't have to be precise
        (! Number ((raise . (-> Boolean Number)))))))
  (check-false (type-check-closed
   '(handler [(raise _ _) 42])
   '(=> (! String ((raise . (-> Boolean Number))))
        (! Boolean ((raise . (-> Boolean Number)))))))
  (check-false (type-check-closed
   '(handler [(print x k) (do (continue k #f) (print x))])
   '(=> (! Boolean ((print . (-> String Boolean))))
        (! Boolean ((print . (-> String Boolean)))))))
  (check-true (type-check-closed
   '(handler [(print x k) (do (continue k #f) (print x))])
   '(=> (! Boolean ((print . (-> String Boolean))))
        Boolean)))
  (check-true (type-check-closed
  '(handler
    [(get _ k) (lambda (s) ([: (continue k s) (-> String Number)] s))]
    [(set s k) (lambda (_) ([: (continue k #f) (-> String Number)] s))]
    [(return x _) (lambda (_) x)])
  '(=> (! Number ((get . (-> Boolean String))
                  (set . (-> String Boolean))
                  (return . (-> Number Number))))
       (-> String Number))))
  (check-true (type-check-closed
  '(handler
    [(get _ k) (lambda (s) ([: (continue k s) (-> Number Number)] s))]
    [(set s k) (lambda (_) ([: (continue k #f) (-> Number Number)] s))]
    [(return x _) (lambda (_) x)])
  '(=> (! Number ((get . (-> Boolean Number))
                  (set . (-> Number Boolean))
                  (return . (-> Number Number))))
       (-> Number Number))))
  (check-false (type-check-closed
  '(handler
    [(get _ k) (lambda (s) ([: (continue k s) (-> Number Number)] s))]
    [(set s k) (lambda (_) ([: (continue k #f) (-> Number Number)] s))]
    [(return x _) (lambda (_) x)])
  '(=> (! Number ((get . (-> Boolean Number))
                  (set . (-> Number Boolean))
                  (return . (-> Number Number))))
       (-> String Number)))))
  
