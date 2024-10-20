#lang racket

(require rackunit)

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
(struct function-closure
  (arg ; arg name : symbol?
   body ; body of the function : Term
   var-env ; env
   op-env ; env
   ) #:transparent)

(define (apply-function-closure clos v)
  (match clos
    [(function-closure arg body var-env op-env) 42]))
;(eval

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

;; (if-cont □ e1 e2)
(struct if-cont (e1 e2 saved-env cont) #:transparent)

;; (□ e)
(struct app-cont1 (e saved-env cont) #:transparent)

;; (v □)
(struct app-cont2 (v saved-env cont) #:transparent)

;; (with handler □)
(struct handler-cont
  (
   handler ; handler val
   saved-cont ; continuation
   ) #:transparent)

;; symbol * continuation -> continuation * continuation * handler-caluse
;; op-name: an operation name
;; cont: a continuation
;; If cont = E1[(with-handler h [E2])]
;; where h maps op-name to clause
;; returns values: E1[(with-handler h [])], E2, clause
(define (capture-handler op-name cont)
  (match cont
    [(end-cont) (error (format "unhandled op: ~a" op-name))]
    [(if-cont e1 e2 saved-env saved-cont)
     (define-values (E1 E2 clause) (capture-handler op-name saved-cont))
     (values E1 (if-cont e1 e2 saved-env E2) clause)]
    [(handler-cont handler saved-cont)
     (define clause? (handles? handler op-name))
     (if clause?
         (values cont saved-cont clause?)
         
           (let-values ([(E1 E2 clause) (capture-handler op-name saved-cont)])
           (values E1 (handler-cont handler E2) clause)))]))


         

;; values
(struct perform-val (op-name) #:transparent)

(struct closure-val (arg body saved-env) #:transparent)

(define (apply-cont cont v)
  (match cont
    [(end-cont) v]
    [(if-cont e1 e2 saved-env cont)
     (if v
         (eval e1 saved-env cont)
         (eval e2 saved-env cont))]
    [(app-cont1 e saved-env cont)
     'todo]
    [_ (error 'todo)]))

(define (eval e env cont)
  (match e
    ['() (apply-cont cont '())]
    [(? number? n) (apply-cont cont n)]
    [(? symbol? x) (apply-cont cont (apply-env env x))]
    [`(if ,e1 ,e2 ,e3)
     (eval e1 env (if-cont e2 e3 env cont))]
    [`(perform ,op ,arg) 'todo]
    [`(continue ,k ,e) 'todo]
    [`(,f ,arg) 'todo]
    [`(handle ([(,ops ,xs ,ks) ,es] ...) ,body)
     (define clauses (for/list ([op ops] [x xs] [k ks] [e es])
       (cons op (handler-clause x k e env))))
     (define new-cont
       (handler-cont (handler clauses) cont))
     (eval body env new-cont)]
    [_ (error 'bad-syntax)]))

(define (eval-closed e)
  (eval e (empty-env) (end-cont)))

(check-equal? (eval-closed 1) 1)

; (check-equal? (eval 'x (extend-env 'x 1 (empty-env)) (empty-stack)) 1)
