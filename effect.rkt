#lang racket

(require "exp.rkt")

(provide
 #%datum
 #%app
 #%top
 (rename-out [module-begin #%module-begin]
             [top-interaction #%top-interaction]))

(define-syntax-rule (module-begin expr ...)
  (#%module-begin
   (eval-closed (expand-define expr ...))))

(define-syntax-rule (top-interaction . expr)
  (eval-closed `expr))

(define-syntax expand-define
  (syntax-rules ()
    [(_ expr) `expr]
    [(_ (define x e) expr ...)
     `(let (x e)
        ,(expand-define expr ...))]
    [(_ expr1 expr ...)
     `(let (_ expr1)
        ,(expand-define expr ...))]))
