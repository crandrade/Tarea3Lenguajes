#lang play
(require "machine.rkt")
;;;;;;;;;;;;;;;;;;;;;;;
;; Language definition
;;;;;;;;;;;;;;;;;;;;;;;

#|
<expr> ::= <num>
         | <bool>
         | <id>
         | {+ <expr> <expr>}
         | {- <expr> <expr>}
         | {= <expr> <expr>}
         | {< <expr> <expr>}
         | {and <expr> <expr>}
         | {or <expr> <expr>}
         | {not <expr>}
         | {if <expr> <expr> <expr>}
         | {with : <TYPE> {<id> : <TYPE> <expr>} <expr>}
         | {fun {<id> : <TYPE>} : <TYPE> <expr>}
         | {<expr> <expr>}         
 
<TYPE> ::= Num
         | Bool
         | {<TYPE> -> <TYPE>}}
|#
(deftype Expr
  (num n)
  (bool b)
  (add l r)
  (sub l r)
  (my-eq l r)
  (my-less l r)
  (my-and l r)
  (my-or l r)
  (my-not e)
  (my-if c tb fb)
  (id s)
  (fun id targ body tbody)
  (fun-db body t)
  (acc n)
  (app fun-id arg-expr)
  (cast t e))

(deftype TYPE
  (TNum)
  (TBool)
  (TFun arg ret)
  (TAny))



(define (parse-t s-expr) #f)

(define (parse expr)  #f )

(define (deBruijn expr) #f)

(define (compile expr) #f)

(define (typeof expr) #f)

(define (typeof-with-sub expr) #f)

(define (typeof-with-cast expr) #f)

(define (typed-compile s-expr) #f)