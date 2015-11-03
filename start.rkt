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
         | {cast <TYPE> <expr>}         
 
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


;; parse-t ::= s-expr => TYPE
;; transforms a s-expr into a TYPE expression
(define (parse-t s-expr) 
  (match s-expr
    ['Num (TNum)]
    ['Bool (TBool)]
    ['Any (TAny)]
    [(list a '-> b) (TFun (parse-t a) (parse-t b))]
    [(list a) (parse-t a)]
    ))
 
;; parse ::= s-expr => Expr
;; transform a s-expr into an Expr
(define (parse expr)  
  (match expr
    [(? number?) (num expr)]
    [(? boolean?) (bool expr)]
    [(? symbol?) (id expr)]
    [(list 'cast a b) (cast (parse-t a) (parse b))]
    [(list '+ a b) (add (parse a) (parse b))]
    [(list '- a b) (add (parse a) (parse b))]
    [(list '= a b) (my-eq (parse a) (parse b))]
    [(list '< a b) (my-less (parse a) (parse b))]
    [(list 'and a b) (my-and (parse a) (parse b))]
    [(list 'or a b) (my-or (parse a) (parse b))]
    [(list 'not a) (my-not (parse a))]
    [(list 'if a b c) (my-if (parse a) (parse b) (parse c))]
    [(list 'fun a ': c d) (fun (first a) (parse-t (third a)) (parse d) (parse-t c))]
    [(list 'with ': type id-expr s-expr) 
     (app (fun (first id-expr) (parse-t (third id-expr)) (parse s-expr) (parse-t type)) (parse (fourth id-expr)))]
    [(cons a b) (app (parse (first expr)) (parse (second expr)))]
    ))

;; deBruijn :: Expr => Expr
;; replaces non-free vars with indexes, keeping lexic scope
(define (deBruijn expr) 
  (define (deBruijn-expr expr ids)
  ;indefOf:: value list[value] pos -> pos
    (define (indexOf-rec v l [pos 0])
      (match l
        [(list) -1]
        [(cons x r)(if (equal? x v) pos 
                       (indexOf-rec v r (add1 pos)))]))
    (define (indexOf v l)
      (indexOf-rec v l 0))
    (match expr
      [(num n) (num n)]    
      [(bool n) (bool n)]   
      [(add l r) (add (deBruijn-expr l ids) (deBruijn-expr r ids))]
      [(sub l r) (sub (deBruijn-expr l ids) (deBruijn-expr r ids))]
      [(my-eq l r) (my-eq (deBruijn-expr l ids) (deBruijn-expr r ids))]
      [(my-less l r) (my-less (deBruijn-expr l ids) (deBruijn-expr r ids))]
      [(my-and l r) (my-and (deBruijn-expr l ids) (deBruijn-expr r ids))]
      [(my-or l r) (my-or (deBruijn-expr l ids) (deBruijn-expr r ids))]
      [(my-not l) (my-not (deBruijn-expr l ids))]
      [(my-if l r z) (my-if (deBruijn-expr l ids) (deBruijn-expr r ids) (deBruijn-expr z ids))]
      [(id x) 
       (define pos (indexOf x ids))
       (if (>= pos 0)
           (acc pos)
           (error "unbound identifier"))]  
      [(app a b) (app (deBruijn-expr a ids) (deBruijn-expr b ids))]
      [(fun id targ body tbody) (fun-db (deBruijn-expr body (append (list id) ids)) (TFun targ tbody))]
    ))
  (deBruijn-expr expr '()))


(define (compile expr) 
  (define (stack-type expr)
    (match expr
      [(TNum) (MTNum)]
      [(TBool) (MTBool)]
      [(TAny) (MTAny)]
      [(TFun a b) (MTFun (stack-type a) (stack-type b))]))
  (define (stack expr lst)
    (match expr
      [(num n) (append lst (list (INT_CONST n)))]
      [(bool n) (append lst (list (BOOL_CONST n)))]
      [(acc n) (append lst (list (ACCESS n)))]
      [(cast a b) (append lst (stack b lst) (list (CHECKCAST (stack-type a))))]
      [(add l r) (append lst (stack r lst) (stack l lst) (list (ADD)))]
      [(sub l r) (append lst (stack r lst) (stack l lst) (list (SUB)))]
      [(my-eq l r) (append lst (stack r lst) (stack l lst) (list (EQ)))]
      [(my-less l r) (append lst (stack r lst) (stack l lst) (list (LESS)))]
      [(my-and l r) (append lst (stack r lst) (stack l lst) (list (AND)))]
      [(my-or l r) (append lst (stack r lst) (stack l lst) (list (OR)))]
      [(my-not l) (append lst (stack l lst) (list (NOT)))]
      [(fun-db a b) (append lst (list (CLOSURE 
                     (append lst (stack a lst) (list (RETURN))) (stack-type b))))]
      [(app a b) (append lst (stack b lst) (stack a lst) (list (APPLY)))]
      ))
  (stack expr '()))


(define (typeof expr)
 (define (typeof-env sexpr typed-env)
  (match sexpr
    [(num n) (TNum)]
    [(bool b) (TBool)]
    [(id x) (if (hash-has-key? typed-env x) 
                (hash-ref typed-env x) 
                (error "TYPE_ERROR: unbound identifier"))]
    ;AE
    [(add l r)
     (if (and (TNum? (typeof-env l typed-env)) (TNum (typeof-env r typed-env)))
         (TNum)
         (error "TYPE_ERROR: TNum expression expected"))]
    [(sub l r)      
     (if (and (TNum? (typeof-env l typed-env)) (TNum (typeof-env r typed-env)))
         (TNum)
         (error "TYPE_ERROR: TNum expression expected"))]
    ;preposiciones        
    [(my-and p q) 
     (if (and (TBool? (typeof-env p typed-env))
              (TBool? (typeof-env q typed-env)))
         (TBool)
         (error "TYPE_ERROR: TBool expression expected"))]
    [(my-or p q) (if (and (TBool? (typeof-env p typed-env))
                          (TBool? (typeof-env q typed-env)))
                     (TBool)
                     (error "TYPE_ERROR: TBool expression expected"))]
    [(my-not p) (if (TBool? (typeof-env p typed-env))
                    (TBool)
                    (error "TYPE_ERROR: TBool expression expected"))]
    [(my-less a b) (if (and (TNum? (typeof-env a typed-env))
                           (TNum? (typeof-env b typed-env)))
                      (TBool)
                      (error "TYPE_ERROR: TNum expression expected"))]
    [(my-if c l r) 
     (let [(tc (typeof-env c typed-env))
           (tl (typeof-env l typed-env))
           (tr (typeof-env r typed-env))]
       (if (not (TBool? tc))
           (error "TYPE_ERROR: TBool expression expected")
           (if (equal? tl tr)
               tl
               (error "TYPE_ERROR: same type expected"))))]
    [(my-eq a b)
     (let ([ta (typeof-env a typed-env)]
           [tb (typeof-env b typed-env)])
       (if (equal? a b)
           (TBool)
           (error "TYPE_ERROR: same type expected")))]
    [(fun x e b tb) (if (equal? (typeof-env b (hash-set typed-env x e)) 
                                tb)
                        (TFun e tb)
                        (error "TYPE_ERROR: same type expected"))]
    [(app f-id b) (if (fun? f-id)
                      (let [(f (typeof-env f-id typed-env)) 
                            (r (typeof-env b typed-env))] 
                        (if (equal? (TFun-arg f) r)
                            f
                            (error "TYPE_ERROR: same type expected")))
                      (error "TYPE_ERROR: function expected"))]
    ))
  (typeof-env expr (make-immutable-hash '())))

(define (typeof-with-sub expr) #f)

(define (typeof-with-cast expr) #f)

(define (typed-compile s-expr) #f)