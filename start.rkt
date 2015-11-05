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
    [(list '- a b) (sub (parse a) (parse b))]
    [(list '= a b) (my-eq (parse a) (parse b))]
    [(list '< a b) (my-less (parse a) (parse b))]
    [(list 'and a b) (my-and (parse a) (parse b))]
    [(list 'or a b) (my-or (parse a) (parse b))]
    [(list 'not a) (my-not (parse a))]
    [(list 'if a b c) (my-if (parse a) (parse b) (parse c))]
    [(list 'fun a ': c d) (fun (first a) (parse-t (third a)) (parse d) (parse-t c))]
    [(list 'with ': type id-expr s-expr) 
     (app (fun (first id-expr) 
               (parse-t (third id-expr)) 
               (parse s-expr) 
               (parse-t type)) 
          (parse (fourth id-expr)))]
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
      [(cast a b) (cast a (deBruijn-expr b ids))]
    ))
  (deBruijn-expr expr '()))

;; compile ::= Expr => List
;; compiles the expression into an executable list/stack of operations
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
      [(my-if c tb tf) (append lst (stack c lst) (list (IF (stack tb lst) (stack tf lst))))]
      ;;[(fun id targ body tbody) (append lst (list (CLOSURE (append lst (stack body lst) (list (RETURN))) (stack-type (TFun targ tbody)))))]
      [(fun-db a b) (append lst (list (CLOSURE 
                     (append lst (stack a lst) (list (RETURN))) (stack-type b))))]
      [(app a b) (append lst (stack b lst) (stack a lst) (list (APPLY)))]
      ))
  (stack expr '()))

;; typeof ::= Expr => TYPE
;; gives the TYPE of a valid expression, or an error
(define (typeof expr)
 (define (typeof-env sexpr typed-env)
  (match sexpr
    [(num n) (TNum)]
    [(bool b) (TBool)]
    [(id x) (if (hash-has-key? typed-env x) 
                (hash-ref typed-env x) 
                (error "TYPE_ERROR"))]
    ;AE
    [(add l r)
     (if (and (TNum? (typeof-env l typed-env)) (TNum? (typeof-env r typed-env)))
         (TNum)
         (error "TYPE_ERROR"))]
    [(sub l r)      
     (if (and (TNum? (typeof-env l typed-env)) (TNum? (typeof-env r typed-env)))
         (TNum)
         (error "TYPE_ERROR"))]
    ;preposiciones        
    [(my-and p q) 
     (if (and (TBool? (typeof-env p typed-env))
              (TBool? (typeof-env q typed-env)))
         (TBool)
         (error "TYPE_ERROR"))]
    [(my-or p q) (if (and (TBool? (typeof-env p typed-env))
                          (TBool? (typeof-env q typed-env)))
                     (TBool)
                     (error "TYPE_ERROR"))]
    [(my-not p) (if (TBool? (typeof-env p typed-env))
                    (TBool)
                    (error "TYPE_ERROR"))]
    [(my-less a b) (if (and (TNum? (typeof-env a typed-env))
                           (TNum? (typeof-env b typed-env)))
                      (TBool)
                      (error "TYPE_ERROR"))]
    [(my-if c l r) 
     (let [(tc (typeof-env c typed-env))
           (tl (typeof-env l typed-env))
           (tr (typeof-env r typed-env))]
       (if (not (TBool? tc))
           (error "TYPE_ERROR")
           (if (equal? tl tr)
               tl
               (error "TYPE_ERROR"))))]
    [(my-eq a b)
     (let ([ta (typeof-env a typed-env)]
           [tb (typeof-env b typed-env)])
       (if (equal? ta tb)
           (TBool)
           (error "TYPE_ERROR")))]
    [(fun x e b tb) (if (equal? (typeof-env b (hash-set typed-env x e)) 
                                tb)
                        (TFun e tb)
                        (error "TYPE_ERROR"))]
    [(app f-id b) (if (fun? f-id)
                      (let [(f (typeof-env f-id typed-env)) 
                            (r (typeof-env b typed-env))] 
                        (if (equal? (TFun-arg f) r)
                            (TFun-ret f)
                            (error "TYPE_ERROR")))
                      (error "TYPE_ERROR"))]
    ))
  (typeof-env expr (make-immutable-hash '())))


;; typeof-with-sub ::= Expr => TYPE
;; gives the TYPE of a valid expression, or an error, 
;; considering subtypes
(define (typeof-with-sub expr)
  (define (subtype? t1 t2)
    (if (equal? t1 t2) #t
      (match t1
        [(TFun a b) (and (TFun? t2) 
                          (and (subtype? (TFun-arg t1) 
                                           (TFun-arg t2)) 
                               (subtype? (TFun-ret t1) 
                                           (TFun-ret t2))))]
        [(TNum) (or (TNum? t2) (TAny? t2))]
        [(TBool) (or (TBool? t2) (TAny? t2))]
        [_ #f])))
  (define (typeof-sub-env sexpr typed-env)
  (match sexpr
    [(num n) (TNum)]
    [(bool b) (TBool)]
    [(id x) (if (hash-has-key? typed-env x) 
                (hash-ref typed-env x) 
                (error "TYPE_ERROR"))]
    ;AE
    [(add l r)
     (if (and (TNum? (typeof-sub-env l typed-env)) (TNum? (typeof-sub-env r typed-env)))
         (TNum)
         (error "TYPE_ERROR"))]
    [(sub l r)      
     (if (and (TNum? (typeof-sub-env l typed-env)) (TNum? (typeof-sub-env r typed-env)))
         (TNum)
         (error "TYPE_ERROR"))]
    ;preposiciones        
    [(my-and p q) 
     (if (and (TBool? (typeof-sub-env p typed-env))
              (TBool? (typeof-sub-env q typed-env)))
         (TBool)
         (error "TYPE_ERROR"))]
    [(my-or p q) (if (and (TBool? (typeof-sub-env p typed-env))
                          (TBool? (typeof-sub-env q typed-env)))
                     (TBool)
                     (error "TYPE_ERROR"))]
    [(my-not p) (if (TBool? (typeof-sub-env p typed-env))
                    (TBool)
                    (error "TYPE_ERROR"))]
    [(my-less a b) (if (and (TNum? (typeof-sub-env a typed-env))
                           (TNum? (typeof-sub-env b typed-env)))
                      (TBool)
                      (error "TYPE_ERROR"))]
    [(my-if c l r) 
     (let [(tc (typeof-sub-env c typed-env))
           (tl (typeof-sub-env l typed-env))
           (tr (typeof-sub-env r typed-env))]
       (if (not (TBool? tc))
           (error "TYPE_ERROR")
           (if (equal? tl tr)
               tl
               (TAny))))]
    [(my-eq a b)
     (let ([ta (typeof-sub-env a typed-env)]
           [tb (typeof-sub-env b typed-env)])
       (if (equal? ta tb)
           (TBool)
           (error "TYPE_ERROR")))]
    [(fun x e b tb) (if (subtype? 
                         (typeof-sub-env 
                          b 
                          (hash-set typed-env x e))
                         tb)
                        (TFun e tb)
                        (error "TYPE_ERROR"))]
    [(app f-id b) 
     (let [(ff (typeof-sub-env f-id typed-env))
           (bb (typeof-sub-env b typed-env))]
       (if (subtype? bb (TFun-arg ff))
                          (TFun-ret ff)
                          (error "TYPE_ERROR")))]
    ))
  (typeof-sub-env expr (make-immutable-hash '())))

;; typeof-with-cast ::= Expr => TYPE
;; gives the TYPE of a valid expression, or an error
;; considering subtypes and cast
(define (typeof-with-cast expr)
  (define (subtype? t1 t2)
    (if (equal? t1 t2) #t
      (match t1
        [(TFun a b) (or (and (TFun? t2) 
                          (and (subtype? (TFun-arg t1) 
                                           (TFun-arg t2)) 
                               (subtype? (TFun-ret t1) 
                                           (TFun-ret t2))))
                        (TAny? t2))]
        [(TNum) (or (TNum? t2) (TAny? t2))]
        [(TBool) (or (TBool? t2) (TAny? t2))]
        [_ #f])))
 (define (typeof-cast-env sexpr typed-env)
  (match sexpr
    [(num n) (TNum)]
    [(bool b) (TBool)]
    [(id x) (if (hash-has-key? typed-env x) 
                (hash-ref typed-env x) 
                (error "TYPE_ERROR"))]
    [(cast targ arg) 
     (match targ
       [(TBool) (if (TBool? (typeof-cast-env arg typed-env)) 
                    targ 
                    (error "TYPE_ERROR"))]
       [(TNum) (if (TNum? (typeof-cast-env arg typed-env)) 
                    targ 
                    (error "TYPE_ERROR"))]
       [(TAny) (error "TYPE_ERROR")]
       [(TFun a b) (let [( f (typeof-cast-env arg typed-env))]
                          (if (or (TAny? a) (TAny? b))
                              (error "TYPE_ERROR")
                       targ))])]
    ;AE
    [(add l r)
     (if (and (TNum? (typeof-cast-env l typed-env)) 
              (TNum? (typeof-cast-env r typed-env)))
         (TNum)
         (error "TYPE_ERROR"))]
    [(sub l r)      
     (if (and (TNum? (typeof-cast-env l typed-env)) 
              (TNum? (typeof-cast-env r typed-env)))
         (TNum)
         (error "TYPE_ERROR"))]
    ;preposiciones        
    [(my-and p q) 
     (if (and (TBool? (typeof-cast-env p typed-env))
              (TBool? (typeof-cast-env q typed-env)))
         (TBool)
         (error "TYPE_ERROR"))]
    [(my-or p q) (if (and (TBool? (typeof-cast-env p typed-env))
                          (TBool? (typeof-cast-env q typed-env)))
                     (TBool)
                     (error "TYPE_ERROR"))]
    [(my-not p) (if (TBool? (typeof-cast-env p typed-env))
                    (TBool)
                    (error "TYPE_ERROR"))]
    [(my-less a b) (if (and (TNum? (typeof-cast-env a typed-env))
                           (TNum? (typeof-cast-env b typed-env)))
                      (TBool)
                      (error "TYPE_ERROR"))]
    [(my-if c l r) 
     (let [(tc (typeof-cast-env c typed-env))
           (tl (typeof-cast-env l typed-env))
           (tr (typeof-cast-env r typed-env))]
       (if (not (TBool? tc))
           (error "TYPE_ERROR")
           (if (equal? tl tr)
               tl
               (error "TYPE_ERROR"))))]
    [(my-eq a b)
     (let ([ta (typeof-cast-env a typed-env)]
           [tb (typeof-cast-env b typed-env)])
       (if (equal? ta tb)
           (TBool)
           (error "TYPE_ERROR")))]    
    [(fun x e b tb) (if (subtype? 
                         (typeof-cast-env 
                          b 
                          (hash-set typed-env x e))
                         tb)
                        (TFun e tb)
                        (error "TYPE_ERROR"))]
    [(app f-id b) 
     (let [(ff (typeof-cast-env f-id typed-env))
           (bb (typeof-cast-env b typed-env))]
       (if (subtype? bb (TFun-arg ff))
                          (TFun-ret ff)
                          (error "TYPE_ERROR")))]
    ))
  (typeof-cast-env expr (make-immutable-hash '())))

;; typed-compile ::= sexpr => List
;; compiles the s-expression into an executable list/stack of 
;; operations, replacing vars with indexes and type-checking
(define (typed-compile s-expr)
  (begin
    (define expr (parse s-expr))
    (typeof-with-cast expr)
    (compile (deBruijn expr))))