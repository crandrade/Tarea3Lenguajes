#lang play
(require "start.rkt")
(require "machine.rkt")
(print-only-errors #f)

;; test parse-t
(test (parse-t '{{Num -> Num} -> Bool})
      (TFun (TFun (TNum) (TNum)) (TBool)))
(test (parse-t 'Num) (TNum))
(test (parse-t '{Bool -> (Bool -> Num)}) (TFun (TBool) (TFun (TBool) (TNum))))

;; test parse
(test (parse '{+ {- 1 2} 3}) 
      (add (sub (num 1) (num 2)) (num 3)))

(test (parse '{fun {x : Num} : Bool #f})
      (fun 'x (TNum) (bool #f) (TBool)))

(test (parse '{with : Num {y : Num 2}
                                 {+ x y}}) 
      (app (fun 'y (TNum) (add (id 'x) (id 'y)) (TNum)) (num 2)))

(test (parse '#t) (bool #t))

(test (parse '{cast Num #f}) (cast (TNum) (bool #f)))


;; test DeBruijn
;;(test (deBruijn 'expr) #t)
(test (deBruijn 
       (parse '{with : Num {x : Num 3} 
                     {+ {- 1 2} x}}))
      (app
       (fun-db (add (sub (num 1) (num 2)) (acc 0)) (TFun (TNum) (TNum)))
       (num 3)))


(test (deBruijn (parse '{+ 1 {with : Num {x : Num 1}
                           {with : Num {y : Num 2}
                                 {+ x y}}}}))
(add
 (num 1)
 (app
  (fun-db
   (app (fun-db (add (acc 1) (acc 0)) (TFun (TNum) (TNum))) (num 2))
   (TFun (TNum) (TNum)))
  (num 1))))

(test/exn (deBruijn (add (num 1) (id 'x))) "unbound identifier")

;;test compile
(test (compile (deBruijn (parse '2)))
(list (INT_CONST 2)))

(test (compile (deBruijn (parse '{{fun {x : Num} : Bool
                                   {< x 10}} {+ 2 3}}))) 
      (list
       (INT_CONST 3) (INT_CONST 2) (ADD)
       (CLOSURE 
        (list (INT_CONST 10) 
              (ACCESS 0) 
              (LESS) 
              (RETURN)) 
        (MTFun (MTNum) (MTBool)))
       (APPLY)))

(test (compile (parse '{cast Num (and #t #f)}))
      (list (BOOL_CONST #f) (BOOL_CONST #t) (AND) 
            (CHECKCAST (MTNum))))

(test (compile (deBruijn (parse '{with : Num {y : Num 2}
                                 {+ 1 y}}))) 
      (list
       (INT_CONST 2)
       (CLOSURE
        (list (ACCESS 0) (INT_CONST 1) (ADD) (RETURN))
        (MTFun (MTNum) (MTNum)))
       (APPLY)))

;; test typeof
(test (typeof (parse '{with : Num {x : Num 3} 
                     {+ {- 1 2} x}})) 
      (TNum))

(test/exn (typeof (parse '{{fun {f : Num} : Bool #t} #t}))
          "TYPE_ERROR")

(test (typeof (parse '{fun {f : Num} : Bool #t}))
  (TFun (TNum) (TBool)))

(test (typeof 
       (app (fun 'x 
                 (TNum) 
                 (add (num 1) (id 'x)) 
                 (TNum))
            (num 2))) 
       (TNum))

;; test typeof-with-sub

(test (typeof (parse '{fun {f : Num} : Bool #t}))
  (TFun (TNum) (TBool)))

(test/exn (typeof (parse '{{fun {f : Num} : Bool #t} #t}))
          "TYPE_ERROR")

(test (typeof-with-sub (parse '{if #t
                             1
                             #t}))
      (TAny))

(test  (typeof-with-sub 
   (parse 
     '{with : Any {f : {Any -> Any}
                  {fun {x : Any} : Any x}}
            {f 1}
         }))
       (TAny))

(test 
 (typeof-with-sub (parse '{with : Num {f : {Num -> Num}
                  {fun {x : Num} : Num x}}                                 
         {with : Num {h : {Any -> Num}
                        {fun {x : Any} : Num 5}}
               {with : Num {applyTo1 : {{Num -> Num} -> Num}
                                     {fun {x : {Num -> Num}} : Num {x 1}}}
                     {applyTo1 f}}}}))
 (TNum))
(test (typeof-with-sub (parse '{with : Any {f : {Num -> Num}
                  {fun {x : Num} : Num x}}
         {with : Any {g : {Num -> Any}
                        {fun {x : Num} : Any #f}}
               {with : Any {applyTo1 : {{Num -> Any} -> Any}
                                     {fun {x : {Num -> Any}} : Any {x 1}}}
                     {applyTo1 g}}}}))
      (TAny))

;;test typeof-with-cast
(test (typeof-with-cast 
       (parse 
        '{{fun { x : Num} : Any  {cast Num x}} 3})) 
      (TAny))

(test/exn (typeof-with-cast (parse '{cast Any 1})) "TYPE_ERROR")

(test (typeof-with-cast (parse '{{fun { x : Num} : Any  x} 3})) (TAny))

(test (typeof-with-cast (parse '{with : Bool {id : (Any -> Any)
                           {fun {x : Any} : Any x}}
                {with : Bool {g : ((Bool -> Bool) -> Bool)
                                {fun {f : (Bool -> Bool)} : Bool {f #t}}}
                      {with : Bool {f : (Bool -> Bool)
                                      {fun {x : Bool} : Bool x}}
                            {g {cast (Bool -> Bool) {id f}}}}}}))
      (TBool))

;; test typed-compile
(test (typed-compile '{cast Num (and #t #f)})
(list (BOOL_CONST #f) (BOOL_CONST #t) (AND) (CHECKCAST (MTNum))))

(test
 (typed-compile '{+ 3 {with : Num {x : Num 1}
                               {with : Num {y : Num 2}
                                     {cast Num {+ x y}}}}}) 
 (list
    (INT_CONST 1)
    (CLOSURE (list (INT_CONST 2) 
                   (CLOSURE (list (ACCESS 0) 
                                  (ACCESS 1) 
                                  (ADD)
                                  (CHECKCAST (MTNum))
                                  (RETURN)) 
                            (MTFun (MTNum) (MTNum))) 
                   (APPLY) (RETURN)) 
             (MTFun (MTNum) (MTNum)))
    (APPLY)
    (INT_CONST 3)
    (ADD)))

(test
  (typed-compile 
   '{cast Bool {{fun {x : Num} : Bool {< x 10}} {+ 2 3}}})
  (list (INT_CONST 3) 
        (INT_CONST 2) 
        (ADD) 
        (CLOSURE (list (INT_CONST 10) 
                       (ACCESS 0) 
                       (LESS) 
                       (RETURN)) 
                 (MTFun (MTNum) (MTBool))) 
        (APPLY)
        (CHECKCAST (MTBool))))

;; test m-subtype?
(test (m-subtype? (MTNum) (MTNum)) #t)

(test (m-subtype? (MTNum) (MTAny)) #t)

(test (m-subtype? (MTBool) (MTAny)) #t)

(test (m-subtype? (MTAny) (MTAny)) #t)

(test (m-subtype? (MTAny) (MTBool)) #f)

(test (m-subtype? (MTFun (MTAny) (MTBool)) (MTFun (MTAny) (MTAny))) #t)

(test (m-subtype? (MTFun (MTNum) (MTBool)) (MTFun (MTAny) (MTAny))) #t)

(test (m-subtype? (MTFun (MTAny) (MTAny)) (MTFun (MTAny) (MTBool))) #f)