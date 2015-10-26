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
(test (parse '{fun {x : Num} : Bool #f})
      (fun 'x (TNum) (bool #f) (TBool)))

(test (parse '{with : Num {y : Num 2}
                                 {+ x y}}) 
      (app (fun 'y (TNum) (add (id 'x) (id 'y)) (TNum)) (num 2)))

(test (parse '#t) (bool #t))

(test (parse '{cast Num #f}) (cast (TNum) (bool #f)))


;; test DeBruijn
;;(test (deBruijn 'expr) #t)

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
(test (compile 'expr) #t)

(test (compile 'expr) #t)

(test (compile 'expr) #t)

;; test typeof
(test (typeof 'expr) #t)

(test (typeof 'expr) #t)

(test (typeof 'expr) #t)

;; test typeof-with-sub
(test (typeof-with-sub 'expr) #t)

(test (typeof-with-sub 'expr) #t)

(test (typeof-with-sub 'expr) #t)

;;test typeof-with-cast
(test (typeof-with-cast 'expr) #t)

(test (typeof-with-cast 'expr) #t)

(test (typeof-with-cast 'expr) #t)

;; test typed-compile
(test (typed-compile 'expr) #t)

(test (typed-compile 'expr) #t)

(test (typed-compile 'expr) #t)