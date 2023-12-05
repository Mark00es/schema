#lang play
(print-only-errors #t) ; Para ver solo los errores.

#|
<expr> ::=   <num> | <bool> | <id>
            | (+ <expr> <expr>)
            | (with <id> <expr> <expr>)
            | (app <id> <expr>) 
|#

#|
<FAE> ::=   <num> | <bool> | <id>
            | (prim op-name <FAE> ... <FAE>)
            | (if-tf <FAE> <FAE> <FAE>)
            | (with <id> <FAE> <FAE>)
            | (app <FAE> <FAE>) ; puedo aplicar una funcion a otra funcion / puedo usar una funcion como argumento. 
            | (fun <id> <FAE>) ; fun(que es una lambda) nombre-arg body
|#
(define (not-equal? a b)
  (not (equal? a b)))
(define (and? a b)
  (and a b))
(define (or? a b)
  (or a b))
(define primitives
  (list
   (cons '+ +)
   (cons '- -)
   (cons '* *)
   (cons '/ /)
   (cons '< <)
   (cons '> >)
   (cons '<= <=)
   (cons '>= >=)
   (cons '== eq?)
   (cons '!= not-equal?)
   (cons '&& and?)
   (cons '|| or?)   
   (cons 'concat string-append)
   (cons 'tamstr string-length)   
   (cons 'mayustr string-upcase)   
  )
)
; (apply (cdr (assq '+ primitives)) '(1 2 3 4))

(deftype Expr
  [num n]                                 ; <num>
  [bool b]                                ; <bool>
  [str s]                                 ; <string>  
  [if-tf c et ef]                         ; (if-tf <FAE> <FAE> <FAE>)
  [with id-name named-expr body-expr]     ; (with <id> <FAE> <FAE>)
  [id name]                               ; <id> 
  [app fname arg-expr]                    ; (app <FAE> <FAE>) ; ahora podemos aplicar una funcion a otra
  [fun arg body]                          ; (fun <id> <FAE>) ; mantenemos el <id> como el nombre del argumento
  [prim name args]
) 


#|
<env> ::= (mtEnv)
          | (aEnv <id> <val> <env>)
|#
(deftype Env
  (mtEnv)
  (aEnv id val env)
  )

; empty-env -> (mtEnv)
(define empty-env (mtEnv))

; extend-env:: <id> <val> <env> -> <env>
(define extend-env aEnv)
; env-lookup :: <id> <env> -> <val>
; buscar el valor de una variable dentro del ambiete
(define (env-lookup x env)
  (match env
    [(mtEnv) (error "undefined: " x)]
    [(aEnv id val tail)(if (eq? id x) val (env-lookup x tail))]
    )
  )

; concat-withs: Src -> Expr
; concatena una cadena de with anidada
(define (concat-withs vars body)
  (match vars
    [(list) (parse body)]
    [(cons (list id expr) rest-vars)
     (with id (parse expr) (concat-withs rest-vars body))]
    [var (with (car var) (parse (cadr var)) (parse body))]))

; parse: Src -> Expr
; parsea codigo fuente
(define (parse src)
  (match src
    [(? number?) (num src)]
    [(? boolean?) (bool src)]
    [(? symbol?) (id src)]
    [(? string?)(str src)]    
    [(list 'if-tf c et ef) (if-tf (parse c) (parse et) (parse ef))]
    [(list 'with vars body) (concat-withs vars body)]
    [(list arg e) (app (parse arg) (parse e))]; 2. Subir de nivel nuestras funciones
    [(list 'fun (list arg) body) (fun arg (parse body))] ; 1. Agregar el caso del fun
    [(cons prim-name args) (prim prim-name (map parse args))]
    )
  )

(deftype Val
  (valV v) ; numero, booleano, string, byte, etc.
  (closureV arg body env) ; closure = fun + env

  )

; interp :: Expr  Env -> Val
; interpreta una expresion
(define (interp expr env)
  (match expr
    [(num n) (valV n)]
    [(bool b) (valV b)]
    [(str s) (valV s)]
    [(list l)(valV l)]
    [(id x) (env-lookup x env)]; buscar el valor de x en env
    [(prim prim-name args) (prim-ops prim-name (map (λ (x) (interp x env)) args))]
    [(if-tf c et ef) (if (interp c env)
                         (interp et env)
                         (interp ef env))]
    [(with x e b) (interp b (extend-env x (interp e env) env))] ; Si asociamos una funcion a una variable, la funcion entra al env
    [(fun arg body) (closureV arg body env)] ; Por ahora, devolvemos la misma expresion que nos llego
    [(app f e)
     (def (closureV arg body fenv) (interp f env)) ; Esto permite encontrar (fun 'x (add (id 'x) (id 'x))) por ejemplo y tomar arg y body    
     (interp body (extend-env arg (interp e env) fenv)) ; parece que no funciona ni con estatico ni dinamico
     ]
))



; prim-ops: op-name list[Val] -> Val
(define (prim-ops op-name args)
  (let ([vals (map (λ (x) (valV-v x)) args)])
    (valV (apply (cdr (assq op-name primitives)) vals))
    )
  )

; run: Src -> Src
; corre un programa
(define (run prog)
  (let ([res (interp (parse prog) empty-env)])
    (match res
      [(valV v) v]
      [(closureV arg body env) res])
    )
  )
;(run '{size (list 1 2 3 4)})
;(run '{mayustr "hola mundo"})


; Pruebas base para el intérprete final

(test (run '{+ 3 4}) 7)
(test (run '{- 5 1}) 4)
(test (run '{+ 3 4}) 7)
(test (run '{- 5 1}) 4)
(test (run '{+ 1 2 3 4}) 10)
(test (run '{* 2 3 4}) 24)
(test (run '{/ 12 2 2}) 3)
(test (run '{< 12 3}) #f)
(test (run '{<= 12 3}) #f)
(test (run '{< 12 12}) #f)
(test (run '{<= 12 12}) #t)
(test (run '{> 12 3}) #t)
(test (run '{>= 12 3}) #t)
(test (run '{> 12 12}) #f)
(test (run '{>= 12 12}) #t)
(test (run '{>= 12 12}) #t)
(test (run '{== 12 12}) #t)
(test (run '{== 12 11}) #f)
(test (run '{!= 12 12}) #f)
(test (run '{!= 12 11}) #t)
(test (run '{&& 12 11}) 11)
(test (run '{&& #f #t}) #f)
(test (run '{|| #f #t}) #t)
(test (run '{|| 12 11}) 12)
(test (run '{with {x 3} 2}) 2)
(test (run '{with {x 3} x}) 3)
(test (run '{with {x 3} {with {y 4} x}}) 3)
(test (run '{with {x 3} {+ x 4}}) 7)
(test (run '{with {x 3} {with {x 10} {+ x x}}}) 20)
(test (run '{with {x 3} {with {x x} {+ x x}}}) 6)
(test (run '{with {x 3} {with {y 2} {+ x y}}}) 5)
(test (run '{with {x 3} {+ 1 {with {y 2} {+ x y}}}}) 6)
(test (run '{with {x 3} {with {y {+ 2 x}} {+ x y}}}) 8)
(test (run '{* 1 1 1 1}) 1)
;(test/exn (run '{* 1 #t 1 1}) "type error")
;(test/exn (run '{with {x #t} {* 1 x x x}}) "type error")
;(test/exn (run '{with {x #t} {* x x x x}}) "type error")
(test (run '{with {x 3} {+ x x}}) 6)
(test (run '{with {x 3} {with {y 2} {+ x y}}}) 5)
(test (run '{with {{x 3} {y 2}} {+ x y}}) 5)
(test (run '{with {{x 3} {x 5}} {+ x x}}) 10)
(test (run '{with {{x 3} {y {+ x 3}}} {+ x y}}) 9)
(test (run '{with {{x 10} {y 2} {z 3}} {+ x {+ y z}}}) 15)
(test (run '{with {x 3} {if-tf {+ x 1} {+ x 3} {+ x 9}}}) 6)
(test/exn (run '{f 10}) "undefined")
(test (run '{with {f {fun {x} {+ x x}}}{f 10}}) 20)
(test (run '{{fun {x} {+ x x}} 10}) 20)
(test (run '{with {add1 {fun {x} {+ x 1}}}{add1 {add1 {add1 10}}}}) 13)
(test (run '{with {add1 {fun {x} {+ x 1}}}
                  {with {foo {fun {x} {+ {add1 x} {add1 x}}}}
                        {foo 10}}}) 22)
(test (run '{with {add1 {fun {x} {+ x 1}}}
                  {with {foo {fun {f} {+ {f 10} {f 10}}}}
                        {foo add1}}}) 22)
(test (run '{{fun {x}{+ x 1}} {+ 2 3}}) 6)
(test (run '{with {apply10 {fun {f} {f 10}}}
                  {with {add1 {fun {x} {+ x 1}}}
                        {apply10 add1}}}) 11)
(test (run '{with {addN {fun {n}
                       {fun {x} {+ x n}}}}
            {{addN 10} 20}}) 30)