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

; parse: Src -> Expr
; parsea codigo fuente
(define (parse src)
  (match src
    [(? number?) (num src)]
    [(? boolean?) (bool src)]
    [(? symbol?) (id src)]
    [(? string?)(str src)]    
    [(list 'if-tf c et ef) (if-tf (parse c) (parse et) (parse ef))]
    [(list 'with (list x e) b) (with x (parse e) (parse b))]
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

(define (typeof expr)
  (match expr
    [(num n) 'Num]
    [(bool b) 'Bool]
    [(str s) 'String]
    [(id x) (error "Type error")]
    [(prim name args)(match name
                        [(cons '+ _) 'Num]
                        [(cons '- _) 'Num]
                        [(cons '* _) 'Num]
                        [(cons '/ _) 'Num]
                        [(cons '< _) 'Bool]
                        [(cons '<= _) 'Bool]
                        [(cons '> _) 'Bool]
                        [(cons '>= _) 'Bool]
                        [(cons '== _) 'Bool]
                        [(cons '!= _) 'Bool]
                        [(cons '&& _) 'Bool]
                        [else (error "type error")]
                        )]
  )
)

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
(run '{mayustr "hola mundo"})