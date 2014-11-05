#lang plai

(define-type Binding
  [binding (name symbol?) (named-expr CFWAE?)])

(define *reserved-words* '(+ - * / with if0 fun))
(define exp-map (hash '+ + '- - '* * '/ /))
(define (reserved? i)
  (if (member i *reserved-words*)
      true
      false))

(define-type CFWAE
  [num (n number?)]
  [binop (op procedure?) (lhs CFWAE?) (rhs CFWAE?)]
  [with (b Binding?) (body CFWAE?)]
  [id (name symbol?)]
  [if0 (c CFWAE?) (t CFWAE?) (e CFWAE?)]
  [fun (args (listof symbol?)) (body CFWAE?)]
  [app (f CFWAE?) (args (listof CFWAE?))])

(define-type Env
  [mtEnv]
  [anEnv (name symbol?) (value CFWAE-Value?) (env Env?)])

(define-type CFWAE-Value
  [numV (n number?)]
  [thunkV (body CFWAE?) (env Env?)]
  [closureV (param symbol?)
            (body CFWAE?)
            (env Env?)])


;; parse : expression -> CFWAE
;; This procedure parses an expression into a CFWAE
(define (parse sexp)
  (match sexp
    [(? number?) (num sexp)]
    [(and (? symbol?) (? (compose not reserved?))) (id sexp)]
    [(list 'with (list (and (? symbol?) (? (compose not reserved?)) id)
                       named-expr)
           body-expr)
     (with (binding id (parse named-expr)) (parse body-expr))]
    [(list 'if0 cond-expr t-expr f-expr) (if0 (parse cond-expr) (parse t-expr) (parse f-expr))]
    [(list 'fun fun-args fun-body) (fun fun-args (parse fun-body))]
    [(list 'fun fun-body) (fun empty (parse fun-body))]
    [(list (and (? symbol?) (? ((curry hash-has-key?) exp-map)) op) lhs rhs) (binop (hash-ref exp-map op) (parse lhs) (parse rhs))]
    ;;[(list (list 'fun fun-expr)) (app (parse fun-expr) empty)]
    [(cons fun-expr args) (app (parse fun-expr) (map parse args))]
    [else (error 'parse "Unable to parse ~s" sexp)]))

;; pre-process : CFWAE -> CFWAE
;; Consumes a CFWAE and constructs a corresponding CFWAE without
;; with expressions (which are replaced by function application) 
;; and with no functions or applications of more than one argument.
;; (Assumes the input was successfully produced by parse.)


(define (pre-process expr)
  (type-case CFWAE expr
    [num (n) (num n)]
    [binop (op lhs rhs) (binop op (pre-process lhs) (pre-process rhs))]
    [with (binding body) (app (fun (list (binding-name binding))
                                   (pre-process body)) 
                              (list (pre-process (binding-named-expr binding))))]
    [id (name) (id name)]
    [if0 (c t f) (if0 (pre-process c) (pre-process t) (pre-process f))]
    [fun (args body) (cond [(= 1 (length args)) (fun args (pre-process body))]
                           [(= 0 (length args)) (fun empty (pre-process body))]
                           [(fun (list (first args))
                              (pre-process (fun (rest args) (pre-process body))))])]
    [app (f args) (cond [(= 1 (length args)) (app (pre-process f) (list (pre-process (first args))))]
                        [(= 0 (length args)) (app (pre-process f) empty)]
                        [else (app (pre-process (app (pre-process f) (take args (- (length args) 1)))) 
                                   (list (pre-process (last args))))])]))

;; interp : CFWAE -> CFWAE-Value
;; This procedure interprets the given CFWAE and produces a result 
;; in the form of a CFWAE-Value (either a closureV, thunkV, or numV).
;; (Assumes the input was successfully produced by pre-process.)
(define (interp expr)
  (local [(define (helper expr env)
            (type-case CFWAE expr
               [num (n) (numV n)]
               [binop (op lhs rhs) 
                      (compute-nums op (helper lhs env) (helper rhs env))]
               [id (name) (local [(define result (lookup-env env name))]
                            (if result
                                result
                                (error 'interp "Unbound identifier: ~s" name)))]
               [if0 (c t e) (if (eq (helper c env) (numV 0))
                                (helper t env)
                                (helper e env))]
               [with (binding body) (error "preprocessor error")]
               [fun (args body) (cond [(= 1 (length args)) (closureV (first args) body env)]
                                      [(= 0 (length args)) (thunkV body env)]
                                      [else (error 'interp "Should only have one or zero arguments to a function in interp")])]
               [app (fun-expr args) (if (= 0 (length args)) 
                                        (local [(define the-thunk (helper fun-expr env))
                                                (define type-check (if (thunkV? the-thunk) 'noerror (error "fucked off")))
                                                (define thunk-body (thunkV-body the-thunk))
                                                (define thunk-env (thunkV-env the-thunk))]
                                          (helper thunk-body thunk-env))
                                        (local [(define arg (first args))
                                            (define the-function (helper fun-expr env))
                                            (define type-check (if (closureV? the-function) 'noerror (error "fucked off")))
                                            (define arg-value (helper arg env))
                                            (define fun-env (closureV-env the-function))
                                            (define the-body (closureV-body the-function))
                                            (define param-name (closureV-param the-function))]
                                          (helper the-body (extend-env fun-env param-name arg-value))))]))]
    (helper expr (mtEnv))))

;; run : sexp -> CFWAE-Value
;; Consumes an sexp and passes it through parsing, pre-processing,
;; and then interpretation to produce a result.
(define (run sexp)
  (interp (pre-process (parse sexp))))


;; Possibly useful additional functions:

;; failed-tests : -> (listof plai-test-result)
;; Generates a list of only the failed (non-good) tests from plai-all-test-results.
(define (failed-tests)
  (reverse (filter (compose not (curry symbol=? 'good) first) plai-all-test-results)))

;; CFWAE-pre-fold : (CFWAE -> CFWAE) CFWAE -> CFWAE
;; Takes a function and applies it to each expression node in the 
;; given CFWAE.  Note that the function is applied pre-order; so
;; it is applied to a node before its sub-trees.  WARNING: if
;; your function generates a new node that itself needs to be
;; re-processed through the function, CFWAE-pre-fold will not do
;; so.  (It calls f on a node and then recurses into any sub-nodes
;; of whatever node f returns.  It does not reprocess the node 
;; itself.)
(define (CFWAE-pre-fold f expr)
  (local ([define (ffold expr)
            (type-case CFWAE (f expr)
              [num (n) (num n)]
              [binop (op lhs rhs) (binop op (ffold lhs) (ffold rhs))]
              [with (b body) (with (binding (binding-name b)
                                            (ffold (binding-named-expr b)))
                                   (ffold body))]
              [id (name) (id name)]
              [if0 (c t e) (if0 (ffold c) (ffold t) (ffold e))]
              [fun (args body) (fun args (ffold body))]
              [app (f args) (app (ffold f) (map ffold args))])])
    (ffold expr)))

;; Example: 
;; swap-op-args : CFWAE -> CFWAE
;; Consumes a program and generates the corresponding program in which
;; each instance of a binop has had its lhs and rhs swapped.
;(define (swap-op-args program)
;  (CFWAE-pre-fold (lambda (exp)
;                    (type-case CFWAE exp
;                               [binop (op lhs rhs) (binop op rhs lhs)]
;                               [else exp]))
;                  program))
;
;(test (swap-op-args (parse '{+ 1 2})) (parse '{+ 2 1}))
;(test (swap-op-args (parse '{+ 3 {- {* 1 2} {/ 3 4}}}))
;      (parse '{+ {- {/ 4 3} {* 2 1}} 3}))
;(test (swap-op-args (parse '{fun {x} {+ x {if0 0 {+ 1 2} 3}}}))
;      (parse '{fun {x} {+ {if0 0 {+ 2 1} 3} x}}))

(define (eq a b)
	(if (and (numV? a) (numV? b))
	  (= (numV-n a) (numV-n b))
		(error "illegal argument")))


(define (compute-nums exp num1 num2)
  (if (and (numV? num1) (numV? num2))
        (if (and (equal? / exp) (= 0 (numV-n num2))) 
            (error "divide by zero error")
            (numV (exp (numV-n num1) (numV-n num2))))
            (error "Passed invalid numbers")))

(test (compute-nums - (numV 3) (numV 4)) (numV -1))
(test (compute-nums + (numV 3) (numV 4)) (numV 7))
(test (compute-nums * (numV 3) (numV 4)) (numV 12))
(test (compute-nums / (numV 8) (numV 4)) (numV 2))

(define (extend-env env id value)
  (anEnv id value env))

(define (lookup-env env id)
  (if (mtEnv? env)
      (error 'lookup-env "Unbound identifier: ~s" id)
      (if (symbol=? (anEnv-name env) id)
          (anEnv-value env)
          (lookup-env (anEnv-env env) id))))



(test (lookup-env (anEnv 'y (numV 2) (anEnv 'x (numV 1) (mtEnv))) 'y) (numV 2))
(test (lookup-env (anEnv 'y (numV 2) (anEnv 'x (numV 1) (mtEnv))) 'x) (numV 1))

;; PARSE

(test (parse 0) (num 0))
(test (parse '{+ 1 2}) (binop + (num 1) (num 2)))
(test (parse '{+ 1 {+ 1 2}}) (binop + (num 1) (binop + (num 1) (num 2))))
(test (parse '{with {x 1} x}) (with (binding 'x (num 1)) (id 'x)))
(test (parse '{fun {x} x}) (fun (list 'x) (id 'x)))
(test (parse '{fun {x} {+ x 1}}) (fun (list 'x) (binop + (id 'x) (num 1))))

(test (parse 1) (num 1))
(test (parse 2) (num 2))

(test (parse 'x) (id 'x))
(test (parse 'y) (id 'y))

(test (parse '{+ 1 2}) (binop + (num 1) (num 2)))
(test (parse '{+ x y}) (binop + (id 'x) (id 'y)))

(test (parse '{with {x 1} 2}) (with (binding 'x (num 1)) (num 2)))
(test (parse '{with {x y} z}) (with (binding 'x (id 'y)) (id 'z)))  ; these errors are not the parser's job

(test (parse '{fun {x} 1}) (fun (list 'x) (num 1)))
(test (parse '{fun {y} z}) (fun (list 'y) (id 'z)))

(test (parse '{x y}) (app (id 'x) (list (id 'y))))
(test (parse '{1 2}) (app (num 1) (list (num 2))))  ; this error is not the parser's job

(test (parse '(with (x 10) x)) (with (binding 'x (num 10)) (id 'x)))



;; PRE-PROCESS

(test (pre-process (with (binding 'x (num 1)) (id 'x))) 
      (app (fun (list 'x) (id 'x)) (list (num 1))))
(test (pre-process (with (binding 'x (binop + (num 1) (num 2))) (id 'x))) 
      (app (fun (list 'x) (id 'x)) (list (binop + (num 1) (num 2)))))

(test (pre-process (fun (list 'x) (id 'x))) (fun (list 'x) (id 'x)))

(test (pre-process (fun (list 'x 'y) (id 'x)))
      (fun (list 'x) (fun (list 'y) (id 'x))))

(test (pre-process (fun (list 'x 'y 'z) (id 'x)))
      (fun (list 'x) (fun (list 'y) (fun (list 'z) (id 'x)))))

(test (pre-process (with (binding 'x (num 10)) (id 'x))) (app (fun '(x) (id 'x)) (list (num 10))))



;; INTERP
(test (run 5) (numV 5))
(test (run '{+ 1 5}) (numV 6))
(test (run '{- 1 5}) (numV -4))
(test (run '(fun {x} {+ x 1})) (closureV 'x (binop + (id 'x) (num 1)) (mtEnv)))
(test (run '(if0 0 1 5)) (numV 1))
(test (run '(if0 5 1 5)) (numV 5))

(test (run '(- 10 (+ 20 30))) (numV -40))
(test (run '(with (x 10) (+ x x))) (numV 20))
(test (run '(with (f (fun (x y) (+ x y))) (f 10 20))) (numV 30))
(test (run '(with (f (fun (x y z) (- x (+ y z)))) (f 1 2 3))) (numV -4))
(test (run '((fun (x y z) (- x (+ y z))) 1 2 3)) (numV -4))

(test (run '(with (x ((fun (x y) (+ x y)) 3 4)) x)) (numV 7))
(test (run '(with (apply (fun (f x) (f x))) 
                  (apply (fun (a) (+ a 1)) 3))) (numV 4))


(test (run '(with (apply (fun (f x y) (f x y))) 
                  (apply (fun (a b) (+ a b)) 3 4))) (numV 7))

(test (run '(with (apply (fun (x f y) (f x y))) 
                  (apply 3 (fun (a b) (+ a b)) 4))) (numV 7))

(test/exn (interp (pre-process 
                   (with (binding (quote add) (fun (quote (x y)) (binop + (id (quote x)) (id (quote y))))) 
                         (if0 (app (id (quote add)) (list (num 3))) 
                              (app (id (quote add)) (list (num 2) (num 3))) 
                              (app (id (quote add)) (list (num 3) (num 4))))))) "")


(test (run '(with (x 10) (+ x x))) (numV 20))
(test (run '(with (f (fun (x y) (+ x y))) (f 10 20))) (numV 30))
(test (run '(with (f (fun (x y z) (- x (+ y z)))) (f 1 2 3))) (numV -4))
(test (run '((fun (x y z) (- x (+ y z))) 1 2 3)) (numV -4))

;; Thunk test
(test (run '((fun (+ 10 10)))) (numV 20))

(test (run '(with (apply (fun (f x) (f x))) 
                  (apply (fun (a) (+ a 1)) 3))) (numV 4))

(test (run '(with (apply (fun (f x y) (f x y))) 
                  (apply (fun (a b) (+ a b)) 3 4))) (numV 7))

(test (run '(with (apply (fun (x f y) (f x y))) 
                  (apply 3 (fun (a b) (+ a b)) 4))) (numV 7))

(test/exn (run '(/ 5 0)) "")

(test (run '(if0 0 3 (undef x))) (numV 3))
(test/exn (run '(if0 1 3 (undef x))) "")
