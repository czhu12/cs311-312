#lang plai

(define-type Binding
  [binding (name symbol?) (named-expr CFWAE?)])

(define *reserved-words* '(+ - * / with if0 fun))

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
    [(list '+ lhs rhs) (binop + (parse lhs) (parse rhs))]
    [(list 'with (list (and (? symbol?) (? (compose not reserved?)) id)
                       named-expr)
           body-expr)
     (with (binding id (parse named-expr)) (parse body-expr))]
    [(list 'if cond-expr t-expr f-expr) (if0 (parse cond-expr) (parse t-expr) (parse f-expr))]
    [(list 'fun fun-args fun-body) (fun fun-args (parse fun-body))]
    [(list (list 'fun fun-args fun-body) args-expr) (app (fun fun-args (parse fun-body)) (map parse args-expr))]
    [else (error 'parse "Unable to parse ~s" sexp)]))

(test (parse '(with (x 10) x)) (with (binding 'x (num 10)) (id 'x)))

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
    [if0 (c t f) (if c t f)]
    [fun (args body) (if (= 1 (length args))
                         (fun args body)
                         (fun (list (car args))
                              (pre-process (fun (rest args) body))))]
    [app (f args) (local [(define r-args (reverse args))] ;; we need to reverse the args in order to apply them in the correct order.
                    (if (= 1 (length r-args)) 
                        (app (pre-process f) r-args) 
                        (app (pre-process (app f (rest r-args))) (list (first r-args)))))]))

(pre-process (parse '((fun (x y) ( + x y)) (5 6))))
(test (pre-process (with (binding 'x (num 10)) (id 'x))) (app (fun '(x) (id 'x)) (list (num 10))))

;; interp : CFWAE -> CFWAE-Value
;; This procedure interprets the given CFWAE and produces a result 
;; in the form of a CFWAE-Value (either a closureV, thunkV, or numV).
;; (Assumes the input was successfully produced by pre-process.)
(define (interp expr)
  (local [(define (helper expr env)
            (type-case CFWAE expr
               [num (n) (numV n)]
               [binop (op lhs rhs) (add-nums (helper lhs env) (helper rhs env))]
               [id (name) (local [(define result (lookup-env env name))]
                            (if result
                                result
                                (error 'interp "Unbound identifier: ~s" name)))]
               [if0 (c t e) (if (eq (helper c env) (numV 0))
                                (helper t env)
                                (helper e env))]
               [with (binding body) (error "preprocessor is fucked")]
               [fun (args body) (closureV (first args) body env)]
               [app (fun-expr args) (local [(define arg (first args))
                                            (define the-function (helper fun-expr env))
                                            (define arg-value (helper arg env))
                                            (define fun-env (closureV-env the-function))
                                            (define the-body (closureV-body the-function))
                                            (define param-name (closureV-param the-function))]
                                      (helper the-body (extend-env fun-env param-name arg-value)))]))]
    (helper expr (mtEnv))))

(test (run '(with (x 10) (+ x x))) (numV 20))

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
  (= (numV-n a) (numV-n b)))

(define (add-nums num1 num2) 
  (numV (+ (numV-n num1) (numV-n num2))))

(define (extend-env env id value)
  (anEnv id value env))

(define (lookup-env env id)
  (if (mtEnv? env)
      (error "go fuck yourself")
      (if (symbol=? (anEnv-name env) id)
          (anEnv-value env)
          (lookup-env (anEnv-env env)))))