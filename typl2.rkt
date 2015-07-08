#lang racket
(require racket/list)

(struct v (val type))
(struct fn (name ins))
(define (push stk elt) (append stk (list elt)))
(define (pop stk) (car (reverse stk)))
(define (ret-pop stk) (reverse (cdr (reverse stk))))
(define (strcar str) (car (string->list str)))
(define (v=? va vb) (and (=!? (v-val va) (v-val vb)) (equal? (v-type va) (v-type vb))))
(define (write-spec ls) 
  (if (list? ls) (begin (display "(") (map write-spec ls) (display ")"))
      (cond [(v? ls) (begin (display "(v ") (write-spec (v-val ls)) (write-spec (v-type ls)) (display ")"))] 
            [(fn? ls) (begin (display "(f ") (write-spec (fn-name ls)) (write-spec (fn-ins ls)) (display ")"))]
            [else (write ls)])))
(define (=!? a b) ; general purpose 'equals?' meant for the structs.
  (cond [(and (v? a) (v? b)) (v=? a b)]
        [(and (fn? a) (fn? b)) (and (equal? (fn-name a) (fn-name b)) (=!? (fn-ins a) (fn-ins b)))]
        [(and (list? a) (list? b)) (andmap (λ (x y) (=!? x y)) a b)]
        [else (equal? a b)]))

(define outc '())
(define outh '())

; : a (list (Int b) (+ b 1)) (+ b 1)!
; : a (list (+ b c)) (+ b c)!

; list of C-primitives in use so far.
(define prims (list (list "Int" "int") (list "Bool" "int") (list "Char" "char")))

; both defs and funs are mutable.  defs is modified in the function, 'check-fun'.
; funs is modified in function, --.  Otherwise, there are no mutable variables.
#;(define defs (list (list "Lit" (list "Any")) ; a -> b or a implies b
                   (list "Int" (list "Lit"))
                   (list "Sym" (list "Any"))
                   (list "Any" '())
                   #;(list (v (list "Lit" "Sym") "Union") (list "Fun"))))
(define funs (list (list "+" "Int" (list "Int" "Int"))
                   (list "union" '() '()) (list "Int" '() (list "Any"))
                   (list "list" '() '())
                   (list "\\" '() (list "List" "List" "List"))
                   (list "pred" '() (list "Sym" "List"))
                   (list "->" '() (list "Any" "Any")) (list "=" '() (list "Any" "Any"))
                   (list ":" '() (list "Sym" "List" "Any"))))
; e.g. (: Fib Int [a] [(Int a) (> a 0)] 

(define (string-split-spec str) (map list->string (filter (λ (x) (not (empty? x))) (splt (string->list str) '(())))))
(define (splt str n) (let ([q (if (empty? str) #f (member (car str) (list #\( #\) #\{ #\} #\[ #\] #\!)))])
  (cond [(empty? str) n] ;[(empty? n) (splt (cdr str) (if (char-whitespace? (car str)) n (push n (car str))))]
        [(empty? (pop n)) (splt (cdr str) (if (char-whitespace? (car str)) 
                                              n (if q (append n (list (list (car str)) '())) (push (ret-pop n) (push (pop n) (car str))))))]
        [(char=? (car str) #\") (if (char=? (car (pop n)) #\") 
                                    (splt (cdr str) (append (ret-pop n) (list (push (pop n) #\") '()))) 
                                    (splt (cdr str) (push n (list #\"))))]
        [(char=? (car (pop n)) #\") (splt (cdr str) (push (ret-pop n) (push (pop n) (car str))))]
        [(char-whitespace? (car str)) (splt (cdr str) (push n '()))]
        [q (splt (cdr str) (append n (list (list (car str)) '())))]
        [else (splt (cdr str) (push (ret-pop n) (push (pop n) (car str))))])))

(define (lex l)
  (cond [(or (equal? (strcar l) #\-) (char-numeric? (strcar l))) (v l "Int")]
        [(member (strcar l) (list #\} #\) #\])) (v l "$close")]
        [else (v l "Sym")]))
(define (check-parens stk) (cp stk '()))
(define (cp stk n)
  (cond [(empty? stk) n]
        [(equal? (v-type (car stk)) "$close") (let* ([c (case (v-val (car stk)) [("}") "{"] [("]") "["] [(")") "("] [else '()])]
                                                     [l (λ (x) (not (equal? (v-val x) c)))])
           (cp (cdr stk) (push (ret-pop (reverse (dropf (reverse n) l))) (v (reverse (takef (reverse n) l))
                                                                            (case c [("{") "Union"] [("[") "List"] [("(") "PList"] [else '()])))))]
        [else (cp (cdr stk) (push n (car stk)))]))

(define (derive-union a)
  (v (sort (remove-duplicates (map v-type a)) string<?) "Union"))

#;(define (add-def! e f) (let ([ei (if (equal? (v-type e) "Sym") (v-val e) e)] [fi (if (equal? (v-type f) "Sym") (v-val f) f)])
  (set! defs (if (member ei (map car defs)) 
                 (push (filter (λ (x) (not (equal? ei (car x)))) defs)
                   (list ei (push (second (findf (λ (x) (equal? ei (car x))) defs)) fi)))
                 (push defs (list ei (list fi)))))))


(define (equ? e f) (or (equal? e f) (equal? e "Sym") (equal? f "Sym")))
(define (make-gate p o)
  (if (empty? p) '()
    (begin (fprintf o "if !(") (map (λ (x) (begin (out-c x o) (fprintf o "&&"))) (ret-pop p)) (out-c (pop p) o)
           (fprintf o ") { printf(\"ERROR: parameters did not match gate requirements.\\n\"); exit(0); }~nelse { "))))

; 'ei' means an edited 'e'. (edited by 'mk-fun')
(define (make-sig-h ei e o) (let ([c (λ (y) (findf (λ (x) (equal? y (car x))) prims))]) 
  (fprintf o "~a ~a(" (second (c (second ei))) (car ei))
  (map (λ (x) (fprintf o "~a , " x)) (map (λ (x) (second (c x))) (ret-pop (third ei))))
  (fprintf o "~a) ;~n" (second (c (pop (third ei)))))))
(define (make-sig ei e o) (let ([c (λ (y) (findf (λ (x) (equal? y (car x))) prims))]) 
  (fprintf o "~a ~a(" (second (c (second ei))) (car ei))
  (map (λ (x y) (fprintf o "~a ~a, " x y)) (map (λ (x) (second x)) (map c (ret-pop (third ei))))
       (map second (ret-pop (find-prims (v-val (second e)))))) 
  (fprintf o "~a ~a)" (second (c (pop (third ei))))
           (second (pop (find-prims (v-val (second e))))))))
(define (pred-out ei e o oh) (let ([d (filter (λ (x) (not (equal? (v-type x) "Prim"))) (v-val (second e)))])
  (make-sig ei e o) (make-sig-h ei e oh) (fprintf o " {~nreturn ") 
    (if (empty? d) (displayln "ERROR: useless predicate.  use `alias' instead.")
        (begin (map (λ (x) (out-c x o) (fprintf o "&&")) (ret-pop d)) (out-c (pop d) o) (fprintf o "; }~n")))))
(define (fun-out ei e o oh) (let ([d (filter (λ (x) (not (equal? (v-type x) "Prim"))) (v-val (second e)))])
  (make-sig ei e o) (make-sig-h ei e oh) (fprintf o " {~n") (make-gate d o) (fprintf o "return ")
  (out-c (third e) o) (fprintf o ";~n}") (if (empty? d) (fprintf o "~n") (fprintf o " }~n"))))

; filters primitives from the gate.
(define (find-prims preds)
  (map v-val (filter (λ (x) (equal? (v-type x) "Prim")) preds)))
(define (mk-fun e)
  (list (v-val (car e)) (car (member (v-type (third e)) (map car prims)))
        (map caar (find-prims (derive-prims (v-val (second e)))))))
; mk-prim: makes a primitive based off of call. 'e' is any value; 'p' is the function name.
(define (mk-prim e p) (v (list (findf (λ (x) (equal? p (car x))) prims) (v-val e)) "Prim"))
; list-prims: from function 'f' it lists the primitives based off of the parameters.
(define (list-prims f)
  (map mk-prim (filter (λ (x) (equal? (v-type x) "Sym")) (fn-ins f))
               (filter (λ (x) (not (list? x))) (map (λ (x y) (if (equal? (v-type x) "Sym") y '())) (fn-ins f) (third (findf (λ (x) (equal? (car x) (fn-name f))) funs))))))
; derive-prims: applies c-primitive portion of gates to the top-level gate. 'ep' is the gate.
(define (derive-prims ep) (dp ep '()))
(define (dp ep n)
  (cond [(empty? ep) n]
        [(not (equal? (v-type (car ep)) "Prim")) (dp (cdr ep) (append n (cons (car ep) (list-prims (v-val (car ep))))))]
        [else (dp (cdr ep) (push n (car ep)))]))

(define (import-h q) (let ([i (read-line q)])
  (if (eof-object? i) '() (begin (sig->fun i) (import-h q)))))

(define (check-fun f e n) (let ([p (fn-name (v-val f))])
  (cond [(equal? p "list") (v e "List")] 
        [(equal? p "import") (let ([q (open-input-file (string-join (list (v-val (car (v-val e))) ".h") ""))]) 
                               (import-h q) (close-input-port q))]
        [(equal? p "\\") (v e "Lambda")] 
        [(member p (map car prims)) (mk-prim (car e) p)]
        [(equal? p ":") (begin (set! funs (push funs (mk-fun e)))
                               (fun-out (mk-fun e) (list (car e) (v (derive-prims (v-val (second e))) "List") (third e))
                                        outc outh))]
        [(equal? p "pred") (begin (set! funs (push funs (list (v-val (car e)) "Bool" (map caar (find-prims (v-val (second e)))))))
                                  (pred-out (list (v-val (car e)) "Bool" (map caar (find-prims (v-val (second e))))) 
                                            (list (car e) (v (derive-prims (v-val (second e))) "List") (third e)) outc outh))]
        [(andmap equ? (map v-type (fn-ins (v-val f))) (third n)) f]
        [else (v '() "False")])))

(define (simplify e)
  (cond [(equal? (v-type e) "PList") (let* ([n (findf (λ (x) (equal? (car x) (v-val (car (v-val e))))) funs)]
                                            [e2 (map simplify (cdr (v-val e)))] [f (if n (v (fn (v-val (car (v-val e))) e2) (second n)) (begin (printf "function, ~a, does not exist!" (v-val (car (v-val e)))) (v "#f" "None")))])
           (check-fun f e2 n)#;(if (andmap teq? (map v-type (fn-ins (v-val f))) (third n)) f (v "#f" "None")))]
        [else e]))

#;(define (teq? a b) 
  (or (member a (append (list b) (second (findf (λ (x) (equal? (car x) b)) defs))))
      (member b (append (list a) (second (findf (λ (x) (equal? (car x) a)) defs))))))

#;(define (type-check e)
  ())

(define (out-c stk f)
  (cond [(fn? stk) (begin (fprintf f "~a(" (fn-name stk))
                          (map (λ (x) (begin (out-c x f) (fprintf f ","))) (ret-pop (fn-ins stk))) (out-c (pop (fn-ins stk)) f)
                          (fprintf f ")"))]
        [(v? stk) (out-c (v-val stk) f)]
        [else (fprintf f "~a" stk)]))

(define (parse e)
  (if (equal? (v-val (pop e)) "!") (simplify (v (ret-pop e) "PList")) e))

(define (list-exprs e n)
  (if (empty? e) n 
      (if (equal? (v-val (car e)) "!") (list-exprs (cdr e) (push n '()))
          (list-exprs (cdr e) (push (ret-pop n) (push (pop n) (car e)))))))
(define (parse+ e)
  (map simplify (map (λ (x) (v x "PList")) (filter (λ (x) (not (empty? x))) (list-exprs e '(()))))))

(define (read-expr i) (re i '()))
(define (re i n) (let ([c (read-char i)])
  (cond [(eof-object? c) '()]
        [(char=? c #\!) (list->string (append n (list #\!)))]
        [else (re i (push n c))])))

(define (prim->type p)
  (car (findf (λ (x) (equal? p (second x))) prims)))
(define (sig->fun s) (let ([e (filter (λ (x) (not (member x (list "(" ")" ";" ",")))) (string-split-spec s))])
  (set! funs (push funs (list (prim->type (second e)) (prim->type (car e)) 
                              (map prim->type (drop 2 e)))))))

(define (main)
  (let* ([e (check-parens (map lex (string-split-spec (read-line))))]
         [ei (parse e)])
    (write-spec ei) (out-c ei (current-output-port)) 
    (displayln funs) (fprintf (current-output-port) ";~n") (main)))

; just read-char until '!'.
(define (main2)
  (let* ([f (car (vector->list (current-command-line-arguments)))]
         [q (open-input-file (string-join (list f ".typ") ""))]
         #;[e (check-parens (map lex (string-split-spec (read-line q))))])
    (set! outc (open-output-file (string-join (list f ".c") "") #:exists 'replace))
    (set! outh (open-output-file (string-join (list f ".h") "") #:exists 'replace))
    (define (per-expr i) (let ([e (read-expr q)])
      (if (empty? e) '() (begin (parse+ (check-parens (map lex (string-split-spec e)))) (per-expr i)))))
    (per-expr q) (close-output-port outc) (close-output-port outh) (close-input-port q)))
(define (main-test)
  (let ([e (check-parens (map lex (string-split-spec (read-line))))])
    (parse+ e) #;(fprintf (current-output-port) ";~n") (main-test)))

(main2)