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

; list of C-primitives in use so far.
(define prims (list (list "Int" "int") (list "Char" "char")))

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
                   (list "pred" '() (list "List" "List"))
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
(define (fun-out e o)
  (fprintf o "~a ~a(" (findf (λ (x) (equal? (second e) (car x))) prims) (car e))
  
(define (mk-fun e)
  (list (v-val (car e)) (car (member (v-type (third e)) (map car prims)))
        (map car (map v-val (filter (λ (x) (equal? (v-type x) "Prim")) (v-val (second e)))))))
(define (check-fun f e n) (let ([p (fn-name (v-val f))])
  (cond [(equal? p "list") (v e "List")] 
        [(equal? p "\\") (v e "Lambda")] 
        [(member p (map car prims)) (v (findf (λ (x) (equal? p (car x))) prims) "Prim")]
        [(equal? p ":") (begin (set! funs (push funs (mk-fun e)))
                               (fun-out (mk-fun e) (current-output-port)))]
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

(define (main)
  (let* ([e (check-parens (map lex (string-split-spec (read-line))))]
         [ei (parse e)])
    (write-spec ei) (out-c ei (current-output-port)) 
    (displayln funs) (fprintf (current-output-port) ";~n") (main)))

(main)