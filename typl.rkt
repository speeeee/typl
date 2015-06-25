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

(define defs (list (list "Lit" (list "Any")) ; a -> b or a implies b
                   (list "Int" (list "Lit"))
                   (list "Sym" (list "Any"))
                   (list "Any" '())
                   #;(list (v (list "Lit" "Sym") "Union") (list "Fun"))))
(define funs (list (list "+" "Int" (list "Int" "Int"))))

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
#;(define (tequal? a b f) (let ([u (derive-union a)] [d (findf (λ (x) (=!? (car x) b)) f)])
  (or (=!? a b) (=!? u b) (ormap (λ (x) (=!? a x)) (second d)) (ormap (λ (x) (=!? u x)) (second d)))))

#;(define (tequal? a b) (let ([at (findf (λ (x) (equal? (car x) a)) defs)] [bt (findf (λ (x) (equal? (car x) b)) defs)])
                        (write-spec a) (write-spec b)
  (cond [(and at bt) (or (member a (append (list (car bt)) (second bt))) (member b (append (list (car at)) (second at))))]
        [(and (equal? (v-type a) "Union") (equal? (v-type b) "Union")
              (= (length (v-val a)) (length (v-val b)))) (andmap tequal? (v-val a) (v-val b))]
        [else (equal? a b)]))) ; fix some major typing issues.

#;(define (fun? e) (tequal? (derive-union (v-val e)) (v (list "Lit" "Sym") "Union")))
#;(define (simplify e)
  (cond [(member "PList" (map v-type (v-val e))) (simplify (map simplify (v-val e)))]
        [(and (equal? (v-type e) "PList") (fun? e)) (v (v-val e) "Fun")]
        [else e]))
        
(define (simplify e)
  (cond [(equal? (v-type e) "PList") (let* ([n (findf (λ (x) (equal? (car x) (v-val (car (v-val e))))) funs)]
                                            [f (if n (v (fn (car (v-val e)) (map simplify (cdr (v-val e)))) (second n)) (begin (displayln "function does not exist!") (v "#f" "None")))])
           (if (andmap teq? (map v-type (fn-ins (v-val f))) (third n)) f (v "#f" "None")))]
        [else e]))

(define (teq? a b) 
  (or (member a (append (list b) (second (findf (λ (x) (equal? (car x) b)) defs))))
      (member b (append (list a) (second (findf (λ (x) (equal? (car x) a)) defs))))))

#;(define (type-check e)
  ())

(define (parse e)
  (if (equal? (v-val (pop e)) "!") (simplify (v (ret-pop e) "PList")) e))

(define (main)
  (let ([e (check-parens (map lex (string-split-spec (read-line))))])
    (write-spec (parse e)) (main)))

(main)