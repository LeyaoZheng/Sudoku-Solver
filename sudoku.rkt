;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname sudoku) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; Question 1a)
(define (all-satisfy? pred? matrix)
  (foldr (lambda (row acc1)
           (and acc1
                (foldr (lambda (v acc2) (and acc2 (pred? v)))
                       true
                       row)))
         true
         matrix))


; Testing
(check-expect (all-satisfy? integer? '((2 3 4) (5 6 7)))
true)
(check-expect (all-satisfy? integer? '((2 3 4) (5 six 7)))
false)
(check-expect (all-satisfy? number? '()) true)               
(check-expect (all-satisfy? number? '(())) true)              
(check-expect (all-satisfy? number? '((1))) true)             
(check-expect (all-satisfy? number? '((1 2 3))) true)
(check-expect (all-satisfy? number? '((1 a 3))) false)       
(check-expect (all-satisfy? number? '((1 2) (3 4))) true)
(check-expect (all-satisfy? number? '((1 2) (3 a))) false)   
(check-expect (all-satisfy? symbol? '((a b) (c d))) true)
(check-expect (all-satisfy? symbol? '((a b) (1 d))) false)
(check-expect (all-satisfy? even? '((2 4 6) (8 10))) true)
(check-expect (all-satisfy? even? '((2 3 4))) false)

;; Question 1b)
(define (any-satisfy? pred? matrix)
  (foldr (lambda (row acc1)
           (or acc1
               (foldr (lambda (v acc2) (or acc2 (pred? v)))
                      false
                      row)))
         false
         matrix))


; Testing
(check-expect (any-satisfy? symbol? '((2 3 4) (5 6 7)))
false)
(check-expect (any-satisfy? symbol? '((2 3 4) (5 six 7)))
true)
(check-expect (any-satisfy? number? '()) false)
(check-expect (any-satisfy? number? '((a b c))) false)
(check-expect (any-satisfy? number? '((a b 1))) true)         
(check-expect (any-satisfy? number? '((1 b c))) true)        
(check-expect (any-satisfy? number? '((a b) (c d))) false)
(check-expect (any-satisfy? number? '((a b) (c 5))) true       )
(check-expect (any-satisfy? symbol? '((1 2) (3 a))) true)
(check-expect (any-satisfy? even? '((1 3 5))) false)
(check-expect (any-satisfy? even? '((1 3 5) (7 8))) true)

;; Question 1c)
(define (find-where pred? matrix)
  (local [
          (define (find-in-row row row-num col-num)
            (cond
              [(empty? row) empty]
              [(pred? (first row)) (list col-num row-num)]
              [else (find-in-row (rest row) row-num (add1 col-num))]))
          
          (define (find-in-matrix matrix row-num)
            (cond
              [(empty? matrix) empty]
              [else (local [(define result (find-in-row (first matrix) row-num 0))]
                      (cond
                        [(not (empty? result)) result]
                        [else (find-in-matrix (rest matrix) (add1 row-num))]))]))]
    
    (find-in-matrix matrix 0)))

;; Testing
(define wherematrix '(( 1 2 3 4 )
( 4 5 (3 6) (1 2) )
( (7) 8 9 () )))
(check-expect (find-where list? wherematrix) '(2 1))
(check-expect (find-where empty? wherematrix) '(3 2))
(check-expect (find-where integer? wherematrix) '(0 0))
(check-expect (find-where zero? '((0))) '(0 0))
(check-expect (find-where number? '((5))) '(0 0))
(check-expect (find-where even? '((1 3 4 7))) '(2 0))
(check-expect (find-where symbol? '((1 a 3))) '(1 0))
(check-expect (find-where string? '((1) (2) ("hi") (4))) '(0 2))
(check-expect (find-where negative? '((1 -1 2) (3 4))) '(1 0))
(check-expect (find-where even? '((1 3 5) (7 8 9))) '(1 1))
(check-expect
 (find-where empty? '((1 2) (() 3) (4)))
 '(0 1))
(check-expect
 (find-where list? '(((1) 2) (3 4)))
 '(0 0))  
(check-expect
 (find-where boolean? '((1 #t) (#f 3)))
 '(1 0))
(check-expect
 (find-where char? '((1 2) (3 #\a)))
 '(1 1))

;; Question 2
(define (strings->puzzle lst)
  (local
    [(define L (length (string->list (first lst))))

     (define (lst2 lst)
       (string->list lst))
     
     (define (lst1 lst)
       (cond
         [(empty? lst) empty]
         [else (append (list (lst2 (first lst)))
                       (lst1 (rest lst)))]))

     (define (trans lst)
       (cond
         [(empty? lst) empty]
         [(char=? #\? (first lst))
          (cons (build-list L
                            (lambda (x) (add1 x)))
                (trans (rest lst)))]
         [else
          (cons (list (- (char->integer (first lst)) 48))
                (trans (rest lst)))]))

      (define (lst3 lst)
        (cond
          [(empty? lst) empty]
          [else (append (list (trans (first lst)))
                        (lst3 (rest lst)))]))]

    (lst3 (lst1 lst))
  ))

;; Testing
(check-expect (strings->puzzle '("???"
"?3?"
"??2"))
'(( (1 2 3) (1 2 3) (1 2 3) )
( (1 2 3) (3) (1 2 3) )
( (1 2 3) (1 2 3) (2) )))
(check-expect (strings->puzzle '("??3?"
"??2?"
"?4??"
"????"))
'(( (1 2 3 4) (1 2 3 4) (3) (1 2 3 4) )
( (1 2 3 4) (1 2 3 4) (2) (1 2 3 4) )
( (1 2 3 4) (4) (1 2 3 4) (1 2 3 4) )
( (1 2 3 4) (1 2 3 4) (1 2 3 4) (1 2 3 4) )))
(check-expect (strings->puzzle '("?")) '(((1))))
(check-expect (strings->puzzle '("1")) '(((1))))
(check-expect (strings->puzzle '("???"))
              '(((1 2 3) (1 2 3) (1 2 3))))
(check-expect (strings->puzzle '("?3?"))
              '(((1 2 3) (3) (1 2 3))))

(check-expect (strings->puzzle '("??3?"
                                 "??2?"
                                 "?4??"
                                 "????"))
              '(((1 2 3 4) (1 2 3 4) (3) (1 2 3 4))
                ((1 2 3 4) (1 2 3 4) (2) (1 2 3 4))
                ((1 2 3 4) (4) (1 2 3 4) (1 2 3 4))
                ((1 2 3 4) (1 2 3 4) (1 2 3 4) (1 2 3 4))))


;; Question 3

(define (get-cell puzzle col row)
  (list-ref (list-ref puzzle row) col))

(define (set-cell puzzle col row new-cell)
  (map (lambda (r row-index)
         (cond
           [(= row-index row)
            (map (lambda (c col-index)
                   (cond
                     [(= col-index col) new-cell]
                     [else c]))
                 r
                 (build-list (length r) (lambda (x) x)))]
           [else r]))
       puzzle
       (build-list (length puzzle) (lambda (x) x))))

(define (remove-from-row puzzle row value)
  (map (lambda (r row-index)
         (cond
           [(= row-index row)
            (map (lambda (cell)
                   (cond
                     [(list? cell)
                      (filter (lambda (x) (not (= x value))) cell)]
                     [else cell]))
                 r)]
           [else r]))
       puzzle
       (build-list (length puzzle) (lambda (x) x))))

(define (remove-from-col puzzle col value)
  (map (lambda (r)
         (map (lambda (cell col-index)
                (cond
                  [(and (= col-index col)
                        (list? cell))
                   (filter (lambda (x) (not (= x value))) cell)]
                  [else cell]))
              r
              (build-list (length r) (lambda (x) x))))
       puzzle))

(define (remove-singles puzzle)
  (local [
          (define (find-first-single p)
            (local [(define (find-in-row row row-num col-num)
                      (cond
                        [(empty? row) empty]
                        [(and (list? (first row)) 
                              (= (length (first row)) 1))
                         (list col-num row-num (first (first row)))]
                        [else (find-in-row (rest row) row-num (add1 col-num))]))
                    
                    (define (find-in-matrix matrix row-num)
                      (cond
                        [(empty? matrix) empty]
                        [else (local [(define result (find-in-row (first matrix) row-num 0))]
                                (cond
                                  [(not (empty? result)) result]
                                  [else (find-in-matrix (rest matrix) (add1 row-num))]))]))]
              (find-in-matrix p 0)))
          
          (define (process-iteration p)
            (local [(define single (find-first-single p))]
              (cond
                [(empty? single) p]  
                [else
                 (local [(define col (first single))
                         (define row (second single))
                         (define value (third single))
                         
                         (define p-with-value (set-cell p col row value))
                         
                         (define p-cleaned-row (remove-from-row p-with-value row value))
                         (define p-cleaned (remove-from-col p-cleaned-row col value))]
                   
                   (process-iteration p-cleaned))])))]
    
    (process-iteration puzzle)))

;; Testing
(check-expect (remove-singles (strings->puzzle '("???" "?3?" "??2")))
              '((1 2 3) (2 3 1) (3 1 2)))
(check-expect (remove-singles (strings->puzzle '("??3?"
"??2?"
"?4??"
"????")))
'(( (1 2 4) (1 2) 3 (1 2 4) )
( (1 3 4) (1 3) 2 (1 3 4) )
( (2 3) 4 1 (2 3) )
( (1 2 3) (1 2 3) 4 (1 2 3) )))
(check-expect (remove-singles (strings->puzzle '("1")))
              '((1)))
(check-expect (remove-singles (strings->puzzle '("???"
                                                 "?3?"
                                                 "??2")))
              '((1 2 3)
                (2 3 1)
                (3 1 2)))
(check-expect (remove-singles (strings->puzzle '("??3?"
                                                 "??2?"
                                                 "?4??"
                                                 "????")))
              '(((1 2 4) (1 2) 3 (1 2 4))
                ((1 3 4) (1 3) 2 (1 3 4))
                ((2 3) 4 1 (2 3))
                ((1 2 3) (1 2 3) 4 (1 2 3))))
(check-expect (remove-singles '(((1 2) (2 3))
                                ((2 3) (1 3))))
              '(((1 2) (2 3))
                ((2 3) (1 3))))

;; Question 4
;; solve-latin: (Solution -> Bool) Puzzle -> (anyof Solution empty)
;; Produces first solution satisfying predicate, or empty if no solution exists

;
;; Test predicates from the assignment
;; yes: Any -> Bool
(define (yes x) true)

;; no: Any -> Bool  
(define (no x) false)

;; Fixed diagonal-has-2? function
(define (diagonal-has-2? p)
  (local [(define (check-diagonal p row)
            (cond
              [(empty? p) false]
              [else (or (= 2 (list-ref (first p) row))
                        (check-diagonal (rest p) (add1 row)))]))]
    (check-diagonal p 0)))

(define (my-list-ref lst n)
  (cond
    [(zero? n) (first lst)]
    [else (my-list-ref (rest lst) (sub1 n))]))

(define (solve-latin pred? puzzle)
  (local [

          (define (normalize-puzzle p)
            (map (lambda (row)
                   (map (lambda (cell)
                          (cond
                            [(number? cell) (list cell)]
                            [else cell]))
                        row))
                 p))
          
          (define (solved? p)
            (all-satisfy? number? p))

          (define (find-first-choice p)
            (find-where list? p))
          
          (define (get-possibilities p col row)
            (my-list-ref (my-list-ref p row) col))
          
          (define (set-cell-single p col row value)
            (set-cell p col row (list value)))
          
          (define (solve p)
            (local [(define processed (remove-singles p))]
              (cond
                [(any-satisfy? (lambda (cell)
                                 (and (list? cell) (empty? cell))) 
                               processed)
                 empty]
                
                [(solved? processed)
                 (cond
                   [(pred? processed) processed]
                   [else empty])]
                
                [else
                 (local [(define choice-pos (find-first-choice processed))]
                   (cond
                     [(empty? choice-pos) empty]
                     [else
                      (local [(define col (first choice-pos))
                              (define row (second choice-pos))
                              (define possibilities
                                (get-possibilities processed col row))]
                        
                        (local [(define (try-possibilities poss-list)
                                  (cond
                                    [(empty? poss-list) empty]
                                    [else
                                     (local [(define try-value (first poss-list))
                                             (define new-puzzle
                                               (set-cell-single processed col row try-value))
                                             (define result (solve new-puzzle))]
                                       (cond
                                         [(not (empty? result)) result]
                                         [else (try-possibilities (rest poss-list))]))]))]
                          
                          (try-possibilities possibilities)))]))])))]
    
    (solve (normalize-puzzle puzzle))))


;; Testing
(define 23puzzle (strings->puzzle '("???"
"?3?"
"??2")))
(check-expect (solve-latin yes 23puzzle)
'((1 2 3)
(2 3 1)
(3 1 2)))
(define 324puzzle (strings->puzzle '("??3?"
"??2?"
"?4??"
"????")))
(check-expect (solve-latin yes 324puzzle)
'((1 2 3 4)
(4 1 2 3)
(3 4 1 2)
(2 3 4 1)))
(check-expect (solve-latin diagonal-has-2? 324puzzle)
'((1 2 3 4)
(4 3 2 1)
(2 4 1 3)
(3 1 4 2)))
(check-expect (solve-latin no 324puzzle) empty)
(define P3 (strings->puzzle '("???"
                              "?3?"
                              "??2")))
(check-expect (solve-latin yes P3)
              '((1 2 3)
                (2 3 1)
                (3 1 2)))
(define P4 (strings->puzzle '("??3?"
                              "??2?"
                              "?4??"
                              "????")))
(check-expect (solve-latin yes P4)
              '((1 2 3 4)
                (4 1 2 3)
                (3 4 1 2)
                (2 3 4 1)))
(check-expect (solve-latin no P4) empty)
(check-expect (solve-latin diagonal-has-2? P4)
              '((1 2 3 4)
                (4 3 2 1)
                (2 4 1 3)
                (3 1 4 2)))
(check-expect (solve-latin yes
                           '(((1) (1))
                             ((1) (1))))
              empty)


;; Question 5
;; sudoku?: Solution -> Bool
;; Determine if all 3×3 sub-squares of a 9×9 solution
;; contain each of 1..9 exactly once.

;; take: Nat (listof X) -> (listof X)
;; produce the first n elements of lst
(define (take lst n)
  (cond
    [(zero? n) empty]
    [(empty? lst) empty]
    [else (cons (first lst)
                (take (rest lst) (sub1 n)))]))

;; drop: Nat (listof X) -> (listof X)
;; remove the first n elements of lst
(define (drop lst n)
  (cond
    [(zero? n) lst]
    [(empty? lst) empty]
    [else (drop (rest lst) (sub1 n))]))


(define (sudoku? sol)
  (local [
         ;; flattened 3×3 block at (r,c)
         (define (get-block r c)
           (foldr append empty
                  (map (lambda (row)
                         (take (drop row c) 3))
                       (take (drop sol r) 3))))

         (define (list-equal? a b)
           (cond
             [(and (empty? a) (empty? b)) true]
             [(or (empty? a) (empty? b)) false]
             [(= (first a) (first b))
              (list-equal? (rest a) (rest b))]
             [else false]))

         ;; block contains 1–9 exactly once
         (define (valid-block? block)
           (and (= (length block) 9)
                (list-equal? (sort block <)
                             '(1 2 3 4 5 6 7 8 9))))

         ;; no andmap → foldr
         (define (check-blocks starts)
           (foldr (lambda (pos acc)
                    (and acc
                         (valid-block?
                          (get-block (first pos) (second pos)))))
                  true
                  starts))

         (define block-starts
           '((0 0) (0 3) (0 6)
             (3 0) (3 3) (3 6)
             (6 0) (6 3) (6 6)))]
    
    (check-blocks block-starts)))




;; Testing
(define sample-sudoku (strings->puzzle '("?7????1?8"
"??5?1??8?"
"?6?7?5???"
"2??93????"
"3?1???8?6"
"????28??1"
"???2?7?1?"
"?1??4?2??"
"8?2????6?")))
(check-expect
(solve-latin yes sample-sudoku)
'((4 7 3 5 6 2 1 9 8)
(6 2 5 3 1 4 7 8 9)
(1 6 4 7 8 5 9 3 2)
(2 4 8 9 3 1 6 7 5)
(3 5 1 4 7 9 8 2 6)
(7 3 9 6 2 8 5 4 1)
(5 8 6 2 9 7 3 1 4)
(9 1 7 8 4 6 2 5 3)
(8 9 2 1 5 3 4 6 7)))
(check-expect (sudoku? '((1 2 3 4 5 6 7 8 9)
(4 6 5 7 8 9 1 2 3)
(7 8 9 1 2 3 4 5 6)
(4 5 6 7 8 9 1 2 3)
(1 2 3 4 5 6 7 8 9)
(7 8 9 1 2 3 4 5 6)
(7 8 9 1 2 3 6 5 4)
(4 5 6 7 8 9 1 2 3)
(3 2 1 4 5 6 7 8 9))) true)
(check-expect (sudoku? '((1 2 3 4 5 6 7 8 9)
(4 5 6 7 8 9 1 2 3)
(7 8 9 1 2 3 4 5 6)
(4 5 6 7 8 9 1 2 3)
(1 2 3 4 5 6 7 8 9)
(7 8 9 1 2 3 4 5 6)
(7 8 9 1 2 3 4 5 7)
(4 5 6 7 8 9 1 2 3)
(1 2 3 4 5 6 7 8 9))) false)
(check-expect (sudoku? '((1 2 3 4 5 6 7 8 9)
                         (4 6 5 7 8 9 1 2 3)
                         (7 8 9 1 2 3 4 5 6)
                         (4 5 6 7 8 9 1 2 3)
                         (1 2 3 4 5 6 7 8 9)
                         (7 8 9 1 2 3 4 5 6)
                         (7 8 9 1 2 3 6 5 4)
                         (4 5 6 7 8 9 1 2 3)
                         (3 2 1 4 5 6 7 8 9))) true)

(check-expect (sudoku? '((1 2 3 4 5 6 7 8 9)
                         (4 5 6 7 8 9 1 2 3)
                         (7 8 9 1 2 3 4 5 6)
                         (4 5 6 7 8 9 1 2 3)
                         (1 2 3 4 5 6 7 8 9)
                         (7 8 9 1 2 3 4 5 6)
                         (7 8 9 1 2 3 4 5 7)
                         (4 5 6 7 8 9 1 2 3)
                         (1 2 3 4 5 6 7 8 9))) false)

;; fail in only one sub-square
(check-expect (sudoku? '((1 2 3 4 5 6 7 8 9)
                         (4 5 6 7 8 9 1 2 3)
                         (7 8 9 1 2 3 4 5 6)
                         (1 2 3 4 5 6 7 8 9)
                         (4 5 6 7 8 9 1 2 3)
                         (7 8 9 1 2 3 4 5 6)
                         (1 2 3 4 5 6 7 8 9)
                         (4 5 6 7 8 9 1 2 3)
                         (7 8 9 1 2 3 4 5 5))) false)