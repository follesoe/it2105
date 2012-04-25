; Student nummber: 673677
; Homework Set V

; Task 1
(define (generator start step end-test filter term)
 (if (end-test start)
     '()
     (if (filter start)
	 (append (list (term start)) (generator (step start) step end-test filter term))
	 (append '() (generator (step start) step end-test filter term)))))

; Test
(generator 1 
	   (lambda (x) (+ x 11)) 
	   (lambda (x) (> x 100)) 
	   (lambda (x) (= 0 (modulo x 2))) square)
;Value: (144 1156 3136 6084 10000)

;1a

(define (gen-intlist2 a b s)
  (generator a 
	     (lambda (x) (+ x s))
	     (lambda (x) (> x b))
	     (lambda (x) x)
	     (lambda (x) x)))

; Tests
(gen-intlist2 1 10 1)
;Value: (1 2 3 4 5 6 7 8 9 10)
(gen-intlist2 0 100 10)
;Value: (0 10 20 30 40 50 60 70 80 90 100) 

;1b
; ------- from textbook
(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))
; ---------- /from texbook

(define (gen-primelist2 a b s)
  (generator a
             (lambda (x) (+ x s))
             (lambda (x) (> x b))
             prime?
             (lambda (x) x)))



(gen-primelist2 0 10 1)
;Value: (0 1 2 3 5 7)
(gen-primelist2 1000 1100 1)
;Value 111: (1009 1013 1019 1021 1031 1033 1039 1049 1051 1061 1063 1069 1087 1091 1093 1097)


;1c
(define (gen-intlists a b s)
 (generator a
             (lambda (x) (+ x s))
             (lambda (x) (> x b))
             prime?
             (lambda (x) (gen-primelist2 a x s)))) 

; Test
(gen-intlists 1 10 2) 
;Value: ((1) (1 3) (1 3 5) (1 3 5 7))




; Task 2
(define (for-each proc elements)
  (cond ((null? elements) '())
	(else
	 (proc (car elements))
	 (for-each proc (cdr elements)))))

; Test
(for-each (lambda (x) (display (* x x)) (display ",") ) '(1 2 3 4 5 6))


;From homework 3
(define (digit-count n d)
  (define (digit-c n d carry)
    (cond ((>= n 10)
           (digit-c (quotient n 10) d 
                  ; if remainder = d return ++carry else return carry
                    (if (= (remainder n 10) d) (+ 1 carry) carry)))
          ((and (< n 10) (= d n)) (+ carry 1))
          (else carry)))
  (digit-c n d 0))
; end homework3

(define (special-primes2 a b m d)
  (define (printprime x)
    (cond ((= (digit-count x d) m)
	   (display x)
	   (display ", "))))
    (for-each printprime (gen-primelist2 a b 1)))

; Test
(special-primes2 100 10000 3 1)
;1117, 1151, 1171, 1181, 1511, 1811, 2111, 4111, 8111,
(special-primes2 1 1000 2 1)
;11, 101, 113, 131, 151, 181, 191, 211, 311, 811, 911,


; Task 3
(define (deep-reverse x)
  (if (pair? x) (map deep-reverse (reverse x)) x))

(deep-reverse '(1 2 3 (1 2 3 (1 2 3))))
;Value: (((3 2 1) 3 2 1) 3 2 1)
(deep-reverse '(a b c d e f (a b c)))
;Value: ((c b a) f e d c b a)
(deep-reverse '(a b ((c (d e))) (f (g (h i j)))))
;Value: ((((j i h) g) f) (((e d) c)) b a)


; Task 4 
(define (tree-map proc tree)
  (cond ((null? tree) ())
	((not (pair? tree)) (proc tree))
	(else (cons (tree-map proc (car tree))
		     (tree-map proc (cdr tree))))))


;Test
(tree-map square '(1 ((2 3 (4)))))
;Value 12: (1 ((4 9 (16))))

; 4a
(define (calc-sqabs list)
  (tree-map (lambda (x) (sqrt (abs x))) list))

; test
(calc-sqabs '(10 20 30)) 
;Value: (3.1622776601683795 4.47213595499958 5.477225575051661)

;4b
(define (tree-add val tree)
  (tree-map (lambda (x) (+ x val)) tree))

;Test
(tree-add 1 '(1 ((2 3 (4)))))  
;Value 15: (2 ((3 4 (5))))

(tree-add 1 '(2 ((4 6 (8)))))
;Value: (3 ((5 7 (9))))

;4c
(define (tree-mul val tree)
  (tree-map (lambda (x) (* x val)) tree))

;Test
(tree-mul 2 '(1 ((2 3 (4)))))
;Value 18: (2 ((4 6 (8))))


; Task 5
(define (merge-lists l1 l2 key test)
  (define (real-merge l1 l2 result)
    (cond ((and (null? l1) (null? l2)) result)
	  ((null? l1) (append result l2))
	  ((null? l2) (append result l1))
	  (else (if (test (key (car l1)) (key (car l2)))
	    (real-merge (cdr l1) l2 (append result (cons (car l1) '())))
	    (real-merge l1 (cdr l2) (append result (cons (car l2) '())))))))
  (real-merge l1 l2 '()))


; tests
(merge-lists '((1) (3) (5)) '((2) (3) (5)) car <))
;Value: ((1) (2) (3) (3) (5) (5))

(merge-lists '((c 1) (b 4) (a 12)) '((d 3) (f 5) (g 10)) cadr <)
;Value: ((c 1) (d 3) (b 4) (f 5) (g 10) (a 12))

; Test unbalanced
(merge-lists '((c 1) (b 4) (a 12) (p 15)) '((d 3) (f 5) (g 10)) cadr <) 
;Value: ((c 1) (d 3) (b 4) (f 5) (g 10) (a 12) (p 15))

; Task 6
; from previous excersise.
(define (nth n elements)
  (cond ((> n 0) (nth (- n 1) (cdr elements)))
        (else (car elements))))
  
(define (before cur end elems)
  (if (>= cur end)
     ()
      (cons (nth cur elems) (before (+ cur 1) end elems))))

(define (after n cur elems)
   (if (< cur n) (after n (+ cur 1) (cdr elems))
       elems))
; end

(define (merge-sort elems key test)
  (if (= (length elems) 1)
        elems
        (let ((center (round (/ (length elems) 2))))
	  (merge-lists 
           (merge-sort (before 0 center elems) key test) 
	   (merge-sort (after center 0 elems) key test ) key test))))

; Tests
(merge-sort '((c 9) (b 1) (e 7) (p 3)) cadr < )
;Value: ((b 1) (p 3) (e 7) (c 9))
(merge-sort '((c 9) (b 1) (e 7) (p 3)) cadr > )
;Value: ((c 9) (e 7) (p 3) (b 1))


; Task 7
(define (basic-card-sort cards)
  (merge-sort (merge-sort cards cddr >) cadr <))

; Test

;; FROM PREVIOUS HOMEWORK! (I Should really learn include/load ;)
(define (remove-nth n elems)
  (append (before 0 n elems) (after (+ n 1) 0 elems)))

(define (insert-nth n item elems)
  (append (before 0 n elems) (list item) (after n 0 elems)))

(define (make-scard suit value) (cons 'scard (cons suit value)))



(define (gen-card-deck)
  ;(define (card-fabric suit type
  (append
   (map (lambda (x) (make-scard 1 x)) (gen-intlist2 2 14 1))
   (map (lambda (x) (make-scard 2 x)) (gen-intlist2 2 14 1))
   (map (lambda (x) (make-scard 3 x)) (gen-intlist2 2 14 1))
   (map (lambda (x) (make-scard 4 x)) (gen-intlist2 2 14 1))
  ))



(define (shuffle deck)
  (define (shuffle-helper lista listb)
    (if (> (length lista) 0)
        (shuffle-helper (remove-nth 0 lista) (insert-nth (random (+ 1 (length listb))) (car lista) listb))
        listb))
  (shuffle-helper deck '()))


; Tests
; I only ran one test, since the test was so "large".

(basic-card-sort (shuffle (gen-card-deck)))
;Value 79: ((scard 1 . 2) (scard 1 . 3) (scard 1 . 4) (scard 1 . 5) (scard 1 . 6) (scard 1 . 7) 
;(scard 1 . 8) (scard 1 . 9) (scard 1 . 10) (scard 1 . 11) (scard 1 . 12) (scard 1 . 13)
;(scard 1 . 14) (scard 2 . 2) (scard 2 . 3) (scard 2 . 4) (scard 2 . 5) (scard 2 . 6) 
;(scard 2 . 7) (scard 2 . 8) (scard 2 . 9) (scard 2 . 10) (scard 2 . 11) (scard 2 . 12) 
;(scard 2 . 13) (scard 2 . 14) (scard 3 . 2) (scard 3 . 3) (scard 3 . 4) (scard 3 . 5) 
;(scard 3 . 6) (scard 3 . 7) (scard 3 . 8) (scard 3 . 9) (scard 3 . 10) (scard 3 . 11) 
;(scard 3 . 12) (scard 3 . 13) (scard 3 . 14) (scard 4 . 2) (scard 4 . 3) (scard 4 . 4)
;(scard 4 . 5) (scard 4 . 6) (scard 4 . 7) (scard 4 . 8) (scard 4 . 9) (scard 4 . 10) 
;(scard 4 . 11) (scard 4 . 12) (scard 4 . 13) (scard 4 . 14))


; Task 8
(define (partition elems key eq-test)
()) ; I didn't get this!

; Test
(define test8 (gen-intlist2 1 20 1))
(partition test8 (lambda (elem) (modulo elem 4)) =)