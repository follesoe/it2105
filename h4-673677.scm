; Student nummber: 673677
; Homework Set IV

; Task 1
(define X '(A B (C D) ((E F G H) I) J))

(cadr x)
;Value: b

(car (caddr x))
;Value: c

(car (car (cadddr x)))
;Value: e

(cadr (car (cadddr x)))
;Value: f

(cadr (cadddr x))
;Value: i

(cadr (cdddr x))
;Value: j


; Task 2

(define (nth n elements)
  (cond ((> n 0) (nth (- n 1) (cdr elements)))
	(else (car elements))))
  

; tests

(nth 0 x)
;Value: a
(nth 3 x)
;Value 187: ((e f g h) i)




; Task 3
;(define X '(A B (C D) ((E F G H) I) J))

(nth 1 x)
;Value: b

(nth 0 (nth 2 x))
;Value: c

(nth 0 (nth 0 (nth 3 x)))
;Value: e

(nth 1 (nth 0 (nth 3 x)))
;Value: f

(nth 1 (nth 3 x)) 
;Value: i

(nth 4 x))
;Value: j
 


; Task 4

;A
(cons (cons (cons 55 ()) ()) ())
;Value 184: (((55)))

;b
(cons 55 (cons 33 ()))
;Value 185: (55 33)

;c
(cons (cons 55 44) (cons 33 (cons 22 ())))
;Value 186: ((55 . 44) 33 22)

;d
(cons 55 (cons 33 (cons 44 ())))
;Value 183: (55 33 44)

;e
(cons (cons 55 33) (cons (cons 44 11) ()))
;Value 182: ((55 . 33) (44 . 11))

;f
(cons 1 (cons (cons 2 (cons (cons 3 (cons (cons 4 (cons 5 ()))()))())) ()))
;Value 181: (1 (2 (3 (4 5))))


; Task 5

; textbook
(define (map proc items)
  (if (null? items)
      ()
-      (cons (proc (car items))
	    (map proc (cdr items)))))
; end textbook

;a
(map (lambda (x) (* x 2)) '(1 2 3 4 5 6 7 8 9))

;b
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

(define (primefunc x)
  (cond ((prime? x) x)
  (else ())))


(define (appmap proc items)
  (if (null? items)
      ()
      (append (proc (car items))
            (appmap proc (cdr items)))))


(define (primefunc2 x)
  (cond ((prime? x) (list x))
  (else ())))


; Tests
(map primefunc '(1 2 3 4 5 6 7 8 9))
;Value 179: (1 2 3 () 5 () 7 () ())

(appmap primefunc2 '(1 2 3 4 5 6 7 8 9))
;Value 180: (1 2 3 5 7)

; Task 6

(define (make-scard suit value) (cons 'scard (cons suit value)))
(define (make-pcard pretty-suit pretty-value) (list 'pcard pretty-suit pretty-value))

(define (istype card suite)
  (if (= (cadr pcard) suite) #t #f))

(define (card-eq card type)
  (if (eq? (car card) type) #t #f))

(define (scard? scard) (card-eq scard 'scard))
(define (pcard? pcard) (card-eq pcard 'pcard))  

(define (getval card) (cddr card))
(define (pcard-getval pcard) (getval pcard))
(define (scard-getval scard) (getval scard))

(define (getsuit card) (cadr card))
(define (pcard-getsuit pcard) (getsuit pcard)) 
(define (scard-getsuit scard) (getsuit scard))   


(define (pcard-getname pcard)
  (cons (car (cdr pcard))  (cadr (cdr pcard))))


(define (scard-getname scard) 
  (let ((cval (scard-getval scard)) (csuit (scard-getsuit scard)))
    (cons  (cond ((= csuit 1) 'spade)
                ((= csuit 2) 'heart)
                ((= csuit 3) 'club)
                ((= csuit 4) 'diamond))

     (cond ((= cval 11) 'jack)
		((= cval 12) 'queen)
		((= cval 13) 'king)
		((= cval 14) 'ace)
		(else cval)))))
	 
	  
	  

(scard-getname (make-scard 1 12)) 

(define (getname card)
  (if (scard? card)
      (scard-getname card)
      (pcard-getname card)))

(define (card-suite card)
      (cdr (getname card)))

(define (card-value card)
      (car (getname card)))
 

(define (jack? card)
  (if (eq? (card-value card) 'jack) #t #f))

(define (queen? card)
  (if (eq? (card-value card) 'queen) #t #f))

(define (king? card)
  (if (eq? (card-value card) 'king) #t #f))

(define (ace? card)
  (if (eq? (card-value card) 'ace) #t #f))


; Tests
(jack? (make-scard 3 11))
;Value: #t
(jack? (make-scard 3 12))
;Value: ()
(getname (make-pcard 'heart 'jack))
;Value 176: (heart . jack)
(getname (make-scard 2 11)
;Value 177: (heart . jack))



; Task 7
(define (gen-numlist a b increment)
  (if (> a b)
      ()
      (cons a (gen-numlist (+ a increment) b increment))))


(define (gen-card-deck)
  ;(define (card-fabric suit type
  (append
   (map (lambda (x) (make-scard 1 x)) (gen-numlist 2 14 1))
   (map (lambda (x) (make-scard 2 x)) (gen-numlist 2 14 1))  
   (map (lambda (x) (make-scard 3 x)) (gen-numlist 2 14 1)) 
   (map (lambda (x) (make-scard 4 x)) (gen-numlist 2 14 1))
  ))
  
; Tests
(gen-numlist 1 10 3)
;Value 85: (1 4 7 10)                                                                                                                                                                                            
(gen-numlist 1 20 1)
;Value 87: (1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20)

(gen-card-deck)
;Value 153: ((scard 1 . 2) (scard 1 . 3) (scard 1 . 4) (scard 1 . 5) (scard 1 . 6) (scard 1 . 7) (scard 1 . 8) 
;(scard 1 . 9) (scard 1 . 10) (scard 1 . 11) (scard 1 . 12) (scard 1 . 13) (scard 1 . 14) (scard 2 . 2) 
;(scard 2 . 3) (scard 2 . 4) (scard 2 . 5) (scard 2 . 6) (scard 2 . 7) (scard 2 . 8) (scard 2 . 9) (scard 2 . 10) 
;(scard 2 . 11) (scard 2 . 12) (scard 2 . 13) (scard 2 . 14) (scard 3 . 2) (scard 3 . 3) (scard 3 . 4) (scard 3 . 5) 
;(scard 3 . 6) (scard 3 . 7) (scard 3 . 8) (scard 3 . 9) (scard 3 . 10) (scard 3 . 11) (scard 3 . 12) (scard 3 . 13) 
;(scard 3 . 14) (scard 4 . 2) (scard 4 . 3) (scard 4 . 4) (scard 4 . 5) (scard 4 . 6) (scard 4 . 7) (scard 4 . 8) 
;(scard 4 . 9) (scard 4 . 10) (scard 4 . 11) (scard 4 . 12) (scard 4 . 13) (scard 4 . 14))

(length (gen-card-deck))
;Value: 52



; Task 8
(define (before cur end elems)
  (if (>= cur end)
     ()
      (cons (nth cur elems) (before (+ cur 1) end elems))))

(define (after n cur elems)
   (if (< cur n) (after n (+ cur 1) (cdr elems))
       elems))

(define (insert-nth n item elems)
  (append (before 0 n elems) (list item) (after n 0 elems)))

(define (remove-nth n elems)
  (append (before 0 n elems) (after (+ n 1) 0 elems))) 


; Tests

(Define p '(1 2 3 4 5 6 7 8 9))
(insert-nth 0 'bingo p)
;Value 119: (1 bingo 2 3 4 5 6 7 8 9)

(remove-nth 4 p)
;Value 117: (1 2 3 4 6 7 8 9)


 
; Task 9

(define (shuffle deck)
  (define (shuffle-helper lista listb)
    (if (> (length lista) 0)
	(shuffle-helper (remove-nth 0 lista) (insert-nth (random (+ 1 (length listb))) (car lista) listb))
	listb))
  (shuffle-helper deck '()))   

;test
(shuffle (gen-card-deck))
;Value 150: ((scard 1 . 4) (scard 4 . 5) (scard 1 . 5) (scard 3 . 12) (scard 4 . 4) (scard 4 . 7) 
;(scard 2 . 10) (scard 1 . 9) (scard 2 . 14) (scard 2 . 5) (scard 4 . 8) (scard 1 . 7) (scard 4 . 11) 
;(scard 2 . 13) (scard 1 . 3) (scard 3 . 5) (scard 2 . 11) (scard 2 . 4) (scard 4 . 6) (scard 4 . 10) 
;(scard 2 . 6) (scard 2 . 2) (scard 4 . 14) (scard 3 . 9) (scard 2 . 12) (scard 2 . 7) (scard 1 . 6) 
;(scard 1 . 12) (scard 3 . 13) (scard 4 . 13) (scard 3 . 2) (scard 1 . 2) (scard 2 . 8) (scard 4 . 2) 
;(scard 3 . 7) (scard 3 . 6) (scard 1 . 11) (scard 2 . 3) (scard 1 . 10) (scard 3 . 10) (scard 3 . 11) 
;(scard 1 . 8) (scard 3 . 3) (scard 4 . 9) (scard 3 . 4) (scard 1 . 13) (scard 2 . 9) (scard 1 . 14) 
;(scard 3 . 8) (scard 3 . 14) (scard 4 . 12) (scard 4 . 3))


; Task 10
(define (shuffle2 deck)
  (define (helper lista listb)
    (if (eq? lista ())
	listb
	(let ((pos (random (length lista))))
	  (helper (remove-nth pos lista) (append (list (nth pos lista)) listb)))))
  (helper deck '()))

; I think the latter is more random, since the first (task 9) has (random 1) and (random 2) ...
; the latter has larger random in the beginning (random 52) (random 51).....

; test
(shuffle2 (gen-card-deck))
;Value 149: ((scard 1 . 9) (scard 3 . 4) (scard 2 . 12) (scard 4 . 14) (scard 2 . 8) (scard 4 . 13) 
;(scard 4 . 12) (scard 4 . 10) (scard 2 . 7) (scard 3 . 5) (scard 4 . 2) (scard 2 . 3) (scard 2 . 5) 
;(scard 1 . 6) (scard 1 . 5) (scard 2 . 13) (scard 1 . 7) (scard 1 . 10) (scard 1 . 2) (scard 3 . 12) 
;(scard 2 . 6) (scard 4 . 5) (scard 3 . 13) (scard 4 . 7) (scard 2 . 2) (scard 4 . 9) (scard 3 . 8) 
;(scard 2 . 4) (scard 4 . 11) (scard 4 . 8) (scard 3 . 7) (scard 3 . 9) (scard 3 . 6) (scard 2 . 11) 
;(scard 1 . 8) (scard 3 . 14) (scard 3 . 11) (scard 4 . 4) (scard 2 . 14) (scard 3 . 10) (scard 3 . 3) 
;(scard 1 . 12) (scard 1 . 3) (scard 4 . 6) (scard 1 . 14) (scard 4 . 3) (scard 1 . 4) (scard 2 . 10) 
;(scard 3 . 2) (scard 1 . 13) (scard 1 . 11) (scard 2 . 9))
