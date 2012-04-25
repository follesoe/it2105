; Student nummber: 673677
; Homework Set 6

; include answer-keys :)
(load "h2")
(load "h3")
(load "h4")
(load "h5")


; Task 1
(define (n-of n f)
  (cond ((= n 0) *nil*)
	(else (append (n-of (- n 1) f) (list (f))))))

(define (do-times n f)
  (cond ((= n 0) *nil*)
	(else (f) (do-times (- n 1) f))))


;(n-of 10 (lambda () 'dust))
;Value 153: (dust dust dust dust dust dust dust dust dust dust)
;(do-times 30 (lambda() (display "-")))
;------------------------------
;Value: ()

; Task 2
(define (best-n-cards cards n)
  (firstn n (merge-sort cards cddr >)))

(define (nbest n cards)
  (if (>= (length cards) n) (best-n-cards cards n) '() ))

(define (5best cards) (nbest 5 cards))

(define (highest-sublist sublist)
  (let ((highest 0))
    (map
     (lambda (x)
       (if (pair? x)
	   (let ((elem (cddr (car x))))
	     (if (> elem highest) (set! highest elem) *nil*))
	   *nil*))
       sublist)
    highest))

; 2a - Best Card 
(define (best-card cards)
  (map cddr (merge-sort cards cddr >)))
	
(define (group-eq-val cards)
  (sorted-partition (basic-card-sort cards) cddr = <))

;2b - Pair
(define (pair cards)
  ;(display (map (lambda (elems) (nbest 2 elems)) (group-eq-val cards)))
  (highest-sublist (map (lambda (elems) (nbest 2 elems)) (group-eq-val cards))))

; 2c - Two Pair 
(define (twopairs cards)
  (let (
	(pair1 *nil*)
	(pair2 *nil*)
	(candidates (map (lambda (elems) (nbest 2 elems)) (group-eq-val cards))))
    (map 
     ;dirty lambda thats set pair1 and pair2 of the parent-scope
     (lambda (elem)
       (cond ((eq? pair1 *nil*) (set! pair1 elem))
	     ((eq? pair2 *nil*) (set! pair2 elem)))elem)
    candidates)
    (if (and (pair? pair1) (pair? pair2))  
        (list (cddr (car pair1)) (cddr (car pair2)))
	*nil*)))


; Test
;(best-card (insert-shuffle (gen-card-deck)))
;Value 57: (14 14 14 14 13 13 13 13 12 12 12 12 11 11 11 11 10 10 1..........
;(pair (list (make-scard 1 1) (make-scard 2 2)))
;Value 50: 0
;(pair (list (make-scard 1 1) (make-scard 1 1)))
;Value 51: 1 

;(twopairs (list (make-scard 1 1) (make-scard 2 2) (make-scard 1 3) (make-scard 2 4)))
;Value: ()
;(twopairs (list (make-scard 1 1) (make-scard 2 2) (make-scard 1 1) (make-scard 2 2)))
;Value 117: (2 1)
;(twopairs (insert-shuffle (gen-card-deck))) 
 

;2d - 3 of a kind
(define (3kind cards)
  (highest-sublist (map (lambda (elems) (nbest 3 elems)) (group-eq-val cards))))

;test
;(3kind (list (make-scard 1 1) (make-scard 2 2) (make-scard 3 3) (make-scard 4 4)))
;Value 58: 0
;(3kind (list (make-scard 1 1) (make-scard 2 1) (make-scard 3 1) (make-scard 4 1)))
;Value 59: 1



;2e - Straight
; Gets all straights 0..n (not 0..4)
(define (getlongstraights cards)
  (sorted-partition (basic-card-sort cards) cddr (lambda (x y) (= (- x y) 1)) >))

(define (straight cards)
  (highest-sublist (map 5best (getlongstraights cards))))))

; test
;(straight (list (make-scard 1 1) (make-scard 2 2) (make-scard 3 3) (make-scard 4 4) (make-scard 4 5))) 
;Value 62: 5
;(straight (list (make-scard 1 1) (make-scard 2 2) (make-scard 3 9) (make-scard 4 4) (make-scard 4 5))) 
;Value 61: 0



; 2f - Flush
; Gets all flushes 0..n
(define (getlongflushes cards); returns long flushes (not limited to best 5)
  (sorted-partition cards cadr = >))

(define (flush cards)
    (highest-sublist (map 5best (getlongflushes cards))))))

;test
;(flush (list (make-scard 1 1) (make-scard 2 2) (make-scard 3 3) (make-scard 4 4) (make-scard 4 5)))
;Value 63: 0
;(flush (list (make-scard 1 1) (make-scard 1 2) (make-scard 1 3) (make-scard 1 4) (make-scard 1 5)))
;Value 64: 5

; 2g House
(define (house cards)
  (let ((pair *nil*) (three *nil*))
    (map
     (lambda (subl)
       ; dirty lambda to set pair an three in scope to the 
       ; first available element of a list sorted n..0 
       (cond ((and (> (length subl) 2) (eq? three *nil*)) 
	      (set! three (nbest 3 subl)))
	     ((and (> (length subl) 1) (eq? pair *nil*))
	      (set! pair (nbest 2 subl))))
       subl)
     (group-eq-val cards))
    ; Pick out vals of cards and return them with power-index
    (if (and (pair? pair) (pair? three)) 
	(list (cddr (car three)) (cddr (car pair)))
	*nil*)))
; Test
;(house (insert-shuffle (gen-card-deck)))


; 2h 4-of-a-kind
(define (4kind cards)
  (highest-sublist (map (lambda (elems) (nbest 4 elems)) (group-eq-val cards)))) 

; 2i - straight flush INCOMPLETE
;(define (longstraightflush cards)
;  (map getlongflushes (getlongstraights cards)))
;(longstraightflush (insert-shuffle (gen-card-deck))) 
(define (straigh-flush cards) 0)


(define (calc-cards-power cards) 
  (cond ;((> (strait-flush cards) 0) (list 9 (strait-flush)))
	((> (4kind cards) 0) (list 8 (4kind cards)))
	((pair? (house cards)) (append '(7) (house cards)))
	((> (flush cards) 0) (list 6 (flush cards)))
	((> (straight cards) 0) (list 5 (straight cards)))
	((> (3kind cards) 0) (list 4 (3kind cards)))
	((pair? (twopairs cards)) (append '(3) (twopairs cards)))
	((> (pair cards) 0) (list 2 (pair cards)))
	(else (append '(1) (best-card cards)))))


;test
;(calc-cards-power (gen-card-deck))
;Value 138: (8 14)



;Task 3
(define (gen-active-deck)
  (let ((deck *nil*))
    (define (dispatch op)
      (cond ((eq? op 'init)
	     (set! deck (insert-shuffle(gen-card-deck))))
	    ((eq? op 'get)
	     (let ((ret (car deck)))
	       (set! deck (cdr deck))
	       ret))))
    dispatch))
  


; Test		    
;(define dck (gen-active-deck))
;(dck 'init)
;(dck 'get)
;Value 139: (scard 4 . 12)
;(dck 'get)
;Value 140: (scard 2 . 3)



;Task 4
(define (gen-card-player)
  (let ((hole-cards *nil*) (share-cards *nil*))
    (define (dispatch op . args)
      (cond ((eq? op 'reset)
	     (set! hole-cards *nil*)
	     (set! share-cards *nil*))
	    ((eq? op 'recieve)
      	     (set! hole-cards (append hole-cards args)))
	    ((eq? op 'share) (set! share-cards (car args)))
	    ((eq? op 'power)
	     (calc-cards-power  (append hole-cards share-cards)))
	    ((eq? op 'pp)
	     (for-each 
	      (lambda (card) 
		(display (card-name card))
		;(display  " : ")
		;(display (card-value card))
		(newline) card) 
	      (basic-card-sort (append hole-cards share-cards))) 
	     *nil*)))dispatch))

;Test
;(define player (gen-card-player))
;Value: player
;(player 'reset)
;Value: ()
;(player 'recieve (make-scard 1 14))
;Value: () 
;(player 'recieve (make-scard 2 2))
;Value: () 
;(player 'power)
;Value 156: (1 14)
;(player 'pp)
;(ace spade)


; Task 5
(define (gen-card-dealer n)
  
  (let ((players (n-of n gen-card-player)) (deck *nil*) (flop *nil*))

    (define (reset)
      (map (lambda (x) (x 'reset)) players) ;call reset on all palyers
      (set! deck (gen-active-deck))
      (deck 'init)
      (set! flop *nil*))

    (define (texas)
      (define (giveallcards)
	(for-each (lambda (player) (player 'recieve (deck 'get))) players))
     
      (define (addflop)
	(set! flop (append flop (list (deck 'get)))))

      (define (dist-flop)
	(for-each (lambda (x) (x 'share flop)) players))

      ;a 2 cards to all player
      (do-times 2 giveallcards)
      ;b 3 cards to the flop
      (do-times 3 addflop)(dist-flop)
      ;c 1 card to the flop
      (do-times 1 addflop)(dist-flop)
      ;d 1 card to the flop
      (do-times 1 addflop)(dist-flop) 
      (set! player (merge-sort players (lambda (player) (car (player 'power))) >))
      
      (for-each 
       (lambda (player) 
	 (newline)(newline)(player 'pp)
	 (display (player 'power))) 
       players)
       *nil*)

    (define (dispatch op)
      (cond ((eq? op 'reset) (reset))
	    ((eq? op 'texas) (texas))))
    dispatch))


;(define test-dealer (gen-card-dealer 3))
;(test-dealer 'reset)
;(test-dealer 'texas)

;(ace spade)
;(4 heart)
;(5 heart)
;(ace heart)
;(10 club)
;(queen club)
;(6 diamond)
;(2 14)

;(4 spade)
;(ace spade)
;(4 heart)
;(ace heart)
;(queen club)
;(ace club)
;(6 diamond)
;(7 14 4)

;(ace spade)
;(4 heart)
;(ace heart)
;(5 club)
;(queen club)
;(2 diamond)
;(6 diamond)
;(2 14)
 
