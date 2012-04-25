;; MNFIT-215 Autumn 2006   Homework #4 Answer key



;; 1) List picking from X = (A B (C D) ((E F G H) I) J)
;; B => (cadr x) = (car (cdr x))
;; C => (caaddr x) = (car (car (cdr (cdr x))))
;; E => (caaar (cdddr x))
;; F => (cadr (caar (cdddr x)))
;; I => (cadr (cadddr x))
;; J => (car (cddddr x))

;; 2) nth

(define (nth index elems)
  (define (loop n elems)
    (if (= n 0) (car elems)
	(loop (- n 1) (cdr elems))))
  (if (>= index 0) (loop index elems)
      (error "Invalid index for NTH")))

;; 3) Using nth on exercise 1

;; B => (nth 1 x)
;; C => (nth 0 (nth 2 x))
;; E => (nth 0 (nth 0 (nth 3 x)))
;; F => (nth 1 (nth 0 (nth 3 x)))
;; I => (nth 1 (nth 3 x))
;; J => (nth 4 x)

;; 4) Create lists using cons

(define *nil* #f)
(define (nil? x)
  (eq? *nil* x))

;; (cons (cons (cons 55 *nil*) *nil*) *nil*) => (((55)))
;; (cons 55 (cons 33 *nil*)) => (55 33)
;; (cons (cons 55 44) (cons 33 (cons 22 *nil*))) => ((55 . 44) 33 22)
;; (cons 55 (cons 33 (cons 44 *nil*))) => (55 33 44)
;; (cons (cons 55 33) (cons (cons 44 11) *nil*)) => ((55 . 33) (44 . 11))
;; (cons 1 (cons (cons 2 (cons (cons 3 (cons (cons 4 (cons 5 *nil*)) *nil*)) *nil*)) *nil*))
;; => (1 (2 (3 (4 5))))

;; 5) Mapping practice

(define (map proc items)
  (if (null? items) *nil*
      (cons (proc (car items)) (map proc (cdr items)))))


(define (factor-map elems k) (map (lambda (x) (* k x)) elems))
(define (prime-map x) (map (lambda (x) (if (prime? x) x *nil*)) x))

;; appmap (similar to common lisp's mapcan)

(define (appmap f elems)
  (if (null? elems) *nil*
	 (append (f (car elems)) (appmap f (cdr elems)))))

;; (appmap (lambda (x) (list x (square x))) '(1 2 3 4)) => (1 1 2 4 3 9 4 16)

(define (prime-map2 elems)
 (appmap (lambda (x) (if (prime? x) (list x) *nil*)) elems))


;; For mapping procedures that take 1 OR MORE arguments, this is very useful.

(define (mapn proc . list-of-lists)
  (if (null? (car list-of-lists)) *nil*
      (cons (apply proc (map car list-of-lists))
	    (apply mapn proc (map cdr list-of-lists)))))

(define (greater-map x y)
  (mapn (lambda (a b) (if (> a b) a b)) x y))

(define (compare-map x y pred)
  (mapn (lambda (a b) (if (pred a b) a b)) x y))

(define (sum-map . lists)
  (apply mapn + lists))



;; 6)  Representations for playing cards

;; Dotted pair notation where (s . n) is a pair of numbers, the first of which denotes a suit, the second
;; a card between two (2) and ace (14).

(define (make-scard suit value) (cons 'scard (cons suit value)))  ;; Constructor
(define (scard? card) (eq? (car card) 'scard)) ;; Type Checker
(define (scard-suit card) (car (cdr card))) ;; Selectors
(define (scard-value card) (cdr (cdr card))) ;;
(define (scard-name card)
  (let ((v (scard-value card))) 
    (list
	    (cond ((<= 2 v 10) v)
	          ((= v 11) 'jack)
	          ((= v 12) 'queen)
	          ((= v 13) 'king)
	          ((= v 14) 'ace)) 
       (nth (- (scard-suit card) 1) '(spade heart club diamond)))))

;; Querries

(define (jack? card) (= (card-value card) 11))
(define (queen? card) (= (card-value card) 12))
(define (king? card) (= (card-value card) 13))
(define (ace? card) (= (card-value card) 14))


;; Simple 2-item lists, but with each item being the name of the suit and name of the card type
;; pcard = pretty card

(define (make-pcard suit type) (cons 'pcard (list type suit)))
(define (pcard? card) (eq? (car card) 'pcard)) ;; Type Checker

(define (pcard-suit pcard)
  (let ((psuit (second pcard)))
	 (cond ((eq? psuit 'spade) 1)
	       ((eq? psuit 'heart) 2)
	       ((eq? psuit 'club) 3)
	       ((eq? psuit 'diamond) 4))))

(define (pcard-value pcard)
	(let ((pval (third pcard)))
	  (cond ((eq? pval 'ace) 14)
		     ((eq? pval 'king) 13)
			  ((eq? pval 'queen) 12)
	        ((eq? pval 'jack) 11))))

(define (pcard-name pcard) (cdr pcard))

;; Generic routines.  Above this, we don't need to know the classification (scard or pcard) of the card.

(define (card-suit card)
  (cond ((scard? card) (scard-suit card))
	     ((pcard? card) (pcard-suit card))
	     (else "Error")))

(define (card-value card)
  (cond ((scard? card) (scard-value card))
	     ((pcard? card) (pcard-value card))
	     (else "Error")))

(define (card-name card)
  (cond ((scard? card) (scard-name card))
	     ((pcard? card) (pcard-name card))
	     (else "Error")))




;; Querries of general cards.  Again, no need to know whether it's an scard or a pcard.

(define (jack? card) (= (card-value card) 11))
(define (queen? card) (= (card-value card) 12))
(define (king? card) (= (card-value card) 13))
(define (ace? card) (= (card-value card) 14))



;; 7) Generating a deck of cards using map, appmap and gen-numlist
 
 (define (gen-numlist start end increment)
	(cond ((or (and (> increment 0) (> start end))
	           (and (< increment 0) (< start end)))
	       *nil*)
	      (else (cons start (gen-numlist (+ start increment) end increment)))))

 (define (gen-card-deck)
	(appmap (lambda (suit)
	        (map (lambda (value) (make-scard suit value))
	             (gen-numlist 2 14 1)))
	        (gen-numlist 1 4 1))) 

(define (pp-card-deck deck) (map card-name deck))

;; 8) Inserting and removing

  (define (insert-nth n item elems)
	 (cond ((= n 0) (cons item elems))
	       ((nil? elems) *nil*)
		    (else (cons (car elems) (insert-nth (- n 1) item (cdr elems))))))

  (define (remove-nth n elems)
	 (cond ((= n 0) (cdr elems))
	       ((nil? elems) *nil*)
	       (else (cons (car elems) (remove-nth (- n 1) (cdr elems))))))

;; 9)  Insertion shuffle

(define (insert-shuffle deck)
  (define (rec cards new-deck)
	 (if (null? cards) new-deck
	     (rec (cdr cards) (insert-nth  (random (+ 1 (length new-deck))) (car cards) new-deck))))
 (rec deck *nil*))

;; 10) Removal shuffle

(define (remove-shuffle deck)
  (define (rec cards new-deck)
    (cond ((null? cards) new-deck)
	       (else
	         (let* ((spot (random (length cards)))
	                (card (nth spot cards)))
				  (rec (remove-nth spot cards) (cons card new-deck))))))
 (rec deck *nil*))

;; The removal shuffle is quite a bit better, since the insertion shuffle produces decks in which the 
;; probability of a card ending up near the middle of shuffled deck is inversely proportional to its
;; position in A.  That is, the top cards in A are inserted into B early on and are then pushed both 
;; both forward and backwards in the deck.  The odds of such an early card only being pushed in one
;; direction are rather slim, so these early cards will rarely end up near the front or back of the deck.
;; Such a bias is unacceptable for most card games.

;; With the removal shuffle, the chance of being chosen first, second, third, etc., and thus ending up
;; being the first, second, third, etc. card of B should be the same for each card in A.







