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

(define *nil* '())
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
  (let ((psuit (nth 1 pcard)))
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







;; IT-2105 Autumn 2006  Homework #5 Answer Key

;; 1) The general generator

(define (generator start step end-test filter term)
  (define (loop x)
    (cond ((end-test x) *nil*)
	  ((filter x)
	     (cons (term x) (loop (step x))))
	  (else (loop (step x)))))
  (loop start))

(define (gen-intlist2 a b s)
  (generator a (lambda (x) (+ x s)) (lambda (x) (> x b))
	     identity identity))

(define (gen-primelist2 a b)
  (generator a (lambda (x) (+ x 1)) (lambda (x) (> x b))
	     prime? identity))

(define (last elems)
  (car (nthcdr (- (length elems) 1) elems)))

(define (gen-intlists a b s)
  (generator (list a) (lambda (x) (append x (list (+ (last x) s))))
	     (lambda (x) (> (last x)  b))
	     identity identity))


;; 2) for-each (Same as mapc in Common Lisp)

(define (for-each proc elems)
  (if (null? elems) *nil*
      (begin
	(proc (car elems))
	(for-each proc (cdr elems)))))

(define (special-primes2 a b m d)
  (for-each (lambda (p) (if (>= (count-digit p d) m)
			    (begin (newline)(display p))))
	    (gen-primelist2 a b)))

;; (special-primes2 100 10000 3 1) =>
;; 1117 1151 1171 1181 1511 1811 2111 4111 8111


;; 3) deep-reverse

(define (deep-reverse tree)
  (cond ((null? tree) *nil*)
	((pair? tree)
	   (append (deep-reverse (cdr tree))
		   (list (deep-reverse (car tree)))))
	(else tree)))



;; 4) tree-mapping

(define (tree-map proc tree)
  (cond ((null? tree) *nil*)
	((pair? tree)
	   (cons (tree-map proc (car tree))
		 (tree-map proc (cdr tree))))
	(else (proc tree))))

(define (square-tree2 tree) (tree-map square tree))
(define (sqrt-tree tree) (tree-map (lambda (x) (sqrt (abs x))) tree))
(define (tree-plus tree n) (tree-map (lambda (x) (+ x n)) tree))
(define (tree-scale tree factor)
  (tree-map (lambda (x) (* x factor)) tree))

;; 5) Merging sorted lists

(define (merge-lists l1 l2 key test)
  (define (rec l1 l2)
		(cond ((null? l1) l2)
	     	   ((null? l2) l1)
	     	   ((test (key (car l1)) (key (car l2)))
	            (cons (car l1) (rec (cdr l1) l2)))
		      (else (cons (car l2) (rec l1 (cdr l2))))))
	(rec l1 l2))

;; 6) Merge sort

(define (nthcdr n elems)
	(if (or (null? elems) (= n 0)) 
	    elems
	   (nthcdr (- n 1) (cdr elems))))

(define (firstn n elems)
  (if (or (null? elems) (= n 0)) *nil*
	  (cons (car elems) (firstn (- n 1) (cdr elems)))))
	   

(define (merge-sort elems key test)
	(define (rec elems)
	   (cond ((null? elems) *nil*)
	         ((null? (cdr elems)) elems)
	         (else
				 (let ((middle (round (/ (length elems) 2))))
	           (merge-lists
				    (rec (firstn middle elems))
	             (rec (nthcdr middle elems))
				   key test)))))
	(rec elems))

;; 7) Sorting a list of cards

(define (basic-card-sort cards)
  (sort-cards  cards identity
    (lambda (card1 card2)
	   (or (< (card-suit card1) (card-suit card2))
	       (and (= (card-suit card1) (card-suit card2))
			      (< (card-value card1) (card-value card2)))))))

(define (sort-cards cards key test) (merge-sort cards key test))

(define (gen-shuffled-deck) (remove-shuffle (gen-card-deck)))

;; 8) General partitioning

(define (add-to-partition elem partition key eq-test)
  (define (rec groups)
	 (cond ((null? groups) (list (list elem)))
	       ((eq-test (key elem) (key (caar groups)))
	          (cons (cons elem (car groups)) (cdr groups)))
		    (else (cons (car groups) (rec (cdr groups))))))
	(rec partition))

(define (partition elems key eq-test)
  (define (rec elems partition)
	 (if (null? elems)
	      partition
	     (rec (cdr elems) (add-to-partition (car elems) partition key eq-test))))
	(rec elems *nil*))

;; This sorts the partitions themselves, using the order-test and the first item in each group.
;; We will use it on homework set VI.
 
(define (sorted-partition elems key eq-test order-test)
  (merge-sort (partition elems key eq-test) (lambda (group) (key (car group))) order-test))


;; This retains the order of elements in partitions but removes the borders between them.
;; Hence, (flatten-partition '((1 3 5 7) (2 4 6 8))) => (1 3 5 7 2 4 6 8)
;; This is also used on homework set VI.

(define (flatten-partition partition) (apply append partition))

;; IT-2105 Autumn 2006 Homework #6 Answer Key

;; 1) n-of and do-times

;; Do the same thing (call f) n times and collect the results
(define (n-of n f)
  (if (<= n 0) *nil*
	  (cons (f) (n-of (- n 1) f))))

(define (do-times n f)
  (if (> n 0) 
	 (begin 
	   (f) 
	   (do-times (- n 1) f))))

;; 2)  Classifying cards, assuming the standard poker hands

;; Hand rankings from weakest to strongest, with power list beside each
;; 1) Highest card  - (1 val(high card), val(2nd high card) .... val(low-card))
;; 2) Pair          - (2 val(pair) val(highest other card), val(next-highest-other-card)...)
;; 3) 2 pairs       - (3 val(high pair) val(low pair) val(unpaired card))
;; 4) 3 of a kind   - (4 val(triple) val(next high card) val(last card))
;; 5) Straight      - (5 val(high card))
;; 6) Flush	        - (6 val(high card) val(2nd card) ... val(5th card))
;; 7) Full House    - (7 val(triple) val(pair))
;; 8) 4 of a kind   - (8 (val quad))
;; 9) Straight flush - (9 (val high card))

;; Some auxiliary stuff so that card-playing code refers to "grouping cards" instead of "partitioning".

(define (group-cards cards key eq-test) (partition cards key eq-test))

(define (sort-group-cards cards key eq-test order-test)
	 (sorted-partition cards key eq-test order-test))

;; This builds the straight in descending order so that it can stop as soon as it gets one of sufficient length (target-len)

(define (find-straight cards target-len)
  (define (scan cards straight) 
	 (cond ((< (+ (length cards) (length straight)) target-len) *nil*)
	 		 ((= (length straight) target-len) straight)
	       ((= (card-value (car cards)) (- (card-value (car straight)) 1))
	          (scan (cdr cards) (cons (car cards) straight)))
		    ((= (card-value (car cards)) (card-value (car straight)))
			    (scan (cdr cards) straight))
		    (else (scan (cdr cards) (list (car cards))))))
		       
  (let ((scards (sort-cards cards card-value >)))  ;; sorted (in descending value order) cards
	 (scan (cdr scards) (list (car scards)))))

;; This assumes that there is only one flush, which is a reasonable assumption in most
;; poker games.  

(define (find-flush cards target-len)
  (let* ((groups (group-cards cards card-suit = ))
	      (sgroups (merge-sort groups length >)))
	(if (>= (length (car sgroups)) target-len)
	    (car sgroups)
		 *nil*)))

;; Since a flush is worth more than a straight, once a flush is detected, we only need to check for
;; a straight WITHIN the flush.  We don't need to check for non-flush straights. Ratings are 6 for a
;; normal flush and 9 for a straight flush.

;; The target-len is a standard, normally 5, for all hand evaluations.  Hence, straights and flushes need to
;; have the same target-len in any type of poker, and at most target-len cards are used to compute the power of
;; a group of cards.
	    
(define (calc-flush-power flush target-len)
  (let ((str (find-straight flush target-len)))  ;; check for straight flush
	 (cond (str (list 9 (apply max (map card-value str))))
	       (else
		      (let ((sorted-flush (sort-cards flush card-value >)))  ;; sort into descending card-value order
				  (cons 6 (map card-value (firstn target-len sorted-flush))))))))

(define (straight-flush? power) (= (car power) 9))

;; This assumes that the straight is not a flush
(define (calc-straight-power str) (list 5 (apply max (map card-value str))))
(define (calc-4-kind-power quad) (list 8 (card-value (car quad))))  ;; quad is the four same-value cards
(define (calc-full-house-power 2-groups) (cons 7 (map (lambda (g) (card-value (car g))) 2-groups)))
  ;; No need to consider other cards, regardless of target-len, since there can be no ties with full houses.  If I have a
  ;; full house with kings high, nobody else can have 3 kings.

(define (calc-3-kind-power triple) (list 4 (card-value (car triple))))
  ;; Again, no need for a lot of tie-breaker info with 3 of a kind.
 
(define (calc-2-pair-power pairs other-cards target-len)
	(cons 3 (append (map (lambda (pair) (card-value (car pair))) pairs)
				       (map card-value (firstn (- target-len 4) (sort-cards other-cards card-value >))))))
(define (calc-pair-power pair other-cards target-len)
	(cons 2 (cons (card-value (car pair))
					  (map card-value (firstn (- target-len 2) (sort-cards other-cards card-value >))))))
(define (calc-high-card-power all-cards target-len)
	(cons 1 (map card-value (firstn target-len (sort-cards all-cards card-value >)))))

;; Although target-len is normally 5, by making it a variable, I can test the routine's ability to find
;; large straights and flushes...just for fun!

(define (calc-cards-power cards target-len)
	(let* ((flush (find-flush cards target-len))
	       (power (if flush (calc-flush-power flush target-len) (list 0))))
	 (if (straight-flush? power)
	     power
	     (let* ((groups (merge-sort (group-cards cards card-value =) length >))  ;; group same-value cards and sort groups by size.
			      (g1 (car groups))
	            (g-len (length groups))
	            (len (length g1)))
		    (cond ((= len 4) (calc-4-kind-power g1))
					 ((and (= len 3) (> g-len 1) (= (length (second groups)) 2)) (calc-full-house-power (firstn 2 groups)))
			       (flush power) ;; power already calc'd when assessing straight flush possibility.
		          (else
				      (let ((str (find-straight cards target-len)))
						  (cond (str (calc-straight-power str))
								  ((and (= len 2) (> g-len 1) (= (length (second groups)) 2))
										(calc-2-pair-power (firstn 2 groups) (flatten-partition (nthcdr 2 groups)) target-len))
								  ((= len 2)
										(calc-pair-power g1 (flatten-partition (cdr groups)) target-len))
								  (else (calc-high-card-power cards target-len))))))))))

(define (card-power-eq p1 p2)
  (cond ((or (null? p1) (null? p2)) *t*)  ;; if we make it this far, they're equal
	     ((= (car p1) (car p2))
	        (card-power-eq (cdr p1) (cdr p2)))
	     (else *nil*)))

(define (card-power-gt p1 p2)
  (cond ((or (null? p1) (null? p2)) *nil*)  ;; if we make it this far, they're equal, so p1 isn't less than p2
	     ((= (car p1) (car p2))
	        (card-power-gt (cdr p1) (cdr p2)))
	     ((> (car p1) (car p2)) *t*)
	     (else *nil*)))

(define (power-test n)
  (let* ((deck (gen-shuffled-deck))
	      (hand (firstn n deck)))
	(display (map card-name hand))
	(display " Power: ") (display (calc-cards-power hand 5))))



;; 3) Generating a deck closure.

(define (gen-active-deck)
  (let ((cards (gen-shuffled-deck)))
	 (lambda (command)
	  (cond ((eq? command 'init) (set! cards (gen-shuffled-deck)))
		     ((eq? command 'get)
	   	       (if cards
	    			    (let ((card (car cards)))
	      	          (set! cards (cdr cards))
	       				  card)
	     				*nil*))))))

;; 4) Generating a card-player closure

(define (gen-card-player)
  (let ((hole-cards *nil*)
	     (shared-cards *nil*)
	     (power *nil*))
	 (define (reset) (set! hole-cards *nil*) (set! power *nil*))
	 (define (receive-card c) (set! hole-cards (cons c hole-cards)))
	 (define (hand) (append hole-cards shared-cards))
	 (define (pp-hand) (display (map card-name (hand))) (display " Power: ") (display (get-hand-power)))
	 (define (get-hand-power) (or power 
										  (begin (set! power (calc-cards-power (hand) 5))
													power)))

	(lambda (command . args)
     (cond ((eq? command 'receive) (apply receive-card args))
	        ((eq? command 'reset) (reset))
	        ((eq? command 'share) (set! shared-cards (car args)))
	        ((eq? command 'power) (get-hand-power))
	        ((eq? command 'hand) (hand))
	        ((eq? command 'pp-hand) (pp-hand))
))))

;; 5) Sorting routine for players based on the power of their hand

(define (powersort-players players)
  (merge-sort players
	 (lambda (p) (p 'power))
	 card-power-gt))
	     
;; 6) Generating a card-dealer closure

(define (gen-card-dealer num-players)
  (let ((players (n-of num-players gen-card-player))
	     (deck (gen-active-deck))
	     (flop *nil*))  ;; flop = the shared cards

  (define (deal-card player) (player 'receive (deck 'get)))
  (define (deal-round) (map deal-card players))
  (define (deal-flop) (set! flop (cons (deck 'get) flop)))
  (define (show-flop) (map (lambda (player) (player 'share flop)) players))
  (define (pp-all-hands) (for-each (lambda (player) (player 'pp-hand) (newline)) players))
  (define (powersort) (set! players (powersort-players players)))
  (define (reset) 
	  (map (lambda (player) (player 'reset)) players)
	  (set! flop *nil*)
	  (deck 'init))
  (define (texas-hold-em)
    (do-times 2 deal-round)
	 (do-times 3 deal-flop) (show-flop)
	 ;; betting
	 (deal-flop) (show-flop)
    ;; betting
    (deal-flop) (show-flop)
    ;; betting
	 (powersort)
    (pp-all-hands))
	   
    ;; Dispatcher
	(lambda (command . args)
	  (cond ((eq? command 'reset) (reset))
	        ((eq? command 'texas) (texas-hold-em))))))


	
  
 
