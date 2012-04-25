; IT-2105 Homework #1 Answer Key

;; 1a) Calculate the length of the hypotenuse of a right triangle, given the
;; lengths of the two sides of the right angle.

(define (square x) (* x x))

(define (hypotenuse a b) (sqrt (+ (square a) (square b))))

;; 1b) Calculate length of side a of right triangle, given side c, the hypotenuse, and
;; side b.

(define (right-angle-side c b) (sqrt (- (* c c) (* b b))))


;; 2) Calculate the missing side of a right triangle, given 2 sides and
;;  information as to whether or not one of those sides is the hypotenuse.
;;   flag > 0 says that the hypotenuse is the missing side.

(define (missing-side a b flag)
 (sqrt ((if (> flag 0) + -) (square (max a b)) (square (min a b)))))

;; 3) Define a simple recursive exponent function, where the power,b, is assumed
;; to be 0 or a positive integer.

(define (my-expt a b)
  (cond ((and (= a 0) (< b 0)) (display "Error: Divide by zero"))
	     ((= b 0) 1)
	     ((> b 0) (* a (my-expt a (- b 1))))
	     (else (/ (my-expt a (+ b 1)) a))))

;; 3b) Include a coefficient to calc c*a**b
(define (pterm c a b) (* c (my-expt a b)))

;; 4) Use my-expt to write a function to compute this polynomial:
;;  7X**5 + 12Y**4 + 8XY 

(define (big-poly x y)
 (+ (pterm 7 x 5) (pterm 12 y 4) (* 8 x y)))


;; 5) Print the winner
(define (who-wins? a b)
  (if (> a b) (write "A wins!")
    (if (> b a) (write "B wins!")
	   (write "A and B tie"))))

(define (who-wins2? a b)
  (or (and (> a b) (write "A wins!"))
	   (and (> b a) (write "B wins!"))
	   (write "A and B tie")))

;; 6) scissors, rock, paper winner

(define (rps-winner a b)
  (cond ((eq? a 'scissors)
	 	     (if (eq? b 'paper) 1
			    (if (eq? b 'rock) 2
					0)))
			((eq? a 'rock)
	 	     (if (eq? b 'scissors) 1
			    (if (eq? b 'paper) 2
					0)))
			((eq? a 'paper)
	 	     (if (eq? b 'rock) 1
			    (if (eq? b 'scissors) 2
					0)))))

(define (rps-alternative-winner a b)
  (if (eq? a b) 0
      (cond
       ((eq? a 'rock) (if (eq? b 'scissors) 1 2))
       ((eq? a 'paper) (if (eq? b 'rock) 1 2))
       ((eq? a 'scissors) (if (eq? b 'paper) 1 2)))))


(define (rps-dominator item)
  (cond ((eq? item 'scissors) 'rock)
	     ((eq? item 'rock) 'paper)
	     ((eq? item 'paper) 'scissors)))

;; 7) RPS players.  Inputs are the play last made by the player and his opponent.

(define (rps-player1 my-last other-last)
	(if (eq? my-last other-last) (rps-dominator my-last)
	    other-last))

(define (rps-player2 my-last other-last) other-last)
(define (rps-player3 my-last other-last) 'paper)
;; 8)
  (define (play-rps p1 p2 num-rounds p1-last p2-last p1-wins p2-wins)
	(cond ((<= num-rounds 0)
	          (display " Player 1 wins = ") (display p1-wins)
		       (display " Player 2 wins = ") (display p2-wins))
	      (else
	         (let* ((p1-move (p1 p1-last p2-last))
						 (p2-move (p2 p2-last p1-last))
	                (winner (rps-winner p1-move p2-move)))
	           (play-rps p1 p2 (- num-rounds 1) p1-move p2-move
	              (+ p1-wins (if (= winner 1) 1 0))
					  (+ p2-wins (if (= winner 2) 1 0)))))))

(define (rps p1 p2 num-rounds)
	(play-rps p1 p2 num-rounds 'scissors 'scissors 0 0))



;; 9 - 10) Substitution models for simple recursive and iterative processes.

(define (dec x) (- x 1))
(define (inc x) (+ x 1))
(define (funny+ a b)
  (if (= a 0) b (inc (funny+ (dec a) b))))




(define (funny+2 a b)
 (if (= a 0) b (funny+2 (dec a) (inc b))))

;; funny+ is recursive.  It calls itself with a reduced version of
;; the problem, solves the smaller problem, then returns that result to
;; be processed by waiting instantiations of the same procedure.  As
;; described on page 34, it builds up a series of deferred actions.  The
;; current STATE of the process is embodied in this series of deferred
;; actions.

;; Substitution model for funny+ (abbreviated as +)

;; (+ 4 5)
;; (inc (+ 3 5)) ;; deferred action = inc
;; (inc (inc (+ 2 5)))
;; (inc (inc (inc (+ 1 5))))
;; (inc (inc (inc (inc (+ 0 5)))))
;; (inc (inc (inc (inc 5))))
;; (inc (inc (inc 6)))
;; (inc (inc 7))
;; (inc 8)
;; 9

;; funny+2 is linear.  The entire STATE can be summarized by the CURRENTLY
;; active call to the procedure, since the b variable houses the running
;; sum.  There are no deferred actions, since the deepest call to funny+2
;; simply returns its b value as the final answer.

;; Substitution model for funny+2 (abbreviated as +)

;; (+ 4 5)
;; (+ 2 6)
;; (+ 1 7)
;; (+ 0 8)
;; 8

;; IT-2105 Autumn 2006 Homework #2 Answer key


;; 1) Recursive and iterative procedures to compute b**n/n!

(define (seq1-rec b n)
	(if (= n 0) 1 
	  (* (/ b n) (seq1-rec b (- n 1)))))

(define (seq1-iter b n)
  (define (iter m prod)
	 (if (= m 0) prod
	    (iter (- m 1) (* (/ b m) prod))))
  (iter n 1))

;; 2) This is similar to exercise 1.11 on page 42.

(define (g-rec n)
 (cond ((< n 4) n)
       (else (+ (g-rec (- n 1)) (* 4 (g-rec (- n 2)))
	             (* 8 (g-rec (- n 3))) (* 4 (g-rec (- n 4)))))))

;; Note how this iterative version computes g(n) for the SMALLEST values of
;; n first, then on each recursive step, it computes the next value and moves
;; the older values back one step.  The new values are always based on the
;; older values, so we save a lot of redundant computation.

(define (g-iter n) 
  (define (iter count n1 n2 n3 n4)
     (cond ((= count 0) n1)
           (else (iter (- count 1)
	                (+ n1 (* 4 n2) (* 8 n3) (* 4 n4)) n1 n2 n3))))
	(if (< n 4) 
       n
	    (iter (- n 3) 3 2 1 0)))


;; 3) Exercise 1.12, pg. 42

;; This treats the triangle as 2-d array aligned to the left.  Then, given the row and
;; column number, this returns the value in that loc of pascal's trangle.
(define (pascal row col)
 (cond ((or (< col 1) (< row 1) (< row col)) *nil*) ;; out of bounds
		  ((or (= col 1) (= col row)) 1) ;; Edges are always 1
        (else (+ (pascal (- row 1) col) (pascal (- row 1) (- col 1))))))


;; 4) Prime tests

;; First some stuff from the book:

(define (smallest-divisor n) (find-divisor n 2))
(define (square x) (* x x))

(define (find-divisor n test-div)
  (cond ((> (square test-div) n) n)
	((divides? test-div n) test-div)
	(else (find-divisor n (+ test-div 1)))))

(define (divides? a b) (= (remainder b a) 0))

(define (prime? n) (= n (smallest-divisor n)))

(define (print-prime p)
 (newline)  (display " *** Prime: ") (display p))

(define (primes-in-range a b)
 (if (prime? a) (print-prime a))
 (if (> a b) *nil*
     (primes-in-range (+ a 1) b)))

(define (first-n-primes a n)
  (cond ((= n 0) *nil*)
	((prime? a)
	   (print-prime a)
	   (first-n-primes (+ a 1) (- n 1)))
	(else (first-n-primes (+ a 1) n))))

;; (first-n-primes 1000000 10)

;; *** Prime: 1000003
;; *** Prime: 1000033
;; *** Prime: 1000037
;; *** Prime: 1000039
;; *** Prime: 1000081
;; *** Prime: 1000099
;; *** Prime: 1000117
;; *** Prime: 1000121
;; *** Prime: 1000133
;; *** Prime: 1000151()


;; (first-n-primes 1000000000 1)
;; *** Prime: 1000000007

;; 5) Goldbach's Conjecture = all even numbers > 2 are the sum of 2 primes.

;; Find largest prime less than or equal to n.  This is used in goldbach.
(define (max-prime n)
  (if (< n 2) 0
     (if (prime? n) n (max-prime (- n 1)))))

;; Find smallest prime greater than or equal to n.  This is used later (exercise 9).
(define (min-prime n)
  (cond ((<= n 2) 2)
	     ((prime? n) n)
	     (else (min-prime (+ n 1)))))
	

(define (goldbach k)
  (define (loop m)
    (cond ((= m 0) (display "Goldbach conjecture false for ") (display k))
	  ((prime? (- k m))
	   (display "Goldbach conjecture true for ") (display k)
	   (print-prime m) (print-prime (- k m)))
	  (else (loop (max-prime (- m 1))))))
  (if (and (> k 2)(even? k))
      (loop (/ k 2))
      (display "Goldbach conjecture only covers even numbers > 2")))

;; (goldbach 1000000000) =>
;; Goldbach conjecture true for 1000000000
;;  *** Prime: 499999931
;;  *** Prime: 500000069


;; 6) The accumulator 

;; Recursive version

(define (accum combiner null-value term a next b)
  (if (> a b) null-value
	(combiner (term a)
		  (accum combiner null-value term (next a) next b))))

;; Iterative version
(define (accum2 combiner null-value term a next b)
  (define (iter sum a)
    (if (> a b) sum
	(iter (combiner (term a) sum) (next a))))
  (iter null-value a))

;; Note how the variable a in the iter procedure SHADOWS the argument to accum2
;; that is also named a.

;; 7) Using the accumulators

;; Useful auxiliary functions

(define (increment x) (+ x 1))
(define (identity x) x)
(define (factorial n)
  (cond ((= n 0) 0)
	     ((= n 1) 1)
	     (* n (factorial (- n 1)))))

(define (cube x) (* x x x))
(define (composite? x) (not (prime? x)))


;; A few diverse sums

(define (sum-primes a b)
  (define (next-prime a) (min-prime (+ a 1)))
  (accum + 0 identity (min-prime a) next-prime b))

;; factorial
(define (acc-fact n)
  (accum * 1 identity 1 increment n))

(define (sum-fracs a b)
  (define (inverse x) (/ 1 x))
  (accum + 0 inverse a increment b))

(define (sum-fact-exp a b)
  (define (term k) (/ (acc-fact k) (expt k k)))
  (accum + 0 term a increment b))

;; 8) Use the accumulator to compute the max and min values of any function f: y = f(x).

(define (mm-y f a b delta)
  (define (delta-incr x) (+ x delta))
  (display " min y value: ")
  (display (accum min 1000000 f a delta-incr b))
  (display " max y value: ") 
  (display (accum max -1000000 f a delta-incr b)))
  

;; Using the accumulator to compute the values of x that yield the max and min values of f(x).

(define (mm-x f a b delta)
  (define (delta-incr x) (+ x delta))
  (define (max-x x1 x2) (if (> (f x2) (f x1)) x2 x1))
  (define (min-x x1 x2) (if (< (f x2) (f x1)) x2 x1))
  (display " min x value: ")
  (display (accum min-x a identity a delta-incr b))
  (display " max x value: ")
  (display (accum max-x a identity a delta-incr b)))
 
;; Using these to compute the min and max (x and y) of x**3 - 4x**2 + 12

(define (mm-poly a b delta)
  (define (poly x) (+ (expt x 3) (* -4 (square x)) 12))
  (mm-x poly a b delta)
  (mm-y poly a b delta))

;; MNFIT-215   Autumn 2006  Homework # 3 Answer Key

;; 1) A filtering accumulator

(define (filt-accum combiner filter null-value term a next b)
  (cond ((> a b) null-value)
	((filter a)
	   (combiner (term a)
		     (filt-accum combiner filter null-value term (next a) next b)))
	(else
	   (filt-accum combiner filter null-value term (next a) next b))))

;; or, a version that saves a little typing.

(define (filt-accum-b comb filt null term a next b)
  (define (rec) (filt-accum-b comb filt null term (next a) next b))
  (cond ((> a b) null)
	((filt a)
	   (comb (term a) (rec)))
	(else (rec))))

;; Iterative version

(define (filt-accum2 comb filt null term a next b)
  (define (iter sum a)
    (cond ((> a b) sum)
	       ((filt a) (iter (comb (term a) sum) (next a)))
	       (else (iter sum (next a)))))
  (iter null a))

;; *** Tests of filtering accumulators *****

;; (filt-accum + prime? 0 cube 10000 increment 20000)

 ;  Value: 3830591053975187

;; (filt-accum * composite? 1 sqrt 100 increment 150)

;Value: 9.725745324848637e42

;; 2) Digit counting

;; Count number of occurences of digit d in integer n
(define (count-digit n d)
  (define (iter n sum)
    (if (< n 1) sum
        (iter (floor (/ n 10)) (if (= (remainder n 10) d) (+ sum 1) sum))))
  (iter (abs n) 0))

;; (count-digit 132333435363738393 3)  => 10

(define (sum-digits n)
  (define (iter n sum)
    (if (< n 1) sum
	(iter (floor (/ n 10)) (+ (remainder n 10) sum))))
  (iter (abs n) 0))

;; (sum-digits 8799999999991111111111)  => 115


;; 3) Filter-accum using lambdas

;; Sum all primes between a and b with m or more occurrences of digit d in them.
;; Print each before combining it.

(define (special-primes a b m d)
  (filt-accum + (lambda (x) (and (prime? x) (>= (count-digit x d) m))) 0
	      (lambda (x) (print-prime x) (newline)  x) a inc b))

;; (special-primes 100 10000 3 1)

;; *** Prime: 1117
;; *** Prime: 1151
;; *** Prime: 1171
;; *** Prime: 1181
;; *** Prime: 1511
;; *** Prime: 1811
;; *** Prime: 2111
;; *** Prime: 4111
;; *** Prime: 8111
;; 22275

;; (filt-accum + (lambda (x) (or (= (modulo x 3) 0) (= (modulo x 5) 0))) 0 identity 1 increment 100)

;Value: 2418

;; (filt-accum2 + (lambda (x) (and (composite? x) (not (= (modulo x 11) 0)))) 0 identity 1 increment 1000)

  ;Value: 379338

;; 4) Calculating x**8 + x**6 + x**4 + x**2 + x using lambda and let

(define (simp-poly1 x)
  (let* ((x2 (* x x))
	      (x4 (* x2 x2)))
	(+ (* x4 x4) (* x4 x2) x2 x)))

(define (simp-poly2 x)
  (let ((x2 (* x x)))
	 (let ((x4 (* x2 x2)))
     	(+ (* x4 x4) (* x4 x2) x2 x))))

(define (simp-poly3 x)
  ((lambda (x2)
      ((lambda (x4)
		   	(+ (* x4 x4) (* x4 x2) x2 x))
	    (* x2 x2)))
	(* x x)))



;; 5) Defining funky functions to return 42

;; a) for the call (funky-0)
(define (funky-0) 42)
;; b) for the call ((funky-1))
(define (funky-1) (lambda () 42))
;; c) for the call ((funky-2) 10)
(define (funky-2) (lambda (x) (+ x 32)))
;; d) for the call (((funky-3)))
(define (funky-3) (lambda () (lambda () 42)))

;; e) for the call (((funky-4 2) 3) 7)
(define (funky-4 x)
	(lambda (y) 
	  (lambda (z) (* x y z))))
	    
;; 6) Defining REALLY funky functions to return 42.  Restriction:  All funcs must take FUNCS as arguments.

;; for the call (rfunk-1 (rfunk-2))

(define (rfunk-1 f2) (f2))
(define (rfunk-2) (lambda () 42))

;; for the call (rfunk-3 (rfunk-4) (rfunk-4))

(define (rfunk-3 f1 f2) (+ (f1) (f2)))
(define (rfunk-4) (lambda () 21))

;; for the call ((rfunk-5 (rfunk-5 (rfunk-5 (lambda () 1))))), with the added restriction that
;; ((rfunk-5 (lambda () 1))) => 2

(define (rfunk-5 f1)
 (lambda () (* (f1) (+ (f1) 1))))


;; 7) Continued fractions (Ex. 1.37 pg. 71)

(define (cont-frac n d k)
  (define (rec i)
     (if (= i k)
	 (/ (n k) (d k))
	 (/ (n i) (+ (d i) (rec (+ i 1))))))
  (rec 1))

;; Note that the recursive version builds the fraction from the top
;; down, while the iterative version builds it from the bottom up.

(define (cont-frac2 n d k)
  (define (iter i quotient)
    (if (= i 0) quotient
	(iter (- i 1) (/ (n i) (+ (d i) quotient)))))
  (iter (- k 1) (/ (n k) (d k))))

(define *golden-ratio* (/ (+ 1 (sqrt 5)) 2))

;; This assumes that n and d return the save value for each input
(define (cont-frac3 n d target error)
  (define (iter i quotient)
    (if (< (abs (- target quotient)) error) (list i quotient)
	(iter (+ i 1) (/ (n i) (+ (d i) quotient)))))
  (iter 1 (/ (n 1) (d 1))))

;; (cont-frac (lambda (x) 3) (lambda (x) 5) 10))  => 13464825/24871243
;; (cont-frac (lambda (x) 1) (lambda (x) 1) 50)  =>  12586269025/20365011074


;; To find 4-digit approximation to the golden ratio:
;; (cont-frac3 oner oner (/ 1 *golden-ratio*) .00001)
;; (12 0.618025751072961) ;; it only took 12 steps!

;; To find a value within 1 trillionth of the correct value:

;;(cont-frac3 oner oner (/ *golden-ratio*) .000000000001)
;; (29 0.618033988750541)
;; (/ 1 *golden-ratio*) => 0.618033988749895



;; 8) Derivatives via lambdas

(define *dx* .00001)
(define *tolerance* .00001)

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x *dx*)) (g x)) *dx*)))

(define (2nd-deriv g)
  (deriv (deriv g)))

(define (fixed-point f first-guess)
  (define (close? v1 v2) (< (abs (- v2 v1)) *tolerance*))
  (define (try guess)
    (let ((next (f guess)))
      (if (close? guess next)
	  next
	  (try next))))
  (try first-guess))

(define (zero-cross f first-guess)
	(fixed-point (lambda (x) (+ (f x) x)) first-guess))


(define (find-extremum f first-guess)
	(let* ((deriv1 (deriv f))
	       (deriv2 (deriv deriv1))
	       (d0 (zero-cross deriv1 first-guess))
	       (d2val (deriv2 d0)))
	 (display "Found a ") 
	 (display (cond ((> d2val 0) "minimum")
				       ((< d2val 0) "maximum")
						 (else "possible inflection point")))
	 (display " at f(" ) (display d0) (display ") = ")
     (display (f d0))))



;; 9)  Pam Picky's wrappers for Quinn Quickee

(define (range-wrap f a b)
  (lambda (x)
	 (cond ((<= a x b) (f x))
	       (else
	         (display "Error: Input value out of range")
	         0))))

(define (scale-input-wrap f factor)
	(lambda (x) (f (/ x factor))))

(define (scale-output-wrap f factor)
   (lambda (x) (/ (f x) factor)))

;; Putting them all together 

(define (super-wrap f in-scale out-scale range-a range-b)
  (scale-output-wrap 
	  (scale-input-wrap
	     (range-wrap f range-a range-b)
	    in-scale)
	out-scale))



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


	
  
 