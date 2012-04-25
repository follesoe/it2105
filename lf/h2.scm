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
 (if (> a b) nil
     (primes-in-range (+ a 1) b)))

(define (first-n-primes a n)
  (cond ((= n 0) nil)
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

