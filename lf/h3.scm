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



