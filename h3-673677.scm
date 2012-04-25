; Student nummber: 673677
; Homework Set III


; Task 1
(define (filter-accum-rec combiner filter null-value term a next b)
  (if (> a b) 
      null-value      
      (if (filter a)
	 (combiner (term a) 
	  (filter-accum-rec combiner filter null-value term (next a) next b))
         (filter-accum-rec combiner filter null-value term (next a) next b))))



(define (filter-accum-iter combiner filter null-value term a next b pos)
  (if (> a b)   
      pos
      (if (filter a)
         (combiner (term a)
         (filter-accum-iter combiner filter null-value term (next a) next b 
			    (combiner (term a) pos))) 
	 (filter-accum-rec combiner filter null-value term (next a) next b))))


(define (filter-accum-iterative combiner filter null-value term a next b)
  (filter-accum-iter combiner filter null-value term a next b 0))




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

(define (tr a) (= 0 0))
(define (++ x) (+ x 1))


; tests.
(filter-accum-rec + prime? 0 (lambda (x) (* x x x)) 10000 ++ 20000)
 ;Value: 3830591053975187
(filter-accum-iterative + prime? 0 (lambda (x) (* x x x)) 10000 ++ 20000)
;Value: 3830591053975187
(filter-accum-iterative * (lambda (x) (not (prime? x))) 1 sqrt 100 ++ 150)
;Value: 9.725745324848637e42


; Task2
(define (digit-count n d)
  (define (digit-c n d carry)
    (cond ((>= n 10)
	   (digit-c (quotient n 10) d 
		  ; if remainder = d return ++carry else return carry
		    (if (= (remainder n 10) d) (+ 1 carry) carry)))
	  ((and (< n 10) (= d n)) (+ carry 1))
	  (else carry)))
  (digit-c n d 0))

(digit-count 666 6)
;Value: 3
(digit-count 88877888 8)
;Value: 6
(digit-count 56 8)
;Value: 0

; Task3
(define (special-primes a b m d)
  (filter-accum-iterative +
   (lambda (x) (and (>= (digit-count x d) m) (prime? x))) 0 
   (lambda (x) x) a ++ b))

(define (special-myown1 a b)
  (filter-accum-iterative +
   (lambda (x) (= (remainder x 3) 0)) 
   0
   (lambda (x) x) 
   a ++ b))

(special-primes 100 10000 1 1)
;Value: 2325539

(special-myown1 100 10000)
;Value: 16666650


; Task 4
(define (calc1 x)
  (let* ((x2 (* x x)) (x4 (* x2 x2)) (x6 (* x4 x2)) (x8 (* x4 x4)))
    (+ x8 x6 x4 x2 x)))

(define (calc2 x)
  (let ((x2 (* x x)))
    (let ((x4 (* x2 x2)))
      (let ((x6 (* x4 x2)))
	(let ((x8 (* x4 x4)))
	  (+ x8 x6 x4 x2 x))))))

(define (calc3 x)
  (+ x ((lambda (a) ; x^2
	  ((lambda (b) ; x^4
	     (+ a b (* a b) (* b b))) (* a a))) (* x x))))
	     

(calc1 23)
;Value: 78459301563
(calc2 23)
;Value: 78459301563
(calc3 23)
;Value: 78459301563


; Task 5
(define (funky x)
  ;(display x)
  (let ((p (- x 42)))
    (- x p)))

(define (funky-0)
  (funky 0))

(define (funky-1)
  (lambda ()(funky 0)))

(define (funky-2)
  (lambda (x) (funky x)))

(define (funky-3)
  (lambda () (lambda () (funky 0))))

(define (funky-4 x)
  (lambda (y) (lambda (z) (funky (+ x y z)))))

; Tests
(funky-0)
;Value: 42

((funky-1))
;Value: 42 

((funky-2) 10)
;Value: 42 

(((funky-3)))
;Value: 42 

(((funky-4 2) 3) 8)
;Value: 42 


;Task 6
(define (rfunk-1 x) 
  (let ((p (- (x) 42)))
    (- (x) p))) 

(define (rfunk-2) (lambda () 0))

(define (rfunk-3 x y) (+ (x) (y)))

(define (rfunk-4) (lambda () (/ 42 2)))
; test5 not implemented! to funky


;tests
(rfunk-1 (rfunk-2)) 
;Value: 42

(rfunk-3 (rfunk-4) (rfunk-4))
;Value: 42

(rfunk-1 ()))

; Task 7
(define (cont-frac n d k)
  (cond ((> k 0) (+ (d k) (/ (n k) (cont-frac n d (- k 1)))))
        (else (n k))))


(define (cont-frac-iterative n d k)
  (define (cont-frac-it n d k carry)
    (cond ((= k 0) carry)
	  (else (cont-frac-it n d (- k 1) (+ (d k) (/ (n k) carry))))))
  (cont-frac-it n d k 1))



(define (cont-frac3 n d target error)
  (define (cont-frac-it n d k carry target error)
    ;(display "\n trget - carry: ")
    ;(display (- target carry))   
   (cond ( (and (< (- target carry) error)  (> (- target carry) 0)) carry)
          (else (cont-frac-it n d (+ k 1) (+ (d k) (/ (n k) carry)) target error))))
  (cont-frac-it n d 1 1 target error))  
)


; Tests
(define (phi) (/ (+ 1 (sqrt 5)) 2)) 

(cont-frac-iterative (lambda (i) 1.0) (lambda (i) 1.0) 11)
;Value: 1.6180555555555556
(cont-frac-iterative (lambda (i) 1.0) (lambda (i) 1.0) 11) 
;Value: 1.6180555555555556
; k = 11 is correct for four decimals

(cont-frac3 (lambda (i) 1.0) (lambda (i) 1.0) (phi) 0.00001)
(cont-frac3 (lambda (i) 1.0) (lambda (i) 1.0) (phi) 0.1)
(cont-frac3 (lambda (i) 1.0) (lambda (i) 1.0) (phi) 0.000000000000001)
;Value: 1.618033988749894
 

; Task 8

; begin textbook
(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define  dx 0.00001)

(define (close-enough? x y)
  (< (abs (- x y)) 0.001))

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))



(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))
; end textbook

(define (2deriv g) 
  (deriv (deriv g)))

; This doesn't work! Sorry!
; To rusty on the math :(
(define (find-extremum f first-guess)
  (fixed-point-of-transform f deriv first-guess)) 

(define (testfunc x ) (+ (* x x) x)

; Task 9

;a
(define (range-wrap f a b)
  (lambda (x) 
    (cond ((and (<= a x) (>= b x)) (f x))
	  (else (display "out-of-range")))))
;b	  
(define (scale-input-wrap f factor)
  (lambda (x) ( f (/ x factor))))
;c
(define (scale-output-wrap f factor)
  (lambda (x) (/ (f x) factor)))

;d
(define  (super-wrap f in-scale out-scale range-a range-b)
  (lambda (x) 
    ((scale-input-wrap (range-wrap (scale-output-wrap f out-scale) range-a range-b) in-scale) x))) 

; Tests

; test function
(define (test-print x) x)

;a
((range-wrap test-print 0 10) 10)
;Value: 10                                                                                                                                                             
((range-wrap test-print 0 10) 11)
; out-of-range                                                                                                                                                         ;Unspecified return value  

;b
((scale-input-wrap test-print 3) 4)
;Value: 4/3                                                                                                                                                            
((scale-input-wrap test-print 100) 10)
;Value: 1/10 


;c 
 ((scale-output-wrap test-print 1) 100)
;Value: 100                                                                                                                                                            
((scale-output-wrap test-print 4) 20)
;Value: 5                                                                                                                                                              
;d
((super-wrap test-print 1 1 1 10) 5)
;Value: 5
((super-wrap test-print 1 1 1 10) 50)
; out-of-range

;number twice the size of range to show that scale happens first
((super-wrap test-print 2 1 2 200) 400)
;Value: 200



