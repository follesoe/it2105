; Student nummber: 673677
; Homework Set II

; Task 1

(define (fact-recursive b n)
  (if (> n 1)
      (* (fact-recursive b (- n 1)) (/ b n))
      (/ b n))
)

; tests
(fact-recursive 3 4)
;Value: 27/8
(fact-recursive 5 6)
;Value: 3125/144
(fact-recursive 7 8)
;Value: 823543/5760


(define (fact-it b n cur)
  (if (> n 0)
     (fact-it b (- n 1) (* (/ b n) cur))
     cur)
)

(define (fact-iterative b n)
  (fact-it b n 1)
)

; tests
(fact-iterative 3 4)
;Value: 27/8
(fact-iterative 5 6)
;Value: 3125/144
(fact-iterative 7 8)
;Value: 823543/5760



; Task 2


; recursive
(define (g n)
  (if (< n 4)
      n
  (+ 
   (g (- n 1)) 
   (* 4 (g (- n 2))) 
   (* 8 (g (- n 3)))
   (* 4 (g (- n 4))))))

(g 4)
; test
(g 3)
;Value: 3
(g 4)
;Value: 19
(g 20)
;Value: 1742488767

;; I didn't manage to make it iterative, here is what i tried
;(define (g-it a b c d count)
;  (if (> count 0)
;      (g-it (+ 1 a) (* 4 (+ 1 b)) (* 8 (+ 1 c)) (* 4 (+ 1 d)) (- count 1))
;      (+ a b c d))
;)

;(define (g-ither n)
;  (+ (g-it 4 3 2 1 (- 4 n)) 4)
;)
;(g-ither 1000)



;Task 3

(define (pascal row col)
  (cond ((= col 1) 1)
        ((= col row) 1)
        (else (+ 
               (pascal (- row 1) (- col 1)) 
               (pascal (- row 1) col)))))

(pascal 3 2)
;Value: 2
(pascal 4 3)
;Value: 3
(pascal 5 3)
;Value: 6


; Task 4

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

; 4a

(define (primes-in-range a b)
  (if (prime? a)
      (BEGIN
       (display a)
       (display ", ")
       )
      )
  (if (< a b)
      (primes-in-range (+ a 1) b)))

; tests
(primes-in-range 1 5)
;1, 2, 3, 5,
(primes-in-range 20 50)
;23, 29, 31, 37, 41, 43, 47,
(primes-in-range 100 150)
;101, 103, 107, 109, 113, 127, 131, 137, 139, 149


;4b

(define (first-n-primes a n)
  (cond ((= n 0) 0)
        ((prime? a) 
         (display a)
         (display ", ")
         (first-n-primes (+ a 1) (- n 1))
      )
  (else (first-n-primes (+ a 1) n))))

(first-n-primes 5 10)
; 5, 7, 11, 13, 17, 19, 23, 29, 31, 37
(first-n-primes 10 4)
; 11, 13, 17, 19,
(first-n-primes 10000 4)
; 10007, 10009, 10037, 10039

; Task 5

(define (find-match prime n k)
  (cond ((>= n k) 0) ; not found
        ((< k (+ prime n)) 0) ; avoid unessecary calcs (cuts ALOT of time)
        ((and (prime? n) (= (+ prime n) k)) n) ;found
        (else (find-match prime (+ n 1) k))))


(define (find-prime n k)
  (cond ((or (>= n k) (> 2 n)) 0)
        ((and (prime? n) (< 0 (find-match n 3 k))) 
         (display "found: ")
         (display (find-match n 3 k)) ; I know this isnt optimal
         (display " and ") (display n) (display "\n") 0)
        (else (find-prime (- n 1) k))))


(define (goldbach k)
  (find-prime (- k 1) k))

;tests

(goldbach 14)
; found: 3 and 11

(goldbach 678)
;found: 5 and 673

; Ten billions
(goldbach 10000000000)
;found: 71 and 9999999929


; Task 6

(define (accum-recursive combiner null-value term a next b)
  ;(display a)
  ;(display "\n")
  (if (or (> a b) (<= a 0))
      null-value
      (combiner 
       (term a)
       (accum-recursive combiner null-value term (next a) next b))))


(define (accum-it combiner null-value term a next b pos)
  (if (> a b)
      pos
      (accum-it combiner null-value term (next a) next b 
		(combiner (term a) pos))))

(define (accum-iterative combiner null-value term a next b)
  (accum-it combiner null-value term a next b 0))


(define (increment x) (+ x 1))

;tests
(accum-recursive + 0 square 1 increment 3)
;Value: 14

(accum-iterative + 0 square 1 increment 3) 
;Value: 14

(accum-iterative + 0 square 1 increment 8)
;Value: 204


; Task 7
; 7a
(define (nxt-prime a)
  (if (prime? (+ a 1))
      (+ a 1)
      (nxt-prime (+ 1 a))))


(define (sum-of-primes a b)
  (accum-recursive + 0 + a nxt-prime b)   )

(sum-of-primes 1 10)
;Value: 18
(sum-of-primes 1 11)
;Value: 29
(sum-of-primes 40 60) 
;Value: 283

; 7b
(define (faculty a)
  (accum-recursive * 1 + 1 increment a))

; tests
(faculty 5)
;Value: 120
(faculty 7)
;Value: 5040
(faculty 11)
;Value: 39916800

; 7c
(define (divide k)(/ 1 k))
(define (sum-c a b)
  (accum-recursive + 1 divide a increment b))

; tests
(sum-c 1 10)
;Value: 9901/2520
(sum-c 5 11)
;Value: 53681/27720
(sum-c 9 13)
;Value: 37639/25740


; 7d
(define (pow-it k n)
  (if (> n 0)
      (* k (pow-it k (- n 1)))
      1)
)

(define (power k) (pow-it k k))
  

(define (funkydiv k)
  (/ (faculty k) (power k)))


(define (sum-d a b)
  (accum-recursive + 0 funkydiv a increment b))
)

; tests
(sum-d 1 5)
;Value: 333787/180000
(sum-d 2 3)
;Value: 13/18
(sum-d 6 7)
;Value: 821525/38118276


; Task 8
(define (mm-y f a b delta)
  (define (gt x y) (if (> x y) x y))
  (define (lt x y) (if (< x y) x y))
  (define (nxt x) (+ x delta))
  (display "\nmax:")
  (display (accum-recursive gt 0 f a nxt b))
  (display " min: ")
  (display (accum-recursive lt 0 f a nxt b))
  (display " \n"))


(define (f1 x) (cos 5))

(mm-y random 1 10 1)
;max:7 min: 0
(mm-y tan 1 10 1)
;max:1.5574077246549023 min: -6.799711455220379
(mm-y square 1 10 1)
;max:100 min: 0







; Task 9
; NBNBNB! Does not compute :)
; I didn't figure any clean way to compare one value in the term and
; then send another value back to the comparer. What I tried to do
; was to make term return a function so that If i fed it 0 it would
; return x and 1 f(x). So i could compare the functions only put back
; the arguments. I got a little lost in my own code :(
;(define (mm-x f a b delta)
;  (define (gt x y) 
;    ((if (< (x 0) (y 0)) (x 1) (y 1))))
;  (define (lt x y)
;    ((if (> (x 0) (y 0)) (x 1) (y 1))))
;  (define (nxt x) (+ x delta) )
;  (define (function-wrapper f)
;  (lambda (val)
;    (lambda (x) (if (= x 0) (f val) val))))
;   (display "\nmax:")
;  ;(display (accum-recursive gt (function-wrapper (lambda (x) (0))) (function-wrapper f) a nxt b))
;  (display " min: ")
;  ;(display (accum-recursive lt 0 (function-wrapper f) a nxt b))
;  (display " \n"))
;(mm-x tan 1 10 1) 