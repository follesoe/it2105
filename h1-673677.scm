; Student nummber: 673677
; Homework Set I

; Task 1
(define (pow x) 
  (* x x)
)

(define (hyp a b) 
  (sqrt (+ (pow a) (pow b)))
)

(define (kat c a) ;; c is hyp
  (sqrt (- (pow c) (pow a)))
)

;tests
(hyp 3 4)
;Value: 8
(kat 9 10)
;Value: +4.358898943540674i
(kat 1 2)
;Value: +1.7320508075688772i



; Task 2
(define (choose x y flag)
  (cond ((> flag 0) (hyp x y))
	(else (kat x y)))
)

; tests
(choose 7 8 1)
;Value: 10.63014581273465
(choose 7 8 0)
;Value: +3.872983346207417i






; Task 3
(define (my-expt a b)
  (cond ((= b 1) a)
	((= b 0) 1)
	((> b 1)(* a (my-expt a (- b 1))))
	((< b 0)(/ 1 (my-expt a (abs b))))
	)  
)

(define (pterm c a b)
  (my-expt (* c a) b)
)
; tests
(my-expt 2 8)
;Value: 256
(my-expt 2 -1)
;Value: 1/2
(my-expt 2 0)
;Value: 1
(pterm 1 2 3)
;Value: 8



; Task 4
(define (calc x y)
  (+ (pterm 7 x 5) (pterm 12 y 4) (* 8 x y))
)
; tests
(calc 1.1 2)
;Value: 358861.44157
(calc 3 4.3)
;Value: 11173429.953599997



; Task 5a
(define (larger a b)
  (if (= a b) (display "TIE!\n")
      (if (< a b ) (display "B wins!\n")
	  (display "A wins!\n")))
)
;tests
(larger 1 2)
;B wins!
(larger 2 1)
;;A wins!
(larger 2 2)
;;TIE!


; Task 5b
(define (larger2 a b)
  ;;Order matters here!
  (or (and (= a b)(display "TIE\n"))
      (and (< a b)(display "B wins!\n"))
      (and (> a b)(display "A wins!\n")))
)

(larger2 1 2)
;B wins!
(larger2 2 1)
;A wins!
(larger2 2 2)
;TIE

;Task 6
;defines rock,paper, scisscors
(define rock 0)
(define paper 1)
(define scissors 2)


(define (rps-winner a b)
  (if (= a b) 0 
      (cond ((and (= a rock) (= b scissors)) 1)
	    ((and (= a scissors) (= b paper)) 1)
	    ((and (= a paper) (= b rock)) 1)
	    (else 2); else b wins
      )
  )
)
;tests
(rps-winner rock rock)
;Value: 0
(rps-winner scissors rock)
;Value: 2
(rps-winner paper rock)
;Value: 1



; Task 7
;uses defines from task 6
(define (rps-stonehead mylast opplast)
  rock
)
(define (rps-sum mylast opplast)
 (modulo (+ mylast opplast) 3)
)

(define (rps-rand mylast opplas)
  (random 3)
)
;tests
(rps-stonehead rock paper)
;Value: 0
(rps-sum scissors paper)
;Value: 0
(rps-rand paper rock)
;Value: 1


; Task 8
(define (play-rps p1 p2 num-rounds p1-last p2-last p1-wins p2-wins)
  (if (> num-rounds 1)
   (let ((p1-draw (p1 p1-last p2-last)) 
	 (p2-draw (p2 p2-last p1-last)))
	 (let ((winner (rps-winner p1-draw p2-draw))) 
	   (if (= winner 1)
	       (play-rps p1 p2 (- num-rounds 1) p1-draw p2-draw (+ p1-wins 1) p2-wins))
	   (if (= winner 2)
	       (play-rps p1 p2 (- num-rounds 1) p1-draw p2-draw p1-wins (+ p2-wins 1)))
	   (if (= winner 0)
	       (play-rps p1 p2 (- num-rounds 1) p1-draw p2-draw p1-wins p2-wins)))))
  (if (= num-rounds 1)
   (BEGIN
   (display "\np1: ")
   (display p1-wins)
   (display "\t p2: " )
   (display p2-wins)
   (display "\n")))
)

(define (rps p1 p2 num-rounds)
 (play-rps p1 p2 num-rounds 0 0 0 0)
)

;tests
(rps rps-sum rps-stonehead 140)
;p1: 0    p2: 0
(rps rps-stonehead rps-rand 30)
;p1: 13   p2: 11
(rps rps-sum rps-rand 2000)
;p1: 661  p2: 650



; I had some problems visualizing recursion in Task 9 and 10.
; Hope you understand :)

; Task 9
(define (dec x) (- x 1))
(define (inc x) (+ x 1))
(define (funny+ a b) (if (= a 0) b (inc (funny+ (dec a) b))))
(funny+ 4 5)

; full recursion
;(funny+ 4 5) (if (= 4 0) 5 (+ (funny+ (- 4 1) 5) 1))
;(funny+ 3 5) (if (= 3 0) 5 (+ (funny+ (- 3 1) 5) 1))
;(funny+ 2 5) (if (= 2 0) 5 (+ (funny+ (- 2 1) 5) 1))
;(funny+ 1 5) (if (= 1 0) 5 (+ (funny+ (- 1 1) 5) 1))
;(funny+ 0 5) (if (= 0 0) 5 (+ (funny+ (- 0 1) 5) 1))

;evaluates to
;(funny+ 4 5) (+ (funny+ (- 4 1) 5) 1)
;(funny+ 3 5) (+ (funny+ (- 3 1) 5) 1)
;(funny+ 2 5) (+ (funny+ (- 2 1) 5) 1)
;(funny+ 1 5) (+ (funny+ (- 1 1) 5) 1)
;(funny+ 0 5) 5
;(+ (+ (+ (+ 5 1) 1) 1) 1)
;(+ (+ (+ 6 1) 1) 1)
;(+ (+ 7 1) 1)
;(+ 8 1)
;9
; returns


; Taskj 10
(define (funny+2 a b) (if (= a 0) b (funny+2 (dec a) (inc b))))
(funny+2 4 5)

; full recursion
;(funny+2 4 5) (if (= 4 0) 5 (funny+2 (- 4 1) (+ 5 1)))
;(funny+2 3 6) (if (= 3 0) 6 (funny+2 (- 3 1) (+ 6 1)))
;(funny+2 2 7) (if (= 2 0) 7 (funny+2 (- 2 1) (+ 7 1)))
;(funny+2 1 8) (if (= 1 0) 8 (funny+2 (- 1 1) (+ 8 1)))
;(funny+2 0 9) (if (= 0 0) 9 (funny+2 (- 0 1) (+ 9 1)))

; evaluates to
;(funny+2 4 5) (funny+2 (- 4 1) (+ 5 1))
;(funny+2 3 6) (funny+2 (- 3 1) (+ 6 1))
;(funny+2 2 7) (funny+2 (- 2 1) (+ 7 1))
;(funny+2 1 8) (funny+2 (- 1 1) (+ 8 1))
;(funny+2 0 9) 9 
; returns
