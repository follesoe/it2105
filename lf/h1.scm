;; IT-2105 Homework #1 Answer Key

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

