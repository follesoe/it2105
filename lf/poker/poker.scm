(load "lf.scm")

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


(define player (gen-card-player))
(define deck (gen-active-deck))

(player 'reset)
(player 'receive (deck 'get))
(player 'receive (deck 'get))
(player 'power)
(player 'receive (deck 'get))
(player 'receive (deck 'get))
(player 'receive (deck 'get))
(player 'power)
(player 'receive (deck 'get))
(player 'receive (deck 'get))
(player 'power)
(player 'pp-hand)