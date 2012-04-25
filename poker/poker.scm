; Imports definitions.
(load "lf.scm")

; Defines a player object.
(define (gen-card-player)
  (let ((hole-cards *nil*)
        (shared-cards *nil*)
        (power *nil*)
        (name "PlayerX"))
    
    (define (reset) (set! hole-cards *nil*) (set! power *nil*))
    (define (receive-card c) (set! hole-cards (cons c hole-cards)))
    (define (hand) (append hole-cards shared-cards))
    (define (pp-hand) (display (map card-name (hand))) (display " Power: ") (display (get-hand-power)))
    (define (get-hand-power) (or power 
                                 (begin (set! power (calc-cards-power (hand) 5)) power)))
    
    (define (bet) (cons 'raise 100))
    
    (lambda (command . args)
      (cond ((eq? command 'receive) (apply receive-card args))
            ((eq? command 'reset) (reset))
            ((eq? command 'share) (set! shared-cards (car args)))
            ((eq? command 'power) (get-hand-power))
            ((eq? command 'hand) (hand))
            ((eq? command 'bet) (bet))
            ((eq? command 'set-name) (set! name (car args)))
            ((eq? command 'get-name) name)
            ((eq? command 'pp-hand) (pp-hand))
))))

; Defines a basic betting procedure
(define (simple-betting hand shared betting-state)
  (let ((power-hand (calc-cards-power hand 2))
        (power-shared (calc-cards-power shared (length shared)))
        (power (calc-cards-power (append hand shared) 5)))
             
    (if (nil? betting-state)
        (list '(call 0))
        (list '(fold 0) betting-state))
    ))

(define deck (gen-active-deck))
(define hand (list (deck 'get) (deck 'get)))
(define shared (list (deck 'get) (deck 'get) (deck 'get) (deck 'get) (deck 'get)))

; Defines a dealer object.
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
    
    ; Betting is iterative. We're done betting
    ; when all players call, fold or we reach max-rounds.
    ; The function should return a listf with the pot and a list of players who
    ; are still active.
    (define (betting active-players max-rounds)      
      (define (iter active-players) ; Iterator.
        (let ((rount-pot 0)     ; The pot collected this round.
              (current-bet 0))  ; The current bet to match.
          (for-each 
           (lambda (player)
             (let ((bet (player 'bet)))
               (cond ((eq? (car bet) 'raise) (display (player 'get-name)) (display " raises..."))
                     ((eq? (car bet) 'call) (display (player 'get-name)) (display " calls..."))
                     ((eq? (car bet) 'fall) (display (player 'get-name)) (display " falls..."))))
             (newline))
           players)))
      (iter active-players))
    
    ; Plays one round of texas hold-em.
    ; The state variables is the pot-total and the list of
    ; active players. We "trim" the list of active players
    ; on each round of betting.
    (define (texas-hold-em)
      (let ((pot-total 0) ; Start with an empty pot.
            (active-players players)) ; All players around the table are active.
        (do-times 2 deal-round)
        (do-times 3 deal-flop) (show-flop)
        ; Betting                
        (set! active-players (betting active-players 10)) ; Maximum 10 rounds of betting to avoid infinite loop.
        ; Now we need to check the number of active players.
        ; If only one active player, give him the pot.
        ; If not, countinue.
        (deal-flop)
        (show-flop)
        ; Betting
        (deal-flop)
        (show-flop)
        ; Betting
        (powersort)
        (pp-all-hands)))
	   
    ;; Dispatcher
    (lambda (command . args)
      (cond ((eq? command 'reset) (reset))
            ((eq? command 'texas) (texas-hold-em))))))

(define dealer (gen-card-dealer 3))
(dealer 'reset)
(dealer 'texas)