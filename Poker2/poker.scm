; Imports definitions.
(load "intro.scm")
(load "lf.scm")
(load "betting.scm")
(load "rollout.scm")

; Set "debug" mode 0 = none, 5 = medium, 10 = insane
(define debug 0) ; Defines the guts of the players
(define balls 6) ; Defines max rounds.
(define max-rounds 3)    ; Defines number of players
(define play-players 7)  ; Defines number of games
(define play-games 25)   ; Defines which betting procedure to use.

;(define betting-procedure rollout-betting)
;(define betting-procedure simple-betting)
(define betting-procedure rollout-table-betting)

(define (dprint level . args) 
  (if (>= debug level)
      (print "DEBUG: " args)))


; Defines a player object.
(define (gen-card-player)
  (let ((hole-cards *nil*)
        (shared-cards *nil*)
        (power *nil*)
        (name "PlayerX")
        (money 0)
        (round-state *nil*)
        (round-bet 0))
    
    (define (reset) (set! hole-cards *nil*) (set! power *nil*) (set! round-state *nil*) (set! round-bet 0))
    (define (reset-round) (set! round-state *nil*) (set! round-bet 0))
    (define (receive-card c) (set! hole-cards (cons c hole-cards)))
    (define (hand) (append hole-cards shared-cards))
    (define (pp-hand) (display name) (display ": ") (display (map card-name (hand))) (display " Power: ") (display (get-hand-power)))
    (define (get-hand-power) (begin (set! power (calc-cards-power (hand) 3)) power))
    (define (bet active-players) (betting-procedure hole-cards shared-cards active-players))
    
    (lambda (command . args)
      (cond ((eq? command 'receive) (apply receive-card args))
            ((eq? command 'reset) (reset))
            ((eq? command 'reset-round) (reset-round))
            ((eq? command 'share) (set! shared-cards (car args)))
            ((eq? command 'power) (get-hand-power))
            ((eq? command 'hand) (hand))
            ((eq? command 'bet) (bet (cadr args)))
            ((eq? command 'set-name) (set! name (car args)))
            ((eq? command 'get-name) name)
            ((eq? command 'get-round-state) round-state)
            ((eq? command 'set-round-state) (set! round-state (car args)))
            ((eq? command 'get-round-bet) round-bet)
            ((eq? command 'set-round-bet) (set! round-bet (car args)))
            ((eq? command 'get-money) (set! money (- money (car args))) (car args))
            ((eq? command 'receive-money) (set! money (+ money (car args))))
            ((eq? command 'count-money) money)
            ((eq? command 'pp-hand) (pp-hand))
            ((eq? command 'get-hole-cards) hole-cards)
            ((eq? command 'get-shared-cards) shared-cards)
            (else (error "Player: Unknown command" command))))))

; Prints a list of arguments
(define (print . args) 
  (map (lambda (x) (display x) (display " ")) args) (newline))

; Gives a list of players names.
(define (givenames player-list) 
  
  (define names '("James Bond" "Le Chiffre" "Clarence Leiter" "Chef DePartre" "Valerie Mathis" "Croupier" "Zoltan" "Basil"
                               "Jonas" "Haavard" "Boye" "Keith" "Jacob" "Emily" "Michael" "Emma" "Joshua" 
                               "Madison" "Matthew" "Abigail" "Ethan" "Olivia" "Andrew" "Isabella"))
  (define (mapnames pl nam) 
    (if (pair? pl) 
        (begin ((car pl) 'set-name (car nam)) 
               (mapnames (cdr pl) (cdr nam))))) 
  (mapnames player-list names))

; Count player round state.
(define (count-player-state players state)
  (define (iter players count)
    (if (null? players)
        count
        (if (eq? ((car players) 'get-round-state) state)
            (iter (cdr players) (+ count 1))
            (iter (cdr players) count))))
  (iter players 0))



(define (power-cmp powa powb)
  (let* ((lena (length powa))
         (lenb (length powb))
         (minlen (if (< lena lenb) lena lenb)))
    (define (re cur)
      (cond ((>= cur minlen) 'eq) 
            ((> (nth cur powa) (nth cur powb)) 'a)
            ((< (nth cur powa) (nth cur powb)) 'b)
            (else (re (+ cur 1)))))
    (re 0)))

(define (power-eq? powa powb)
  (eq? (power-cmp powa powb) 'eq)) 

(define (showdown-winners players)
  (let ((best-power ((car players) 'power)))
    (define (recu players winners)
      (cond ((<= (length players) 0) winners)
            ((not (power-eq? best-power ((car players) 'power))) winners)
            (else (recu (cdr players) (if (eq? *nil* winners) (cons (car players) *nil*) (append winners (list (car players))))))))
    (recu players *nil*)))

; Defines a dealer object.
(define (gen-card-dealer num-players)
  (let ((players (n-of num-players gen-card-player))
        (deck (gen-active-deck))
        (flop *nil*))  ;; flop = the shared cards
    
    (define (deal-card player) (player 'receive (deck 'get)))
    (define (deal-round) (map deal-card players))
    (define (deal-flop) (set! flop (cons (deck 'get) flop)))
    (define (show-flop) (map (lambda (player) (player 'share flop)) players))
    (define (pp-flop) (print "Shared cards:" (map card-name flop)))
    (define (pp-all-hands) (for-each (lambda (player) (player 'pp-hand) (newline)) players))
    (define (pp-players players) (for-each (lambda (player) (player 'pp-hand) (newline)) players))
    (define (powersort) (set! players (powersort-players players)))
    (define (reset)
      (map (lambda (player) (player 'reset)) players)
      (givenames players)
      (set! flop *nil*)
      (deck 'init))
    
    ; Betting is iterative. We're done betting
    ; when all players call, fold or we reach max-rounds.
    ; The function should return a list with the pot and a list of players who
    ; are still active.    
    ; Data structure: (pot (player1, player2, player3))
    (define (betting active-players max-rounds pot-total)
      (let ((round-pot 0)     ; The pot collected this round.
            (current-bet 0)
            (round-state *nil*))  ; The current bet to match.
        (define (iter active-players counter) ; Iterator.
          
          (if (or (= counter 0) ; Reached maximum numbers of rounds.
                  (all-fold? active-players) ; Everyone except one folds.
                  (showdown? active-players)) ; Everyone folds or calls.
              (list round-pot active-players) ; Returns a (pot (player1, player2, player 3)) data structure.
              (begin
                (print "Bet:" current-bet "$ - Round pot:" round-pot "$ - Total pot:" pot-total "$")
                (for-each 
                 (lambda (player)
                   (if (not (eq? (player 'get-round-state) 'fold))
                       (begin
                         (if (>= debug 1) (begin (player 'pp-hand) (newline)))
                         (let ((bet (player 'bet current-bet active-players)))
                           
                           ; Conding on the different bet responses.
                           (cond ((eq? (car bet) 'raise)
                                  (print (player 'get-name) "raises with" (cdr bet) "$.")
                                  (set! round-pot (+ round-pot (cdr bet)))
                                  (set! pot-total (+ pot-total (cdr bet)))
                                  (set! current-bet (+ current-bet (cdr bet)))
                                  (player 'set-round-state 'raise)
                                  (player 'get-money (cdr bet))                            
                                  (player 'set-round-bet (+ (player 'get-round-bet) (cdr bet))))
                                 
                                 ((eq? (car bet) 'call) ; The player calls
                                  (let ((call-price (- current-bet (player 'get-round-bet)))) ; The price for calling = currentbet - sum of your bets
                                    (print (player 'get-name) "calls.")
                                    (set! round-pot (+ round-pot call-price)) ; Add your "call price" to the pot
                                    (set! pot-total (+ pot-total call-price)) ; Update the pot total
                                    (player 'set-round-state 'call)
                                    (player 'get-money call-price)
                                    (player 'set-round-bet (+ (player 'get-round-bet) call-price))))
                                 
                                 ((eq? (car bet) 'fold) 
                                  (print (player 'get-name) "folds.")
                                  (player 'set-round-state 'fold))
                                 
                                 )))))
                 active-players)
                (newline)
                (iter active-players (- counter 1))
                )))
        (iter active-players max-rounds)))
    
    ; Simple predicate that checks if the round is over.
    (define (round-over? players)
      (or 
       (showdown? players)   ; Everyone folds or calls.
       (all-fold? players))) ; Everyone except one folds.
    
    ; Simple predicate that checks if everyone except one folds.
    (define (all-fold? players)
      (= (count-player-state players 'fold) (- (length players) 1)))
    
    ; Simple predicate that checks if the players want's a showdown.
    (define (showdown? players)
      (= (- (length players) (+ (count-player-state players 'call) (count-player-state players 'fold))) 0)) ; 
    
    ; Plays one round of texas hold-em.
    ; The state variables is the pot-total and the list of
    ; active players. We "trim" the list of active players
    ; on each round of betting.
    (define (texas-hold-em)            
      (print "[♥♦♣♠ TEXAS HOLD-EM ♠♣♦♥]")
      
      (let ((pot 0)                   ; Start with an empty pot.
            (active-players players)
            (bet-result *nil*)) ; All players around the table are active.
        
        (print "Dealing two cards to each player.")
        (do-times 2 deal-round) ; Deal two cards to each player.
        
        ; Betting
        (print "First round of betting.") (newline)
        (set! bet-result (betting active-players max-rounds pot))       ; Betting procedure
        (set! active-players (find-winners (cadr bet-result))) ; Updates the list of active players.
        (set! pot (+ pot (car bet-result)))                    ; Updates the pot.
        
        ; Checking if the round is ower.
        (if (all-fold? active-players) ; All players except one have folded.            
            (begin
              (print "DONE1")
              (print "Pot to give to winner: " pot "$")
              ((car active-players) 'receive-money pot)
              (pp-players active-players))
            
            (begin
              (map (lambda (player) (player 'reset-round)) active-players)              
              (print "Dealing three cards to the flop.")
              (do-times 3 deal-flop)
              (show-flop)
              (pp-flop)
              
              ; Betting
              (print "Second round of betting.") (newline)
              (set! bet-result (betting active-players max-rounds pot))
              (set! active-players (find-winners(cadr bet-result)))
              (set! pot (+ pot (car bet-result)))
              
              ; Checking if the round is ower.
              (if (all-fold? active-players) ; All players except one have folded.
                  (begin
                    (dprint 4 "DONE2")
                    (print "Pot to give to winner: " pot "$")
                    ((car active-players) 'receive-money pot)
                    (pp-players active-players))
                  
                  (begin
                    (map (lambda (player) (player 'reset-round)) active-players)                    
                    (print "Dealing the turn.")
                    (deal-flop)
                    (show-flop)
                    (pp-flop)
                    
                    ;Betting
                    (print "Third round of betting.") (newline)
                    (set! bet-result (betting active-players max-rounds pot))
                    (set! active-players (find-winners (cadr bet-result)))
                    (set! pot (+ pot (car bet-result)))
                    
                    ; Checking if the round is ower.
                    (if (all-fold? active-players) ; All players except one have folded.
                        (begin 
                          (dprint 4 "DONE3")
                          (print "Pot to give to winner: " pot "$")
                          ((car active-players) 'receive-money pot)
                          (pp-players active-players))
                        
                        (begin
                          (map (lambda (player) (player 'reset-round)) active-players)                          
                          (print "Dealing the river.")
                          (deal-flop)
                          (show-flop)
                          (pp-flop)
                          
                          ; Betting
                          (print "Final round of betting!") (newline)
                          (set! bet-result (betting active-players max-rounds pot))
                          (set! active-players (find-winners (cadr bet-result)))
                          (set! pot (+ pot (car bet-result)))
                          
                          ; Checking if the round is ower.
                          (if (all-fold? active-players) ; All players except one have folded.
                              (begin 
                                (dprint 4 "DONE4")
                                (print "Pot to give to winner: " pot "$")
                                ((car active-players) 'receive-money pot)
                                (pp-players active-players))
                              
                              (begin
                                (print "SHOWDOWN!")
                                (print "Pot to give to winner(s): " pot "$")
                                (let ((winners (showdown-winners active-players)))
                                  (map (lambda (x) (x 'receive-money (round (/ pot (length winners))))) winners))
                                (pp-players active-players))
                              )))))))))
    
    ; Shows the current leaderboard, all players sorted by money.
    (define (show-leaderboard)
      (newline)
      (print "[♥♦♣♠ LEADERBOARD ♠♣♦♥]")
      (for-each (lambda (player)
                  (display " ")
                  (display (player 'get-name))
                  (do-times (- 15 (string-length (player 'get-name))) (lambda () (display " ")))
                  (display " : ")
                  (display (player 'count-money))
                  (display "$")
                  (newline))
                (merge-sort players (lambda (player) (player 'count-money)) >)) (newline))
    
    ;; Dispatcher
    (lambda (command . args)
      (cond ((eq? command 'reset) (reset))
            ((eq? command 'texas) (texas-hold-em))
            ((eq? command 'leaderboard) (show-leaderboard))
            (else (error "Dealer: Unknown command" command))))))

; Finds the winners. Funkyzeit
(define (find-winners players)
  
  ; Iterator.
  (define (iter players winners)
    (if (null? players) ; We've iterated over all the players, return winners.
        winners
        (begin
          (if (eq? ((car players) 'get-round-state) 'fold)
              (iter (cdr players) winners) ; If the players folded, he cant be a winner.
              (iter (cdr players) (append winners (list (car players)))) ; Add the players to the winner list.
              ))))
  
  (iter (powersort-players players) ()))

; Runs the Texas Hold-em simulation n-times.
(define (run-simulation num-players num-rounds)
  (define dealer (gen-card-dealer num-players))
  (define (play) 
    (dealer 'reset) 
    (dealer 'texas)
    (dealer 'leaderboard))
  (do-times num-rounds play))

(run-simulation play-players play-games)

