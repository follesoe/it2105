; Defines a basic betting procedure
(define (simple-betting hand shared-cards active-players)
  (dprint 6 "Running: Simple Betting")
  (dprint 8 "Simple betting Hand: " hand)
  (dprint 8 "Simple betting shared: " shared-cards)
  (dprint 8 "Simple betting all:" (append hand shared-cards))
  
  
  (let* ((all-cards (append hand shared-cards)) 
        (num-of-card (length all-cards))
        (power-hand (calc-cards-power hand 2))
        (power-shared (calc-cards-power shared-cards (length shared-cards)))
        (power (calc-cards-power all-cards num-of-card))
        (num-of-fold (count-player-state active-players 'fold)))
    
    (if (or (< num-of-card 2) (> num-of-card 7)) (error "Invalid amount of cards: " num-of-card))
    
    (if (and (> num-of-card 4)
              (power-cmp power-hand power-shared)) (dprint 5 "The power is in the shared cards!"))
    
    (dprint 8 "Power rating: " power)
    (cond ((=  (- (length active-players) num-of-fold) 1) (cons 'call 0))
          ((and (= num-of-card 2) (> (car power) 1)) (cons 'raise 25))
          ((and (= num-of-card 2) (= (car power) 1) (> (cadr power) 9) (cons 'call 0)))
          ;balls with two cards
          ((and (= num-of-card 2) (= (random 4) 1) (cons 'raise 25)))
          ((and (= num-of-card 2) (= (random 3) 1) (cons 'call 0)))
          
          
          ((and (= num-of-card 2) (= (car power) 1) (cons 'fold 0)))
          ((and (> num-of-card 4) (power-cmp power-hand power-shared) (> (random 2) 1)) (cons 'fold 0)) ; power in the shared-cards
          ((and (> num-of-card 4) (> (car power) 3) (cons 'raise 25)))
          ((and (> num-of-card 4) (> (car power) 2) (cons 'call 0)))
          
          ;ball with more than 4 cards
          ((and (> num-of-card 4) (> (random 4) 1) (cons 'call 0)))
          ((and (> num-of-card 4) (> (random 3) 1) (cons 'raise 25)))
          
          ((> num-of-card 4) (cons 'fold 0))
          (else (list (cons 'fold 0))))))

; Extends the simple betting with rollout
(define (rollout-betting hand shared-cards active-players)
  (dprint 6 "Running: rollout Betting")
  (dprint 8 "Rollout betting Hand: " hand)
  (dprint 8 "Rollout betting shared: " shared-cards)
  (dprint 8 "Rollout betting all:" (append hand shared-cards))
  
  (let* ((all-cards (append hand shared-cards)) 
        (num-of-card (length all-cards))
        (power-hand (calc-cards-power hand 2))
        (power-shared (calc-cards-power shared-cards (length shared-cards)))
        (power (calc-cards-power all-cards num-of-card))
        (num-of-fold (count-player-state active-players 'fold))
        (win-percentage (run-rollout-simulation 100 hand shared-cards (- (length active-players) num-of-fold))))
    (dprint 5 "Win percentage:" win-percentage)
    
    (if (or (< num-of-card 2) (> num-of-card 7)) (error "Invalid amount of cards: " num-of-card))
    
    (if (eq? power-hand power-shared) (dprint 5 "The power is in the shared cards!"))
    (if (eq? power-hand power-shared) (error "The power is in the shared cards!"))
    (dprint 8 "Power rating: " power)
    (cond ((=  (- (length active-players) num-of-fold) 1) (cons 'call 0)) ; last man standing, no point in folding!
          ((and (> win-percentage 30) (< (random 10) balls)) (cons 'raise 25)) ; add some balls
          ((and (> win-percentage 10) (< num-of-card 3) (< (random 10) balls)) (cons 'call 0)) ; lets be a little more agressive in the start
          
          ; basics
          ((> win-percentage 40) (cons 'raise 25))
          ((> win-percentage 30) (cons 'call 0))
          (else (cons 'fold 0)))))

; Extends the rollout betting with opening table
(define (rollout-table-betting hand shared-cards active-players)
  (dprint 6 "Running: rollout Betting")
  (dprint 8 "Rollout betting Hand: " hand)
  (dprint 8 "Rollout betting shared: " shared-cards)
  (dprint 8 "Rollout betting all:" (append hand shared-cards))
  
  (let* ((all-cards (append hand shared-cards)) 
        (num-of-card (length all-cards))
        (power-hand (calc-cards-power hand 2))
        (power-shared (calc-cards-power shared-cards (length shared-cards)))
        (power (calc-cards-power all-cards num-of-card))
        (num-of-fold (count-player-state active-players 'fold))
        (opening-rank (get-opening-rank hand))
        (win-percentage (run-rollout-simulation 100 hand shared-cards (- (length active-players) num-of-fold))))
    (dprint 5 "Win percentage:" win-percentage)
    
    (if (or (< num-of-card 2) (> num-of-card 7)) (error "Invalid amount of cards: " num-of-card))
    
    (if (eq? power-hand power-shared) (dprint 5 "The power is in the shared cards!"))
    (if (eq? power-hand power-shared) (error "The power is in the shared cards!"))
    (dprint 8 "Power rating: " power)
    (cond ((=  (- (length active-players) num-of-fold) 1) (cons 'call 0)) ; last man standing, no point in folding!
          ((= num-of-card 2)
           (let ((opening-rank (get-opening-rank hand)))
             (cond ((> 6 opening-rank) (cons 'fold 0))
                   ((> 3 opening-rank) (cons 'hold 0))
                   (else (cons 'raise 25))))) ; This is the opening bet. Bet based on lookup table.
          ((and (> win-percentage 30) (< (random 10) balls)) (cons 'raise 25)) ; add some balls
          ((and (> win-percentage 10) (< num-of-card 3) (< (random 10) balls)) (cons 'call 0)) ; lets be a little more agressive in the start
          
          ; basics
          ((> win-percentage 40) (cons 'raise 25))
          ((> win-percentage 30) (cons 'call 0))
          (else (cons 'fold 0)))))

; David Sklansky and Mason Malmuth opening hand table.
(define opening-table
  (list
   (list 1 1 2 2 3 5 5 5 5 5 5 5 5)
   (list 2 1 2 3 4 6 7 7 7 7 7 7 7)
   (list 3 4 1 3 4 5 7 9 9 9 9 9 9)            
   (list 4 5 5 1 3 4 6 8 9 9 9 9 9) 
   (list 6 6 6 5 2 4 5 9 9 9 9 9 9)             
   (list 8 8 8 7 7 3 4 5 8 9 9 9 9)        
   (list 9 9 9 8 8 7 4 5 6 8 9 9 9)      
   (list 9 9 9 9 9 9 8 5 5 7 8 9 9)
   (list 9 9 9 9 9 9 9 8 6 5 7 9 9)
   (list 9 9 9 9 9 9 9 9 8 6 6 8 9)
   (list 9 9 9 9 9 9 9 9 9 8 7 7 8)
   (list 9 9 9 9 9 9 9 9 9 9 9 7 8)
   (list 9 9 9 9 9 9 9 9 9 9 9 9 7)))

; Gets the Davi Sklansky/Manson Malmuth opening value for
; the two opening cards.
(define (get-opening-rank cards)
  (let* ((card1 (car cards))
         (card2 (cadr cards))
         (index1 (card-value card1))
         (index2 (card-value card2)))
    
    (if (= (card-suit card1)
           (card-suit card2))
        (if (> index1 index2)
            (nth (- 14 index2) (nth (- 14 index1) opening-table))
            (nth (- 14 index1) (nth (- 14 index2) opening-table)))
        (if (> index1 index2)
            (nth (- 14 index1) (nth (- 14 index2) opening-table))
            (nth (- 14 index2) (nth (- 14 index1) opening-table))))))