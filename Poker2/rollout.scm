; Removes a set of cards from a deck.
(define (remove-cards deck cards)
  
  ; Internal predicate returning true/false if a card should
  ; be removed or not.
  (define (remove? card)
    (define (itter cards-to-remove)      
      (cond ((nil? cards-to-remove) #f)
            ((and (= (card-value card) (card-value (car cards-to-remove)))
                  (eq? (card-suit card) (card-suit (car cards-to-remove)))) #t)
            (else (itter (cdr cards-to-remove)))))
    (itter cards))
  
  ; Internal function building the new deck where the
  ; given cards are removed.
  (define (itter new-deck old-deck)
    (cond ((nil? old-deck) new-deck)
          ((remove? (car old-deck)) (itter new-deck (cdr old-deck)))
          (else  (itter (append new-deck (list (car old-deck))) (cdr old-deck)))))
  
  (itter () deck))


; Runs the simulation once returning true or false if you win or not.
(define (win-simulation? hole-cards shared-cards deck num-opponents)
  (set! deck (remove-shuffle deck))
  (do-times (- 5 (length shared-cards)) 
            (lambda () 
              (set! shared-cards (append shared-cards (list (car deck))))
              (set! deck (cdr deck))))
  
  (let* ((player (gen-card-player))
         (opponents (n-of num-opponents gen-card-player)))
    
    (player 'share shared-cards) ; Give our player the shared cards.
    (map (lambda (c) (player 'receive c)) hole-cards) ; Give our player the hole-cards
    (map (lambda (p) (p 'share shared-cards)) opponents) ; Give our opponents the shared cards.
    (for-each (lambda (p) 
                (p 'receive (car deck)) 
                (set! deck (cdr deck))
                (p 'receive (car deck))
                (set! deck (cdr deck))) opponents)

   (eq? (car (powersort-players (cons player opponents))) player)))

; Function copying a list.
(define (list-copy lis) 
  (cond ((null? lis) '()) 
        (else (cons (car lis) (list-copy (cdr lis))))))

; Runs the rollout simulation
(define (run-rollout-simulation rounds hole-cards shared-cards num-opponents)
  (let ((deck (remove-cards (gen-shuffled-deck) (append hole-cards shared-cards))))
    
    (define (itter i wins)
      (if (= i 0)
          wins
          (if (win-simulation? hole-cards shared-cards (list-copy deck) num-opponents)
              (itter (- i 1) (+ wins 1))
              (itter (- i 1) wins))))
    
    (* (/ (itter rounds 0) rounds) 100)))