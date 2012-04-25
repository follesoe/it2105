(load "all.scm")
;(define calc-cards-power calc-cards-power-new)

(display "straight flush (power 9): ")
(calc-cards-power '((scard 1 . 14) (scard 1 . 13) (scard 1 . 12) (scard 1 . 11) (scard 1 . 10)) 5)
(newline)

(display "4 of a kind (power 8): ")
(calc-cards-power '((scard 1 . 11) (scard 2 . 11) (scard 3 . 11) (scard 4 . 11) (scard 1 . 10)) 5)
(newline)

(display "house (power 7): ")
(calc-cards-power '((scard 1 . 10) (scard 3 . 10) (scard 3 . 12) (scard 1 . 12) (scard 2 . 12)) 5)
(newline)

(display "flush (power 6): ")
(calc-cards-power '((scard 1 . 5) (scard 1 . 13) (scard 1 . 12) (scard 1 . 11) (scard 1 . 10)) 5)
(newline)


(display "straight (power 5): ")
(calc-cards-power '((scard 2 . 2) (scard 1 . 3) (scard 1 . 4) (scard 1 . 5) (scard 1 . 6)) 5)
(newline)


(display "3 of a kind (power 4): ")
(calc-cards-power '((scard 1 . 11) (scard 2 . 11) (scard 3 . 11) (scard 1 . 3) (scard 1 . 10)) 5)
(newline)


(display "two pairs (power 3): ")
(calc-cards-power '((scard 1 . 10) (scard 3 . 10) (scard 3 . 11) (scard 1 . 11) (scard 1 . 14)) 5)
(newline)

(display "pair (power 2):  ")
(calc-cards-power '((scard 1 . 3) (scard 3 . 10) (scard 3 . 11) (scard 1 . 11) (scard 1 . 14)) 5)
(newline)

(display "single: ")
(calc-cards-power '((scard 1 . 3) (scard 3 . 10) (scard 3 . 11) (scard 1 . 5) (scard 1 . 7)) 5)
(newline)

;(calc-cards-power-new '((scard 1 . 11) (scard 1 . 13) (scard 3 . 2) (scard 1 . 6) (scard 2 . 9)) 5)
;(calc-cards-power-old '((scard 1 . 11) (scard 1 . 13) (scard 3 . 2) (scard 1 . 6) (scard 2 . 9)) 5)
;(calc-cards-power-new '((scard 4 . 8) (scard 4 . 12) (scard 1 . 11) (scard 3 . 9) (scard 2 . 12)) 5)
