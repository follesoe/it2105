;; IT-2105 Autumn 2006  Homework #5 Answer Key

;; 1) The general generator

(define (generator start step end-test filter term)
  (define (loop x)
    (cond ((end-test x) *nil*)
	  ((filter x)
	     (cons (term x) (loop (step x))))
	  (else (loop (step x)))))
  (loop start))

(define (gen-intlist2 a b s)
  (generator a (lambda (x) (+ x s)) (lambda (x) (> x b))
	     identity identity))

(define (gen-primelist2 a b)
  (generator a (lambda (x) (+ x 1)) (lambda (x) (> x b))
	     prime? identity))

(define (last elems)
  (car (nthcdr (- (length elems) 1) elems)))

(define (gen-intlists a b s)
  (generator (list a) (lambda (x) (append x (list (+ (last x) s))))
	     (lambda (x) (> (last x)  b))
	     identity identity))


;; 2) for-each (Same as mapc in Common Lisp)

(define (for-each proc elems)
  (if (null? elems) *nil*
      (begin
	(proc (car elems))
	(for-each proc (cdr elems)))))

(define (special-primes2 a b m d)
  (for-each (lambda (p) (if (>= (count-digit p d) m)
			    (begin (newline)(display p))))
	    (gen-primelist2 a b)))

;; (special-primes2 100 10000 3 1) =>
;; 1117 1151 1171 1181 1511 1811 2111 4111 8111


;; 3) deep-reverse

(define (deep-reverse tree)
  (cond ((null? tree) *nil*)
	((pair? tree)
	   (append (deep-reverse (cdr tree))
		   (list (deep-reverse (car tree)))))
	(else tree)))



;; 4) tree-mapping

(define (tree-map proc tree)
  (cond ((null? tree) *nil*)
	((pair? tree)
	   (cons (tree-map proc (car tree))
		 (tree-map proc (cdr tree))))
	(else (proc tree))))

(define (square-tree2 tree) (tree-map square tree))
(define (sqrt-tree tree) (tree-map (lambda (x) (sqrt (abs x))) tree))
(define (tree-plus tree n) (tree-map (lambda (x) (+ x n)) tree))
(define (tree-scale tree factor)
  (tree-map (lambda (x) (* x factor)) tree))

;; 5) Merging sorted lists

(define (merge-lists l1 l2 key test)
  (define (rec l1 l2)
		(cond ((null? l1) l2)
	     	   ((null? l2) l1)
	     	   ((test (key (car l1)) (key (car l2)))
	            (cons (car l1) (rec (cdr l1) l2)))
		      (else (cons (car l2) (rec l1 (cdr l2))))))
	(rec l1 l2))

;; 6) Merge sort

(define (nthcdr n elems)
	(if (or (null? elems) (= n 0)) 
	    elems
	   (nthcdr (- n 1) (cdr elems))))

(define (firstn n elems)
  (if (or (null? elems) (= n 0)) *nil*
	  (cons (first elems) (firstn (- n 1) (cdr elems)))))
	   

(define (merge-sort elems key test)
  (define (rec elems)
    (cond ((null? elems) *nil*)
	  ((null? (cdr elems)) elems)
	  (else
	   (let ((middle (round (/ (length elems) 2))))
	     (merge-lists
	      (rec (firstn middle elems))
	      (rec (nthcdr middle elems))
	      key test)))))
  (rec elems))

;; 7) Sorting a list of cards

(define (basic-card-sort cards)
  (sort-cards  cards identity
    (lambda (card1 card2)
	   (or (< (card-suit card1) (card-suit card2))
	       (and (= (card-suit card1) (card-suit card2))
			      (< (card-value card1) (card-value card2)))))))

(define (sort-cards cards key test) (merge-sort cards key test))

(define (gen-shuffled-deck) (remove-shuffle (gen-card-deck)))

;; 8) General partitioning

(define (add-to-partition elem partition key eq-test)
  (define (rec groups)
	 (cond ((null? groups) (list (list elem)))
	       ((eq-test (key elem) (key (caar groups)))
	          (cons (cons elem (car groups)) (cdr groups)))
		    (else (cons (car groups) (rec (cdr groups))))))
	(rec partition))

(define (partition elems key eq-test)
  (define (rec elems partition)
	 (if (null? elems)
	      partition
	     (rec (cdr elems) (add-to-partition (car elems) partition key eq-test))))
	(rec elems *nil*))

;; This sorts the partitions themselves, using the order-test and the first item in each group.
;; We will use it on homework set VI.
 
(define (sorted-partition elems key eq-test order-test)
  (merge-sort (partition elems key eq-test) (lambda (group) (key (car group))) order-test))


;; This retains the order of elements in partitions but removes the borders between them.
;; Hence, (flatten-partition '((1 3 5 7) (2 4 6 8))) => (1 3 5 7 2 4 6 8)
;; This is also used on homework set VI.

(define (flatten-partition partition) (apply append partition))

