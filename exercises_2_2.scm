#lang sicp

;; (define nil '())

;;------------------------------------------------------------------------
;;  Exercise 2.17
;;  -------------

(define (last-pair l)
  (if (null? (cdr l))
      l
      (last-pair (cdr l)) ))

;;------------------------------------------------------------------------
;;  Exercise 2.18
;;  -------------

(define (reverse l)
  (define (iter-reverse l r)
    (if (null? l)
	r
	(iter-reverse (cdr l) (cons (car l) r)) ))
  (iter-reverse l '()) )

;;------------------------------------------------------------------------

(define (count-change amount)
  (cc amount 5))
(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

;;------------------------------------------------------------------------

(define us-coins (list 50 25 10 5 1))

(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

;;------------------------------------------------------------------------
;;  Exercise 2.19
;;  -------------

(define no-more? (lambda (x) (null? x)))
(define except-first-denomination (lambda (x) (cdr x)))
(define first-denomination (lambda (x) (car x)))


;;------------------------------------------------------------------------
;;  Exercise 2.20
;;  -------------

(define (same-parity x . y)
  (define (filter p xs)
    (if (null? xs)
	'()
	(if (p (car xs))
	    (cons (car xs) (filter p (cdr xs)))
	    (filter p (cdr xs)) )))
	    
  (if (even? x)
      (filter even? y)
      (filter odd?  y) ))


;;------------------------------------------------------------------------
;;  Exercise 2.21
;;  -------------

(define (square-list items)
  (if (null? items)
      '()
      (cons (* (car items) (car items)) (square-list (cdr items)))))

(define (square-list-map items)
  (map (lambda (x) (* x x))
       items))

;;------------------------------------------------------------------------
;;  Exercise 2.22
;;  -------------

;; (define (square-list items)
;;   (define (iter things answer)
;;     (if (null? things)
;;         answer
;;         (iter (cdr things)
;;               (cons (square (car things))
;;                     answer))))
;;   (iter items nil))

;; (define (square-list items)
;;   (define (iter things answer)
;;     (if (null? things)
;;         answer
;;         (iter (cdr things)
;;               (cons answer
;;                     (square (car things))))))
;;   (iter items nil))


;;------------------------------------------------------------------------
;;  Exercise 2.23
;;  -------------

;; (define (my-for-each proc lst)
;;   (if (null? lst)
;;       ()
;;       (begin
;; 	(proc (car lst))
;; 	(my-for-each proc (cdr lst)) )))

(define (my-for-each proc lst)
  (cond ( (null? lst) () )
	( else        (proc (car lst))
		      (my-for-each proc (cdr lst)) )))




;;------------------------------------------------------------------------
;;  Exercise 2.24
;;  -------------

;; (1 (2 (3 4)))


;;------------------------------------------------------------------------
;;  Exercise 2.25
;;  -------------

;; (car (cdaddr '(1 3 (5 7) 9))))

;; (car (car '((7))))

;; (cadadr (cadadr (cadadr '(1 (2 (3 (4 (5 (6 7)))))) )))


;;------------------------------------------------------------------------
;;  Exercise 2.26
;;  -------------

;; (1 2 3 4 5 6)

;; ((1 2 3) 4 5 6)

;; ((1 2 3) (4 5 6))


;;------------------------------------------------------------------------
;;  Exercise 2.27
;;  -------------

;; (define (deep-reverse lst)
;;   (reverse (map (lambda (x) (if (list? x)
;; 				(deep-reverse x)
;; 				x ))
;; 		lst)))

(define (deep-reverse lst)
  (if (list? lst)
      (reverse (map deep-reverse lst))
      lst ))


;;------------------------------------------------------------------------
;;  Exercise 2.28
;;  -------------
  
(define (fringe tree)
  (define (iter tree lst)
    (cond ((pair? tree)  (iter (cdr tree) (iter (car tree) lst)))
	  ((null? tree)  lst)
	  (else          (cons tree lst)) ))
  (reverse (iter tree '()) ))
	   

    
;;------------------------------------------------------------------------

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

;;------------------------------------------------------------------------
;;  Exercise 2.29
;;  -------------


(define (left-branch mobile)
  (car mobile) )

(define (right-branch mobile)
  (car (cdr mobile)) )

(define (branch-length branch)
  (car branch) )

(define (branch-structure branch)
  (car (cdr branch)) )

(define (branch-weight branch)
  (let ((structure (branch-structure branch)))
    (if (is-mobile? structure)
	(total-weight structure)
	structure )))

(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile)) ))

(define (is-mobile? structure)
  (pair? structure) )
	
(define (balanced? mobile)
  (define (branch-balanced? branch)
    (let ((structure (branch-structure branch)))
      (if (is-mobile? structure)
	  (balanced? structure)
	  #t )))
  (and (branch-balanced? (left-branch mobile))
       (branch-balanced? (right-branch mobile))
       (= (* (branch-weight (left-branch mobile)) (branch-length (left-branch mobile)))
	  (* (branch-weight (right-branch mobile)) (branch-length (right-branch mobile))) )))



;;------------------------------------------------------------------------
;;  Exercise 2.30
;;  -------------

;; (define (square-tree tree)
;;   (cond ((null? tree)       '())
;; 	((not (pair? tree)) (square tree))
;; 	(else               (cons (square-tree (car tree)) (square-tree (cdr tree)))) ))


(define (square-tree-map tree)
  (map (lambda (x)
	 (cond ((pair? x) (square-tree-map x))
	       (else      (square x)) ))
       tree))

;;------------------------------------------------------------------------
;;  Exercise 2.31
;;  -------------

(define (tree-map f tree)
  (map (lambda (x)
	 (cond ((pair? x) (square-tree-map x))
	       (else      (f x)) ))
       tree))

(define (square-tree tree) (tree-map square tree))

;;------------------------------------------------------------------------
;;  Exercise 2.32
;;  -------------

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))


;;------------------------------------------------------------------------

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

;;------------------------------------------------------------------------
;;  Exercise 2.33
;;  -------------

(define (my-map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(define (my-append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (my-length sequence)
  (accumulate (lambda (x n) (+ n 1))  0 sequence))

;;------------------------------------------------------------------------
;;  Exercise 2.34
;;  -------------

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ (* higher-terms x) this-coeff))
              0
              coefficient-sequence))

;;------------------------------------------------------------------------
;;  Exercise 2.35
;;  -------------

(define (count-leaves t)
  (accumulate +
	      0
	      (map (lambda (x) (if (pair? x) (count-leaves x) 1))
		   t)))

;;------------------------------------------------------------------------
;;  Exercise 2.36
;;  -------------

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

;;------------------------------------------------------------------------

(define (dot-product v w)
  'sorry-map-is-not-implemented-yet
  (accumulate + 0 (map * v w)))

;;------------------------------------------------------------------------
;;  Exercise 2.37
;;  -------------

(define (matrix-*-vector m v)
  (map (lambda (w) (dot-product v w)) m))

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (v) (matrix-*-vector cols v)) m)))



;;------------------------------------------------------------------------
;;  Exercise 2.38
;;  -------------

;; 3/2
;; 1/6
;; (1(2(3 ())))
;; (((() 1) 2) 3)

;; op doit etre commutative pour que fold-right et fold-left donnent le mm valeur.
;; (= (op x y) (op y x)) --> #t


;;------------------------------------------------------------------------
;;  Exercise 2.39
;;  -------------

(define (reverse1 sequence)
  (fold-right (lambda (x y) (append y (list x))) nil sequence))

(define (reverse2 sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))


;;------------------------------------------------------------------------

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

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)
  (fold-right append nil (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))


;;------------------------------------------------------------------------
;;  Exercise 2.40
;;  -------------

(define (unique-pairs n)
  (flatmap (lambda (i)
	     (map (lambda (j) (list i j))
		  (enumerate-interval 1 (- i 1)) ))
	   (enumerate-interval 1 n) ))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
	       (unique-pairs n) )))
;;------------------------------------------------------------------------
;;  Exercise 2.41
;;  -------------

(define (unique-triples n)
  (flatmap (lambda (i)
	     (flatmap (lambda (j)
		    (map (lambda (k) (list i j k))
			 (enumerate-interval 1 (- j 1)) ))
		  (enumerate-interval 1 (- i 1)) ))
	   (enumerate-interval 1 n) ))

(define (sum l) (fold-left + 0 l))

(define (sum-triples-eq n s)
  (filter (lambda (t) (= (sum t) s))
	  (unique-triples n) ))

;;------------------------------------------------------------------------

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

;;------------------------------------------------------------------------
;;  Exercise 2.42
;;  -------------

(define (adjoin-position new-row k rest-of-queens)
  (cons new-row rest-of-queens))

(define empty-board '())

(define (diagonal-safe? dir x rest)
  (if (null? rest)
      #t
      (let ((next (dir x 1)))
	(if (= next (car rest))
	    #f
	    (diagonal-safe? dir next (cdr rest)) ))))

(define (safe? k positions)
  (if (null? positions)
      #t
      (and
       (not (find (lambda (x) (= x (car positions))) (cdr positions)))
       (diagonal-safe? - (car positions) (cdr positions))
       (diagonal-safe? + (car positions) (cdr positions)) )))

;;------------------------------------------------------------------------
;;  Exercise 2.43
;;  -------------

;; (define (queens board-size)
;;   (define (queen-cols k)
;;     (if (= k 0)
;;         (list empty-board)
;;         (filter
;;          (lambda (positions) (safe? k positions))
;; 	 (flatmap
;; 	  (lambda (new-row)
;; 	    (map (lambda (rest-of-queens)
;; 		   (adjoin-position new-row k rest-of-queens))
;; 		 (queen-cols (- k 1))))
;; 	  (enumerate-interval 1 board-size)) )))
;;   (queen-cols board-size))
	 













