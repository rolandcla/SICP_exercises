;;--------------------------------------------------------------------------
;;  Exercise 1.11
;;  -------------

(define (g n)
  (if (< n 3)
      n
      (+ (g (- n 1))
         (* (g (- n 2)) 2)
         (* (g (- n 3)) 3)
      )
  )
)
  
(define (f n)
  (define (iter f1 f2 f3 m)
    (if (> m n)
        f1
        (iter (+ f1 (* 2 f2) (* 3 f3))
              f1
              f2
              (+ m 1)
        )
    )
  )
  (if (< n 3)
      n
      (iter 2 1 0 3)
  )
  )

;;--------------------------------------------------------------------------
;;  Exercise 1.12
;;  -------------

(define (pascal row col)
  (cond ((> col row) 0)
	((= col 0)   1)
        ((= col row) 1)
        (else (+ (pascal (- row 1) (- col 1)) (pascal (- row 1) col)))
	)
  )

;;--------------------------------------------------------------------------
;;  Exercise 1.16
;;  -------------

(define (fast-expt b n)
  (define (iter b n a)
    (cond ((= n 0) a)
	  ((odd? n) (iter b (- n 1) (* a b)))
	  (else     (iter (* b b) (/ n 2) a))
	  )
    )
  (iter b n 1)
  )

;;--------------------------------------------------------------------------
(define (double x) (+ x x))
(define (halve x) (/ x 2))

;;--------------------------------------------------------------------------
;;  Exercise 1.17
;;  -------------

;; (define (mul a b)
;;   (cond ((= b 0) 0)
;; 	((even? b) (double (mul a (halve b))))
;; 	(else      (+ a (mul a (- b 1))))
;; 	)
;;   )

;;--------------------------------------------------------------------------
;;  Exercise 1.18
;;  -------------

(define (mul a b)
  (define (iter a b s)
    (cond ((= b 0)  s)
	  ((odd? b) (iter a (- b 1) (+ s a)))
	  (else     (iter (double a) (halve b) s))
	  )
    )
  (iter a b 0)
  )

;;--------------------------------------------------------------------------
(define (fib n)
  (fib-iter 1 0 0 1 n))

;;--------------------------------------------------------------------------
;;  Exercise 1.19
;;  -------------

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (* p p) (* q q)) ; compute p'
                   (+ (* 2 p q) (* q q))      ; compute q'
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

;;--------------------------------------------------------------------------
;;  Exercise 1.20
;;  -------------

(define (rem a b) (remainder a b))

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (gcd a b)
  (new-if (= b 0)
      a
      (gcd b (rem a b))))


;;--------------------------------------------------------------------------
;;  Exercise 1.21
;;  -------------

(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))
