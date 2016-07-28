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

;;--------------------------------------------------------------------------
;; (define (find-divisor n test-divisor)
;;   (cond ((> (square test-divisor) n) n)
;;         ((divides? test-divisor n) test-divisor)
;;         (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))



(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

;;--------------------------------------------------------------------------
;; Exercise 1.22
;;--------------

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

;; (define (start-prime-test n start-time)
;;   (if (prime? n)
;;       (report-prime (- (runtime) start-time))
;;       #f))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

;; (define (search-for-primes a b)
;;   (define (incr a b)
;;     (if (< a b)
;; 	(
;; 	 (if (start-prime-test a (runtime))
;; 	     (newline)
;; 	     #f)
;; 	 (incr (+ a 2) b)
;; 	 )
;; 	#f))
;;   (incr a b)
;;   )
  
(define (search-for-primes a b)
  (define (iter a b)
    (if (< a b)
	(begin
	  (timed-prime-test a)
	  (search-for-primes (+ a 2) b)
	  )
	#f))
  (iter (if (odd? a) a (+ a 1)) b)
  )

;;--------------------------------------------------------------------------
;; Exercise 1.23
;;--------------

(define (find-divisor n test-divisor)
  (define (next x) (if (= x 2) 3 (+ x 2)))
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

;;--------------------------------------------------------------------------
;; Exercise 1.24
;;--------------
(define (start-prime-test n start-time)
  (if (fast-prime? n 2000)
      (report-prime (- (runtime) start-time))
      #f))

;;--------------------------------------------------------------------------
;; Exercise 1.25
;;--------------

;; (define (fast-expt b n)
;;   (cond ((= n 0) 1)
;;         ((even? n) (square (fast-expt b (/ n 2))))
;;         (else (* b (fast-expt b (- n 1))))))

;; (define (expmod base exp m)
;;   (remainder (fast-expt base exp) m))

;;--------------------------------------------------------------------------
;; Exercise 1.26
;;--------------

(define (fermat-test-all n)
  (define (fermat-test n a)
    (= (expmod a n n) a)
    )
  
  (define (failed-mess a)
    (newline)
    (display "fermat-test failed on ")
    (display a)
    )
    
  (define (iter n a)
    (if (< a n)
	(if (fermat-test n a)
	    (iter n (+ a 1))
	    (begin (failed-mess a) #f)
	    )
	#t
	)
    )
  (iter n 2)
  )
    
	    
       

;;--------------------------------------------------------------------------
;; Exercise 1.27
;;--------------
	
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (square_check-mod x m)
  (if (and (not (= x 1))
	   (not (= x (- m 1)))
	   (= (remainder (* x x) m) 1)
	   )
      0
      (remainder (* x x) m))
  )
		

(define (miller-rabin-test n)
  (define (expmod base exp m)
    (cond ((= exp 0) 1)
	  ((even? exp)
	   (square_check-mod (expmod base (/ exp 2) m)
			     m))
	  (else
	   (remainder (* base (expmod base (- exp 1) m))
		      m))))

  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (try-it (+ 2 (random (- n 2)))))
