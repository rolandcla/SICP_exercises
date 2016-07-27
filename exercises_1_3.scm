;;---------------------------------------------------------------------
;;  Exercise 1.29
;;  -------------
(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (term a k)
    (* (f a)
       (cond ((= k 0)    1)
	     ((= k n)    1)
	     ((odd? k)   4)
	     (else       2) )))
  (define (iter a k s)
    (if (> k n)
	(* s (/ h 3))
	(iter (+ a h) (+ k 1) (+ s (term a k))) ))
  (iter a 0 0)
  )
       

;;---------------------------------------------------------------------
;;  Exercise 1.30
;;  -------------
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a))) ))
  (iter a 0) )

;;---------------------------------------------------------------------



;;  Exercise 1.31
;;  -------------
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a))) ))
  (iter a 1) )


;;---------------------------------------------------------------------
;;  Exercise 1.32
;;  -------------
(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a))) ))
  (iter a null-value) )
  
;;---------------------------------------------------------------------
;;  Exercise 1.33
;;  -------------
(define (filtered-accumulate combiner null-value term a next b filter?)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (if (filter? a)
			   (combiner result (term a))
			   result ))))
  (iter a null-value) )

;;---------------------------------------------------------------------
;; (define tolerance 0.00001)

;; (define (fixed-point f first-guess)
;;   (define (close-enough? v1 v2)
;;     (< (abs (- v1 v2)) tolerance))
;;   (define (try guess)
;;     (let ((next (f guess)))
;;       (if (close-enough? guess next)
;;           next
;;           (try next))))
;;   (try first-guess))

;;---------------------------------------------------------------------
;;  Exercise 1.35
;;  -------------
(define (phi)
  (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)
  )


;;---------------------------------------------------------------------
;;  Exercise 1.36
;;  -------------
  
(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  
  (try first-guess))

;;---------------------------------------------------------------------
;;  Exercise 1.37
;;  -------------
  
(define (cont-frac n d k)
  (define (iter k result)
    (if (= k 0)
	result
	(iter (- k 1) (/ (n k) (+ (d k) result))) ))
  (iter k 0) )

;;---------------------------------------------------------------------
;;  Exercise 1.38
;;  -------------

(define (euler)
  (define (n k) 1)
  (define (d k)
    (cond ((= (remainder k 3) 2) (+ 2 (* 2 (quotient k 3))))
	  (else                  1)))
  (cont-frac n d 10)
  )
;;---------------------------------------------------------------------
;;  Exercise 1.39
;;  -------------

(define (tan-cf x k)
  (define (d k) (- (* k 2) 1))
  (define (n k) (if (= k 1) x (-(* x x))) )
  (cont-frac n d k)
  )


