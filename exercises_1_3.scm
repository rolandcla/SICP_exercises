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


;;---------------------------------------------------------------------
(define tolerance 0.00001)

;; (define (fixed-point f first-guess)
;;   (define (close-enough? v1 v2)
;;     (< (abs (- v1 v2)) tolerance))
;;   (define (try guess)
;;     (let ((next (f guess)))
;;       (if (close-enough? guess next)
;;           next
;;           (try next))))
;;   (try first-guess))

(define dx 0.00001)

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (average x y) (/ (+ x y) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))

;;-------------------------------------------------------------------------
;;  Exercise 1.40
;;  -------------

(define (cube a b c)
  (lambda (x) (+ (* x x x) (* a x x) (* b x) c)) )


;;-------------------------------------------------------------------------
;;  Exercise 1.41
;;  -------------

(define (double f)
  (lambda (x) (f (f x))) )


;;-------------------------------------------------------------------------
;;  Exercise 1.42
;;  -------------

(define (compose f g)
  (lambda (x) (f (g x))))

;;-------------------------------------------------------------------------
;;  Exercise 1.43
;;  -------------
;; (define (repeated f n)
;;   (define (iter n x)
;;     (if (= n 0)
;; 	x
;; 	(iter (- n 1) (f x)) ) )
;;   (lambda (x) (iter n x)) )

(define (repeated f n)
  (define (iter n g)
    (if (= n 0)
	g
	(iter (- n 1) (compose f g)) ))
  (iter n (lambda (x) x))
  )


;;-------------------------------------------------------------------------
;;  Exercise 1.44
;;  -------------
(define dx 0.1)

(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))


;;-------------------------------------------------------------------------
;;  Exercise 1.45
;;  -------------

(define (root n x)
  (let ( (cnt (round (/ (log n) (log 2))) )
	 )
    (fixed-point ((repeated average-damp cnt) (lambda (y) (/ x (expt y (- n 1))))) 1.0) ))


;;-------------------------------------------------------------------------
;;  Exercise 1.46
;;  -------------

(define (iterative-improve good-enough? improve)
  (define (iter guess)
    (let ((next (improve guess)))
      (if (good-enough? guess next)
	  next
	  (iter next) ) ) )
  
  (lambda (guess) (iter guess)) )

;; (define (iterative-improve good-enough? improve)
;;   (define (iter guess)
;;     (if (good-enough? guess)
;; 	guess
;; 	(iter (improve guess)) ))
;;   iter
;;   )

	  
(define (fixed-point f first-guess)
  (( iterative-improve
     (lambda (v1 v2) (< (abs (- v1 v2)) tolerance))
     f)
   first-guess))

(define (sqrt x)
  (( iterative-improve
     (lambda (v1 v2) (< (abs (- v1 v2)) tolerance))
     (lambda (guess) (average guess (/ x guess))))
   1.0))


