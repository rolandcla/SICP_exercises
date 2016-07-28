(define (gcd x y)
  (let ( (r (remainder x y)))
    (if (= r 0)
	y
	(gcd y r) )))


;;--------------------------------------------------------------------------

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (print-rat x)
  (display (numer x))
  (display '/)
  (display (denom x))
  (newline))

;;--------------------------------------------------------------------------
;;  Exercise 2.1
;;  ------------

(define (make-rat n d)
  (let ((g (abs (gcd n d))))
    (if (> d 0)
	(cons (/ n g) (/ d g))
	(cons (/ (- n) g) (/ (- d) g)) )))


;;--------------------------------------------------------------------------
;;  Exercise 2.2
;;  ------------

(define (average x y)
  (/ (+ x y) 2) )

(define (make-point x y)
  (cons x y) )

(define (x-point p)
  (car p) )

(define (y-point p)
  (cdr p) )

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  (newline))

(define (make-segment p q)
  (cons p q) )

(define (start-segment s)
  (car s) )

(define (end-segment s)
  (cdr s) )

(define (midpoint-segment s)
  (let ( (p (start-segment s))
	 (q (end-segment s)) )
    (make-point (average (x-point p) (x-point q))
		(average (y-point p) (y-point q)) )
    ))


;;--------------------------------------------------------------------------
;;  Exercise 2.3
;;  ------------

(define (make-rect bottom-left top-right)
  (cons (make-point (min (x-point bottom-left) (x-point top-right))
		    (min (y-point bottom-left) (y-point top-right)) )
	(make-point (max (x-point bottom-left) (x-point top-right))
		    (max (y-point bottom-left) (y-point top-right)) ) ))

(define (bottom-left-rect r)
  (car r) )

(define (top-right-rect r)
  (cdr r) )

(define (width-rect r)
  (- (x-point (top-right-rect r)) (x-point (bottom-left-rect r)) )
  )

(define (height-rect r)
  (- (y-point (top-right-rect r)) (y-point (bottom-left-rect r)) )
  )

(define (perimeter-rect r)
  (* 2 (+ (width-rect r) (height-rect r))) )

(define (area-rect r)
  (* (width-rect r) (height-rect r)) )

  


;;--------------------------------------------------------------------------
;;  Exercise 2.5
;;  ------------


(define (cons- a b)
  (* (expt 2 a) (expt 3 b)) )

(define (car- x)
  (if (= (remainder x 2) 0)
      (+ 1 (car- (/ x 2)))
      0 ))

(define (cdr- x)
  (if (= (remainder x 3) 0)
      (+ 1 (cdr- (/ x 3)))
      0 ))


;;--------------------------------------------------------------------------
;;  Exercise 2.6
;;  ------------

    
(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))



(define one (lambda (f) (lambda (x) (f x))))

(define two (lambda (f) (lambda (x) (f (f x)))))

(define (add n m) (lambda (f) (lambda (x) ((n f) ((m f) x)))))

;;--------------------------------------------------------------------------

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

(define (make-interval a b) (cons a b))

;;-----------------------------------------------------------------
;;  Exercise 2.7
;;  ------------

(define (lower-bound interval)
  (car interval) )

(define (upper-bound interval)
  (cdr interval) )

;;-----------------------------------------------------------------
;;  Exercise 2.8
;;  ------------

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
		 (- (upper-bound x) (lower-bound y)) ))


;;-----------------------------------------------------------------
;;  Exercise 2.9
;;  ------------

(define (width x)
  (/ (- (upper-bound x) (lower-bound x)) 2) )




;;-----------------------------------------------------------------
;;  Exercise 2.10
;;  -------------

(define (div-interval x y)
  (let ( (ub (upper-bound y))
	 (lb (lower-bound y)) )
    (cond ((or (= ub 0) (= lb 0)) (error "Division by zero !!!"))
	  (else (mul-interval x (make-interval (/ 1.0 ub) (/ 1.0 lb)))
		))))


;;-----------------------------------------------------------------
;;  Exercise 2.10
;;  -------------


(define (mul-interval x y)
  (let ( (xl (lower-bound x))
	 (xu (upper-bound x))
	 (yl (lower-bound y))
	 (yu (upper-bound y)))
    (cond ((> xl 0) (cond ((> yl 0) (make-interval (* xl yl) (* xu yu)) )
			  ((> yu 0) (make-interval (* xu yl) (* xu yu)) )
			  (else     (make-interval (* xu yl) (* xl yu)) ) ))
	  
	  ((> xu 0) (cond ((> yl 0) (make-interval (* xl yu) (* xu yu)) )
			  ((> yu 0) (make-interval (min (* xl yu) (* xu yl))
						   (max (* xl yl) (* xu yu))))
			  (else     (make-interval (* xu yl) (* xl yl)) ) ))
	  
	  (else     (cond ((> yl 0) (make-interval (* xl yu) (* xu yl)) )
			  ((> yu 0) (make-interval (* xl yu) (* xl yl)) )
			  (else     (make-interval (* xu yu) (* xl yl)) ) ))
	  )))


;;-----------------------------------------------------------------

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))


;;-----------------------------------------------------------------
;;  Exercise 2.12
;;  -------------

(define (make-center-percent center percent)
  (make-center-width center (/ (* center percent) 100)) )

(define (percent interval)
  (* (/ (width interval) (center interval)) 100) )


;;-----------------------------------------------------------------

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

;;-----------------------------------------------------------------









