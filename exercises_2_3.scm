#lang scheme

;;-----------------------------------------------------------------------------
;; Exercise 2.54
;; -------------

(define (equal? x y)
  (display x)
  (display y)
  (newline)
  (if (and (pair? x) (pair? y))
      (and (equal? (car x) (car y)) (equal? (cdr x) (cdr y)))
      (eq? x y)
      ))

;;-----------------------------------------------------------------------------

;; (define (deriv exp var)
;;   (cond ((number? exp) 0)
;;         ((variable? exp)
;;          (if (same-variable? exp var) 1 0))
;;         ((sum? exp)
;;          (make-sum (deriv (addend exp) var)
;;                    (deriv (augend exp) var)))
;;         ((product? exp)
;;          (make-sum
;;           (make-product (multiplier exp)
;;                         (deriv (multiplicand exp) var))
;;           (make-product (deriv (multiplier exp) var)
;;                         (multiplicand exp))))
;;         (else
;;          (error "unknown expression type -- DERIV" exp))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)) )

(define (=number? exp num)
  (and (number? exp) (= exp num)))

;; ;; Addition
;; (define (make-sum a1 a2)
;;   (cond ((=number? a1 0)                 a2)
;;         ((=number? a2 0)                 a1)
;;         ((and (number? a1) (number? a2)) (+ a1 a2))
;;         (else                            (list '+ a1 a2)) ))

;; (define (sum? x)
;;   (and (pair? x) (eq? (car x) '+)))

;; (define (addend s) (cadr s))

;; (define (augend s) (caddr s))

;; ;; Multiplication
;; (define (make-product m1 m2)
;;   (cond ((or (=number? m1 0) (=number? m2 0)) 0)
;;         ((=number? m1 1)                      m2)
;;         ((=number? m2 1)                      m1)
;;         ((and (number? m1) (number? m2))      (* m1 m2))
;;         (else                                 (list '* m1 m2)) ))

;; (define (product? x)
;;   (and (pair? x) (eq? (car x) '*)))

;; (define (multiplier p) (cadr p))

;; (define (multiplicand p) (caddr p))

;;-----------------------------------------------------------------------------
;; Exercise 2.56
;; -------------

;; Exponentiation

;; (define (make-exponentiation b e)
;;   (cond ((=number? e 0)                  1)
;;         ((=number? e 1)                  b)
;;         ((=number? b 0)                  0)
;;         ((and (number? b) (number? e)) (expt b e))
;;         (else                            (list '** b e)) ))

;; (define (exponentiation? x)
;;   (and (pair? x) (eq? (car x) '**)) )

;; (define (base x) (cadr x))

;; (define (exponent x) (caddr x))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiation? exp)
         (make-product
          (make-product (exponent exp)
                        (make-exponentiation (base exp)
                                             (make-sum (exponent exp) -1) ))
          (deriv (base exp) var) ))
        (else
         (error "unknown expression type -- DERIV" exp))))


;;-----------------------------------------------------------------------------
;; Exercise 2.57
;; -------------

;; ;; Addition
;; (define (make-sum a1 a2)
;;   (cond ((=number? a1 0)                 a2)
;;         ((=number? a2 0)                 a1)
;;         ((and (number? a1) (number? a2)) (+ a1 a2))
;;         (else                            (list '+ a1 a2)) ))

;; (define (sum? x)
;;   (and (pair? x) (eq? (car x) '+)))

;; (define (addend s) (cadr s))

;; (define (augend s)
;;   (if (null? (cddr s))
;;       0
;;       (cons '+ (cddr s)) ))

;; ;; Multiplication
;; (define (make-product m1 m2)
;;   (cond ((or (=number? m1 0) (=number? m2 0)) 0)
;;         ((=number? m1 1)                      m2)
;;         ((=number? m2 1)                      m1)
;;         ((and (number? m1) (number? m2))      (* m1 m2))
;;         (else                                 (list '* m1 m2)) ))

;; (define (product? x)
;;   (and (pair? x) (eq? (car x) '*)))

;; (define (multiplier p) (cadr p))

;; (define (multiplicand p)
;;   (if (null? (cddr p))
;;       1
;;       (cons '* (cddr p)) ))


;;-----------------------------------------------------------------------------
;; Exercise 2.58
;; -------------

(define (simplify exp )
  (if (null? (cdr exp))
      (car exp)
      exp ))

;; Addition
(define (make-sum a1 a2)
  (cond ((=number? a1 0)                 a2)
        ((=number? a2 0)                 a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else                            (list a1 '+ a2)) ))

(define (sum? x)
  (and (pair? x)
       (pair? (cdr x))
       (or (eq? (cadr x) '+)
           (sum? (cddr x)) )))

(define (addend s) (simplify (takef s (lambda (x) (not (eq? x '+))))))

(define (augend s) (simplify (cdr (dropf s (lambda (x) (not (eq? x '+)))))))

;; Multiplication
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1)                      m2)
        ((=number? m2 1)                      m1)
        ((and (number? m1) (number? m2))      (* m1 m2))
        (else                                 (list m1 '* m2)) ))

(define (product? x)
  (and (pair? x)
       (pair? (cdr x))
       (or (eq? (cadr x) '*)
           (sum? (cddr x)) )))

(define (multiplier p) (simplify (takef p (lambda (x) (not (eq? x '*))))))

(define (multiplicand p) (simplify (cdr (dropf p (lambda (x) (not (eq? x '*)))))))

;; Exponentiation
(define (make-exponentiation b e)
  (cond ((=number? e 0)                  1)
        ((=number? e 1)                  b)
        ((=number? b 0)                  0)
        ((and (number? b) (number? e)) (expt b e))
        (else                            (list  b '** e)) ))

(define (exponentiation? x)
  (and (pair? x)
       (pair? (cdr x))
       (eq? (cadr x) '**)) )

(define (base x) (car x))

(define (exponent x) (caddr x))



