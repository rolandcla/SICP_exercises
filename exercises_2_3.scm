#lang scheme

;;-----------------------------------------------------------------------------
;; Exercise 2.54
;; -------------

;; (define (equal? x y)
;;   (if (and (pair? x) (pair? y))
;;       (and (equal? (car x) (car y)) (equal? (cdr x) (cdr y)))
;;       (eq? x y)
;;       ))

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

;;-----------------------------------------------------------------------------

;; (define (element-of-set? x set)
;;   (cond ((null? set) false)
;;         ((equal? x (car set)) true)
;;         (else (element-of-set? x (cdr set)))))

;; (define (adjoin-set x set)
;;   (if (element-of-set? x set)
;;       set
;;       (cons x set)))

;; (define (intersection-set set1 set2)
;;   (cond ((or (null? set1) (null? set2)) '())
;;         ((element-of-set? (car set1) set2)
;;          (cons (car set1)
;;                (intersection-set (cdr set1) set2)))
;;         (else (intersection-set (cdr set1) set2))))

;;-----------------------------------------------------------------------------
;; Exercise 2.59
;; -------------

;; (define (union-set set1 set2)
;;   (cond ((null? set1) set2)
;;         ((null? set2) set1)
;;         (else (adjoin-set (car set1) (union-set (cdr set1) set2))) ))


;;-----------------------------------------------------------------------------
;; Exercise 2.60
;; -------------

;; (define (element-of-set? x set)
;;   (cond ((null? set) false)
;;         ((equal? x (car set)) true)
;;         (else (element-of-set? x (cdr set)))))

;; (define (adjoin-set x set)
;;   (cons x set))

;; (define (intersection-set set1 set2)
;;   (cond ((or (null? set1) (null? set2)) '())
;;         ((element-of-set? (car set1) set2)
;;          (cons (car set1)
;;                (intersection-set (cdr set1) set2)))
;;         (else (intersection-set (cdr set1) set2))))

;; (define (union-set set1 set2)
;;   (append set1 set2) )

;;-----------------------------------------------------------------------------

;; (define (element-of-set? x set)
;;   (cond ((null? set) false)
;;         ((= x (car set)) true)
;;         ((< x (car set)) false)
;;         (else (element-of-set? x (cdr set)))))

;; (define (intersection-set set1 set2)
;;   (if (or (null? set1) (null? set2))
;;       '()
;;       (let ((x1 (car set1)) (x2 (car set2)))
;;         (cond ((= x1 x2)
;;                (cons x1
;;                      (intersection-set (cdr set1)
;;                                        (cdr set2))))
;;               ((< x1 x2)
;;                (intersection-set (cdr set1) set2))
;;               ((< x2 x1)
;;                (intersection-set set1 (cdr set2)))))))

;;-----------------------------------------------------------------------------
;; Exercise 2.61
;; -------------

;; (define (adjoin-set x set)
;;   (cond ((null? set) (list x))
;;         ((< x (car set)) (cons x set))
;;         ((= x (car set)) set)
;;         (else            (cons (car set) (adjoin-set x (cdr set)))) ))

;;-----------------------------------------------------------------------------
;; Exercise 2.62
;; -------------

;; (define (union-set set1 set2)
;;   (cond ((null? set1)              set2)
;;         ((null? set2)              set1)
;;         ((< (car set1) (car set2)) (cons (car set1) (union-set (cdr set1) set2)))
;;         ((= (car set1) (car set2)) (cons (car set1) (union-set (cdr set1) (cdr set2))))
;;         (else                      (cons (car set2) (union-set set1 (cdr set2)))) ))

;;-----------------------------------------------------------------------------

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))


(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))


;;-----------------------------------------------------------------------------
;; Exercise 2.63
;; -------------

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

(define set1 '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ()))) )


;;-----------------------------------------------------------------------------
;; Exercise 2.64
;; -------------

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

;;-----------------------------------------------------------------------------
;; Exercise 2.65
;; -------------

(define (intersection-sorted-list set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-sorted-list (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (intersection-sorted-list (cdr set1) set2))
              ((< x2 x1)
               (intersection-sorted-list set1 (cdr set2)))))))

(define (intersection-set set1 set2)
  (list->tree (intersection-sorted-list (tree->list-2 set1)
                                        (tree->list-2 set2))))

;;-----------------------------------------------------------------------------
;; Exercise 2.65
;; -------------

(define (lookup given-key set-of-records)
  (if (null? set-of-records)
      #f
      (let ((e (entry set-of-records))
            (k (key (entry set-of-records))))
        (cond ((= given-key k) e)
              ((< given-key k) (lookup given-key (left-branch set-of-records)))
              (else            (lookup given-key (right-branch set-of-records))) ))))

(define key identity)





