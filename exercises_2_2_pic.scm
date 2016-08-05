#lang sicp
(#%require sicp-pict)

;;-----------------------------------------------------------------------
(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

;;-----------------------------------------------------------------------
;; Exercise 2.44
;; -------------

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))


;;-----------------------------------------------------------------------
;; Exercise 2.45
;; -------------

(define (split fst_dir snd_dir)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller ((split fst_dir snd_dir) painter (- n 1))))
          (fst_dir painter (snd_dir smaller smaller)) ))))

;;-----------------------------------------------------------------------
(define right-split1 (split beside below))
(define up-split1 (split below beside))

;;-----------------------------------------------------------------------
;; Exercise 2.46
;; -------------

;;(define (make-vect x y) (cons x y))

(define (xcor-vect v) (car v))

(define (ycor-vect v) (cdr v))

(define (add-vect v1 v2) (make-vect (+ (xcor-vect v1) (xcor-vect v2)) (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2) (make-vect (- (xcor-vect v1) (xcor-vect v2)) (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect s v) (make-vect (* s (xcor-vect v)) (* s (ycor-vect v))))

;;-----------------------------------------------------------------------
;; Exercise 2.46
;; -------------

;;(define (make-frame origin edge1 edge2)
;;  (list origin edge1 edge2))

(define (origin-frame f) (car f))
(define (edge1-frame f) (cadr f))
(define (edge2-frame f) (caddr f))


(define (make_frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin_frame f) (car f))
(define (edge1_frame f) (cadr f))
(define (edge2_frame f) (cddr f))

;;-----------------------------------------------------------------------
;; Exercise 2.47
;; -------------

;(define (make-segment start-v end-v) (cons start-v end-v))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

;;-----------------------------------------------------------------------

;(define (segments->painter segment-list)
;  (lambda (frame)
;    (for-each
;     (lambda (segment)
;       (draw-line
;        ((frame-coord-map frame) (start-segment segment))
;        ((frame-coord-map frame) (end-segment segment))))
;     segment-list)))

;;-----------------------------------------------------------------------
;; Exercise 2.47
;; -------------

(define ONE 0.99)
(define ZERO 0.0)
(define HALF 0.5)

(define outline
  (segments->painter (list (make-segment (make-vect ZERO ZERO) (make-vect ONE ZERO))
                           (make-segment (make-vect ONE ZERO) (make-vect ONE ONE))
                           (make-segment (make-vect ONE ONE) (make-vect ZERO ONE))
                           (make-segment (make-vect ZERO ONE) (make-vect ZERO ZERO)) )))

(define draw-X
  (segments->painter (list (make-segment (make-vect ZERO ZERO) (make-vect ONE ONE))
                           (make-segment (make-vect ZERO ONE) (make-vect ONE ZERO)) )))
(define diamond
  (segments->painter (list (make-segment (make-vect HALF ZERO) (make-vect ONE HALF))
                           (make-segment (make-vect ONE HALF)  (make-vect HALF ONE))
                           (make-segment (make-vect HALF ONE)  (make-vect ZERO HALF))
                           (make-segment (make-vect ZERO HALF) (make-vect HALF ZERO)) )))
