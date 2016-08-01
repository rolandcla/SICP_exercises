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

