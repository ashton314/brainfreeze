#lang racket

(require rascas)
(require math/matrix)

(vars x y z w)

(define (f1 x y z w)
  (for ([i (range w)])
    (set! z (+ z x))
    (set! y (+ y x))
    (set! x y)
    (set! y 0))
  (list x y z w))

(define v (* (+ 1 x (* x x))
             (+ 1 y (* y y))
             (+ 1 z (* z z))
             (+ 1 w (* w w))))

;; Now I need to get all the monomials of degree <= 2
(define vv (algebraic-expand v))
(define vvv (filter (λ (e) (>= 2 (degree-gpe e '(x y z w)))) (cdr vv)))

(define points
  (for/list ([_ (range (length vvv))])
    (for/list ([_ (range 4)]) (random 1 100))))

(for/hash ([v '(x y z w)]
           [vi (in-naturals)])
  ;; for each variable, build a system
  (let-values ([(m b)
                (for/lists (_m _b)
                           ([p points])
                  (let* ([f-of-p (apply f1 p)]
                         ;; key was changing [v f-of-p] → [v p]
                         [subst-map (for/hash ([k '(x y z w)] [v p]) (values k v))]
                         [f-of-p-var (list-ref f-of-p vi)])
                    (values
                     ;; values for the coefficients
                     (map (λ (term) (substitute/dict term subst-map)) vvv)
                     ;; value for this term
                     (list f-of-p-var))))])
    ;; So now I've got 15 equations like
    ;;
    ;;     c₀1 + c₁w + c₂w² + c₃x + c₄xw + ... = f(random-point)[var-of-interest]
    ;;     ...
    ;;
    ;; for *every* variable. I need 15 equations to find a solution to
    ;; the coefficients, and I need an equation for every variable.
    (let ([m (list->matrix (length vvv) (length vvv) (flatten m))]
          [b (->col-matrix (list->matrix 1 (length (flatten b)) (flatten b)))])
      (let ([coeff-soln (matrix->list (matrix-solve m b))])
        (values v (smart-simplify (cons '+ (map * coeff-soln vvv))))))))

; x = x + y; y = 0; z = w * x -y + w * y + z; w = w
