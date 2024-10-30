#lang racket

(provide poly2-func)

(require rascas)
(require math/matrix)

(define (poly2-func f the-vars)
  (let* ([all-eqns (cons '*
                     (map (λ (e) (cons '+ e))
                          (for/list ([v the-vars]) `(1 ,v (* ,v ,v)))))]
         ;; All possible polynomials with degree <= 2
         [eqns-2ord (filter (λ (e) (>= 2 (degree-gpe e the-vars)))
                            (cdr (algebraic-expand all-eqns)))]

         ;; Now generate random points, sufficient to solve equations
         [points (for/list ([_ eqns-2ord])
                   (for/list ([_ the-vars]) (random 1 100)))])

    (for/hash ([v the-vars]
               [v-idx (in-naturals)])
      ;; For each variable we're trying to solve, generate a system of
      ;; equations m with result b
      (let-values ([(m b)
                    (for/lists (_m _b)
                               ([p points])
                      (let* ([f-of-p (apply f p)]
                             [subst-map (for/hash ([k the-vars] [v p]) (values k v))]
                             [f-of-p-var (list-ref f-of-p v-idx)])
                        (values
                         (map (λ (term) (substitute/dict term subst-map)) eqns-2ord)
                         (list f-of-p-var))))])

        ;; So now I've got 15 equations like
        ;;
        ;;     c₀1 + c₁w + c₂w² + c₃x + c₄xw + ... = f(random-point)[var-of-interest]
        ;;     ...
        ;;
        ;; for *every* variable. I need 15 equations to find a solution to
        ;; the coefficients, and I need an equation for every variable.
        (let ([m-mat (list->matrix (length eqns-2ord)
                                   (length eqns-2ord)
                                   (flatten m))]
              [b-mat (->col-matrix (list->matrix 1 (length (flatten b)) (flatten b)))])
          (let ([coeff-soln (matrix->list (matrix-solve m-mat b-mat))])
            (values v (smart-simplify (cons '+ (map * coeff-soln eqns-2ord))))))))))

(define (f1 x y z w)
  (for ([i (range w)])
    (set! z (+ z x))
    (set! y (+ y x))
    (set! x y)
    (set! y 0))
  (list x y z w))
