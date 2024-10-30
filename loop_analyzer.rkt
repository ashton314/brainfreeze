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

;; Generage coefficients for each one of those
(define coeffs (for/list ([i (range (length vvv))])
                 (string->symbol (format "c~a" i))))

(define points
  (for/list ([_ (range (length vvv))])
    (for/list ([_ (range 4)]) (random 1 100))))

#;
(for/list ([p points])
  (let* ([f-res (apply f1 p)]
         [s-map (for/hash ([k '(x y z w)] [v f-res]) (values k v))])
    (append
     ;; values for the coefficients
     (map (λ (term) (substitute/dict term s-map)) vvv)
     )))

#;
(for/list ([_v '(x y z w)]
           [vi (in-naturals)])
  ;; for each variable, build a system
  (for/list ([p points])
    (let* ([f-of-p (apply f1 p)]
           [subst-map (for/hash ([k '(x y z w)] [v f-of-p]) (values k v))]
           [f-of-p-var (list-ref f-of-p vi)])
      (append
       ;; values for the coefficients
       (map (λ (term) (substitute/dict term subst-map)) vvv)
       ;; value for this term
       (list f-of-p-var)))))

; x = x + y; y = 0; z = w * x -y + w * y + z; w = w

(for/list ([_v '(x y z w)]
           [vi (in-naturals)])
  ;; for each variable, build a system
  (let-values ([(m b)
                (for/lists (_m _b)
                           ([p points])
                  (let* ([f-of-p (apply f1 p)]
                         [subst-map (for/hash ([k '(x y z w)] [v p]) (values k v))]
                         [f-of-p-var (list-ref f-of-p vi)])

                    ;; (displayln "-----------------------")
                    ;; (printf "f~a = ~a\n" p f-of-p)
                    ;; (printf "map: ~a\n" subst-map)
                    ;; (printf "c_i: ~a\n" (map (λ (term) (substitute/dict term subst-map)) vvv))

                    (values
                     ;; values for the coefficients
                     (map (λ (term) (substitute/dict term subst-map)) vvv)
                     ;; value for this term
                     (list f-of-p-var))))])
    (let ([m (list->matrix (length vvv) (length vvv) (flatten m))]
          [b (->col-matrix (list->matrix 1 (length (flatten b)) (flatten b)))])
      (matrix-solve m b))))

;; (define expr `(+ ,@(map (λ (c t) `(* ,c ,t)) coeffs (cdr vv))))

;; (define points
;;   (for/list ([_ (range (- (length vv) 1))])
;;     (for/list ([_ (range 4)]) (random 1 100))))

;; (define syms '(x y z w))

;; ;; var "eqs"
;; (for/list ([s syms]
;;            [i (range (length syms))])
;;   (for/list ([p points])
;;     (substitute/dict expr (for/hash ([k syms] [v p]) (values k v))))
;;   )
