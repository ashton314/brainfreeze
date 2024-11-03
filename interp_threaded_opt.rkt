#lang racket

(require syntax/parse/define)
(require "loop_analyzer_general.rkt")

(provide (all-defined-out))

(define (parse-prog-str str)
  (filter (λ (c) (member c '(#\> #\< #\+ #\- #\. #\, #\[ #\])))
          (string->list str)))

(define (parse-file filename)
  (parse-prog-str (file->string filename)))

(struct loop (body) #:transparent)
(struct add (amount) #:transparent)
(struct shift (amount) #:transparent)
(struct set-cell (value) #:transparent)
(struct bf-write () #:transparent)
(struct bf-read () #:transparent)
(struct add-cell-0 (dest) #:transparent)   ; zero current cell; add value to another cell
(struct mult-block-0 (body) #:transparent) ; cells in body + current * cell count; current zeroed
(struct search-0 (stride) #:transparent)   ; scan for zero
(struct poly-block (var->idx idx->poly orig) #:transparent)

(define (tree-size thingy)
  (match thingy
    [(list is ...) (apply + (map tree-size is))]
    [(loop body) (+ 1 (tree-size body))]
    [(add _) 1]
    [(add-cell-0 _) 1]
    [(mult-block-0 body) (length (hash-keys body))]
    [(poly-block _var-idx var-poly _) (length (hash-keys var-poly))]
    [(search-0 _) 1]
    [(shift _) 1]
    [(set-cell _) 1]
    [(bf-write) 1]
    [(bf-read) 1]))

(define (instr-size thingy)
  (match thingy
    [(list is ...) (apply + (map instr-size is))]
    [(loop body) (+ 4 (tree-size body))]
    [(add _) 3]
    [(add-cell-0 _) 7]
    [(mult-block-0 body) (* 2 (length (hash-keys body)))]
    [(poly-block _var-idx var-poly _) (* 3 (length (hash-keys var-poly)))]
    [(search-0 _) 19]
    [(shift _) 1]
    [(set-cell _) 2]
    [(bf-write) 2]
    [(bf-read) 2]))

(define (parse-prog char-list)
  (match char-list
    [(cons #\+ rst)
     (let-values ([(next unparsed) (parse-prog rst)])
       (values (cons (add 1) next) unparsed))]
    [(cons #\- rst)
     (let-values ([(next unparsed) (parse-prog rst)])
       (values (cons (add -1) next) unparsed))]
    [(cons #\< rst)
     (let-values ([(next unparsed) (parse-prog rst)])
       (values (cons (shift -1) next) unparsed))]
    [(cons #\> rst)
     (let-values ([(next unparsed) (parse-prog rst)])
       (values (cons (shift 1) next) unparsed))]
    [(cons #\. rst)
     (let-values ([(next unparsed) (parse-prog rst)])
       (values (cons (bf-write) next) unparsed))]
    [(cons #\, rst)
     (let-values ([(next unparsed) (parse-prog rst)])
       (values (cons (bf-read) next) unparsed))]
    [(cons #\[ rst)
     (let*-values ([(body after-loop) (parse-prog rst)]
                   [(next unparsed) (parse-prog after-loop)])
       (values (cons (loop body) next) unparsed))]
    [(cons #\] rst)
     (values '() rst)]
    ['() (values '() '())]))

(define-syntax (opt-pass stx)
  (syntax-parse stx
    [(_ opt-name prog-input)
     #:with opt-name-stx (datum->syntax #'opt-name (format "~a" (syntax->datum #'opt-name)))
     #'(let* ([input prog-input]
              [input-size (tree-size input)])
         (eprintf "~a opts (~a)> " input-size opt-name-stx)
         (let* ([output (opt-name input)]
                [output-size (tree-size output)])
           (eprintf "~a opts (~a -> ~a asm instrs)\n" output-size (instr-size input) (instr-size output))
           output))]))

(define-syntax (opt-chain stx)
  (syntax-parse stx
    [(_ (prog) opt-name)
     #'(opt-pass opt-name prog)]
    [(_ (prog) opt-name opt-names ...)
     #'(opt-pass opt-name (opt-chain (prog) opt-names ...))]))

(define (optimize-base prog)
  (opt-chain (prog)
             opt/zero-out
             opt/useless
             combine-instrs))

(define (optimize prog)
  (opt-chain (prog)
             opt/0-scan
             opt/2nd-order-loop
             opt/zero-add->set
             opt/basic-loop
             opt/zero-out
             opt/add
             opt/useless
             combine-instrs))

(define (optimize-no-basic prog)
  (opt-chain (prog)
             opt/0-scan
             opt/2nd-order-loop
             opt/zero-add->set
             opt/zero-out
             opt/add
             opt/useless
             combine-instrs))

(define (optimize-no-2nd-ord prog)
  (opt-chain (prog)
             opt/0-scan
             opt/zero-add->set
             opt/basic-loop
             opt/zero-out
             opt/add
             opt/useless
             combine-instrs))

(define (optimize-no-12nd-ord prog)
  (opt-chain (prog)
             opt/0-scan
             opt/zero-add->set
             opt/zero-out
             opt/add
             opt/useless
             combine-instrs))

(define (combine-instrs prog [indent 0])
  (cond
    [(null? prog) prog]
    [(list? prog)
     (let ([new-prog (list (combine-instrs (car prog) (+ 1 indent)))])
       (for ([i (cdr prog)])
         (match (cons i (car new-prog))
           [(cons (add a1) (add a2))
            (set! new-prog (cons (add (+ a1 a2)) (cdr new-prog)))]
           [(cons (shift s1) (shift s2))
            (set! new-prog (cons (shift (+ s1 s2)) (cdr new-prog)))]
           [(cons (loop body) _)
            (set! new-prog (cons (loop (combine-instrs body (+ 1 indent))) new-prog))]
           [_
            (set! new-prog (cons i new-prog))]))
       (reverse new-prog))]
    [(loop? prog) (loop (combine-instrs (loop-body prog) (+ 1 indent)))]
    [else prog]))

(define (opt/zero-out prog)
  (match prog
    [(loop (list (add -1))) (set-cell 0)]
    [(loop (list (add +1))) (set-cell 0)]
    [(loop body) (loop (map opt/zero-out body))]
    [(list is ...) (map opt/zero-out is)]
    [_ prog]))

(define (opt/zero-add->set prog)
  (match prog
    [(list (set-cell n) (add m) rst ...)
     (cons (set-cell (+ n m)) (opt/zero-add->set rst))]
    [(list (loop body) rst ...)
     (cons (loop (opt/zero-add->set body)) (opt/zero-add->set rst))]
    [(list hd rst ...)
     (cons hd (opt/zero-add->set rst))]
    ['() prog]))

#;
(define (opt/copy prog)
  (match prog
    [(list (loop (list (shift s1) (add 1) (shift s2) (add 1) (shift s3) (add -1)))
           (shift s4)
           (loop (list (shift s5) (add 1) (shift s6) (add -1)))
           rst ...)
     prog]
    [(list (loop body) rst ...)
     (cons (loop (opt/copy body)) (opt/copy rst))]
    [(list hd rst ...)
     (cons hd (opt/copy rst))]
    [_ prog]))

(define (opt/0-scan prog)
  (match prog
    [(cons (loop (list (shift x))) rst)
     #:when (member x '(1 2 4 8 -1 -2 -4 -8))
     ;; (eprintf "scan: ~a\n" x)
     (cons (search-0 x) (opt/0-scan rst))]
    #;[(cons (loop (list (shift x))) rst)
     (eprintf "missed: ~a\n" (shift x))
     (cons (car prog) (opt/0-scan rst))]
    [(cons (loop body) rst)
     (cons (loop (opt/0-scan body)) (opt/0-scan rst))]
    [(cons hd rst)
     (cons hd (opt/0-scan rst))]
    ['() prog]))

(define (opt/add prog)
  (match prog
    [(cons (loop (list (shift x1) (add a1) (shift x2) (add a2)))
           rst)
     #:when (and (zero? (+ x1 x2)) (< a2 0) (zero? (+ a1 a2)))
     (cons (add-cell-0 x1) (opt/add rst))]
    [(cons (loop (list (add a1) (shift x1) (add a2) (shift x2)))
           rst)
     #:when (and (zero? (+ x1 x2)) (< a1 0) (zero? (+ a1 a2)))
     (cons (add-cell-0 x1) (opt/add rst))]
    [(cons (loop body) rst)
     (cons (loop (opt/add body)) (opt/add rst))]
    [(cons hd rst)
     (cons hd (opt/add rst))]
    ['() prog]))

;; drop useless instructions
(define (opt/useless prog)
  (match prog
    [(list (loop body) rst ...)
     (cons (loop (opt/useless body)) (opt/useless rst))]
    [(cons (shift 0) rst)
     (opt/useless rst)]
    [(cons (add 0) rst)
     (opt/useless rst)]
    [(cons x rst)
     (cons x (opt/useless rst))]
    ['() prog]))

(define (opt/2nd-order-loop prog)
  (match prog
    [(loop body)
     (let-values ([(state ptr-amount) (analyze-2nd-order-loop body)])
       (if (and state
                (zero? ptr-amount)
                (not (hash-empty? state))
                (eqv? -1 (hash-ref state 0 'nothing)))
           ;; begin optimizing
           (let-values ([(var-map poly-map) (discover-poly prog)])
             (if var-map
                 (poly-block var-map poly-map prog)
                 (loop (map opt/2nd-order-loop body))))
           (loop (map opt/2nd-order-loop body))))]
    [(list rst ...) (map opt/2nd-order-loop rst)]
    [_ prog]))

(define (analyze-2nd-order-loop instrs)
  (let/cc return
    (for/fold ([state (make-immutable-hash)]
               [ptr 0])
              ([i instrs])
      (match i
        [(add amount)
         (values (hash-update state ptr (λ (x) (+ x amount)) 0)
                 ptr)]
        [(add-cell-0 dest)
         (values (hash-update state dest (λ (x) (+ x (hash-ref state ptr 0))) 0)
                 ptr)]
        [(set-cell v)
         (values (hash-set state ptr v)
                 ptr)]
        [(shift amount)
         (values state
                 (+ ptr amount))]
        [(mult-block-0 inner-state)
         ;; just make sure the loop variable doesn't get touched
         (if (zero? (hash-ref inner-state (- ptr) 0))
             (values
              state
              ptr)
             (return #f #f))]
        [_ (return #f #f)]))))

;; returns (values final-idx cell-idxs)
(define (loop-range prog)
  (let/cc fail
    (for/fold ([idxs (set)]
               [ptr 0])
              ([i prog])
      (match i
        [(loop body)
         (let-values ([(new-idxs new-ptr) (loop-range body)])
           (if (and new-ptr (zero? new-ptr))
               (values (set-union (list->set (set-map new-idxs (λ (x) (+ x ptr))))
                                  idxs)
                       ptr)
               (fail #f #f)))]
        [(add _) (values (set-add idxs ptr) ptr)]
        [(shift v) (values idxs (+ ptr v))]
        [(add-cell-0 dest) (values (set-add (set-add idxs ptr) dest)
                                   ptr)]
        [(set-cell _) (values (set-add idxs ptr) ptr)]
        [(mult-block-0 inner-state)
         (values (set-union (list->set (map (λ (x) (+ x ptr)) (hash-keys inner-state)))
                            idxs)
                 ptr)]))))

(define (discover-poly loop-prog)
  (let*-values ([(body) (loop-body loop-prog)]
                [(touched-idxs end-idx) (loop-range body)])
    (if (and (zero? end-idx) (<= (set-count touched-idxs) 5))
        (let* ([var-idx (for/hash ([i touched-idxs])
                          (values (string->symbol (format "x~a" i)) i))]
               [vars (hash-keys var-idx)] ; freeze order here
               [idxs (map (λ (k) (hash-ref var-idx k)) vars)]
               [state-range (+ (apply max (hash-values var-idx))
                               (abs (apply min (hash-values var-idx))))]
               [state (make-vector (* 3 state-range))]

               ;; compile the loop for interpretation
               ;; we don't do any optimizations here; we must be
               ;; careful not to recursively invoke this optimization
               [loop-prog-c (compile (list loop-prog))]
               [loop-fn (λ point
                          ;; set up state with point
                          (let ([state
                                 (for/fold ([s state])
                                           ([p point]
                                            [i idxs])
                                   (vector-set! s (+ i state-range) p)
                                   s)])
                            (let-values
                                ([(_pointer tape) (loop-prog-c state-range state)])
                              (for/list ([i idxs])
                                (vector-ref tape (+ i state-range))))))]
               [poly (poly2-func loop-fn vars)])
          #;(eprintf "number coeffs: ~a; nice? ~a\n" (set-count touched-idxs) (nice-soln? poly))
          (if (nice-soln? poly)
              (values (make-hash (map cons vars idxs))
                      (for/hash ([(k v) poly]) ; go from var ↦ poly to idx ↦ poly
                        (values (hash-ref var-idx k) v)))
              (values #f loop-prog)))
        (values #f loop-prog))))

(define (opt/basic-loop prog)
  (match prog
    [(loop body)
     (let-values ([(state ptr-amount) (analyze-loop body)])
       (if (and state
                (zero? ptr-amount)
                (not (hash-empty? state))
                (eqv? -1 (hash-ref state 0 'nothing)))
           (mult-block-0 state)
           (loop (map opt/basic-loop body))))]
    [(list rst ...) (map opt/basic-loop rst)]
    [_ prog]))

(define (analyze-loop instrs)
  (let/cc return
    (for/fold ([state (make-immutable-hash)]
               [ptr 0])
              ([i instrs])
      (match i
        [(add amount)
         (values (hash-update state ptr (λ (x) (+ x amount)) 0) ptr)]
        [(shift amount)
         (values state (+ ptr amount))]
        [_ (return #f #f)]))))

(define (parse-combine filename)
  (let-values ([(p _) (parse-prog (parse-file filename))])
    (combine-instrs p)))

#;(define p2 (parse-combine "./bench/benches/hanoi.b"))

(begin-for-syntax (define *instrument?* #f))

(define-syntax (dbg stx)
  (syntax-parse stx
    [(_ fmt vars ...)
     (if *instrument?*
         #'(eprintf fmt vars ...)
         #'"")]))

(define (comp-poly poly-expr var->idx)
  (match poly-expr
    [(? number? n)
     (λ (old-idx->val) n)]
    [(? symbol? x)
     (let ([offset (hash-ref var->idx x)])
       (λ (old-idx->val) (hash-ref old-idx->val offset)))]
    [(cons op args)
     (let ([args_ (map (λ (e) (comp-poly e var->idx)) args)]
           [op_ (case op [(+) +] [(*) *] [(/) /] [(^) expt] [(-) -])])
       (λ (old) (apply op_ (map (λ (f) (f old)) args_))))]))

(define/match (compile program)
  [('()) (λ (sp st) (values sp st))]
  [((cons instr instr-rst))
   (let ([rest-progn (compile instr-rst)])
     (match instr
       [(add amount)
        (λ (sp st)
          (dbg "[add ~a]\n" amount)
          (vector-set! st sp (+ amount (vector-ref st sp)))
          (dbg "\tsp: ~a\n\tst: ~a\n" sp st)
          (rest-progn sp st))]
       [(set-cell value)
        (λ (sp st)
          (dbg "[set-cell ~a]\n" value)
          (vector-set! st sp value)
          (dbg "\tsp: ~a\n\tst: ~a\n" sp st)
          (rest-progn sp st))]
       [(add-cell-0 dest)
        (λ (sp st)
          (dbg "[add-cell-0 ~a]\n" dest)
          (vector-set! st (+ sp dest) (+ (vector-ref st sp) (vector-ref st (+ sp dest))))
          (vector-set! st sp 0)
          (dbg "\tsp: ~a\n\tst: ~a\n" sp st)
          (rest-progn sp st))]
       [(mult-block-0 body)
        (λ (sp st)
          (dbg "[multi-block-0 ~a]\n" body)
          (let ([cur (vector-ref st sp)])
            (for ([(k v) body])
              (match v
                [(cons 'abs av) (vector-set! st (+ sp k) av)]
                [_ (vector-set! st (+ sp k) (+ (vector-ref st (+ sp k)) (* cur v)))])))
          (dbg "\tsp: ~a\n\tst: ~a\n" sp st)
          (rest-progn sp st))]
       [(poly-block var->idx idx->poly orig)
        (let ([idx->poly-clos
               (for/hash ([(idx poly) idx->poly])
                 (values idx (comp-poly poly var->idx)))])
          (λ (sp st)
            (if (zero? (vector-ref st sp))
                (rest-progn sp st)
                (let-values ([(old-vals) (for/hash ([i (hash-values var->idx)])
                                           (values i (vector-ref st (+ sp i))))])
                  (for ([(idx poly) idx->poly-clos])
                    (vector-set! st (+ sp idx) (poly old-vals)))
                  (rest-progn sp st)))))]
       [(search-0 stride)
        (letrec ([the-loop (λ (sp st)
                             (if (zero? (vector-ref st sp))
                                 (rest-progn sp st)
                                 (the-loop (+ sp stride) st)))])
          the-loop)]
       [(shift amount)
        (λ (sp st)
          (dbg "[shift ~a]\n" amount)
          (dbg "\tsp: ~a\n\tst: ~a\n" (+ sp amount) st)
          (rest-progn (+ sp amount) st))]
       [(bf-write)
        (λ (sp st)
          (display (integer->char (vector-ref st sp)))
          (dbg "\tsp: ~a\n\tst: ~a\n" sp st)
          (rest-progn sp st))]
       [(bf-read)
        (λ (sp st)
          (vector-set! st sp (char->integer (read-char)))
          (dbg "\tsp: ~a\n\tst: ~a\n" sp st)
          (rest-progn sp st))]
       [(loop body)
        (let ([body-progn (compile body)])
          (letrec ([the-loop (λ (sp st)
                               (dbg "[loop(top)]\n")
                               (dbg "\tsp: ~a\n\tst: ~a\n" sp st)
                               (if (zero? (vector-ref st sp))
                                   (rest-progn sp st)
                                   (let-values ([(new-sp new-st) (body-progn sp st)])
                                     (the-loop new-sp new-st))))])
            the-loop))]))])

;; (define p1 (parse-combine "mult.bf"))
;; (define p2 (opt-chain (p1)
;;                       opt/0-scan
;;                       opt/2nd-order-loop
;;                       opt/zero-add->set
;;                       opt/basic-loop
;;                       opt/zero-out
;;                       opt/add
;;                       opt/useless
;;                       combine-instrs))

;; (define l1 (list
;;             (shift 1)
;;             (loop (list (shift 1) (add 1) (shift 1) (add 1) (shift -2) (add -1)))
;;             (shift 1)
;;             (loop (list (shift -1) (add 1) (shift 1) (add -1)))
;;             (shift -2)
;;             (add -1)))

;; (define l2 (opt/basic-loop l1))

;; (define l3 (list
;;             (shift 1)
;;             (loop (list (shift -1) (add -1) (shift 1) (shift 1) (add 1) (shift 1) (add 1) (shift -2) (add -1)))
;;             (shift 1)
;;             (loop (list (shift -1) (add 1) (shift 1) (add -1)))
;;             (shift -2)
;;             (add -1)))

;; (define l4 (opt/basic-loop l3))
