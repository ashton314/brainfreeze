#lang racket

(require syntax/parse/define)

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
(struct add-cell-0 (dest) #:transparent) ; zero current cell; add value to another cell
(struct mult-block-0 (body) #:transparent) ; cells in body + current * cell count; current zeroed
(struct search-0 (stride) #:transparent)

(define (tree-size thingy)
  (match thingy
    [(list is ...) (apply + (map tree-size is))]
    [(loop body) (+ 1 (tree-size body))]
    [(add _) 1]
    [(add-cell-0 _) 1]
    [(mult-block-0 body) (length (hash-keys body))]
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

(define (optimize prog)
  (opt-chain (prog)
             opt/0-scan
             opt/zero-add->set
             opt/basic-loop
             opt/zero-out
             opt/add
             opt/useless
             combine-instrs))

(define (optimize-no-loop prog)
  (opt-chain (prog)
             ;; opt/0-scan
             opt/zero-add->set
             opt/basic-loop
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

#;
(define (opt/mult prog))

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
