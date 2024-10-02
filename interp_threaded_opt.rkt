#lang racket

(require syntax/parse/define)

(provide (all-defined-out))

(define (parse-file filename)
  (filter (λ (c) (member c '(#\> #\< #\+ #\- #\. #\, #\[ #\])))
          (string->list (file->string filename))))

(struct loop (body) #:transparent)
(struct add (amount) #:transparent)
(struct shift (amount) #:transparent)
(struct set-cell (value) #:transparent)
(struct bf-write () #:transparent)
(struct bf-read () #:transparent)

(define (tree-size thingy)
  (match thingy
    [(list is ...) (apply + (map tree-size is))]
    [(loop body) (+ 1 (tree-size body))]
    [(add _) 1]
    [(shift _) 1]
    [(set-cell _) 1]
    [(bf-write) 1]
    [(bf-read) 1]))

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
           (eprintf "~a opts\n" output-size)
           output))]))

(define (optimize prog)
  (opt-pass opt/zero-add->set
            (opt-pass opt/zero-out
                      (opt-pass combine-instrs prog))))

(define (combine-instrs prog)
  (cond
    [(list? prog)
     (let ([new-prog (list (combine-instrs (car prog)))])
       (for ([i (cdr prog)])
         (match (cons i (car new-prog))
           [(cons (add a1) (add a2))
            (set! new-prog (cons (add (+ a1 a2)) (cdr new-prog)))]
           [(cons (shift s1) (shift s2))
            (set! new-prog (cons (shift (+ s1 s2)) (cdr new-prog)))]
           [(cons (loop body) _)
            (set! new-prog (cons (loop (combine-instrs body)) new-prog))]
           [_
            (set! new-prog (cons i new-prog))]))
       (reverse new-prog))]
    [(loop? prog) (loop (combine-instrs (loop-body prog)))]
    [else prog]))

(define (opt/zero-out prog)
  (match prog
    [(loop (list (add -1))) (set-cell 0)]
    [(loop body) (loop (map opt/zero-out body))]
    [(list is ...) (map opt/zero-out is)]
    [_ prog]))

(define (opt/zero-add->set prog)
  (match prog
    [(loop body) (loop (opt/zero-add->set body))]
    [(list (set-cell n) (add m) rst ...)
     (cons (set-cell (+ n m)) (opt/zero-add->set rst))]
    [(list hd rst ...)
     (cons hd (opt/zero-add->set rst))]
    ['() prog]))

(define/match (compile program)
  [('()) (λ (sp st) (values sp st))]
  [((cons instr instr-rst))
   (let ([rest-progn (compile instr-rst)])
     (match instr
       [(add amount)
        (λ (sp st)
          (vector-set! st sp (+ amount (vector-ref st sp)))
          (rest-progn sp st))]
       [(set-cell value)
        (λ (sp st)
          (vector-set! st sp value)
          (rest-progn sp st))]
       [(shift amount)
        (λ (sp st)
          (rest-progn (+ sp amount) st))]
       [(bf-write)
        (λ (sp st)
          (display (integer->char (vector-ref st sp)))
          (rest-progn sp st))]
       [(bf-read)
        (λ (sp st)
          (error "unimplemented")
          (rest-progn sp st))]
       [(loop body)
        (let ([body-progn (compile body)])
          (letrec ([the-loop (λ (sp st)
                               ;; Help! Can I make this more efficient?
                               (if (zero? (vector-ref st sp))
                                   (rest-progn sp st)
                                   (let-values ([(new-sp new-st) (body-progn sp st)])
                                     (the-loop new-sp new-st))))])
            the-loop))]))])


;; (define p1 (parse-file "bench/benches/hello.b"))
;; (define-values (pp1 _blah) (parse-prog p1))
;; (define o1 (optimize pp1))

;; (define p2 (parse-file "bench/benches/mandel.b"))
;; (define-values (pp2 _blah2) (parse-prog p2))
;; (define o2 (optimize pp2))

