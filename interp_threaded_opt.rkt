#lang racket

(define (parse-file filename)
  (filter (λ (c) (member c '(#\> #\< #\+ #\- #\. #\, #\[ #\])))
          (string->list (file->string filename))))

(struct loop (body) #:transparent)
(struct add (amount) #:transparent)
(struct shift (amount) #:transparent)
(struct bf-write () #:transparent)
(struct bf-read () #:transparent)

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

(define (combine-instrs prog)
  (let ([new-prog (list (car prog))])
    (for ([i (cdr prog)])
      (match (cons i (car new-prog))
        [(cons (add a1) (add a2))
         (set! new-prog (cons (add (+ a1 a2)) (cdr new-prog)))]
        [(cons (shift s1) (shift s2))
         (set! new-prog (cons (shift (+ s1 s2)) (cdr new-prog)))]
        [(cons (loop body) _)
         (set! new-prog (cons (loop (combine-instrs body)) new-prog))]
        [_ (set! new-prog (cons i new-prog))]))
    (reverse new-prog)))

(define/match (compile program)
  [('()) (λ (sp st) 'finished)]
  [((cons instr instr-rst))
   (let ([rest-progn (compile instr-rst)])
     (match instr
       [(add amount)
        (λ (sp st)
          (printf "+~a" amount)
          (vector-set! st sp (+ amount (vector-ref st sp)))
          (rest-progn sp st))]
       [(shift amount)
        (λ (sp st)
          (printf ">~a" amount)
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
                               (body-progn sp st)
                               (if (zero? (vector-ref st sp))
                                   (rest-progn sp st)
                                   (the-loop sp st)))])
            the-loop))]))])

(define (run-file filename)
  (displayln "Parsing...")
  (let-values ([(program _) (parse-prog (parse-file filename))])
    (displayln "Parsed. Executing...")
    ((compile (combine-instrs program)) 5000 (make-vector 10000))))

#;
(let* ([the-file (command-line #:program "interp_threaded_opt" #:args (filename) filename)])
  (run-file the-file))
