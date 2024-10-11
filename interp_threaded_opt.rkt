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
(struct add-cell-0 (dest) #:transparent) ; zero current cell; add value to another cell

(define (tree-size thingy)
  (match thingy
    [(list is ...) (apply + (map tree-size is))]
    [(loop body) (+ 1 (tree-size body))]
    [(add _) 1]
    [(add-cell-0 _) 1]
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
    [(shift _) 1]
    [(set-cell _) 2]
    [(bf-write) 2]
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
             opt/zero-add->set
             opt/zero-out
             ;; opt/copy
             opt/add
             combine-instrs))

(define (combine-instrs prog [indent 0])
  ;; (printf "~a prog: ~a\n\n\n" (string-append* (for/list ([i (range indent)]) "\t")) prog)
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

(define (opt/copy prog)
  (match prog
    [(list (loop (list (shift s1) (add 1) (shift s2) (add 1) (shift s3) (add -1)))
           (shift s4)
           (loop (list (shift s5) (add 1) (shift s6) (add -1)))
           rst ...)
     (eprintf "copy fired\n")
     prog]
    [(list (loop body) rst ...)
     (cons (loop (opt/copy body)) (opt/copy rst))]
    [(list hd rst ...)
     (cons hd (opt/copy rst))]
    [_ prog]))

(define add-tries 0)
(define (opt/add prog)
  (set! add-tries (+ add-tries 1))
  (match prog
    [(list (loop (list (shift x1) (add a1) (shift x2) (add a2)))
           rst ...)
     #:when (and (zero? (+ x1 x2)) (< a2 0) (zero? (+ a1 a2)))
     (cons (add-cell-0 x1) (opt/add rst))]
    [(list (loop (list (add a1) (shift x1) (add a2) (shift x2)))
           rst ...)
     #:when (and (zero? (+ x1 x2)) (< a1 0) (zero? (+ a1 a2)))
     (cons (add-cell-0 x1) (opt/add rst))]
    [(list (loop body) rst ...)
     (cons (loop (opt/add body)) (opt/add rst))]
    [(list hd rst ...)
     (cons hd (opt/add rst))]
    ['() prog]))
#;(define (opt/subt prog))
#;(define (opt/mult prog))

(define (parse-combine filename)
  (let-values ([(p _) (parse-prog (parse-file filename))])
    (combine-instrs p)))

#;(define p2 (parse-combine "./bench/benches/hanoi.b"))

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
       [(add-cell-0 dest)
        (λ (sp st)
          (vector-set! st (+ sp dest) (+ (vector-ref st sp) (vector-ref st (+ sp dest))))
          (vector-set! st sp 0)
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
          (vector-set! st sp (char->integer (read-char)))
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

