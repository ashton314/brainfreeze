#lang racket

(define (parse-file filename)
  (list->vector
   (filter (λ (c) (member c '(#\> #\< #\+ #\- #\. #\, #\[ #\])))
           (string->list (file->string filename)))))

(struct jmp (amount) #:transparent)
(struct jmp-forward jmp () #:transparent)
(struct jmp-backward jmp () #:transparent)
(struct add (amount) #:transparent)
(struct shift (amount) #:transparent)

(define (preprocess-loops! prog)
  (for ([i (in-range (vector-length prog))])
    (match (vector-ref prog i)
      [#\[ (vector-set! prog i (find-matching prog i 1 'close))]
      [#\] (vector-set! prog i (find-matching prog i -1 'open))]
      [_ 42])))

(define (combine-instrs prog)
  (let ([new-prog '()])
    (for ([i prog])
      (cond
        [(null? new-prog)
         (set! new-prog (cons i '()))]
        [(and (add? (car new-prog)) (eqv? i #\+))
         (set! new-prog (cons (add (+ (add-amount (car new-prog)) 1)) (cdr new-prog)))]
        [(and (add? (car new-prog)) (eqv? i #\-))
         (set! new-prog (cons (add (- (add-amount (car new-prog)) 1)) (cdr new-prog)))]
        [(and (shift? (car new-prog)) (eqv? i #\>))
         (set! new-prog (cons (shift (+ (shift-amount (car new-prog)) 1)) (cdr new-prog)))]
        [(and (shift? (car new-prog)) (eqv? i #\<))
         (set! new-prog (cons (shift (- (shift-amount (car new-prog)) 1)) (cdr new-prog)))]
        [(eqv? i #\+)
         (set! new-prog (cons (add 1) new-prog))]
        [(eqv? i #\-)
         (set! new-prog (cons (add -1) new-prog))]
        [(eqv? i #\>)
         (set! new-prog (cons (shift 1) new-prog))]
        [(eqv? i #\<)
         (set! new-prog (cons (shift -1) new-prog))]
        [else (set! new-prog (cons i new-prog))]))
    (list->vector (reverse new-prog))))

(define (find-matching prog start offset kind [stack 0])
  (define (close? x) (or (jmp-backward? x) (eqv? x #\])))
  (define (open? x) (or (jmp-forward? x) (eqv? x #\[)))
  (define addr (+ start offset))
  (let-values ([(needle-pred other-pred bump jmp-maker)
                (if (eq? kind 'close)
                    (values close? open? 1 jmp-forward)
                    (values open? close? -1 jmp-backward))])
    (if (needle-pred (vector-ref prog addr))
        (if (zero? stack)
            (jmp-maker offset)
            (find-matching prog start (+ bump offset) kind (- stack 1)))
        (if (other-pred (vector-ref prog addr))
            (find-matching prog start (+ bump offset) kind (+ stack 1))
            (find-matching prog start (+ bump offset) kind stack)))))

(define (compile program c-ip jmp-targets inst-cache)
  (hash-ref
   inst-cache c-ip
   (λ ()
     (let ([compiled
            (if (< c-ip (vector-length program))
                (match (vector-ref program c-ip)
                  [(add amount) (let ([rest-progn (compile program (+ 1 c-ip) jmp-targets inst-cache)])
                                  (λ (state sp)
                                    (println "add")
                                    (vector-set! state sp (+ (vector-ref state sp) amount))
                                    (rest-progn state sp)))]
                  [(shift amount) (let ([rest-progn (compile program (+ 1 c-ip) jmp-targets inst-cache)])
                                    (λ (state sp)
                                      (println "shift")
                                      (rest-progn state (+ sp amount))))]
                  [#\+ (let ([rest-progn (compile program (+ 1 c-ip) jmp-targets inst-cache)])
                         (λ (state sp)
                           (println "+")
                           (vector-set! state sp (+ (vector-ref state sp) 1))
                           (rest-progn state sp)))]
                  [#\- (let ([rest-progn (compile program (+ 1 c-ip) jmp-targets inst-cache)])
                         (λ (state sp)
                           (println "-")
                           (vector-set! state sp (- (vector-ref state sp) 1))
                           (rest-progn state sp)))]
                  [#\> (let ([rest-progn (compile program (+ 1 c-ip) jmp-targets inst-cache)])
                         (λ (state sp)
                           (println ">")
                           (rest-progn state (+ sp 1))))]
                  [#\< (let ([rest-progn (compile program (+ 1 c-ip) jmp-targets inst-cache)])
                         (λ (state sp)
                           (println "<")
                           (rest-progn state (- sp 1))))]
                  [#\. (let ([rest-progn (compile program (+ 1 c-ip) jmp-targets inst-cache)])
                         (λ (state sp)
                           (println ".")
                           (display (integer->char (vector-ref state sp)))
                           (rest-progn state sp)))]
                  [(jmp-forward target)
                   (letrec ([loop-start (λ (state sp)
                                          (println "[")
                                          (if (zero? (vector-ref state sp))
                                              (loop-end state sp)
                                              (loop-body state sp)))]
                            [loop-past-end (compile program (+ c-ip target 1) jmp-targets inst-cache)]
                            [loop-end (compile program (+ c-ip target) (cons loop-start loop-past-end) inst-cache)]
                            [loop-body (compile program (+ 1 c-ip) null inst-cache)])
                     loop-start)]
                  [(jmp-backward _)
                   (λ (state sp)
                     (println "]")
                     (if (zero? (vector-ref state sp))
                         ((cdr jmp-targets) state sp)
                         ((car jmp-targets) state sp)))])
                (λ (_state _sp)           ; finished compiling program
                  void))])
       (hash-set! inst-cache c-ip compiled)
       compiled))))

(define (run-file filename)
  (let ([program (parse-file filename)])
    (preprocess-loops! program)
    ((compile (combine-instrs program) 0 null (make-hash)) (make-vector 10000) 5000)))

#;
(let* ([the-file (command-line #:program "interp_threaded_opt" #:args (filename) filename)]
       [program (parse-file the-file)])
  (preprocess-loops! program)
  ((compile (combine-instrs program) 0 null (make-hash)) (make-vector 10000) 5000))
