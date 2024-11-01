#lang racket

(provide interp-run)

(define (parse-program str)
  (list->vector
   (filter (λ (c) (member c '(#\> #\< #\+ #\- #\. #\, #\[ #\])))
           (string->list str))))

(define (parse-file filename)
  (parse-program (file->string filename)))

(struct jmp (amount) #:transparent)
(struct jmp-forward jmp () #:transparent)
(struct jmp-backward jmp () #:transparent)

(define (preprocess-loops! prog)
  (for ([i (in-range (vector-length prog))])
    (match (vector-ref prog i)
      [#\[ (vector-set! prog i (find-matching prog i 1 'close))]
      [#\] (vector-set! prog i (find-matching prog i -1 'open))]
      [_ 42])))

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
                  [#\+ (let ([rest-progn (compile program (+ 1 c-ip) jmp-targets inst-cache)])
                         (λ (state sp)
                           (vector-set! state sp (+ (vector-ref state sp) 1))
                           (rest-progn state sp)))]
                  [#\- (let ([rest-progn (compile program (+ 1 c-ip) jmp-targets inst-cache)])
                         (λ (state sp)
                           (vector-set! state sp (- (vector-ref state sp) 1))
                           (rest-progn state sp)))]
                  [#\> (let ([rest-progn (compile program (+ 1 c-ip) jmp-targets inst-cache)])
                         (λ (state sp)
                           (rest-progn state (+ sp 1))))]
                  [#\< (let ([rest-progn (compile program (+ 1 c-ip) jmp-targets inst-cache)])
                         (λ (state sp)
                           (rest-progn state (- sp 1))))]
                  [#\. (let ([rest-progn (compile program (+ 1 c-ip) jmp-targets inst-cache)])
                         (λ (state sp)
                           (display (integer->char (vector-ref state sp)))
                           (rest-progn state sp)))]
                  [#\,
                   (let ([rest-progn (compile program (+ 1 c-ip) jmp-targets inst-cache)])
                     (λ (state sp)
                       (vector-set! state sp (char->integer (read-char)))
                       (rest-progn state sp)))]
                  [(jmp-forward target)
                   (letrec ([loop-start (λ (state sp)
                                          (if (zero? (vector-ref state sp))
                                              (loop-end state sp)
                                              (loop-body state sp)))]
                            [loop-past-end (compile program (+ c-ip target 1) jmp-targets inst-cache)]
                            [loop-end (compile program (+ c-ip target) (cons loop-start loop-past-end) inst-cache)]
                            [loop-body (compile program (+ 1 c-ip) null inst-cache)])
                     loop-start)]
                  [(jmp-backward _)
                   (λ (state sp)
                     (if (zero? (vector-ref state sp))
                         ((cdr jmp-targets) state sp)
                         ((car jmp-targets) state sp)))])
                (λ (_state _sp)           ; finished compiling program
                  void))])
       (hash-set! inst-cache c-ip compiled)
       compiled))))

(define (interp-run prog)
  (let ([p (parse-program prog)])
    (preprocess-loops! p)
    ((compile p 0 null (make-hash)) (make-vector 1000000) 500000)))

(let* ([the-file (command-line #:program "interp_threaded" #:args (filename) filename)]
       [program (parse-file the-file)])
  (preprocess-loops! program)
  ((compile program 0 null (make-hash)) (make-vector 10000) 5000))
