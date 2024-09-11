#lang racket

(define (parse-file filename)
  (list->vector
   (filter (λ (c) (member c '(#\> #\< #\+ #\- #\. #\, #\[ #\])))
           (string->list (file->string filename)))))

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

(define (interp program state [ip 0] [sp 0])
  ;; (printf "p: ~a st: ~a\n" program state)
  (match (vector-ref program ip)
    [#\+ (begin
           (vector-set! state sp (+ (vector-ref state sp) 1))
           (interp program state (+ 1 ip) sp))]
    [#\- (begin (vector-set! state sp (- (vector-ref state sp) 1)) (interp program state (+ 1 ip) sp))]
    [#\> (interp program state (+ 1 ip) (+ sp 1))]
    [#\< (interp program state (+ 1 ip) (- sp 1))]
    [#\. (begin
           (display (integer->char (vector-ref state sp)))
           (interp program state (+ 1 ip) sp))]
    [(jmp-forward amount) (if (zero? (vector-ref state sp))
                              (interp program state (+ amount ip) sp)
                              (interp program state (+ 1 ip) sp))]

    [(jmp-backward amount) (if (zero? (vector-ref state sp))
                               (interp program state (+ 1 ip) sp)
                               (interp program state (+ amount ip) sp))]))

;; (define the-prog (parse-file "hello.bf"))
;; (preprocess-loops! the-prog)

