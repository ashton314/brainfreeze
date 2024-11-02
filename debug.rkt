#lang racket

(require "interp_threaded_opt.rkt")

(define (run-file filename [start 0] [size 10])
  (displayln "Parsing...")
  (let-values ([(program _) (parse-prog (parse-file filename))])
    (printf "Parsed. Program is ~a instructions. Optimizing...\n" (tree-size program))
    (let ([optimized (optimize program)]
          [unopt program])
      (printf "Optimized. New program is ~a instructions.\n" (tree-size optimized))
      (let ([compiled (compile optimized)])
        (let-values ([(sp_ st_) ((compile unopt) start (make-vector size))]
                     [(sp st) (compiled start (make-vector size))])
          (printf "Finished.\n\nUnopt:\nPointer: ~a\nState:\n~a\n\nOpt:\nPointer: ~a\nState:\n~a\n" (- sp_ 0) st_ (- sp 0) st))))))

(let* ([the-file (command-line #:program "runner.rkt" #:args (filename) filename)])
  (run-file the-file))

