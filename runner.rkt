#lang racket

(require "interp_threaded_opt.rkt")

(define (run-file filename [start 5000] [size 10000])
  (displayln "Parsing...")
  (let-values ([(program _) (parse-prog (parse-file filename))])
    (printf "Parsed. Program is ~a instructions. Optimizing...\n" (tree-size program))
    (let ([optimized (optimize program)])
      (printf "Optimized. New program is ~a instructions.\n" (tree-size optimized))
      (let ([compiled (compile optimized)])
        (let-values ([(sp _st) (compiled start (make-vector size))])
          (printf "Finished. Pointer: ~a\n" (- sp 5000)))))))

(let* ([the-file (command-line #:program "runner.rkt" #:args (filename) filename)])
  (run-file the-file))
