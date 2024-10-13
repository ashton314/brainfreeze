#lang racket

(require "interp_threaded_opt.rkt")
(require rackunit)

(define (diff-baseline prog)
  (parameterize ([current-error-port (open-output-nowhere)]
                 [current-output-port (open-output-nowhere)])
    (let-values ([(program _) (parse-prog (parse-prog-str prog))])
      (let ([opt-program (optimize program)])
        (let-values ([(sp1 st1) ((compile program) 5000 (make-vector 10000))]
                     [(sp2 st2) ((compile opt-program) 5000 (make-vector 10000))])
          (and (= sp1 sp2) (equal? st1 st2)))))))

#;
(define (random-program gas)
  (if (>= 0 gas)
      '()
      (let ([next-instr
             (match (random 0 8)
               [0 #\+] [1 #\-] [2 #\>] [3 #\<])])
        (cons next-instr (random-program (- gas 1))))))

;; (check-true (diff-baseline "+++"))
(check-true (diff-baseline "+++[->++>+<<]"))
