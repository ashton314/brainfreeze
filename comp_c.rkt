#lang racket

(require "interp_threaded_opt.rkt")

(define (compile-to-c program)
  (displayln "#include <stdio.h>")
  (displayln "static int tape[10000] = {0};")
  (displayln "int ptr = 500;")
  (displayln "int main() {")
  (emit-c program)
  (displayln "}"))

(define/match (emit-c program)
  [('()) (displayln "")]
  [((cons instr instr-rst))
   (match instr
     [(add amount)
      (printf "  tape[ptr] += ~a;\n" amount)
      (emit-c instr-rst)]
     [(set-cell value)
      (printf "  tape[ptr] = ~a;\n" value)
      (emit-c instr-rst)]
     [(shift amount)
      (printf "  ptr += ~a;\n" amount)
      (emit-c instr-rst)]
     [(bf-write)
      (printf "   printf(\"%c\", tape[ptr]);\n")
      (emit-c instr-rst)]
     [(loop body)
      (printf "  while (tape[ptr] != 0) {\n")
      (emit-c body)
      (printf "  }\n")
      (emit-c instr-rst)])])

(define (compile-file filename [start 5000] [size 10000])
  (displayln "Parsing..." (current-error-port))
  (let-values ([(program _) (parse-prog (parse-file filename))])
    (eprintf "Parsed. Program is ~a instructions. Optimizing...\n" (tree-size program))
    (let ([optimized (optimize program)])
      (eprintf "Optimized. New program is ~a instructions.\n" (tree-size optimized))
      (compile-to-c optimized))))

(let* ([the-file (command-line #:program "interp_threaded_opt" #:args (filename) filename)])
  (compile-file the-file))
