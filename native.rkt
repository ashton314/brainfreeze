#lang racket

(require "interp_threaded_opt.rkt")

(define prelude
  "	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 14, 0	sdk_version 15, 0
	.globl	_main                           ; -- Begin function main
	.p2align	2
_main:                                  ; @main
	.cfi_startproc
	sub	sp, sp, #32
	stp	x29, x30, [sp, #16]             ; 16-byte Folded Spill
	add	x29, sp, #16
	adrp   x10, _tape@PAGE
	add	x8, x10, _tape@PAGEOFF
	adrp	x10, _ptr@PAGE
	ldrsw	x9, [x10, _ptr@PAGEOFF]
")

(define postlude
  "	mov	w0, #0                          ; =0x0
	ldp	x29, x30, [sp, #16]             ; 16-byte Folded Reload
	add	sp, sp, #32
	ret
	.cfi_endproc
                                        ; -- End function
	.section	__DATA,__data
	.globl	_ptr                            ; @ptr
	.p2align	2, 0x0
_ptr:
	.long	500                             ; 0x1f4

.zerofill __DATA,__bss,_tape,10000,0    ; @tape
	.section	__TEXT,__cstring,cstring_literals
l_.str:                                 ; @.str
	.asciz	\"%c\"

.subsections_via_symbols
")

(define (compile-to-asm program)
  (displayln prelude)
  (emit-c program)
  (displayln postlude))

;; Register reservations:
;; x8 - tape start
;; x9 - data pointer
;; x10 - addr tape[ptr]
;; x11 - misc

(define/match (emit-c program)
  [('()) (displayln "")]
  [((cons instr instr-rst))
   (match instr
     [(add amount)
      ;; (printf "  tape[ptr] += ~a;\n" amount)
      (printf "\tldrsb\tw11, [x8, x9]\n")      ; copy tape[ptr] to w11
      (printf "\tadd\tw11, w11, #~a\n" amount) ; increment w11
      (printf "\tstrb\tw11, [x8, x9]\n")       ; writeback w11 to tape[ptr]
      (emit-c instr-rst)]
     [(set-cell value)
      ;; (printf "  tape[ptr] = ~a;\n" value)
      (printf "\tmov\tw11, #~a\n" value)
      (printf "\tstrb\tw11, [x8, x9]\n")
      (emit-c instr-rst)]
     [(shift amount)
      ;; (printf "  ptr += ~a;\n" amount)
      (printf "\tadd\tx9, x9, #~a\n" amount)
      (emit-c instr-rst)]
     [(bf-write)
      ;; (printf "   printf(\"%c\", tape[ptr]);\n")
      (printf "\tldrsb\tw11, [x8, x9]\n")      ; copy tape[ptr] to w11
      (printf "\tstrb\tw11, [sp]\n")            ; push w11 onto stack
      (printf "\tadrp\tx0, l_.str@PAGE\n")
      (printf "\tadd\tx0, x0, l_.str@PAGEOFF\n")
      (printf "bl\t_printf\n")
      (emit-c instr-rst)]
     [(loop body)
      ;; (printf "  while (tape[ptr] != 0) {\n")
      (emit-c body)
      ;; (printf "  }\n")
      (emit-c instr-rst)])])

(define (compile-file filename)
  (displayln "Parsing..." (current-error-port))
  (let-values ([(program _) (parse-prog (parse-file filename))])
    (eprintf "Parsed. Program is ~a instructions. Optimizing...\n" (tree-size program))
    (let ([optimized (optimize program)])
      (eprintf "Optimized. New program is ~a instructions.\n" (tree-size optimized))
      (compile-to-asm optimized))))

(let* ([the-file (command-line #:program "native.rkt" #:args (filename) filename)])
  (compile-file the-file))
