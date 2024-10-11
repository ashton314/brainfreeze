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
	adrp   x22, _tape@PAGE
	add	x20, x22, _tape@PAGEOFF
	adrp	x22, _ptr@PAGE
	ldrsw	x21, [x22, _ptr@PAGEOFF]
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
	.long	5000

.zerofill __DATA,__bss,_tape,100000,0    ; @tape
	.section	__TEXT,__cstring,cstring_literals

.subsections_via_symbols
")

(define (compile-to-asm program)
  (displayln prelude)
  (emit-c program)
  (displayln postlude))

;; Register reservations:
;; x0 - reserved for function calls
;; x1 - reserved for function calls
;; x20 - tape start
;; x21 - data pointer
;; x22 - misc
;; x11 - misc

(define fresh-label
  (let ([counter 0])                    ; let-over-lambda!
    (λ ()
      (begin0
          (format "LBB0_~a" counter)
        (set! counter (+ 1 counter))))))

(define/match (emit-c program)
  [('()) (displayln "")]
  [((cons instr instr-rst))
   (match instr
     [(add amount)
      ;; (printf "  tape[ptr] += ~a;\n" amount)
      (printf "\tldrsb\tw11, [x20, x21]\t;add ~a\n" amount) ; copy tape[ptr] to w11
      (printf "\tadd\tw11, w11, #~a\n" amount) ; increment w11
      (printf "\tstrb\tw11, [x20, x21]\n")       ; writeback w11 to tape[ptr]
      (emit-c instr-rst)]
     [(add-cell-0 dest)
      (printf "\tldrsb\tw11, [x20, x21]\t;add-cell-0\n") ; copy tape[ptr] to w11
      (printf "\tmov\tw22, #0\n")
      (printf "\tstrb\tw22, [x20, x21]\n") ; zero out current cell
      (printf "\tadd\tx22, x21, #~a\n" dest) ; compute dest
      (printf "\tldrsb\tw23, [x20, x22]\n")
      (printf "\tadd\tw11, w11, w23\n")
      (printf "\tstrb\tw11, [x20, x22]\n") ; write sum to destination
      (emit-c instr-rst)]
     [(set-cell value)
      ;; (printf "  tape[ptr] = ~a;\n" value)
      (printf "\tmov\tw11, #~a\t;set ~a\n" value value)
      (printf "\tstrb\tw11, [x20, x21]\n")
      (emit-c instr-rst)]
     [(shift amount)
      ;; (printf "  ptr += ~a;\n" amount)
      (printf "\tadd\tx21, x21, #~a\t;shift ~a\n" amount amount)
      (emit-c instr-rst)]
     [(bf-write)
      (printf "\tldrb\tw0, [x20, x21]\t;write\n")
      (printf "\tbl _putchar\n")
      (emit-c instr-rst)]
     [(bf-read)
      (printf "\tbl _getchar\t;read\n")
      (printf"\tstrb\tw0, [x20, x21]\n")
      (emit-c instr-rst)]
     [(loop body)
      (let ([start-label (fresh-label)]
            ;; [body-label (fresh-label)]
            [end-label (fresh-label)])
        (printf "~a:\t;start loop\n" start-label)
        (printf "\tldrsb\tw11, [x20, x21]\n")      ; copy tape[ptr] to w11
        (printf "\tsubs\tw11, w11, #0\n")        ; w11 - 0; set the bit
        (printf "\tbeq\t~a\n" end-label)         ; exit loop
        (emit-c body)
        (printf "\tb\t~a\n" start-label)
        (printf "~a:\n" end-label))
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
