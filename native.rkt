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
  (emit-asm program)
  (displayln postlude))

;; Register reservations:
;; x0 - reserved for function calls
;; x1 - reserved for function calls
;; x20 - tape start
;; x21 - data pointer
;; x22 - misc
;; x23 - misc
;; x24 - misc
;; x25 - misc
;; x26 - mult linker
;; x11 - misc

(define fresh-label
  (let ([counter 0])                    ; let-over-lambda!
    (λ ()
      (begin0
          (format "LBB0_~a" counter)
        (set! counter (+ 1 counter))))))

(define (i/load src d1 d2 [comment ""])
  (format "\tldrsb\t~a, [~a, ~a]~a\n" src d1 d2 comment))
(define (i/load1 src d1 [comment ""])
  (format "\tldrsb\t~a, [~a]~a\n" src d1 comment))
(define (i/load-ptr src [comment ""]) (i/load src 'x20 'x21 comment))

(define (i/store src d1 d2 [comment ""])
  (format "\tstrb\t~a, [~a, ~a]~a\n" src d1 d2 comment))
(define (i/store1 src d1 [comment ""])
  (format "\tstrb\t~a, [~a]~a\n" src d1 comment))
(define (i/store-ptr src [comment ""]) (i/store src 'x20 'x21 comment))

(define (i/mov dest src [comment ""])
  (format "\tmov\t~a, ~a~a\n" dest src comment))
(define (i/movi dest val [comment ""])
  (format "\tmov\t~a, #~a~a\n" dest val comment))

(define (i/add dest arg1 arg2 [comment ""])
  (format "\tadd\t~a, ~a, ~a~a\n" dest arg1 arg2 comment))
(define (i/subs dest arg1 arg2 [comment ""])
  (format "\tsubs\t~a, ~a, ~a~a\n" dest arg1 arg2 comment))
(define (i/mul dest arg1 arg2 [comment ""])
  (format "\tmul\t~a, ~a, ~a~a\n" dest arg1 arg2 comment))

(define (i/subcall l [comment ""])
  (format "\tbl ~a~a\n" l comment))
(define (i/label name [comment ""])
  (format "~a:~a\n" name comment))
(define (i/branch label [comment ""])
  (format "\tb\t~a~a\n" label comment))
(define (i/branch-eq label [comment ""])
  (format "\tbeq\t~a~a\n" label comment))

(define (mult-immediate dest arg1 arg2)
  (cond
    [(zero? arg2) (i/mov dest 'wzr)]
    [(and (equal? dest arg1) (= 1 arg2)) ""]
    [(= 1 arg2) (i/mov dest arg1)]
    [else (string-append (i/mov 'x26 (i/i arg2)) (i/mul dest arg1 'x26))]))

(define (i/i thing)
  (format "#~a" thing))

(define/match (emit-asm program)
  [('()) (displayln "")]
  [((cons instr instr-rst))
   (match instr
     [(add amount)
      (display (i/load-ptr 'w11 (format "\t;add ~a" amount)))
      (display (i/add 'w11 'w11 (i/i amount)))
      (display (i/store-ptr 'w11))
      (emit-asm instr-rst)]
     [(add-cell-0 dest)
      (display (i/load-ptr 'w11 "\t;add-cell-0")) ; copy tape[ptr] to w11
      (display (i/store-ptr 'wzr))                ; zero out current cell
      (display (i/add 'x22 'x21 (i/i dest)))      ; compute dest
      (display (i/load 'w23 'x20 'x22))           ; get value at dest
      (display (i/add 'w11 'w11 'w23))            ; add old pointer value
      (display (i/store 'w11 'x20 'x22))          ; write sum to dest
      (emit-asm instr-rst)]
     [(mult-block-0 body)
      (display (i/add 'x22 'x20 'x21              ; put base offset in x22
                      "\t;mult-block-0"))
      (display (i/load1 'x23 'x22))               ; current value in x23
      (for ([idx (sort (hash-keys body) <)]
            #:unless (or (= idx 0) (zero? (hash-ref body idx))))
        (display (i/add 'x24 'x22 (i/i idx)))     ; compute offset address → x24
        (display (i/load1 'x25 'x24))             ; put that value in x25
        (display                                  ; x11 ← x23 * analyzed
         (mult-immediate 'x11 'x23
                         (abs (hash-ref body idx))))
        (cond
          [(negative? (hash-ref body idx))
           (display (i/subs 'w11 'w25 'w11))]     ; x11 = x25 - x11 (value of this cell)
          [else
           (display (i/add 'w11 'w25 'w11))])     ; x11 = x25 + x11 (value of this cell)
        (display (i/store1 'w11 'x24)))           ; write that back
      (display (i/store-ptr 'wzr))                ; zero out current cell
      (emit-asm instr-rst)]
     [(set-cell value)
      (display (i/movi 'w11 value (format "\t;set ~a" value)))
      (display (i/store-ptr 'w11))
      (emit-asm instr-rst)]
     [(shift amount)
      (display (i/add 'x21 'x21 (i/i amount) (format "\t;shift ~a" amount)))
      (emit-asm instr-rst)]
     [(bf-write)
      (display (i/load-ptr 'w0 "\t;write"))
      (display (i/subcall '_putchar))
      (emit-asm instr-rst)]
     [(bf-read)
      (display (i/subcall '_getchar "\t;read"))
      (display (i/store-ptr 'w0))
      (emit-asm instr-rst)]
     [(loop body)
      (let ([start-label (fresh-label)]
            [end-label (fresh-label)])
        (display (i/label start-label "\t;start loop"))
        (display (i/load-ptr 'w11))
        (display (i/subs 'w11 'w11 (i/i 0)))
        (display (i/branch-eq end-label))
        (emit-asm body)
        (display (i/branch start-label))
        (display (i/label end-label)))
      (emit-asm instr-rst)])])

(define (compile-file filename)
  (displayln "Parsing..." (current-error-port))
  (let-values ([(program _) (parse-prog (parse-file filename))])
    (eprintf "Parsed. Program is ~a instructions. Optimizing...\n" (tree-size program))
    (let ([optimized (optimize program)])
      (eprintf "Optimized. New program is ~a instructions.\n" (tree-size optimized))
      (compile-to-asm optimized))))

(let* ([the-file (command-line #:program "native.rkt" #:args (filename) filename)])
  (compile-file the-file))

#;
(define (compile-file-noop filename)
  (displayln "Parsing..." (current-error-port))
  (let-values ([(program _) (parse-prog (parse-file filename))])
    (eprintf "Parsed. Program is ~a instructions. Skipping optimizations.\n" (tree-size program))
    (compile-to-asm program)))

#;
(let* ([the-file (command-line #:program "native.rkt" #:args (filename) filename)])
  (compile-file-noop the-file))
