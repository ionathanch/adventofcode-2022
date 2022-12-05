#lang curly-fn racket

(require "../lib.rkt")

(define input
  (for/list ([instr (problem-input 5)])
    (map string->number (regexp-match* #px"\\d+" instr))))

;; crates? = (listof symbol?)
;; supplies? = (listof crates?)
;; stack? = (integer-in 1 9)

;; initial : supplies?
(define initial
  (list '(||) ;; dummy stack since they start at 1
        '(W L S)
        '(Q N T J)
        '(J F H C S)
        '(B G N W M R T)
        '(B Q H D S L R T)
        '(L R H F V B J M)
        '(M J N R W D)
        '(J D N H F T Z B)
        '(T F B N Q L H)))

;; squish : crates? -> string?
(define (squish symbols)
  (~>> symbols
      (map symbol->string)
      (apply string-append)))

;; move : supplies? (crates? -> crates?) -> number? stack? stack? -> supplies?
(define ((move crates f) num from to)
  (define transfer (take (list-ref crates from) num))
  (~> crates
      (list-update from #{drop % num})
      (list-update to #{append (f transfer) %})))

;; moves : (crates? -> crates?) -> string?
(define (moves f)
  (for/fold ([crates initial]
             #:result (squish (map first crates)))
            ([instr input])
    (apply (move crates f) instr)))

(define part1
  (moves reverse))
  
(define part2
  (moves identity))

(show-solution part1 part2)
