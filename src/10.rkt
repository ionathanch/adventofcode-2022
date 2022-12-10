#lang curly-fn racket

(require "../lib.rkt")

(define input
  (map (∘ (match-lambda
            ['("noop") 'noop]
            [`("addx" ,n) (list 'addx (string->number n))])
          #{string-split %1 " "})
       (problem-input 10)))

(define (part1)
  (define strengths 0)
  (define (update! cycle X)
    (when (zero? (% (- cycle 20) 40))
      (set! strengths (+ strengths (* cycle X)))))
  (for/fold ([X 1] [cycle 1]
             #:result strengths)
            ([instr input])
    (match instr
      ['noop
       (update! cycle X)
       (values X (add1 cycle))]
      [`(addx ,n)
       (update! cycle X)
       (update! (add1 cycle) X)
       (values (+ X n) (+ cycle 2))])))

(define (part2)
  (define CRT (make-vector 240))
  (define (update! pos X)
    (if (<= (sub1 X) (% pos 40) (add1 X))
        (vector-set! CRT pos #\█)
        (vector-set! CRT pos #\ )))
  (for/fold ([X 1] [pos 0])
            ([instr input])
    (match instr
      ['noop
       (update! pos X)
       (values X (add1 pos))]
      [`(addx ,n)
       (update! pos X)
       (update! (add1 pos) X)
       (values (+ X n) (+ pos 2))]))
  (displayln "See below.")
  (for ([s (in-slice 40 CRT)])
    (displayln (list->string s))))

(show-solution* part1 part2)