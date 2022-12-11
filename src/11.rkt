#lang curly-fn racket

(require "../lib.rkt")

(struct monkey ([items #:mutable] op modulus t f [inspections #:mutable]) #:transparent)

(define-syntax monkeys
  (λ (stx)
    (syntax-case stx ()
      [monkeys (syntax (vector (monkey '(91 66) #{* % 13} 19 6 2 0)
                               (monkey '(78 97 59) #{+ % 7} 5 0 3 0)
                               (monkey '(57 59 97 84 72 83 56 76) #{+ % 6} 11 5 7 0)
                               (monkey '(81 78 70 58 84) #{+ % 5} 17 6 0 0)
                               (monkey '(60) #{+ % 8} 7 1 3 0)
                               (monkey '(57 69 63 75 62 77 72) #{* % 5} 13 7 4 0)
                               (monkey '(73 66 86 79 98 87) #{* % %} 3 5 2 0)
                               (monkey '(95 89 63 67) #{+ % 2} 2 1 4 0)))])))
(define monkeys1 monkeys)
(define monkeys2 monkeys)

(define modulus (for/product ([monkey monkeys]) (monkey-modulus monkey)))

(define (give! monkeys i item)
  (define monkey (vector-ref monkeys i))
  (set-monkey-items! monkey (snoc (monkey-items monkey) item)))

(define (round! monkeys f)
  (for* ([monkey monkeys]
         [item (monkey-items monkey)])
    (define item* (f ((monkey-op monkey) item)))
    (if (divisible? item* (monkey-modulus monkey))
        (give! monkeys (monkey-t monkey) item*)
        (give! monkeys (monkey-f monkey) item*))
    (set-monkey-inspections! monkey (+ (monkey-inspections monkey)
                                       (length (monkey-items monkey))))
    (set-monkey-items! monkey '())))

(define (monkey-business monkeys)
  (define is (sort (map monkey-inspections (vector->list monkeys)) >))
  (* (first is) (second is)))

(define (part1)
  (for ([_ (in-range 20)])
    (round! monkeys1 #{floor (/ % 3)}))
  (monkey-business monkeys1))

(define (part2)
  (for ([_ (in-range 10000)])
    (round! monkeys2 #{modulo % modulus}))
  (monkey-business monkeys2))

(show-solution* part1 part2)