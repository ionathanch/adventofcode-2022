#lang racket

(require "../lib.rkt"
         data/queue)

(define input
  (for/hash ([yell (problem-input 21)])
    (match yell
      [(pregexp "(\\w+): (\\d+)" `(,_ ,monkey ,n))
       (values (string->symbol monkey) (string->number n))]
      [(pregexp "(\\w+): (\\w+) (\\+|-|\\*|/) (\\w+)" `(,_ ,monkey ,monkey1 ,op ,monkey2))
       (values (string->symbol monkey) (map string->symbol `(,op ,monkey1 ,monkey2)))])))

(define monkeys
  (let ([Q (make-queue)])
    (enqueue! Q 'root)
    (let loop ([monkeys '()])
      (if (queue-empty? Q)
          monkeys
          (let ([monkey (dequeue! Q)])
            (match (hash-ref input monkey)
              [(? number?) (loop (cons monkey monkeys))]
              [`(,_ ,monkey1 ,monkey2)
               (enqueue! Q monkey1)
               (enqueue! Q monkey2)
               (loop (cons monkey monkeys))]))))))

(define part1-defines
  (for/list ([monkey monkeys])
    (match (hash-ref input monkey)
      [(? number? n) `(define ,monkey ,n)]
      [`(,op ,monkey1 ,monkey2)
       `(define ,monkey (,op ,monkey1 ,monkey2))])))

(define part2-defines
  (for/list ([monkey monkeys])
    (match (hash-ref input monkey)
      [_ #:when (symbol=? monkey 'humn)
         '(define-symbolic humn integer?)]
      [(? number? n) `(define ,monkey ,n)]
      [`(,op ,monkey1 ,monkey2)
       `(define ,monkey (,op ,monkey1 ,monkey2))])))

(display-lines-to-file
 `("#lang racket" ,@part1-defines root)
 "21-part1.rkt" #:exists 'replace)

(display-lines-to-file
 `("#lang rosette/safe"
   ,@part2-defines
   (evaluate humn
             (solve (assert (eq? ,(second (hash-ref input 'root))
                                 ,(third (hash-ref input 'root)))))))
 "21-part2.rkt" #:exists 'replace)