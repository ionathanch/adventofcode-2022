#lang curly-fn racket

(require "../lib.rkt")

(define input
  (map (match-lambda [(pregexp "(\\w) (\\d+)" `(,_ ,d ,n))
                      (list (string->symbol d) (string->number n))])
       (problem-input 9)))

(define (sign H T)
  (cond
    [(> H T)  1]
    [(= H T)  0]
    [(< H T) -1]))

(define (lead d x y)
  (match d
    ['U (values x (add1 y))]
    ['D (values x (sub1 y))]
    ['R (values (add1 x) y)]
    ['L (values (sub1 x) y)]))

(define (follow xH yH xT yT)
  (if (or (and (= xH xT) (= (abs (- yH yT)) 2))
          (and (= yH yT) (= (abs (- xH xT)) 2))
          (> (+ (abs (- xH xT)) (abs (- yH yT))) 2))
      (values (+ xT (sign xH xT)) (+ yT (sign yH yT)))
      (values xT yT)))

(define (model knots)
  (define visited (mutable-set))
  (for*/fold ([xs (repeat (add1 knots) '(0))]
              [ys (repeat (add1 knots) '(0))])
             ([motion input]
              [_ (in-range (second motion))])
    (for/fold ([xs '()] [ys '()]
               #:result (begin (set-add! visited (cons (first xs) (first ys)))
                               (values (reverse xs) (reverse ys))))
              ([x xs] [y ys])
      (define-values (x* y*)
        (if (empty? xs)
            (lead (first motion) x y)
            (follow (first xs) (first ys) x y)))
      (values (cons x* xs) (cons y* ys))))
  (set-count visited))

(show-solution (model 1) (model 9))