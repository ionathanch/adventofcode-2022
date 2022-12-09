#lang curly-fn racket

(require "../lib.rkt")

(define input (map #{string-split %1 " "} (problem-input 9)))

(define (sign H T)
  (cond
    [(> H T)  1]
    [(= H T)  0]
    [(< H T) -1]))

(define (follow xH yH xT yT)
  (if (or (and (= xH xT) (> (abs (- yH yT)) 1))
          (and (= yH yT) (> (abs (- xH xT)) 1))
          (> (+ (abs (- xH xT)) (abs (- yH yT))) 2))
      (values (+ xT (sign xH xT)) (+ yT (sign yH yT)))
      (values xT yT)))

(define (model knots)
  (define visited (mutable-set))
  (for/fold ([xH 0] [yH 0]
             [xTs (repeat knots '(0))]
             [yTs (repeat knots '(0))])
            ([motion input])
    (match-let* ([`(,dir , n) motion]
                 [n (string->number n)])
      (for/fold ([xH  xH]  [yH  yH]
                 [xTs xTs] [yTs yTs])
                ([_ (in-range n)])
        (define-values (xH* yH*)
          (match dir
            ["U" (values xH (add1 yH))]
            ["D" (values xH (sub1 yH))]
            ["R" (values (add1 xH) yH)]
            ["L" (values (sub1 xH) yH)]))
        (define-values (xTs* yTs*)
          (for/fold ([xTs `(,xH)] [yTs `(,yH)]
                     #:result (values (rest (reverse xTs)) (rest (reverse yTs))))
                    ([xT xTs] [yT yTs])
            (define-values (xT* yT*) (follow (first xTs) (first yTs) xT yT))
            (values (cons xT* xTs) (cons yT* yTs))))
        (set-add! visited (cons (last xTs*) (last yTs*)))
        (values xH* yH* xTs* yTs*))))
  (set-count visited))

(show-solution (model 1) (model 9))