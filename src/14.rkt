#lang curly-fn racket

(require "../lib.rkt")

(define input
  (for/list ([line (problem-input 14)])
    (for/list ([coord (string-split line " -> ")])
      (for/list ([xy (string-split coord ",")])
        (string->number xy)))))

(define ymax
  (let ([ys (map second (apply append input))])
    (maximum ys)))

(define cave
  (let ([cave (make-hash)])
    (hash-set! cave `(500 0) #\▲)
    (for ([line input])
      (for/fold ([prev (first line)])
                ([next (rest line)])
        (match (list prev next)
          [`((,x ,y1) (,x ,y2))
           #:when (<= y1 y2)
           (for ([y (in-inclusive-range y1 y2)])
             (hash-set! cave `(,x ,y) #\▉))]
          [`((,x ,y1) (,x ,y2))
           #:when (<= y2 y1)
           (for ([y (in-inclusive-range y2 y1)])
             (hash-set! cave `(,x ,y) #\▉))]
          [`((,x1 ,y) (,x2 ,y))
           #:when (<= x1 x2)
           (for ([x (in-inclusive-range x1 x2)])
             (hash-set! cave `(,x ,y) #\▉))]
          [`((,x1 ,y) (,x2 ,y))
           #:when (<= x2 x1)
           (for ([x (in-inclusive-range x2 x1)])
             (hash-set! cave `(,x ,y) #\▉))])
        next))
    cave))

(define (show)
  (define-values (xmin xmax)
    (let ([xs (map first (hash-keys cave))])
      (values (minimum xs) (maximum xs))))
  (for ([y (in-inclusive-range 0 ymax)])
    (for ([x (in-inclusive-range xmin xmax)])
      (display (or (hash-ref cave `(,x ,y) #f) #\ )))
    (newline)))

(define (count-sand)
  (count #{eq? #\░ %} (hash-values cave)))

(define (unblocked? x y)
  (define tile (hash-ref cave `(,x ,y) #f))
  (and (not (or (eq? #\▉ tile)
                (eq? #\░ tile)))
       (< y (+ ymax 2))))

(define (fall1)
  (let loop ([x 500] [y 0])
    (cond
      [(> y ymax) #f]
      [(unblocked? x (add1 y)) (loop x (add1 y))]
      [(unblocked? (sub1 x) (add1 y)) (loop (sub1 x) (add1 y))]
      [(unblocked? (add1 x) (add1 y)) (loop (add1 x) (add1 y))]
      [else (hash-set! cave `(,x ,y) #\░)])))

(define (fall2)
  (let loop ([x 500] [y 0])
    (cond
      [(unblocked? x (add1 y)) (loop x (add1 y))]
      [(unblocked? (sub1 x) (add1 y)) (loop (sub1 x) (add1 y))]
      [(unblocked? (add1 x) (add1 y)) (loop (add1 x) (add1 y))]
      [(and (= x 500) (= y 0)) (hash-set! cave `(,x ,y) #\░) #f]
      [else (hash-set! cave `(,x ,y) #\░)])))

(define part1
  (let loop ()
    (if (fall1) (loop) (count-sand))))

(define part2
  (let loop ()
    (if (fall2) (loop) (count-sand))))

(show-solution part1 part2)