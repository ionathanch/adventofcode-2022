#lang curly-fn racket

(require "../lib.rkt"
         data/integer-set)

(define input
  (for/list ([positions (problem-input 15)])
    (match-define (pregexp "Sensor at x=(-?\\d+), y=(-?\\d+): closest beacon is at x=(-?\\d+), y=(-?\\d+)" sb)
      positions)
    (map string->number (rest sb))))

(define y 2000000)
(define bound 4000000)

(define (manhat x1 y1 x2 y2)
  (+ (abs (- x1 x2)) (abs (- y1 y2))))

(define sensors
  (for/list ([sb input])
    (match-define `(,sx ,sy ,bx ,by) sb)
    (list sx sy (manhat sx sy bx by))))

(define (impossible? x y)
  (for/or ([sensor sensors])
    (match-define `(,sx ,sy ,d) sensor)
    (<= (manhat sx sy x y) d)))

(define part1
  (for/fold ([impossibles (make-range)]
             #:result (count impossibles))
            ([sb input])
    (match-define `(,sx ,sy ,bx ,by) sb)
    (define span (- (manhat sx sy bx by) (abs (- sy y))))
    (if (> span 0)
        (~> impossibles
            (union (make-range (- sx span) (+ sx span)))
            (subtract (if (= by y) (make-range bx) (make-range))))
        impossibles)))

(define part2
  (for/or ([sensor sensors])
    (match-define `(,sx ,sy ,d) sensor)
    (define d* (add1 d))
    (for/or ([x (in-inclusive-range (- sx d*) (+ sx d*))]
             #:when (<= 0 x bound))
      (define +y (+ sy (- d* (- sx x))))
      (define -y (- sy (- d* (- sx x))))
      (define (freq y)
        (and (<= 0 y bound)
             (not (impossible? x y))
             (+ (* x 4000000) y)))
      (or (freq +y) (freq -y)))))

(show-solution part1 part2)