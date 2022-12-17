#lang curly-fn racket

(require "../lib.rkt")

(define input
  (first (problem-input 17)))
(define jets (string-length input))

(define tower (apply mutable-set (map #{cons % 0} (inclusive-range 1 7))))

(define (unblocked? x y)
  (not (or (set-member? tower (cons x y))
           (= x 0) (= x 8) (= y 0))))

(define (can-move-left? n x y)
  (match n
    [0 (unblocked? (- x 1) y)]
    [1 (and (unblocked? x y)
            (unblocked? (- x 1) (+ y 1))
            (unblocked? x (+ y 2)))]
    [2 (and (unblocked? (- x 1) y)
            (unblocked? (+ x 1) (+ y 1))
            (unblocked? (+ x 1) (+ y 2)))]
    [3 (and (unblocked? (- x 1) y)
            (unblocked? (- x 1) (+ y 1))
            (unblocked? (- x 1) (+ y 2))
            (unblocked? (- x 1) (+ y 3)))]
    [4 (and (unblocked? (- x 1) y)
            (unblocked? (- x 1) (+ y 1)))]))

(define (can-move-right? n x y)
  (match n
    [0 (unblocked? (+ x 4) y)]
    [1 (and (unblocked? (+ x 2) y)
            (unblocked? (+ x 3) (+ y 1))
            (unblocked? (+ x 2) (+ y 2)))]
    [2 (and (unblocked? (+ x 3) y)
            (unblocked? (+ x 3) (+ y 1))
            (unblocked? (+ x 3) (+ y 2)))]
    [3 (and (unblocked? (+ x 1) y)
            (unblocked? (+ x 1) (+ y 1))
            (unblocked? (+ x 1) (+ y 2))
            (unblocked? (+ x 1) (+ y 3)))]
    [4 (and (unblocked? (+ x 2) y)
            (unblocked? (+ x 2) (+ y 1)))]))

(define (can-fall? n x y)
  (match n
    [0 (and (unblocked? x (- y 1))
            (unblocked? (+ x 1) (- y 1))
            (unblocked? (+ x 2) (- y 1))
            (unblocked? (+ x 3) (- y 1)))]
    [1 (and (unblocked? x y)
            (unblocked? (+ x 1) (- y 1))
            (unblocked? (+ x 2) y))]
    [2 (and (unblocked? x (- y 1))
            (unblocked? (+ x 1) (- y 1))
            (unblocked? (+ x 2) (- y 1)))]
    [3 (unblocked? x (- y 1))]
    [4 (and (unblocked? x (- y 1))
            (unblocked? (+ x 1) (- y 1)))]))

(define (settle! n x y)
  (match n
    [0 (set-add! tower `(,x . ,y))
       (set-add! tower `(,(+ x 1) . ,y))
       (set-add! tower `(,(+ x 2) . ,y))
       (set-add! tower `(,(+ x 3) . ,y))]
    [1 (set-add! tower `(,(+ x 1) . ,y))
       (set-add! tower `(,x . ,(+ y 1)))
       (set-add! tower `(,(+ x 1) . ,(+ y 1)))
       (set-add! tower `(,(+ x 2) . ,(+ y 1)))
       (set-add! tower `(,(+ x 1) . ,(+ y 2)))]
    [2 (set-add! tower `(,x . ,y))
       (set-add! tower `(,(+ x 1) . ,y))
       (set-add! tower `(,(+ x 2) . ,y))
       (set-add! tower `(,(+ x 2) . ,(+ y 1)))
       (set-add! tower `(,(+ x 2) . ,(+ y 2)))]
    [3 (set-add! tower `(,x . ,y))
       (set-add! tower `(,x . ,(+ y 1)))
       (set-add! tower `(,x . ,(+ y 2)))
       (set-add! tower `(,x . ,(+ y 3)))]
    [4 (set-add! tower `(,x . ,y))
       (set-add! tower `(,(+ x 1) . ,y))
       (set-add! tower `(,x . ,(+ y 1)))
       (set-add! tower `(,(+ x 1) . ,(+ y 1)))]))

(define (get-height n y)
  (match n
    [0 y]
    [(or 1 2) (+ y 2)]
    [3 (+ y 3)]
    [4 (+ y 1)]))

(define heights (make-hash))
(define seen (make-hash))
(define candidates (mutable-set))
(define start #f)
(define end #f)

(define part1
  (for/fold ([height 0]
             [jet 0]
             #:result height)
            ([block (in-range 2022)])
    (define b (% block 5))
    (unless (and start end)
      (if (hash-has-key? seen (cons jet b))
          (if (subset? (apply set (range (- block 4) block)) candidates)
              (begin (set! start (hash-ref seen (cons jet b))) (set! end block))
              (set-add! candidates block))
          (hash-set! seen (cons jet b) block)))
    (let loop ([x 3]
               [y (+ height 4)]
               [jet jet])
      (define x*
        (match (string-ref input jet)
          [#\< (if (can-move-left? b x y) (sub1 x) x)]
          [#\> (if (can-move-right? b x y) (add1 x) x)]))
      (define height* (max height (get-height b y)))
      (define jet* (% (add1 jet) jets))
      (if (can-fall? b x* y)
          (loop x* (sub1 y) jet*)
          (begin
            (settle! b x* y) ;; add1: number of blocks fallen is offset by 1
            (hash-set! heights (add1 block) height*)
            (values height* jet*))))))

(define part2
  (let-values ([(cycles extra) (quotient/remainder (- 1000000000000 start) (- end start))])
    (+ (* (- (hash-ref heights end) (hash-ref heights start)) cycles)
       (hash-ref heights (+ start extra)))))

(show-solution part1 part2)