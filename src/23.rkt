#lang curly-fn racket

(require "../lib.rkt"
         data/queue)

(define elves
  (for/fold ([elves (set)])
            ([row (problem-input 23)]
             [y (in-naturals)])
    (for/fold ([elves elves])
              ([col (string->list row)]
               [x (in-naturals)])
      (if (char=? col #\#)
          (set-add elves `(,x  ,y))
          elves))))

(define (grounds elves)
  (define xmin (minimum (set-map elves first)))
  (define xmax (maximum (set-map elves first)))
  (define ymin (minimum (set-map elves second)))
  (define ymax (maximum (set-map elves second)))
  (- (* (add1 (- xmax xmin)) (add1 (- ymax ymin)))
     (set-count elves)))

(define ((propose elves r) x y)
  (define N  `(,x        ,(sub1 y)))
  (define NE `(,(add1 x) ,(sub1 y)))
  (define E  `(,(add1 x) ,y))
  (define SE `(,(add1 x) ,(add1 y)))
  (define S  `(,x        ,(add1 y)))
  (define SW `(,(sub1 x) ,(add1 y)))
  (define W  `(,(sub1 x) ,y))
  (define NW `(,(sub1 x) ,(sub1 y)))
  (match (% r 4)
    [0 (cond
         [(set-empty? (set-intersect elves (set N NE E SE S SW W NW))) `(,x ,y)]
         [(andmap #{not (set-member? elves %)} (list N NE NW)) N]
         [(andmap #{not (set-member? elves %)} (list S SE SW)) S]
         [(andmap #{not (set-member? elves %)} (list W NW SW)) W]
         [(andmap #{not (set-member? elves %)} (list E NE SE)) E]
         [else `(,x ,y)])]
    [1 (cond
         [(set-empty? (set-intersect elves (set N NE E SE S SW W NW))) `(,x ,y)]
         [(andmap #{not (set-member? elves %)} (list S SE SW)) S]
         [(andmap #{not (set-member? elves %)} (list W NW SW)) W]
         [(andmap #{not (set-member? elves %)} (list E NE SE)) E]
         [(andmap #{not (set-member? elves %)} (list N NE NW)) N]
         [else `(,x ,y)])]
    [2 (cond
         [(set-empty? (set-intersect elves (set N NE E SE S SW W NW))) `(,x ,y)]
         [(andmap #{not (set-member? elves %)} (list W NW SW)) W]
         [(andmap #{not (set-member? elves %)} (list E NE SE)) E]
         [(andmap #{not (set-member? elves %)} (list N NE NW)) N]
         [(andmap #{not (set-member? elves %)} (list S SE SW)) S]
         [else `(,x ,y)])]
    [3 (cond
         [(set-empty? (set-intersect elves (set N NE E SE S SW W NW))) `(,x ,y)]
         [(andmap #{not (set-member? elves %)} (list E NE SE)) E]
         [(andmap #{not (set-member? elves %)} (list N NE NW)) N]
         [(andmap #{not (set-member? elves %)} (list S SE SW)) S]
         [(andmap #{not (set-member? elves %)} (list W NW SW)) W]
         [else `(,x ,y)])]))

(define (proposals elves r)
  (for/fold ([ps (make-immutable-hash)])
            ([elf elves])
    (hash-update ps (apply (propose elves r) elf) #{cons elf %} '())))

(define (round elves r)
  (for/fold ([elves* (set)]
             [moved? #f])
            ([(p es) (proposals elves r)])
    (match es
      [`(,e1 ,e2) (values (set-add (set-add elves* e1) e2) moved?)]
      [`(,e) #:when (not (equal? p e)) (values (set-add elves* p) #t)]
      [`(,e) (values (set-add elves* p) moved?)])))

(define part1
  (for/fold ([elves elves]
             [_ #f]
             #:result (grounds elves))
            ([r (in-range 10)])
    (round elves r)))

(define part2
  (let loop ([elves elves]
             [r 1])
    (define-values (elves* moved?) (round elves r))
    (if moved? (loop elves* (add1 r)) (add1 r))))

(show-solution part1 part2)