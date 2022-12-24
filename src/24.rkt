#lang curly-fn racket

(require "../lib.rkt"
         data/queue)

(define test
  (list "#.######"
        "#>>.<^<#"
        "#.<..<<#"
        "#>v.><>#"
        "#<^v^^>#"
        "######.#"))

(define input (problem-input 24))

(define blizzards
  (for/fold ([blizzards (make-immutable-hash)])
            ([row input]
             [y (in-naturals)])
    (for/fold ([blizzards blizzards])
              ([col (string->list row)]
               [x (in-naturals)])
      (if ((or/c #\> #\< #\^ #\v) col)
          (hash-update blizzards `(,x ,y) #{cons col %} '())
          blizzards))))

(define xmax (- (string-length (first input)) 2))
(define ymax (- (length input) 2))
(define start '(1 0))
(define end `(,xmax ,(add1 ymax)))

(define (show blizzards)
  (for ([y (in-inclusive-range 1 ymax)])
    (for ([x (in-inclusive-range 1 xmax)])
      (display (first (hash-ref blizzards (list x y) '(#\.)))))
    (newline)))

(define (step blizzards)
  (for*/fold ([blizzards* (make-immutable-hash)])
             ([(xy bs) blizzards]
              [b bs])
    (match (list xy b)
      [`((,x ,y) #\>)
       (define x* (if (= x xmax) 1 (add1 x)))
       (hash-update blizzards* `(,x* ,y) #{cons #\> %} '())]
      [`((,x ,y) #\<)
       (define x* (if (= x 1) xmax (sub1 x)))
       (hash-update blizzards* `(,x* ,y) #{cons #\< %} '())]
      [`((,x ,y) #\v)
       (define y* (if (= y ymax) 1 (add1 y)))
       (hash-update blizzards* `(,x ,y*) #{cons #\v %} '())]
      [`((,x ,y) #\^)
       (define y* (if (= y 1) ymax (sub1 y)))
       (hash-update blizzards* `(,x ,y*) #{cons #\^ %} '())])))

(define (clears blizzards x y)
  (filter #{match-let ([(and xy `(,x ,y)) %])
             (or (and (<= 1 x xmax)
                      (<= 1 y ymax)
                      (empty? (hash-ref blizzards xy '())))
                 (equal? xy start)
                 (equal? xy end))}
          `((,x ,y)
            (,(add1 x) ,y)
            (,(sub1 x) ,y)
            (,x ,(add1 y))
            (,x ,(sub1 y)))))

(define (wayfind blizzards start end)
  (define Q (make-queue))
  (enqueue! Q `(,start ,0 ,blizzards))
  (let loop ([seen (set)])
    (match (dequeue! Q)
      [`(,xy ,t ,blizzards) #:when (equal? xy end) (values blizzards t)]
      [`((,x ,y) ,t ,_) #:when (set-member? seen `(,x ,y ,t)) (loop seen)]
      [`((,x ,y) ,t ,blizzards)
       (define blizzards* (step blizzards))
       (for ([clear (clears blizzards* x y)])
         (enqueue! Q `(,clear ,(add1 t) ,blizzards*)))
       (loop (set-add seen `(,x ,y ,t)))])))

(define-values (part1 part2)
  (let*-values ([(blizzards t1) (wayfind blizzards start end)]
                [(blizzards t2) (wayfind blizzards end start)]
                [(blizzards t3) (wayfind blizzards start end)])
    (values t1 (+ t1 t2 t3))))

(show-solution part1 part2)