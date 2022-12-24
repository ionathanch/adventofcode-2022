#lang curly-fn racket

(require "../lib.rkt"
         data/queue)

(define input (problem-input 24))

(define-values (xblizzards yblizzards)
  (for/fold ([xblizzards (make-immutable-hash)]
             [yblizzards (make-immutable-hash)])
            ([row input]
             [y (in-naturals)])
    (for/fold ([xblizzards xblizzards]
               [yblizzards yblizzards])
              ([col (string->list row)]
               [x (in-naturals)])
      (define x* (sub1 x))
      (define y* (sub1 y))
      (match col
        [(or #\< #\>)
         (values xblizzards (hash-update yblizzards y* #{cons (list x* col) %} '()))]
        [(or #\^ #\v)
         (values (hash-update xblizzards x* #{cons (list y* col) %} '()) yblizzards)]
        [else (values xblizzards yblizzards)]))))

(define xmax (- (string-length (first input)) 2))
(define ymax (- (length input) 2))
(define start '(0 -1))
(define end `(,(sub1 xmax) ,ymax))

(define (blizzard? x y t)
  (or (for/or ([yb (hash-ref xblizzards x '())])
        (match yb
          [`(,y* #\^) (= y (% (- y* t) ymax))]
          [`(,y* #\v) (= y (% (+ y* t) ymax))]))
      (for/or ([xb (hash-ref yblizzards y '())])
        (match xb
          [`(,x* #\<) (= x (% (- x* t) xmax))]
          [`(,x* #\>) (= x (% (+ x* t) xmax))]))))

(define (clears x y t)
  (filter #{match-let ([(and `(,@xy ,t) `(,x ,y ,t)) %])
             (or (and (<= 0 x) (< x xmax)
                      (<= 0 y) (< y ymax)
                      (not (blizzard? x y t)))
                 (equal? xy start)
                 (equal? xy end))}
          `((,x ,y ,t)
            (,(add1 x) ,y ,t)
            (,(sub1 x) ,y ,t)
            (,x ,(add1 y) ,t)
            (,x ,(sub1 y) ,t))))

(define (wayfind start end t)
  (define Q (make-queue))
  (enqueue! Q `(,@start ,t))
  (let loop ([seen (set)])
    (match (dequeue! Q)
      [`(,@xy ,t) #:when (equal? xy end) t]
      [xyt #:when (set-member? seen xyt) (loop seen)]
      [(and xyt `(,x ,y ,t))
       (for ([clear (clears x y (add1 t))])
         (enqueue! Q clear))
       (loop (set-add seen xyt))])))

(define-values (part1 part2)
  (let* ([t1 (wayfind start end 0)]
         [t2 (wayfind end start t1)]
         [t3 (wayfind start end t2)])
    (values t1 t3)))

(show-solution part1 part2)