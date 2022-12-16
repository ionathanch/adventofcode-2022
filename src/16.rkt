#lang curly-fn racket

(require "../lib.rkt"
         data/queue
         (except-in graph transpose))

(define flows (make-hash))

(define graph
  (for/fold ([edges '()]
             #:result (unweighted-graph/undirected edges))
            ([valve (problem-input 16)])
    (match-let* ([(pregexp "Valve (\\w\\w) has flow rate=(\\d+); tunnels? leads? to valves? (.+)" `(,_ ,v ,r ,ts)) valve]
                 [valve (string->symbol v)]
                 [flow (string->number r)])
      (hash-set! flows valve flow)
      (for/fold ([edges edges])
                ([tunnel (string-split ts ", ")])
        (cons (list valve (string->symbol tunnel)) edges)))))

(define graph*
  (weighted-graph/undirected
   (for/list ([(edge weight) (floyd-warshall graph)]
              #:when (or (and (symbol=? 'AA (first edge))
                              (not (zero? (hash-ref flows (second edge)))))
                         (and (not (zero? weight))
                              (not (zero? (hash-ref flows (first edge))))
                              (not (zero? (hash-ref flows (second edge)))))))
     (cons weight edge))))

(define (part1)
  (define Q (make-queue))
  (enqueue! Q (list 'AA 30 0 (set 'AA)))
  (let loop ([pressure 0])
    (if (queue-empty? Q) pressure
        (match-let ([`(,v ,t ,p ,seen) (dequeue! Q)])
          (for ([n (in-neighbors graph* v)]
                #:when (not (set-member? seen n)))
            (define time (- t (edge-weight graph* v n) 1))
            (define flow (hash-ref flows n))
            (when (> time 0)
              (enqueue! Q (list n time (+ p (* flow time)) (set-add seen n)))))
          (loop (max pressure p))))))

(define (part2)
  (define Q (make-queue))
  (define paths (make-hash))
  (enqueue! Q (list 'AA 26 0 (set 'AA)))
  (let loop ()
    (if (queue-empty? Q)
        (for*/max ([(path1 p1) paths]
                   [(path2 p2) paths]
                   #:when (set-empty? (set-remove (set-intersect path1 path2) 'AA)))
          (+ p1 p2))
        (match-let ([`(,v ,t ,p ,seen) (dequeue! Q)])
          (for ([n (in-neighbors graph* v)]
                #:when (not (set-member? seen n)))
            (define time (- t (edge-weight graph* v n) 1))
            (define flow (hash-ref flows n))
            (if (> time 0)
                (enqueue! Q (list n time (+ p (* flow time)) (set-add seen n)))
                (hash-set! paths seen (max p (hash-ref paths seen 0)))))
          (loop)))))

(show-solution* part1 part2)