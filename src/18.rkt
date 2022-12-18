#lang curly-fn racket

(require "../lib.rkt"
         data/queue)

(define droplets
  (~>> (problem-input 18)
       (map #{string-split % ","})
       (mmap string->number)
       (apply set)))

(define xmax (for/max ([droplet droplets]) (first droplet)))
(define ymax (for/max ([droplet droplets]) (second droplet)))
(define zmax (for/max ([droplet droplets]) (third droplet)))

(define (neighbours x y z)
  (filter #{match-let ([`(,x ,y ,z) %])
             (and (<= -1 x (add1 xmax))
                  (<= -1 y (add1 ymax))
                  (<= -1 z (add1 zmax)))}
          `((,(add1 x) ,y ,z)
            (,(sub1 x) ,y ,z)
            (,x ,(add1 y) ,z)
            (,x ,(sub1 y) ,z)
            (,x ,y ,(add1 z))
            (,x ,y ,(sub1 z)))))

(define outside
  (let ([Q (make-queue)])
    (enqueue! Q '(-1 -1 -1))
    (let loop ([outside (set '(-1 -1 -1))])
      (if (queue-empty? Q)
          outside
          (for/fold ([outside outside]
                     #:result (loop outside))
                    ([neighbour (apply neighbours (dequeue! Q))]
                     #:when (not (or (set-member? outside neighbour)
                                     (set-member? droplets neighbour))))
            (enqueue! Q neighbour)
            (set-add outside neighbour))))))

(define part1
  (for/sum ([droplet droplets])
    (length (filter #{not (set-member? droplets %)} (apply neighbours droplet)))))

(define part2
  (for/sum ([droplet droplets])
    (length (filter #{set-member? outside %} (apply neighbours droplet)))))

(show-solution part1 part2)