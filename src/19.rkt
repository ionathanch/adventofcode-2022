#lang curly-fn racket

(require "../lib.rkt"
         data/queue)

(define blueprints
  (for/list ([bp (problem-input 19)])
    (match bp
      [(pregexp (string-append "Blueprint (\\d+): "
                               "Each ore robot costs (\\d+) ore. "
                               "Each clay robot costs (\\d+) ore. "
                               "Each obsidian robot costs (\\d+) ore and (\\d+) clay. "
                               "Each geode robot costs (\\d+) ore and (\\d+) obsidian.")
                blueprint)
       (map string->number (rest blueprint))])))

(define ((run-blueprint time) ore-ore clay-ore obsidian-ore obsidian-clay geode-ore geode-obsidian)
  (define seen (mutable-set))
  (define Q (make-queue))
  (enqueue! Q `(,time 0 0 0 0 1 0 0 0))
  (define (enqueue*! time . args)
    (unless (set-member? seen args)
      (set-add! seen args)
      (enqueue! Q (cons time args))))
  (let loop ([geodes 0]
             [time/geodes (make-immutable-hash)])
    (if (queue-empty? Q) geodes
        (match-let ([`(,time ,ore ,clay ,obsidian ,geode ,ore-robot ,clay-robot ,obsidian-robot ,geode-robot) (dequeue! Q)])
          (cond
            [(zero? time) (loop (max geodes geode) time/geodes)]
            [(< (add1 geode) (hash-ref time/geodes time geode)) (loop geodes time/geodes)]
            [(and (>= ore geode-ore) (>= obsidian geode-obsidian))
             (enqueue*! (sub1 time)
                        (+ (- ore geode-ore) ore-robot)
                        (+ clay clay-robot)
                        (+ (- obsidian geode-obsidian) obsidian-robot)
                        (+ geode geode-robot)
                        ore-robot clay-robot obsidian-robot (add1 geode-robot))
             (loop geodes (hash-update time/geodes time #{max % geode} geode))]
            [else
             (when (and (>= ore obsidian-ore) (>= clay obsidian-clay) (< obsidian-robot geode-obsidian))
               (enqueue*! (sub1 time)
                          (+ (- ore obsidian-ore) ore-robot)
                          (+ (- clay obsidian-clay) clay-robot)
                          (+ obsidian obsidian-robot)
                          (+ geode geode-robot)
                          ore-robot clay-robot (add1 obsidian-robot) geode-robot))
             (when (and (>= ore clay-ore) (< clay-robot obsidian-clay))
               (enqueue*! (sub1 time)
                          (+ (- ore clay-ore) ore-robot)
                          (+ clay clay-robot)
                          (+ obsidian obsidian-robot)
                          (+ geode geode-robot)
                          ore-robot (add1 clay-robot) obsidian-robot geode-robot))
             (when (and (>= ore ore-ore) (< ore-robot (max clay-ore obsidian-ore geode-ore)))
               (enqueue*! (sub1 time)
                          (+ (- ore ore-ore) ore-robot)
                          (+ clay clay-robot)
                          (+ obsidian obsidian-robot)
                          (+ geode geode-robot)
                          (add1 ore-robot) clay-robot obsidian-robot geode-robot))
             (enqueue*! (sub1 time)
                        (+ ore ore-robot)
                        (+ clay clay-robot)
                        (+ obsidian obsidian-robot)
                        (+ geode geode-robot)
                        ore-robot clay-robot obsidian-robot geode-robot)
             (loop geodes (hash-update time/geodes time #{max % geode} geode))])))))

(define part1
  (for/sum ([blueprint blueprints])
    (match-define `(,b ,costs ...) blueprint)
    (* b (apply (run-blueprint 24) costs))))

(define part2
  (for/product ([blueprint (take blueprints 3)])
    (match-define `(,b ,costs ...) blueprint)
    (apply (run-blueprint 32) costs)))

(show-solution part1 part2)