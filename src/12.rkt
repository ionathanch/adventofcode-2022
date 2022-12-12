#lang curly-fn racket

(require "../lib.rkt"
         data/queue)

(define input (list->vector (problem-input 12)))
(define width (string-length (vector-first input)))
(define height (vector-length input))

;; coord? = (number? . number?)

;; get : coord? -> (or/c char? #f)
(define (get xy)
  (match-define `(,x . ,y) xy)
  (if (and (<= 0 x (sub1 width)) (<= 0 y (sub1 height)))
      (string-ref (vector-ref input y) x)
      #f))

;; accessible? : coord? coord? -> boolean?
(define (accessible? from-xy to-xy)
  (define from (get from-xy))
  (define to   (get to-xy))
  (and to (or (and (char=? #\S from) (char=? #\a to))
              (and (char=? #\S from) (char=? #\b to))
              (and (char=? #\y from) (char=? #\E to))
              (and (char=? #\z from) (char=? #\E to))
              (<= (char->integer to) (add1 (char->integer from))))))

;; neighbours : coord? -> (listof coord?)
(define (neighbours xy)
  (match-define `(,x . ,y) xy)
  (filter #{accessible? xy %}
          `(,(cons (add1 x) y)
            ,(cons (sub1 x) y)
            ,(cons x (add1 y))
            ,(cons x (sub1 y)))))

;; shortest : char? -> number?
(define (shortest c)
  (define seen (mutable-set))
  (define Q (make-queue))
  (for*/list ([x (in-range width)]
              [y (in-range height)]
              #:when (char=? c (get (cons x y))))
    (enqueue! Q (cons (cons x y) 0)))
  (let loop ()
    (match-define `(,xy . ,steps) (dequeue! Q))
    (for ([to (filter #{not (set-member? seen %)} (neighbours xy))])
      (enqueue! Q (cons to (add1 steps)))
      (set-add! seen to))
    (if (char=? #\E (get xy)) steps (loop))))

(show-solution (shortest #\S) (shortest #\a))