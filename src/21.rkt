#lang racket

(require "../lib.rkt")

(let ([bindings
       (for/list ([yell (problem-input 21)])
         (match yell
           [(pregexp "(\\w+): (\\d+)" `(,_ ,monkey ,n))
            (format "(define (~a) ~a)" monkey n)]
           [(pregexp "(\\w+): (\\w+) (\\+|-|\\*|/) (\\w+)" `(,_ ,monkey ,monkey1 ,op ,monkey2))
            (format "(define (~a) (~a (~a) (~a)))" monkey op monkey1 monkey2)]))])
  (display-lines-to-file
   `(,"#lang racket" ,@bindings "(root)")
   "21-part1.rkt" #:exists 'replace))

(let ([bindings
       (for/list ([yell (problem-input 21)])
         (match yell
           [(pregexp "humn: \\d+" `(,_))
            "(define-symbolic humn integer?)"]
           [(pregexp "root: (\\w+) (\\+|-|\\*|/) (\\w+)" `(,_ ,monkey1 ,op ,monkey2))
            (format "(define (root) (assert (eq? (~a) (~a))))" monkey1 monkey2)]
           [(pregexp "(\\w+): (\\w+) (\\+|-|\\*|/) humn" `(,_ ,monkey ,monkey1 ,op))
            (format "(define (~a) (~a (~a) humn))" monkey op monkey1)]
           [(pregexp "(\\w+): (\\d+)" `(,_ ,monkey ,n))
            (format "(define (~a) ~a)" monkey n)]
           [(pregexp "(\\w+): (\\w+) (\\+|-|\\*|/) (\\w+)" `(,_ ,monkey ,monkey1 ,op ,monkey2))
            (format "(define (~a) (~a (~a) (~a)))" monkey op monkey1 monkey2)]))])
  (display-lines-to-file
   `(,"#lang rosette/safe" ,@bindings "(evaluate humn (solve (root)))")
   "21-part2.rkt" #:exists 'replace))