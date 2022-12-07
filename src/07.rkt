#lang curly-fn plai

(require "../lib.rkt")

(define-type tree
  (dir [size number?] [entries (listof tree?)])
  (fil [size number?]))

(define (get-size t)
  (type-case tree t
    (dir [size _] size)
    (fil [size] size)))

;; parse : (listof string?) ((listof string?) (listof tree?) -> tree?) -> tree?
;; Parse input in CPS, where the continuation takes a list of the entries at the
;; current directory as well as the remaining input to parse
(define (parse inputs k)
  (let loop ([inputs inputs]
             [trees empty])
    (match inputs
      ['() (k inputs trees)]
      [`(,input . ,inputs)
       (match (string-split input " ")
         [`("$" "cd" "..") (k inputs trees)]
         [`("$" "cd" ,_) (parse inputs #{loop %1 (cons (dir 0 %2) trees)})]
         [`("$" "ls") (loop inputs trees)]
         [`("dir" ,_) (loop inputs trees)]
         [`(,size ,_) (loop inputs (cons (fil (string->number size)) trees))])])))

;; measure : tree? -> tree?
;; Add sizes of directories to filesystem
(define (measure fs)
  (type-case tree fs
    (dir [_ entries]
         (define entries* (map measure entries))
         (dir (sum (map get-size entries*)) entries*))
    (fil [_] fs)))

;; flatten : tree? -> (listof number?)
;; Get a list of the sizes of all the directories
(define (flatten fs)
  (type-case tree fs
    (dir [size entries]
         (cons size (apply append (map flatten entries))))
    (fil [size] '())))

(define filesystem
  (~> (problem-input 7)
      (parse #{first %2})
      measure))

(define dir-sizes (flatten filesystem))

(define part1
  (sum (filter #{<= %1 100000} dir-sizes)))

(define part2
  (let ([need (- (dir-size filesystem) 40000000)])
    (minimum (filter #{>= %1 need} dir-sizes))))

(show-solution part1 part2)