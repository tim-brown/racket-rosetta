#lang racket
(require rackunit)

(define (quick-sort l gt?)
  (if
    (null? l) null
    (call-with-values
     (lambda () (partition (curry gt? (car l)) (cdr l)))
     (lambda (lt-part gt-part)
       (append
        (quick-sort lt-part gt?)
        (cons (car l)
              (quick-sort gt-part gt?)))))))

;;; Candidate for Rosettacode.org
(define (quick-sort/match l gt?)
  (match l
    ['() null]
    [(cons hd tl)
    (call-with-values
     (lambda () (partition (curry gt? hd) tl))
     (lambda (lt-part gt-part)
       (append
        (quick-sort/match lt-part gt?)
        (cons hd (quick-sort/match gt-part gt?)))))]))

(check-equal? (quick-sort (shuffle '(1 2 3 4 5 6 7)) >) '(1 2 3 4 5 6 7))
(check-equal? (quick-sort (shuffle '(1 1 1 4 5 6 7)) >) '(1 1 1 4 5 6 7))
(check-equal? (quick-sort/match (shuffle '(1 2 3 4 5 6 7)) >) '(1 2 3 4 5 6 7))

(let ((big-list (build-list 1000000 (lambda (i) (random 100000000)))))
  (displayln "built")
  (time (quick-sort big-list >))
  (time (quick-sort/match big-list >))
  void)