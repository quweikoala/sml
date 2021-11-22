
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride)
  (if (<= low high)
      (cons low (sequence (+ low stride) high stride))
      null
  )
)

(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs)
)

(define (list-nth-mod xs n)
  (cond [(> 0 n) (error "list-nth-mod: negative number")]
        [(null? xs) ((error "list-nth-mod: empty list"))]
        [#t (let* ([i (remainder n (length xs))])
            (car (list-tail xs i)))]
  )
)

(define (stream-for-n-steps s n)
  (letrec ([f (lambda (s ans cur)
                (let ([pr (s)])
                   (if (> cur n)
                       ans
                       (f (cdr pr) (cons (car pr) ans) (+ cur 1)))))])
  (reverse (f s null 1))
  )
 )

(define nats
  (letrec ([f (lambda (x) (cons x (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

(define funny-number-stream
  (letrec ([f (lambda (x) 
              (if (= (modulo x 5) 0)
               (cons (* -1 x) (lambda () (f (+ x 1))))
               (cons x (lambda () (f (+ x 1))))
       ))])
    (lambda () (f 1))))

(define powers-of-two
(letrec ([f (lambda (x) (cons x (lambda () (f (* x 2)))))])
(lambda () (f 2))))

(define dan-then-dog
  (letrec ([f (lambda (x) (if (= (modulo x 2) 0)
                            (cons "dan.jpg" (lambda () (f (+ x 1))))
                            (cons "dog.jpg" (lambda () (f (+ x 1))))))])
    (lambda () (f 0))))


(define (stream-add-zero s)
  (letrec ([f (lambda (s)
                (let ([pr (s)])
                  (cons (cons 0 (car pr)) (lambda () (f (cdr pr))))))])
  (lambda () (f s))))
