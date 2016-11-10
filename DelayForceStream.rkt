#lang racket
(define (foo) (+ 2 5))
;(foo) = 7    foo = #procedure
(define foof (lambda() (+ 2 5)))
;(foof) = 7    foof = #procedure
(define (fooff) (lambda () (+ 2 5)))
; ((fooff)) = 7    foof = (foof) = #procedure


; 4

(define (mdelay l1)
  ( lambda () l1))
  
(define (inforce li)
  ( li))
  
; 5

(define (stream-cons a b)
  (cons a (mdelay b)))
  
(define (stream-car li )
  (car li))
  
(define (stream-cdr li)
  (force (cdr li)))
  
;6 
;a
(define (ones)
  (stream-cons 1 ones))
;b