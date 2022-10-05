#lang scheme
(define (square x) (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))

(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))
;Exercise 1.3
(define (greater-than-both x y z)
  (and (> x y) (> x z)))

(define (square-sum-with-largest x y z)
  (if (> y z)
      (sum-of-squares x y)
      (sum-of-squares x z)))

(define (sum-of-largest-two x y z)
  (cond ((greater-than-both x y z) (square-sum-with-largest x y z))
        ((greater-than-both y x z) (square-sum-with-largest y x z))
        ((greater-than-both z x y) (square-sum-with-largest z x y))))

(display "Exercise 1.3: (sum-of-largest-two 5 10 8) ")
(sum-of-largest-two 5 10 8)