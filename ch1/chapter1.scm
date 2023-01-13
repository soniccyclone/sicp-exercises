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

(define (average x y )
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))
(define (new-sqrt-iter guess x)
  (new-if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))
(define (new-sqrt x)
  (new-sqrt-iter 1.0 x))

(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))