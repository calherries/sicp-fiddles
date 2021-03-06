#lang sicp

(define x
  (- 21 22))

(+ 5 5)

;; Syntactic sugar of functions
(define square (lambda (x) (* x x)))
;; OR
(define (square x) (* x x))

(square 5)

(define b (+ a (* 5 a)))

b

(+ (* 3
      (+ 7 19.5)))

(square (+ 5 7))

(square (square (square 1001)))

square

(define (average x y)
  (/ (+ x y) 2))

(define (mean-square x y)
  (average (square x)
           (square y)))

;; cond
(define (abs x)
  (cond ((< x 0) (- x))
        (else x)))

(abs -5)

(define (abs x)
  (if (< x 0)
      (- x)
      x))

(define (f a b) (+ (g a) b))

(define (g x) (* 3 x))

;; Fibonacci numbers
;; Naive implementation
(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1))
         (fib (- n 2)))))
;; time = O(fib(n))
;; space = O(n)

;; Note, one way to simplify recursive algorithms is to use memoization

;; An almost identical recursive program, for the towers of Hanoi:
;; It's not exactly the same problem, but it's going to produce a tree. 
(define (move n from to spare)
  (cond ((= n 0) "Done")
        (else
         (move (- n 1) from spare to)
         (print-move from to)
         (move (- n 1) spare to from))))

;; e.g. Recursive
;; (move 3 1 3 2) move a 3-high tower from 1 to 3 using 2 as a spare
;; (move 2 1 2 3)
;; (move 1 1 3 2) move a 1-high tower from 1 to 3 using 2 as a spare

;; Is it possible to write an iterative algorithm for this?
;; Iterative algorithms do intermediate work, and save it.

;; Exercise 1.1
;; Below is a sequence of expressions. What is the result printed by the interpreter in response to each expression? Assume that the sequence is to be evaluated in the order in which it is presented.
10
(+ 5 3 4)
(- 9 1)
(/ 6 2)
(+ (* 2 4) (- 4 6))
(define a 3)
(define b (+ a 1))
(+ a b (* a b))
(= a b)
(if (and (> b a) (< b (* a b)))
    b
    a)
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))
(+ 2 (if (> b a) b a))
(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))

;; Exercise 1.2
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7)))

(define (largest-two-of-three a b c)
  ())

(define a 1)
(define b (+ a 1))
(define c (+ a 2))

(> a b)

(> a c)

(> b c)

(> a c)

;; Exercise 1.3

(define (sum-of-squares a b)
  (+ (square a)
     (square b)))

(define (f a b c)
  (if (> a b)
      (if (> b c)
          (sum-of-squares a b)
          (sum-of-squares a c))
      (if (> a c)
          (sum-of-squares b a)
          (sum-of-squares b c))))

(f 1 2 3)

;; Exercise 1.4
(define (a-plus-abs-b a b)
  ((if (> b 0)
       +
       -) a b))
;; If b is larger than 0, a + b.
;; If b is zero or smaller, a - b.
;; This has the effect of a + (abs b)

;; Exercise 1.5
(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

;; (test 0 (p))
;; Applicative order: hangs because p is evaluated into an infinite loop
;; Normal order: doesn't hang because it will return 0 immediately. p is never evaluated.

;; Exercise 1.6
;; The program will never terminate because the arguments of new-if will always be
;; evaluated, regardless of whether the good-enough? condition is met.
;; In if's special form, the else-clause is only evaluated when the predicate is not met.

;; Exercise 1.7
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define large-number 10000)

(define fair-number 10)

(define small-number 0.000001)

;; small numbers
(good-enough? (* (- 1 0.0000001) large-number) (square large-number))

(good-enough? (* (- 1 0.0000001) fair-number) (square fair-number))

(good-enough? (* (- 1 0.0000001) small-number) (square small-number))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough-change? guess change)
  (< (abs (/ change guess)) 0.001))

(define (improve guess x)
  (average guess (/ x guess)))

(define (sqrt-iter guess x)
  (define (helper guess previous x)
    (if (good-enough-change? guess (- guess previous))
        guess
        (helper (improve guess x) guess x)))
  (helper guess (* 2 guess) x))

(sqrt-iter 1.0 100)
(sqrt-iter 1.0 25)

;; Exercise 1.8
(define (improve-cube y x)
  (/ (+ (/ x (square y)) (* 2 y))
     3))

(define (cube-iter guess x)
  (define (helper guess previous x)
    (if (good-enough-change? guess (- guess previous))
        guess
        (helper (improve-cube guess x) guess x)))
  (helper guess (* 2 guess) x))

(define (cube x)
  (cube-iter 1.0 x))

(cube 27)

;; 1.1.8 Black-box abstractions
