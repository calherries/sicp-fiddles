#lang sicp

;; Ex 1.29
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (inc n) (+ n 1))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(integral cube 0 1 0.01)

(define (simpsons f a b n)
  (define h
    (/ (- b a) n))
  (define (y k)
    (f (+ a (* h k))))
  (define (add-2 x)
    (+ x 2))
  (* (/ h 3)
     (+ (y 0)
        (* 4 (sum y 1 add-2 (- n 1)))
        (* 2 (sum y 2 add-2 (- n 2)))
        (y n))))

(simpsons cube 0 1 100)
(simpsons cube 0 1 1000)

;; Ex 1.30
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))


;; Ex 1.31
;; Recursive approach
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

;; x * x
;; -----------
;; (x-1)*(x+1) 

(define (term-pi x)
  (/ (* x x)
   (* (- x 1) (+ x 1))))

(define (add-2 x)
  (+ x 2))

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(* (product term-pi 2 add-2 1000)
   2.0)

;; Iterative approach

(define (product-iter acc term a next b)
  (if (> a b)
      acc
      (product-iter (* (term a) acc) term (next a) next b)))

(define (product term a next b)
  (product-iter 1 term a next b))


(* (product term-pi 2 add-2 1000)
   2.0)

;; Ex 1.32

;; Recursive
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

;; Iterative
(define (accumulate combiner null-value term a next b)
  (define (accumulate-iter acc a)
    (if (> a b)
        acc
        (accumulate-iter (combiner (term a) acc) (next a))))
  (accumulate-iter null-value a))

(define (product term a next b)
  (accumulate * 1 term a next b))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(* (product term-pi 2.0 add-2 1000)
   2.0)

;; Ex 1.33

(define (filtered-accumulate combiner null-value term a next b filter?)
  (cond ((> a b) null-value)
        ((filter? a) 
         (combiner (term a)
                   (filtered-accumulate combiner null-value term (next a) next b filter?)))
        (else (filtered-accumulate combiner null-value term (next a) next b filter?))))

;; a. the sum of the squares of the prime numbers in the interval a to b (assuming that you have a prime? predicate already written)

(define (square x)
  (* x x))

(define (divides? a b)
  (= (remainder b a) 0))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (smallest-divisor n)
  (find-divisor n 2))

(smallest-divisor 199)

(smallest-divisor 1999)

(smallest-divisor 19999)

(define (prime? n)
  (= n (smallest-divisor n)))

(prime? 13)
(prime? 12)
(inc 12)

(define (sum-squares-of-primes a b)
  (filtered-accumulate + 0 square a inc b prime?))

(sum-squares-of-primes 1 10)

;; b. the product of all the positive integers less than n that are relatively prime to n

(define n 10) 

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(gcd 10 2)

(define (relatively-prime? a b)
  (= (gcd a b) 1))

(relatively-prime? 10 9)

(define (identity x)
  x)

(define (always x) 
  #t)

(define (product-of-relative-primes n)
  (filtered-accumulate * 1 identity 1 inc n (lambda (x) (relatively-prime? x n))))

;; Example: if n=5
;; 1, 2, 3, 4 are relative primes of 5
;; 1 * 2 * 3 * 4 = 24
(product-of-relative-primes 5) ;; 24

;; Example: if n=10
;; 1, 3, 7, 9 are relative primes of 10
(* 1 3 7 9) ;; = 189
(product-of-relative-primes 10) ;; 189
