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
(* 1 2 3 4) ;; 24
(product-of-relative-primes 5) ;; 24

;; Example: if n=10
;; 1, 3, 7, 9 are relative primes of 10
(* 1 3 7 9) ;; = 189
(product-of-relative-primes 10) ;; 189

;; Ex 1.34

(define (pi-sum a b)
  (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
       a
       (lambda (x) (+ x 4))
       b))

(define (integral f a b dx)
  (* (sum f (+ a (/ dx 2.0))
            (lambda (x) (+ x dx))
            b)
     dx))

(integral (lambda (x) (* x 2))
          0
          1
          0.1)

;; Ex 1.34

(define (f g) (g 2))

(f square)

(f f) ; Returns an error, because f expects a procedure

(define (average a b)
  (/ (+ a b) 2.0))

(average 0 3)

(define tolerance 0.00001)

(define (close-enough? x y)
  (< (abs (- x y)) tolerance))

(define (search f neg-point pos-point)
  (let ((midpoint 
         (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond 
           ((positive? test-value)
            (search f neg-point midpoint))
           ((negative? test-value)
            (search f midpoint pos-point))
           (else midpoint))))))

(negative? 5)

(define (half-inverval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value)
                (positive? b-value))
            (search f a b))
          ((and (negative? b-value)
                (positive? a-value))
            (search f b a))
          (else
            (error "Values are not of opposite sign" a b)))))

(search (lambda (x) (+ x 1)) -5 5)
(half-interval-method sin 2.0 4.0)

(define (fixed-point f first-guess)
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try next))))
  (try first-guess))

(fixed-point cos 1.0)

(define (sqrt x)
  (fixed-point (lambda (y) (/ (+ y (/ x y)) 2))
              1.0))

(sqrt 5)

;; Ex 1.35

; Golden ratio satisfies
; φ2=φ+1
; => φ=1+1/φ

(fixed-point (lambda (x) (+ 1 (/ 1 x)))
            1.0)
(/ 1 (fixed-point (lambda (x) (+ 1 (/ 1 x)))
            1.0))

;; Ex 1.36

(define (fixed-point-print f first-guess)
  (define (try guess)
    (newline)
    (display guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try next))))
  (try first-guess))

(fixed-point-print (lambda (x) (+ 1 (/ 1 x)))
            1.0)

(fixed-point-print (lambda (x) (/ (log 1000) (log x)))
            2.0)

;; Ex 1.37

; 1

(define (cont-frac n d k)
  (define (loop i)
    (/ (n i)
      (if (= i k)
        (d i)
        (+ (d i) (loop (+ i 1))))))
  (loop 1))

(cont-frac (lambda (x) 1.0)
           (lambda (x) 1.0)
           20)

(define (try-k k)
  (cont-frac (lambda (x) 1.0)
             (lambda (x) 1.0)
             k))

(define (trying-print) 
  (define (loop prev k)
    (let ((result (try-k k)))
      (if (close-enough? prev result)
        k
        (begin 
         (newline)
         (display k) 
         (display ": ") 
         (display result)
         (loop result (+ k 1))))))
  (loop 0 1))

(trying-print)

; 2

; iterative process for cont-frac

(define (cont-frac-iterative n d k)
  (define (iter i acc)
    (cond 
      ((= i 0)
        acc)
      ((= i k) 
       (iter (- i 1) (/ (n i) (d i))))
      (else 
       (iter (- i 1) (/ (n i) (+ (d i) acc))))))
  (iter k 0))

(cont-frac-iterative (lambda (x) 1.0)
                     (lambda (x) 1.0)
                     20)

; Ex 1.38

(define (d-euler i) 
  (if (= (modulo (+ i 1) 3) 0)
    (* 2 (/ (+ i 1) 3))
    1))

(define (iterate-to-10 f)
  (define (loop i)
    (if (= i 10)
      (begin 
        (newline)
        (display (f i)))
      (begin 
        (newline)
        (display i)
        (display ": ")
        (display (f i))
        (loop (+ i 1)))))
    (loop 0))
    
(iterate-to-10 d-euler) 

(+ 2 (cont-frac (lambda (x) 1.0)
           d-euler
           100))

;; Ex 1.39

(define x 1)

(define (d-tan i) 
  (+ 1.0 (* (- i 1) 2)))

(/ (cont-frac (lambda (i) (* -1 x x))
           d-tan
           10)
    (* -1 x))
    
(define (average-damp f)
  (lambda (x) 
    (average x (f x))))

(define (sqrt x)
  (fixed-point 
    (average-damp 
      (lambda (y) (/ x y)))
    1.0))

;; vs this 

(define (sqrt x)
  (define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess x)
    (average guess (/ x guess)))
  (define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x)))
  (sqrt-iter 1.0 x))

(sqrt 10)

(define (cube-root x)
  (fixed-point 
    (average-damp 
      (lambda (y) (/ x (square y))))
    1.0))

(cube-root 27)

(define dx 0.0001)

(define (deriv g)
  (lambda (x) 
    (/ (- (g (+ x dx))
          (g x))
       dx)))

(define (cube x) (* x x x))

((deriv cube) 5)

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x)
            ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point 
    (newton-transform g) ;; the function for which we want to find a zero
    guess))

(define (sqrt x)
  (newtons-method
    (lambda (y)
      (- (square y) x))
    1.0))

(sqrt 10)

(define (fixed-point-of-transform 
         g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt x)
  (fixed-point-of-transform 
   (lambda (y) (/ x y))
   average-damp
   1.0))

(define (sqrt x)
  (fixed-point-of-transform 
   (lambda (y) (- (square y) x))
   newton-transform
   1.0))

;; Ex 1.40

(define (cubic a b c)
  (lambda (x) 
    (+ (cube x)
       (* a (square x))
       (* b x)
       c)))

(newtons-method (cubic -3 3 -1) 1.0)

;; Ex 1.41

(define (double f)
  (lambda (x)
    (f (f x))))

(((double (double double)) inc) 5)

;; Ex 1.42

(define (compose f g)
  (lambda (x)
    (f (g x))))

((compose square inc) 6)

;; Ex 1.43

(define (repeated f n)
  (lambda (x)
    (define (loop acc k)
      (if (= k 0)
        acc
        (loop (f acc) (- k 1))))
    (loop x n)))

(define (repeated f n)
  (lambda (x)
    (define (loop acc k)
      (if (= k 1)
        acc
        (loop (compose f acc) (- k 1))))
    ((loop f n) x)))

((repeated square 2) 5)

;; Ex 1.44

(define (smooth f)
  (lambda (x)
    (/ (+ (f x) (f (- x dx)) (f (+ x dx)))
       3)))

(define (n-smooth n)
  (repeated smooth n))

(define dx 1.0)

(square 5)
((smooth square) 5)
(((n-smooth 10) square) 5)

;; Ex 1.45

(define (nth-root n x)
  (fixed-point-of-transform
    (lambda (y) (/ x (expt y (- n 1))))
    (repeated average-damp (floor (log n 2)))
    1.0))

(define (fourth-root x)
  (nth-root 4 x))

;; n  damps
;; 3  1 0
;; 3  1
;; 4  2
;; 6  2
;; 7  2
;; 8  3  2^3 8
;; 9  3
;; 10 3
;; 15 3
;; 16 4  2^4 16
;; 20 4
;; 30 5
;; 40 5
;; 50 5
;; 60 5
;; 70 6

(nth-root 70 10000)

;; Ex 1.46

(define (iterative-improve close-enough? improve-guess)
  (define (loop guess) 
    (if (close-enough? guess)
        guess
        (loop (improve-guess guess))))
  loop)

(define (sqrt x)
  (define (close? guess)
    (close-enough? (square guess) x))
  (define (improve-guess guess)
    (average guess (/ x guess))) ;; y = (y + x/y)/2 is a simple transformation of y = x / y
  ((iterative-improve close? improve-guess) x))

(sqrt 100)