;; Ex 1.9
(define (+ a b)
  (if (= a 0)
      b
      (inc (+ (dec a) b))))

(+ 4 5)
(inc (+ (dec 4) 5))
(inc (+ 3 5))
(inc (inc (+ 2 5)))
(inc (inc (inc (+ 1 5))))
(inc (inc (inc (inc (+ 0 5)))))
(inc (inc (inc (inc 5))))
9

(define (+ a b)
  (if (= a 0)
      b
      (+ (dec a) (inc b))))

(+ 4 5)
(+ (dec 4) (inc 5))
(+ (dec 3) (inc 6))
(+ (dec 2) (inc 7))
(+ (dec 1) (inc 8))
9

;; Ex 1.10
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(A 1 10)

(A 0 (A 1 (- y 1)))
(A 0 (A 1 9))
(A 0 (A 0 (A 0 .... (A 0 2))))
; ---------- 10x (A 0 ..) ----- 
; 2^10
; To calculate:
(define (exp x n)
  (if (= 0 n) 1
      (* x (exp x (- n 1)))))

(exp 2 10)

(A 2 4)

(A 1 (A 2 3))
(A 1 (A 1 (A 2 2)))
(A 1 (A 1 (A 1 (A 1 1))))
(A 1 (A 1 (A 1 2)))
(A 1 (A 1 (exp 2 2)))
; (A 1 x) -> (exp 2 x)
(exp 2 (exp 2 (exp 2 2)))

(A 3 3)

(A 2 (A 3 2))
(A 2 (A 2 (A 3 1)))
(A 2 (A 2 2))
(A 2 4)
(exp 2 (exp 2 (exp 2 (exp 2 1))))
;; ----------- 4x (exp 2 ...) ------ 
;; 2^(n*n)

(define (f n) (A 0 n))
;; 2n
(define (g n) (A 1 n))
;; 2^n
(define (h n) (A 2 n))
;; (2^(n^n))

;; Tree recursion
;; Ex 1.11
(define (f n)
  (cond ((< n 3) n)
        (else (+ (f (- n 1))
                 (* 2 (f (- n 2)))
                 (* 3 (f (- n 3)))))))

(f 10)

;; Just like fibonacci, but with three state variables
;; a is f (n + 2)
;; b is f (n + 1)
;; c is f (n)

;; a <- a + 2b + 3c
;; b <- a
;; c <- b

(define (f n)
  (f-iter 2 1 0 n))

(define (f-iter a b c count)
  (if (= count 0)
      c
      (f-iter (+ a (* 2 b) (* 3 c))
              a
              b
              (- count 1))))

(f 10)

;; Ex 1.12
;; Write a procedure that computes elements of Pascal’s triangle
;; by means of a recursive process.

;; 0 <- edge so 1
;; 1 <- edge so 1
;; 2 <- edge so 1
;; 3 <- edge so 1

;; How to tell if an element is an edge?
0 1 2 3 (4) 5 6 (7 8) 9 10 (11 12 13)
1 2 3 4 5

(define (pascals row col)
  (cond ((= col 0) 1)
        ((= col row) 1)
        (else 
         (+ (pascals (- row 1)
                     (- col 1))
            (pascals (- row 1)
                     col)))))

(pascals 4 2)

;; Ex 1.13

;; If n = 0:
;;  (phi^n - theta^n) / (root 5) = (1 - 1) / (root 5)
;; = 0

;; If n = 1, then (phi^n - theta^n) / (root 5) = phi - theta / (root 5)
;; = (1 + (root 5) - (1 - (root 5))) / (root 5) / 2
;; = (2 * (root 5)) / (2 * (root 5))
;; = 1

;; If n holds and n - 1 holds, then it holds for n + 1
;; Fib(n + 1) = Fib(n) + Fib(n - 1)
;; = (phi^(n+1) - theta^(n+1)) / (root 5)
;;=  (phi^n - theta^n) + (phi^(n-1) - theta^(n-1)) / (root 5)
;;=  (phi^(n-1) * (n + 1) - theta^(n - 1) * (n + 1)) / (root 5)
;; = (n + 1) / (root 5) * (phi^(n-1) - theta^(n-1))
;; = (n + 1) * Fib(n-1)

;; Ex 1.16
(define (fast-expt-iter b n a)
  (cond ((= n 0) a)
        ((even? n) (fast-expt-iter (* b b) (/ n 2) a))
        (else (fast-expt-iter b (- n 1) (* a b)))))

(define (fast-expt b n)
  (fast-expt-iter b n 1))

;; b=3, n=3, a=1
;; b=3, n=2, a=3
;; b=9, n=1, a=3
;; b=9, n=0, a=27

;; Ex 1.17
(define (double x)
  (+ x x))

(define (halve x)
  (/ x 2))

(define (mul a b)
  (cond ((= b 0) 0)
        ((even? b) (double (mul a (halve b))))
        (else (+ a (mul a (- b 1))))))

(mul 3 5)

;; Ex 1.18

;; a * b = 2 * (a * (b / 2))
;;       = (2 * a) * (b / 2)

(define (mul a b)
  (mul-iter a b 0))

(define (mul-iter a b acc)
  (cond ((= b 0) acc)
        ((even? b) (mul-iter (double a) (halve b) acc))
        (else (mul-iter a (- b 1) (+ acc a)))))

(mul 3 4)

(mul 5 1)

(mul 5 2)

(mul 5 3)

(mul 5 4)

;; Ex 1.19

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (square x)
  (* x x))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (square p) (square q))
                   (+ (* 2 p q) (square q))
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))


;; Ex 1.21
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

;; Ex 1.22
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fast-expt-iter b n a)
  (cond ((= n 0) a)
        ((even? n) (fast-expt-iter (* b b) (/ n 2) a))
        (else (fast-expt-iter b (- n 1) (* a b)))))

(define (fast-expt b n)
  (fast-expt-iter b n 1))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display (* elapsed-time))
  #t)

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (real-time-clock) start-time))
      #f))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (real-time-clock)))

(define (search-for-primes start stop)
  (define (search-for-n-primes start stop n)
    (cond ((or (<= n 0) (>= start stop))
           #f)
          ((timed-prime-test start)
           (search-for-n-primes (+ 1 start) stop (- n 1)))
          (else (search-for-n-primes (+ 1 start) stop n))))
  (search-for-n-primes start stop 3))

(search-for-primes 10000 110000)

;; Ex 1.23
(define (next n)
  (if (= n 2)
      3
      (+ n 2)))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (smallest-divisor n)
  (find-divisor n 2))

(smallest-divisor 199)

(smallest-divisor 1999)

(smallest-divisor 19999)

;; Ex 1.24

(define (expmod base exp m)
  (remainder (fast-expt base exp) m))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fast-expt-iter b n a)
  (cond ((= n 0) a)
        ((even? n) (fast-expt-iter (* b b) (/ n 2) a))
        (else (fast-expt-iter b (- n 1) (* a b)))))

(define (fast-expt b n)
  (fast-expt-iter b n 1))

(define (try-it a n)
  (= (expmod a n n) a))

(define (fools-fermat-test? n)
  (define (test-fermat-iter a)
    (cond ((or (= n 1) (= n 0)) #f)
          ((= a n) #t)
          ((try-it a n) (test-fermat-iter (+ a 1)))
          (else #f)))
  (test-fermat-iter 1))

(fools-fermat-test? 3)

(fools-fermat-test? 561)

(fools-fermat-test? 1105)

(fermat-test 0)

(define (cal-test n)
  (define (iter a n)
    (cond ((= a n) #t)
          ((= (expmod a n n) a) (iter (+ a 1) n))
          (else #f)))
  (iter 1 n))

(cal-test 10)

(cal-test 561)

(cal-test 1105)

(cal-test 1729)

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (if (= (remainder (square base) m) 1)
             0
             (remainder (square (expmod base (/ exp 2) m))
                     m)))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n)
  (define (try-it a)
    (define expmod-or-zero (expmod a n n))
    (or (= expmod-or-zero a)
        (= expmod-or-zero 0)))
  (try-it (+ 1 (random (- n 1)))))

(fermat-test 700)
