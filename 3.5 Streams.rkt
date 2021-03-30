#lang sicp

(define (stream-ref s n)
  (if (= n 0)
    (stream-car s)
    (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
  (if (stream-null? s)
    the-empty-stream
    (cons-stream
     (proc (stream-car s))
     (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
    'done
    (begin
     (proc (stream-car s))
     (stream-for-each proc
                      (stream-cdr s)))))

(define (stream-car stream)
  (car stream))

(define (stream-cdr stream)
  (force (cdr stream)))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1)
                                  high))))

(cons-stream 1 2)

(stream-car
 (stream-cdr
  (stream-filter
   prime? (stream-enumerate-interval
           10000 1000000))))

(stream-enumerate-interval 1 100)

(define (stream-filter pred stream)
  (cond ((stream-null? stream)
         the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream
          (stream-car stream)
          (stream-filter
           pred
           (stream-cdr stream))))
        (else (stream-filter
               pred
               (stream-cdr stream)))))

(force (delay 5))

(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
            (if (not already-run?)
              (begin (set! result (proc))
                     (set! already-run? true)
                     result)
              result))))

(define (my-delay exp)
  (memo-proc (lambda () (exp))))

(define (my-force p)
  (p))

;; Ex 3.50

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
    the-empty-stream
    (cons-stream
     (apply proc (map stream-car argstreams))
     (apply stream-map
            (cons proc
                  (map stream-cdr
                       argstreams))))))

;; Ex 3.51

(define (show x)
  (display-line x)
  x)

;; Ex 3.52

;; 3.5.2

(define (integers-starting-from n)
  (cons-stream
   n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))

(define (divisible? x y) (= (remainder x y) 0))
(define no-sevens
  (stream-filter (lambda (x)
                         (not (divisible? x 7)))
                 integers))

(stream-ref no-sevens 100)

(define (fibgen a b)
  (cons-stream a (fibgen b (+ a b))))
(define fibs (fibgen 0 1))

(define (sieve stream)
  (cons-stream
   (stream-car stream)
   (sieve (stream-filter
           (lambda (x)
                   (not (divisible?
                         x (stream-car stream))))
           (stream-cdr stream)))))

(define primes
  (sieve (integers-starting-from 2)))

(defn sieve [s]
  (cons (first s)
        (lazy-seq (sieve (remove #(zero? (mod % (first s)))
                                 (rest s))))))


;; Ex 3.53

;; Ex 3.54

(define (stream-map2 p s1 s2)
  (cons-stream
   (p (stream-car s1)
      (stream-car s2))
   (stream-map2 p (stream-cdr s1)
                  (stream-cdr s2))))

(define (add-streams s1 s2)
  (stream-map2 + s1 s2))

(define (mul-streams s1 s2)
  (stream-map2 * s1 s2))

(define integers
  (cons-stream 1
               (stream-map (lambda (x) (+ x 1))
                           integers)))

(define ones
  (cons-stream 1 ones))

(stream-car (stream-cdr ones))

(define integers
  (cons-stream 1 (add-streams ones integers)))

;; OR
(define integers
  (cons-stream 1 (stream-map (lambda (x) (+ x 1))
                             factorials)))

(stream-car integers)
(stream-car (stream-cdr integers))
(take 5 integers)

(define factorials
  (cons-stream 1 (mul-streams integers
                             factorials)))

(define (take n stream)
  (if (< n 1)
    '()
    (cons
     (stream-car stream)
     (take (- n 1) (stream-cdr stream)))))

(stream-car factorials)
(stream-car (stream-cdr factorials))
(take 5 factorials)

;; Ex 3.55
(define (partial-sums s)
  (cons-stream
   (stream-car s)
   (stream-map (lambda (x) (+ x (stream-car s)))
               (partial-sums (stream-cdr s)))))

(define (partial-sums s)
  (cons-stream
   (stream-car s)
   (add-streams (stream-cdr s)
               (partial-sums s))))

(define (partial-sums s)
  (add-streams s (cons-stream 0 (partial-sums s))))

(take 5 (partial-sums integers))

;; Ex 3.56
;;
(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream
                   s1car
                   (merge (stream-cdr s1)
                          s2)))
                 ((> s1car s2car)
                  (cons-stream
                   s2car
                   (merge s1
                          (stream-cdr s2))))
                 (else
                  (cons-stream
                   s1car
                   (merge
                    (stream-cdr s1)
                    (stream-cdr s2)))))))))

(define (scale-stream s n)
  (stream-map (lambda (x) (* x n)) s))

(define S (cons-stream 1 (merge (scale-stream S 2)
                               (merge (scale-stream S 5)
                                      (scale-stream S 3))) ))


(take 5 S)

;; 3.58

(define (div-streams a b)
  (stream-map2 / a b))

(define (integrate-series a-stream)
  (div-streams a-stream integers))

(define exp-series
  (cons-stream
   1 (integrate-series exp-series)))

(define cosine-series
  (cons-stream 1 (scale-stream (integrate-series sine-series) -1)))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

;; Ex 3.60

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1)
                  (stream-car s2))
               (add-streams (scale-stream (stream-cdr s2) (stream-car s1))
                            (mul-series (stream-cdr s1) s2))))

(take 5 (mul-series cosine-series cosine-series))

;; Ex 3.61

(define (invert-unit-series s)
  (cons-stream 1
               (scale-stream (mul-series (stream-cdr s)
                                         (invert-unit-series s))
                             -1)))

;; Ex 3.62

(define (div-series s1 s2)
  (mul-series s1 (invert-unit-series s2)))

;; Ex 3.5.3

(define (pi-summands n)
  (cons-stream
   (/ 1.0 n)
   (stream-map - (pi-summands (+ n 2)))))

(define pi-stream
  (scale-stream
   (partial-sums (pi-summands 1))
   4))

;; Ex 3.63

(define (square x) (* x x))

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))     ; Sₙ₋₁
        (s1 (stream-ref s 1))     ; Sₙ
        (s2 (stream-ref s 2)))    ; Sₙ₊₁
    (cons-stream
     (- s2 (/ (square (- s2 s1))
              (+ s0 (* -2 s1) s2)))
     (euler-transform (stream-cdr s)))))

(take 10 (euler-transform pi-stream))

(define (make-tableau transform s)
  (cons-stream
   s
   (make-tableau
    transform
    (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))

(define (display-stream s)
  (take 10 s))

(display-sequence
 (accelerated-sequence euler-transform
                       pi-stream))

;; Ex 3.63

(define (sqrt-stream x)
  (cons-stream
   1.0
   (stream-map (lambda (guess) (sqrt-improve guess x))
               (sqrt-stream x)))) ;; recursive call is not memoized.

(define (sqrt-stream x)
  (define guesses
    (cons-stream
     1.0
     (stream-map (lambda (guess) (sqrt-improve guess x))
                 guesses)))
  guesses)


;; To access the second element of (sqrt-stream n)
(stream-car (stream-cdr (sqrt-stream n)))

(car
 (force
  (cdr
   (sqrt-stream n))))

(car
 (force
  (delay
   (stream-map (lambda (guess) (sqrt-improve guess x))
               (sqrt-stream x)))))

(car
 (force
  (memo-proc
   (lambda ()
           (stream-map (lambda (guess) (sqrt-improve guess x))
                       (sqrt-stream x))))))

;; or

(car
 (force
  (memo-proc
   (lambda ()
           (stream-map (lambda (guess) (sqrt-improve guess x))
                       guesses)))))

 ;; Accessing 2nd element
 (cons 1.0
       (map sqrt-improve-x
            (cons 1.0  ;; it has to go through one `map` to reach the top-most stream
                  (map sqrt-improve-x
                       (sqrt-stream x)))))

 ;; Accessing 2nd element
 (cons 1.0
       (map sqrt-improve-x
            guesses
            ))

(define (stream-limit s tolerance)
  (if (< (abs (- (stream-car s) (stream-car (stream-cdr s))))
         tolerance)
    (stream-car (stream-cdr s))
    (stream-limit (stream-cdr s) tolerance)))

 (define (sqrt x tolerance)
   (stream-limit (sqrt-stream x) tolerance))

 (define (average a b)
   (/ (+ a b)
      2))

 (define (sqrt-improve guess x)
   (average guess (/ x guess)))

(sqrt 4 0.1)

;; Ex 3.65

(define (natural-log-summands n)
  (cons-stream
   (/ 1.0 n)
   (stream-map -
               (natural-log-summands (+ n 1)))))

(display-stream (natural-log-summands 1))

(display-stream (partial-sums (natural-log-summands 1)))

(define (interleave s1 s2)
  (if (stream-null? s1)
    s2
    (cons-stream
     (stream-car s1)
     (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x)
                        (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

(define integers
  (cons-stream
   1
   (stream-map inc integers)))

(define (integers-starting-from n)
  (cons-stream
   n
   (integers-starting-from (inc n))))

(define integers
  (integers-starting-from 1))

(display-stream (pairs integers integers))

;; Ex 3.66

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (interleave (stream-map (lambda (x)
                                     (list (stream-car s) x))
                             (stream-cdr t))
                (stream-map (lambda (x)
                                    (list x (stream-car t)))
                            (stream-cdr s)))
    (pairs (stream-cdr s) (stream-cdr t)))))

(display-stream (pairs integers integers))

;; Ex 3.68

(define (pairs s t)
  (interleave
   (stream-map
    (lambda (x)
            (list (stream-car s) x))
    t)
   (pairs (stream-cdr s)
          (stream-cdr t))))

;; Ex 3.69

(define (integral integrand initial-value dt)
  (define int
    (cons-stream
     initial-value
     (add-streams (scale-stream integrand dt)
                  int)))
  int)

;; Ex 3.73

(define (RC R C dt)
  (lambda (currents v0)
    (add-streams
     (scale-stream currents R)
     (integral (scale-stream currents (/ 1 C)) v0 dt))))

;; Ex 3.74

(define (make-zero-crossings
         input-stream last-value)
  (cons-stream
   (sign-change-detector
    (stream-car input-stream)
    last-value)
   (make-zero-crossings
    (stream-cdr input-stream)
    (stream-car input-stream))))

(define zero-crossings
  (make-zero-crossings sense-data 0))

(define zero-crossings
  (stream-map sign-change-detector
              (stream-cdr sense-data)
              sense-data))

;; Ex 3.75

(define (make-zero-crossings
         input-stream last-avpt last-value) ;; the last average point
  (let ((avpt
         (/ (+ (stream-car input-stream)
               last-value)
            2)))
    (cons-stream
     (sign-change-detector avpt last-avpt)
     (make-zero-crossings
      (stream-cdr input-stream)
      avpt
      (stream-car input-stream)))))

;; Ex 3.76

(define (smooth input-stream)
  (define (smoother input-stream last-value)
    (cons-stream
     (/ (+ (stream-car input-stream) last-value)
        2)
     (smoother (stream-cdr input-stream)
               (stream-car input-stream))))
  (smoother (stream-cdr input-stream) (stream-car input-stream)))

(display-stream (smooth integers))

(define (make-zero-crossings
         input-stream) ;; the last average point
  (let ((smoothed-input-stream (smooth input-stream)))
    (define (helper input-stream last-value)
      (cons-stream
       (sign-change-detector
        (stream-car input-stream)
        last-value)
       (helper
        (stream-cdr input-stream)
        (stream-car input-stream))))
    (helper (stream-cdr smoothed-input-stream)
            (stream-car smoothed-input-stream))))

;; 3.5.4

(define (solve f y0 dt)
  (define y (integral dy y0 dt))
  (define dy (stream-map f y))
  y)


;; compare

(define (integral integrand initial-value dt)
  (define int
    (cons-stream
     initial-value
     (add-streams (scale-stream integrand dt)
                  int)))
  int)

(define (integral delayed-integrand initial-value dt)
  (define int
    (cons-stream
     initial-value
     (let ((integrand (force delayed-integrand))) ;; only this line is different
       (add-streams (scale-stream integrand dt)
                    int))))
  int)

;; instead of just passing a stream,
;; delayed-integrand is like (delay integrand)

(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt)) ;; we pass a delayed stream to the integral
  (define dy (stream-map f y))
  y)

(define (integral
         integrand initial-value dt)
  (cons-stream
   initial-value
   (if (stream-null? integrand)
     the-empty-stream
     (integral
      (stream-cdr integrand)
      (+ (* dt (stream-car integrand))
         initial-value)
      dt))))

(define (integral
         integrand initial-value dt)
  (cons-stream
   initial-value
   (let ((integrand (force delayed-integrand)))
     (if (stream-null? integrand)
       the-empty-stream
       (integral
        (delay (stream-cdr integrand))
        (+ (* dt (stream-car integrand))
           initial-value)
        dt)))))

;; Ex 3.78

(define (solve-2nd a b dt y0 dy0)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (integral (stream-add
                         (stream-scale dy a)
                         (stream-scale y b))))
  y)

;; Ex 3.79

(define (solve-2nd-general a b dt y0 dy0)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (integral (stream-map f dy y)))
  y)

;; 3.80
