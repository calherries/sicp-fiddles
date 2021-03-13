#lang sicp

(define x (cons 1 2))

(car x)

(define x (cons 1 2))
(define y (cons 3 4))
(define z (cons x y))

(car (car z))

(define (make-rat n d) (cons n d))
(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define one-half (make-rat 1 2))

(define one-third (make-rat 1 3))

(print-rat one-half)

(define (gcd a b)
  (if (= b 0)
    a 
    (gcd b (modulo a b))))

;; Ex 2.1

(define (xor a b)
  (cond ((and a b) #f)
        ((and (not a) (not b)) #f)
        (else #t)))

(xor #f #f)

(define (make-rat a b)
  (let ((g (gcd a b))
        (sign-multiplier (if (xor (negative? a) (negative? b)) 
                             -1
                             1)))
      (cons (* sign-multiplier (abs (/ a g)))
            (abs (/ b g)))))

(gcd 25 10)
(print-rat (make-rat -2 9))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y)) 
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(print-rat (add-rat one-third one-third))

;; Ex 2.2

(define (make-segment start end)
  (cons start end))

(define start-segment car)
(define end-segment cdr)
  
(end-segment (make-segment 1 2))

(define (make-point x y)
  (cons x y))

(define x-point car)
(define y-point cdr)

(y-point (make-point 1 2))

(define (average x y)
  (/ (+ x y) 2))

(define (midpoint-segment seg)
  (let ((start (start-segment seg))
        (end (end-segment seg)))
        (make-point (average (x-point start) (x-point end))
                    (average (y-point start) (y-point end)))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define seg (make-segment (make-point 0 0)
                         (make-point 1 1)))

(midpoint-segment seg)

;; Ex 2.3

(define (make-rectangle width height)
  (cons width height))

(define width-rectangle car)
(define height-rectangle cdr)

(define (area rec)
  (* (width-rectangle rec) (height-rectangle rec)))

(define (perimeter rec)
  (+ (* 2 (width-rectangle rec)) 
     (* 2 (height-rectangle rec))))

(area (make-rectangle 5 2))

(define (make-rectangle width height)
  (cons (make-segment (make-point 0 0) (make-point width 0))
        (make-segment (make-point 0 0) (make-point 0 height))))

(define (square x) (* x x))
(define (sqrt x) (expt x 0.5))

(define (euclidean-distance a b)
  (sqrt (+ (square (- (x-point a) (x-point b)))
           (square (- (y-point a) (y-point b))))))

(define (length seg)
  (euclidean-distance (start-segment seg) (end-segment seg)))

(length seg)

(define (width-rectangle rec) 
  (length (car rec)))

(define (height-rectangle rec) 
  (length (cdr rec)))

(area (make-rectangle 5 2))
(perimeter (make-rectangle 5 2))

;; Ex 2.4

(define (cons x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else 
           (error "Argument not 0 or 1:
                   CONS" m))))
  dispatch)

(define (car z) (z 0))
(define (cdr z) (z 1))


(define (cons x y)
  (lambda (m) (m x y)))

(define (car z) 
  (z (lambda (x y) x)))
(define (cdr z) 
  (z (lambda (x y) y)))

(car (cons 1 2))
(cdr (cons 1 2))

;; Ex 2.5

(define (cons a b)
  (* (expt 2 a)
      (expt 3 b)))

(define (car pair)
  (log 2 ))

(define z (cons 4 5))

(define (factors-of n factor)
  (if (or (<= n 1) (< 0 (modulo n factor)))
    0
    (+ 1 (factors-of (- (quotient n factor) (modulo n factor)) factor))))

(define (car x) (factors-of x 2))
(define (cdr x) (factors-of x 3))

(car (cons 1 2))
(cdr (cons 1 2))

;; Ex 2.6


(define (add-1 n)
  (lambda (f) 
    (lambda (x) 
      (f ((n f) x)))))

(define one 
  (lambda (f) 
    (lambda (x) 
      x)))

(define one 
  (lambda (f) 
    (lambda (x)
      (f x))))

(define two
  (lambda (f)
    (lambda (x)
      (f (f x)))))

(define three
  (lambda (f)
    (lambda (x)
      (f (f (f x))))))

(define (plus a b)
  (lambda (f)
    (lambda (x)
      (((b f) x) ((a f) x)))))

;; 2.1.4

https://sarabander.github.io/sicp/html/2_002e1.xhtml#g_t2_002e1

;; TODO later
