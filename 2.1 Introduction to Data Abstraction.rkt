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

https://sarabander.github.io/sicp/html/2_002e1.xhtml#g_t2_002e1