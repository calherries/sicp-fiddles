#lang sicp

;; Generic operators are defined (for the user)
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  'done)
  
;; Users of the scheme number package:
(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

;; Rational package

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
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
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

;; Complex package

(define (install-complex-package)
  ;; imported procedures from rectangular 
  ;; and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 
          'rectangular) 
     x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) 
     r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag 
     (+ (real-part z1) (real-part z2))
     (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag 
     (- (real-part z1) (real-part z2))
     (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang 
     (* (magnitude z1) (magnitude z2))
     (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang 
     (/ (magnitude z1) (magnitude z2))
     (- (angle z1) (angle z2))))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) 
         (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) 
         (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) 
         (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) 
         (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) 
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) 
         (tag (make-from-mag-ang r a))))
  'done)

;; Ex 2.77

(put 'real-part '(complex) real-part)
(put 'imag-part '(complex) imag-part)
(put 'magnitude '(complex) magnitude)
(put 'angle '(complex) angle)

; This works because there was already an implementation of these functions for all possible 'complex
; data types, i.e. if complex is a sum type of rectangular | polar, then real-part is implemented 
; for rectangular and polar types.

;; Ex 2.78

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  'done)
  
;; Users of the scheme number package:
(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

;; from: 2.4.2 

;; Before
(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum: 
              TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum: 
              CONTENTS" datum)))

;; Now
(define (attach-tag type-tag contents)
  (cond ((eq? type-tag 'symbol) contents)
        ((eq? type-tag 'scheme-number) contents)
        (else (cons type-tag contents))))

(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((symbol? datum) 'symbol)
        ((number? datum) 'scheme-number)
        (else
         (error "Bad tagged datum: 
              TYPE-TAG" datum))))

(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((symbol? datum) datum)
        ((number? datum) datum)
        (else (error "Bad tagged datum: 
              CONTENTS" datum))))

;; Ex 2.79

(define (equ? a b)
  (apply-generic 'equ? a b))

;; Complex package: 

(define (equ-complex? a b)
  (and (= (magnitude a) (magnitude b))
       (= (angle a) (angle b))))

(put 'equ? '(complex complex) equ-complex?)

;; Rational package

(define (equ-rational? x y)
    (and (= (numer x) (numer y))
         (= (denom x) (denom y))))

(put 'equ? '(rational rational) equ-rational)

;; Scheme number package
(put 'equ? '(scheme-number scheme-number) =)

;; Ex 2.80

;; Scheme package
(put '=zero? '(scheme-number) (lambda (x) (= 0 x)))

;; Rational package

(put '=zero? '(rational) (lambda (x) (= 0 (numer x))))

;; Complex package
(put '=zero? '(complex) (lambda (x) (= 0 (magnitude x))))


; 2.5.2

(define (scheme-number->complex n)
  (make-complex-from-real-imag 
   (contents n) 0))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 
                       (get-coercion type1
                                     type2))
                      (t2->t1 
                       (get-coercion type2 
                                     type1)))
                  (cond (t1->t2
                         (apply-generic 
                          op (t1->t2 a1) a2))
                        (t2->t1
                         (apply-generic 
                          op a1 (t2->t1 a2)))
                        (else
                         (error 
                          "No method for 
                           these types"
                          (list 
                           op 
                           type-tags))))))
              (error 
               "No method for these types"
               (list op type-tags)))))))

; Ex 2.81

; 1 it goes into infinite recursion
; 2 no, it works fine, except it allows infinite recursion to creep in if the type-tag is implemented differently.
; 3

(define (apply-generic op . args)
  (define (no-method-error type-tags)
    (error "No method for these types"
               (list op type-tags)))
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if (eq? type1 type2)
                  (no-method-error type-tags)
                  (let ((t1->t2 
                        (get-coercion type1
                                      type2))
                        (t2->t1 
                        (get-coercion type2 
                                      type1)))
                    (cond (t1->t2
                          (apply-generic 
                            op (t1->t2 a1) a2))
                          (t2->t1
                          (apply-generic 
                            op a1 (t2->t1 a2)))
                          (else
                            (no-method-error type-tags)))))))))))

; Ex 2.82

(define (get-coercions to-type from-types)
  (if (null? from-types)
    '()
    (let ((coercion (get-coercion (car from-types) to-type))
      (if coercion
        (cons coercion (get-coercions to-type (cdr from-types)))
        #f)))))

(define (get-common-coercions types)
  (define (loop remaining-types)
    (if (null? remaining-types))
      #f
      (let ((coercions (get-coercions (car remaining-types) types))) ;; assume that one type can be coerced to it's own type trivially.
        (if coercions)
          coercions
          (loop (cdr remaining-types)))))
  (loop types)))


; coercing list to a type 
(define (coerce-list-to-type lst type) 
  (map (lambda (x) 
         (let ((proc (get-coercion (type-tag x) type))) 
           (if proc (proc x) x))) 
       lst))

 (define (apply-generic op . args) 
   ; applying to a list of multiple arguments 
   (define (apply-coerced lst)  ; lst just keeps the remaining arguments to explore trying to coerce all the args to
     (if (null? lst) 
       (error "No method for given arguments") 
       (let ((coerced-list (coerce-list-to-type args (type-tag (car lst))))) 
         (let ((proc (get op (map type-tag coerced-list)))) 
           (if proc 
             (apply proc (map contents coerced-list)) 
             (apply-coerced (cdr lst)))))))
  
   ; logic to prevent always coercing if there is already direct input entry 
   (let ((type-tags (map type-tag args))) 
     (let ((proc (get op type-tags))) 
       (if proc 
         (apply proc (map contents args)) 
         (apply-coerced args))))) 

;; Ex 2.83

(define (integer->rational x)
  (make-rational x 1))

(define (rational->real x)
  (make-real (/ (* 1.0 (numer x)) (* 1.0 (denom x)))))

(define (real->complex x)
  (make-from-real-imag x 0))

(put 'raise '(complex) integer->rational)
(put 'raise '(rational) rational->real)
(put 'raise '(real) real->complex)

;; Ex 2.84

; This exercise deals with raising types through an inheritance hierarchy, which is now considered bad practice. I'm skipping.

;; Ex 2.85

; Same as above. Skipping.

;; Ex 2.86

;; This is the complex package:

;; The internal procedures are the only thing that needs to change.

(define (install-complex-package)
  ;; imported procedures from rectangular 
  ;; and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 
          'rectangular) 
     x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) 
     r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag 
     (+ (real-part z1) (real-part z2))
     (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag 
     (- (real-part z1) (real-part z2))
     (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang 
     (* (magnitude z1) (magnitude z2))
     (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang 
     (/ (magnitude z1) (magnitude z2))
     (- (angle z1) (angle z2))))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) 
         (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) 
         (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) 
         (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) 
         (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) 
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) 
         (tag (make-from-mag-ang r a))))
  'done)


;; These internal procedures need to be changed to operate on generic "numbers"

(define (add-complex z1 z2)
  (make-from-real-imag 
   (+ (real-part z1) (real-part z2))
   (+ (imag-part z1) (imag-part z2))))

(define (sub-complex z1 z2)
  (make-from-real-imag 
   (- (real-part z1) (real-part z2))
   (- (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2)
  (make-from-mag-ang 
   (* (magnitude z1) (magnitude z2))
   (+ (angle z1) (angle z2))))

(define (div-complex z1 z2)
  (make-from-mag-ang 
   (/ (magnitude z1) (magnitude z2))
   (- (angle z1) (angle z2))))

;; becomes 

(define (add-complex z1 z2)
  (make-from-real-imag 
   (add (real-part z1) (real-part z2))
   (add (imag-part z1) (imag-part z2))))

(define (sub-complex z1 z2)
  (make-from-real-imag 
   (sub (real-part z1) (real-part z2))
   (sub (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2)
  (make-from-mag-ang 
   (mul (magnitude z1) (magnitude z2))
   (add (angle z1) (angle z2))))

(define (div-complex z1 z2)
  (make-from-mag-ang 
   (div (magnitude z1) (magnitude z2))
   (sub (angle z1) (angle z2))))

 ;; Additionally the subtypes of complex numbers assumed they were normal scheme numbers, not our "generic" numbers.
 
;; So the internal methods need to be adapted:

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) 
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) 
         (tag (make-from-mag-ang r a))))
  'done)

;; this becomes
;;;........
  (define (real-part z)
    (add (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (mul (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (plus (square x) (square y)))
          (atan y x)))
;;; ........

(define (install-rectangular-package)
  ;; internal procedures. note the data structures are 'untagged'
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) 
    (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x) 
    (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) 
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) 
         (tag (make-from-mag-ang r a))))
  'done)

  ;; We need dispatching on type for sine, cosine for the rational numbers

(define (sine x) (apply-generic 'sine x)) 
(define (cosine x) (apply-generic 'cosine x)) 
(define (arctan x) (apply-generic 'arctan x)) 
(define (exp x y) (apply-generic 'exp x y)) 

 ;;; add into rational package  
(put 'sine '(number) (lambda (x) (tag (sin x)))) 
(put 'cosine '(number) (lambda (x) (tag (cos x)))) 
(put 'arctan '(number) (lambda (x) (tag (atan x)))) 
(put 'exp '(number number) (lambda (x y) (tag (expt x y)))) 


;; 2.5.3 Example: Symbolic Algebra
