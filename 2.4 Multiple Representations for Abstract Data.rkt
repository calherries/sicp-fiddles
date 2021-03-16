#lang sicp

;; Employ "wishful thinking"
;; Assume we have the appropriate selectors and constructors for complex numbers

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


;; 'Rectangular form = simple, polar form = hard' approach

(define (real-part z) (car z))
(define (imag-part z) (cdr z))

(define (magnitude z)
  (sqrt (+ (square (real-part z)) 
           (square (imag-part z)))))

(define (angle z)
  (atan (imag-part z) (real-part z)))

(define (make-from-real-imag x y) 
  (cons x y))

(define (make-from-mag-ang r a)
  (cons (* r (cos a)) (* r (sin a))))

;; Other approach

(define (real-part z)
  (* (magnitude z) (cos (angle z))))

(define (imag-part z)
  (* (magnitude z) (sin (angle z))))

(define (magnitude z) (car z))
(define (angle z) (cdr z))

(define (make-from-real-imag x y)
  (cons (sqrt (+ (square x) (square y)))
        (atan y x)))

(define (make-from-mag-ang r a) 
  (cons r a))

; 2.4.2

; Principle of least commitment:
; the abstraction barrier formed by selectors + constructors permits us to defer to the last possible
; moment the choise of a concrete representation

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

(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))

(define (polar? z)
  (eq? (type-tag z) 'polar))

;; Now the generic operators are dispatching functions, they check the 
;; tag of the argument and pass/dispatch the data to the appropriate handler

;; There are two weaknesses with the below approach:
;; 1. these generic interface / dispatching functions have to know all 
;;    the different representations. A new representation would mean changing all the 
;;    generic interface functions.
;; 2. Even though the individual representations are designed separately, we have to be sure
;;    the implementing handler procedures are not named the same.

(define (real-part z)
  (cond ((rectangular? z)
         (real-part-rectangular (contents z)))
        ((polar? z)
         (real-part-polar (contents z)))
        (else (error "Unknown type: 
               REAL-PART" z))))

(define (imag-part z)
  (cond ((rectangular? z)
         (imag-part-rectangular (contents z)))
        ((polar? z)
         (imag-part-polar (contents z)))
        (else (error "Unknown type: 
               IMAG-PART" z))))

(define (magnitude z)
  (cond ((rectangular? z)
         (magnitude-rectangular (contents z)))
        ((polar? z)
         (magnitude-polar (contents z)))
        (else (error "Unknown type: 
               MAGNITUDE" z))))

(define (angle z)
  (cond ((rectangular? z)
         (angle-rectangular (contents z)))
        ((polar? z)
         (angle-polar (contents z)))
        (else (error "Unknown type: 
               ANGLE" z))))

;; Data directed programming + Additivity

;; Ben's package

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

;; Alyssa's package

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

;; To call a function that is implemented with generic operations 
;; you need to look up the type-tag of the argument in the dispatch 
;; table, and find the handler function for the data type.

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
            "No method for these types: 
             APPLY-GENERIC"
            (list op type-tags))))))

(define (real-part z) 
  (apply-generic 'real-part z))
(define (imag-part z) 
  (apply-generic 'imag-part z))
(define (magnitude z) 
  (apply-generic 'magnitude z))
(define (angle z) 
  (apply-generic 'angle z))

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 
        'rectangular) 
   x y))

(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 
        'polar) 
   r a))

;; Ex 2.73

; 1. we use the operator symbol as a type tag, and dispatch a 'deriv handler function
;    specific to the operator symbol.

; 2. sums and products

(define (deriv-sum exp)
  (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))

(define (deriv-product exp)
  (make-sum
           (make-product 
            (multiplier exp)
            (deriv (multiplicand exp) var))
           (make-product 
            (deriv (multiplier exp) var)
            (multiplicand exp))))

(put 'deriv '+ deriv-sum)
(put 'deriv '* deriv-product)


; 3. skipping, but I get the idea


; You just need to change the put
(put '+ 'deriv deriv-sum)
(put '* 'deriv deriv-product)

; Ex 2.74

(define (type-tag record)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum: 
              TYPE-TAG" datum)))

(define (untag record)
  (if (pair? datum)
      (cadr datum)
      (error "Bad tagged datum: 
              TYPE-TAG" datum)))

(define (get-record record-set key)
  ((get 'get-record (type-tag record-set))
   (untag record-set)
   key))

(define (get-salary employee-record)
  ((get 'get-salary (type-tag employee-record))
   (untag employee-record)))

(define (find-employee-record all-division-files employee-name)
  (if (null? all-division-files)
    (error "Could not find employee" employee-name)
    (let ((found-record (get-record (car all-division-files) employee-name)))
      (if (pair? found-record)
        found-record
        (find-employee-record (cdr all-division-files) employee-name)))))

; part 4

;; Someone has to implement the get-record and get-salary interface functions 
;; for the new record type

;; Message passing

;; Ex 2.75

(define (make-from-mag-ang magnitude angle)
  (define (dispatch op)
    (cond ((eq? op 'magnitude) magnitude)
          ((eq? op 'angle) angle)
          ((eq? op 'real-part) (* (cos angle) magnitude))
          ((eq? op 'imag-part) (* (sin angle) magnitude))))
  dispatch)

;; Ex 2.76

