#lang sicp

; Ex 3.1

(define (make-accumulator acc)
  (lambda (n)
    (set! acc (+ acc n))
    acc))

(define x (make-accumulator 5))

(x 10)

; Ex 3.2

(define (make-monitored f)
  (define counter 0)
  (define (mf x)
    (set! counter (+ counter 1))
    (f x))
  (define (dispatch m)
    (cond ((eq? m 'how-many-calls?) counter)
          ((eq? m 'reset-count)
           (set! counter 0))
          (else (mf m))))
  dispatch)

(define f (make-monitored (lambda (x) (+ x 1))))

(f 'how-many-calls?)
(f 'reset-count)
(f 1)

;; Ex 3.3

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance 
                     (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch pass m)
    (if (eq? password pass)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            (else (error "Unknown request: 
                  MAKE-ACCOUNT" m)))
      (lambda (x) "Incorrect password")))
  dispatch)

(define acc 
  (make-account 100 'secret-password))

((acc 'secret-password 'withdraw) 40)
((acc 'secret-password 'deposit) 40)
((acc 'incorrect-password 'deposit) 40)

(define (make-account balance password)
  (define incorrect-password-counter 0)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance 
                     (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (call-the-cops)
    "Calling the cops")
  (define (check-password password-to-check)
    (eq? password-to-check password))
  (define (dispatch pass m)
    (if (eq? m 'check-password) 
      (check-password pass)
      (if (eq? password pass)
        (cond ((eq? m 'check-password) check-password)
              ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request: 
                    MAKE-ACCOUNT" m)))
        (lambda (x) 
          (set! incorrect-password-counter (+ incorrect-password-counter 1))
          (if (< 7 incorrect-password-counter)
            (call-the-cops)
            "Incorrect password")))))
  dispatch)

(define acc (make-account 100 'secret-password))

((acc 'secret-password 'withdraw) 40)
((acc 'secret-password 'deposit) 40)
((acc 'incorrect-password 'deposit) 40)

;; Ex 3.5

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(random-in-range 1 10)

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1)
                 (+ trials-passed 1)))
          (else 
            (iter (- trials-remaining 1)
                  trials-passed))))
  (iter trials 0))

(define (square x) (* x x))

(define (circle-predicate x y)
  (<= (+ (square (- x 5)) (square (- y 7)))
      9))

(circle-predicate 5 7)

(define (estimate-integral pred x1 x2 y1 y2 trials)
  (define (experiment)
    (let ((x (/ (random-in-range (* 10 x1) (* 10 x2)) 10))
          (y (/ (random-in-range (* 10 y1) (* 10 y2)) 10)))
        (pred x y)))
  (let ((width (- x2 x1))
        (height (- y2 y1)))
    (* 1.0 width height (monte-carlo trials experiment))))

(estimate-integral circle-predicate 0 10 0 10 10000)

;; Ex 3.6

(define rand
  (let ((seed 100))
    (lambda (sym)
      (cond ((eq? sym 'generate)
             (set! seed (+ 1 seed))
             seed)
            ((eq? sym 'reset)
             (lambda (new-seed)
                (set! seed new-seed)))))))

(rand 'generate)
((rand 'reset) 5)

;; 3.1.3

;; Ex 3.7

(define (make-joint account password new-password)
  (define incorrect-password-counter 0)
  (define (dispatch pass m)
    (if (or (eq? password pass) (eq? new-password pass))
      (account password m)
      (lambda (x) 
        (set! incorrect-password-counter (+ incorrect-password-counter 1))
        (if (< 7 incorrect-password-counter)
          (call-the-cops)
          "Incorrect password"))))
  (if (account password 'check-password)
    dispatch
    "Incorrect password"))

(define acc (make-account 100 'secret-password))

((acc 'secret-password 'deposit) 100)
(define jacc (make-joint acc 'secret-password 'new-pass))

((jacc 'secret-password 'deposit) 100)

;; Ex 3.8

(define (f x)
  (let ((counter -1))
    (lambda (x)
      (set! counter (+ counter 1))
      (* x counter))))