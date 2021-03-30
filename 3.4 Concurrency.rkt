#lang sicp

;; 3.38

; Mary last
; 90 -> 45

; Peter, Mary, Paul
; 110 -> 55 -> 35

; Paul, Mary, Peter
; 80 -> 40 -> 50

; Mary first
; 50 -> 40

;; Ex 3.39

; If Peter and Paul happen with 100 as their starting balance
; then mary, 50

;; 3.4.2

(define x 10)
(parallel-execute (lambda () (set! x (* x x)))
                  (lambda () (set! x (+ x 1))))

(define x 10)
(define s (make-serializer))
(parallel-execute
 (lambda ()
         (s (lambda () (set! x (+ x 1)))))
 (set! x ((s (lambda () (* x x)))))

;; Ex 3.40

 (define x 10))

;; Ex 3.41


;; Ex 3.42

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance
                   (- balance amount))
             balance)
      "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((protected (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw)
             (protected withdraw))
            ((eq? m 'deposit)
             (protected deposit))
            ((eq? m 'balance)
             balance)
            (else (error "Unknown request:
                          MAKE-ACCOUNT"
                         m))))
    dispatch))

;; Ex 3.43

;; Ex 3.44

(define
  (transfer from-account to-account amount)
  ((from-account 'withdraw) amount)
  ((to-account 'deposit) amount))

(define (make-account-and-serializer balance)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin
       (set! balance (- balance amount))
       balance)
      "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer
         (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'balance) balance)
            ((eq? m 'serializer)
             balance-serializer)
            (else (error "Unknown request:
                          MAKE-ACCOUNT"
                         m))))
    dispatch))

;; Ex 3.45
(define
  (make-account-and-serializer balance)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin
       (set! balance (- balance amount))
       balance)
      "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer
         (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw)
             (balance-serializer withdraw))
            ((eq? m 'deposit)
             (balance-serializer deposit))
            ((eq? m 'balance)
             balance)
            ((eq? m 'serializer)
             balance-serializer)
            (else (error "Unknown request:
                          MAKE-ACCOUNT"
                         m))))
    dispatch))

(define (deposit account amount)
  ((account 'deposit) amount))

;; The serialized-exchange function will serialize the
;; function with performs withdrawals, which is serialized with
;; the same serializer. This will be a disaster, because
;; the withdraw function will be blocked by its calling function.

(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
            (define (serialized-p . args)
              (mutex 'acquire)
              (let ((val (apply p args)))
                (mutex 'release)
                val))
            serialized-p)))

(define (make-mutex)
  (let ((cell (list false)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
               (the-mutex 'acquire))) ; retry
            ((eq? m 'release) (clear! cell))))
    the-mutex))
(define (clear! cell) (set-car! cell false))

;; Ex 3.46

 (define (make-semaphore n)
   (let ((total 0)
         (access-lock (make-mutex)))
     (define (acquire)
       (access-lock 'acquire)
       (if (< total n)
         (begin (set! total (+ total 1))
                (access-lock 'release))
         (begin (access-lock 'release)
                (acquire))))
     (define (release)
       (access-lock 'acquire)
       (set! total (- total 1))
       (access-lock 'release))
     (define (the-semaphore m)
       (cond ((eq? m 'acquire) (acquire))
             ((eq? m 'release) (release))))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))
