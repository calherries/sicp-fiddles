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
