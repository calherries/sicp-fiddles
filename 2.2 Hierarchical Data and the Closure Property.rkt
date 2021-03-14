#lang sicp

;; 2.2

(list 1 2 3 4)
;; equivalent to
(cons 1 (cons 2 (cons 3 (cons 4 '()))))

;; Ex 2.17

(define (last-pair lst)
  (define (loop prev remaining)
    (if (null? remaining)
      prev
      (loop (car remaining) (cdr remaining))))
  (loop '() lst))

(define lst (list 1 2 3 4))
(last-pair lst)

(define (reverse lst)
  (define (loop acc remaining)
    (if (null? remaining)
      acc
      (loop 
        (cons (car remaining) acc)
        (cdr remaining))))
  (loop '() lst))

(null? '())

(reverse (list 1 2 3 4))

;; Ex 2.19

(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) 
             (= kinds-of-coins 0)) 
         0)
        (else 
         (+ (cc amount (- kinds-of-coins 1))
            (cc (- amount (first-denomination 
                           kinds-of-coins))
                kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

(count-change 100)

(define us-coins 
  (list 50 25 10 5 1))

(define uk-coins 
  (list 100 50 20 10 5 2 1 0.5))

;; Redefining cc
(define (cc amount coin-values)
  (cond ((= amount 0) 
         1)
        ((or (< amount 0) 
             (no-more? coin-values)) 
         0)
        (else
         (+ (cc 
             amount
             (except-first-denomination 
              coin-values))
            (cc 
             (- amount
                (first-denomination 
                 coin-values))
             coin-values)))))

(define (except-first-denomination coins)
  (cdr coins))

(define (first-denomination coins)
  (car coins))

(define no-more? null?)

(define uk-coins 
  (list 20 100 50 10 5 2 1 0.5))

(cc 100 us-coins)
(cc 100 uk-coins)

;; The order of the coins doesn't matter. Every (unordered) combination of coins is explored, even though the ordered combinations of coins are different.

;; Ex 2.20

(define (same-parity x . xs)
  (let ((same-parity? (lambda (y) (= (modulo y 2) 
                                     (modulo x 2))))
        (filter (lambda (pred lst)
                (cond ((null? lst) '())
                      ((pred (car lst)) 
                       (cons (car lst) (filter pred (cdr lst))))
                      (else (filter pred (cdr lst)))))))
    (cons x (filter same-parity? xs))))

(same-parity 1 2 3 4)

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

;; map implementation do not contain car, cdr, or null?
(define (scale-list items factor)
  (map (lambda (x) (* x factor))
       items))

;; Ex 2.21

(define (map f xs)
  (if (null? xs)
    '()
    (cons (f (car xs)) (map f (cdr xs)))))

(define (square x) (* x x))

(define (square-list xs)
  (if (null? xs)
    '()
    (cons (square (car xs)) (square-list (cdr xs)))))

(define (square-list xs)
  (map square xs))

(square-list (list 1 2 3 4))

;; Ex 2.22

;; As you're accumulating the answer, you are taking the front element from the list, and attaching it to the 
;; front of the accumulator. This means that the earlier elements are the later elements of the answer.

;; Next he tries to cons a list onto an element, where he should be cons'ing the elements onto a list.

;; Ex 2.23

(define (for-each f xs)
  (if (null? xs)
    #t
    (begin 
      (f (car xs))
      (for-each f (cdr xs)))))

(for-each 
 (lambda (x) (newline) (display x))
 (list 57 321 88))

;; Ex 2.2.2

;; https://sarabander.github.io/sicp/html/2_002e2.xhtml#g_t2_002e2

(define x (cons (list 1 2) (list 3 4)))

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

;; Ex 2.24

(list 1 (list 2 (list 3 4)))

;; Ex 2.25

(define x (list 1 3 (list 5 7) 9))
(car (cdr (car (cdr (cdr x)))))

(define x (list (list 7)))
(car (car x))

(define x (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr x))))))))))))

;; Ex 2.26

(define x (list 1 2 3))
(define y (list 4 5 6))
(append x y)
(cons x y)
(list x y)

;; Ex 2.27

(define (deep-reverse lst)
  (define (loop acc remaining)
    (if (null? remaining)
      acc
      (loop 
        (cons (if (pair? (car remaining))
                  (deep-reverse (car remaining))
                  (car remaining)) 
              acc)
        (cdr remaining))))
  (loop '() lst))

(null? '())

(define x (list 1 2 (list 3 (list 4 6)) 5))
(deep-reverse (list 1 2 (list 3 (list 4 6)) 5))

;; Ex 2.28

(append (list 1 2) (list 3 4))

;; Recursive solution
(define (fringe t)
  (cond ((null? t) '())
        ((pair? t) (append (fringe (car t)) 
                           (fringe (cdr t))))
        (else (list t))))
(fringe x)

;; Iterative solution with a stack
(define (fringe t)
  (define (loop acc stack)
    (if (null? stack) 
        acc
        (let ((nxt (car stack))
              (remaining (cdr stack))) ;; could be null
          (cond ((pair? nxt)
                 (loop acc (cons (cdr nxt) (cons (car nxt) remaining))))
                ((null? nxt)
                 (loop acc remaining))
                (else
                 (loop (cons nxt acc) remaining))))))
  (loop '() (list t)))
(fringe x)

;; Ex 2.29

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define left-branch car)
(define (right-branch mobile) (car (cdr mobile)))

(define branch-length car)
(define (branch-structure branch) (car (cdr branch)))

(define (is-mobile? x) 
  (pair? x))

(define (branch-weight branch)
  (if (is-mobile? (branch-structure branch))
    (total-weight (branch-structure branch))
    (branch-structure branch)))

(define (total-weight mobile)
    (+ (branch-weight (left-branch mobile))
       (branch-weight (right-branch mobile))))

(define m (make-mobile (make-branch 1 (make-mobile (make-branch 1 1)
                                                  (make-branch 2 2)))
                      (make-branch 2 2)))
(total-weight m)

; part 3

(define (torque branch)
  (* (branch-weight branch) (branch-length branch)))

(define (is-balanced-node mobile)
  (= (torque (left-branch mobile))
     (torque (right-branch mobile))))

(define (is-balanced mobile)
  (let ((left (left-branch mobile))
        (right (right-branch mobile)))
    (and (or (not (is-mobile? (branch-structure left)))
             (is-balanced (branch-structure left)))
         (or (not (is-mobile? (branch-structure right)))
             (is-balanced (branch-structure right)))
         (is-balanced-node mobile))))

(define n (make-mobile (make-branch 1 (make-mobile (make-branch 1 1)
                                                  (make-branch 1 1)))
                      (make-branch 2 1)))
(is-balanced n)

; part 4

(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

(define left-branch car)
(define right-branch cdr)

(define branch-length car)
(define branch-structure cdr)

; Mapping over trees

(define (scale-tree tree factor)
  (cond ((null? tree) '())
        ((not (pair? tree)) 
         (* tree factor))
        (else
         (cons (scale-tree (car tree) 
                           factor)
               (scale-tree (cdr tree) 
                           factor)))))

(define t (list 1 
                  (list 2 (list 3 4) 5) 
                  (list 6 7)))

(scale-tree t 10)

(define (scale-tree tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree sub-tree factor)
             (* sub-tree factor)))
       tree))

;; Ex 2.30

(define (square x) (* x x))

(define (square-tree tree)
  (cond ((null? tree) '())
        ((pair? tree) (cons (square-tree (car tree))
                            (square-tree (cdr tree))))
        (else (square tree))))
(square-tree t)

;; Ex 2.31

(define (map-tree tree f)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (map-tree sub-tree f)
             (f sub-tree)))
       tree))

(define (square x) (* x x))

(define (square-tree tree)
  (map-tree tree square))
(square-tree t)

;; Ex 2.32

(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest 
                (map (lambda (subset) (cons (car s) subset)) 
                     rest)))))

(subsets (list 1 2 3))

;; 2.2.3

;; A design principle: the use of conventional interfaces

(define (filter pred lst)
  (cond ((null? lst) '())
        ((pred (car lst)) 
         (cons (car lst) (filter pred (cdr lst))))
        (else (filter pred (cdr lst)))))

(filter odd? (list 1 2 3 4 5))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op 
                      initial 
                      (cdr sequence)))))

(accumulate + 0 (list 1 2 3 4 5))
(accumulate cons '() (list 1 2 3 4 5))
(accumulate (lambda (x acc) (cons x acc)) '() (list 1 2 3 4 5))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low 
            (enumerate-interval 
             (+ low 1) 
             high))))

(enumerate-interval 1 5)

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append 
               (enumerate-tree (car tree))
               (enumerate-tree (cdr tree))))))

(define (sum-odd-squares tree)
  (accumulate + 0 (map square (filter odd? (enumerate-tree tree)))))

(define (map p sequence)
  (accumulate (lambda (x acc) (cons (p x) acc)) '() sequence))

(map (lambda (x) (+ x 1)) (list 1 2 3))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (foldr f acc coll)
  (if (null? coll)
    acc
    (f (car coll) (foldr f acc (cdr coll)))))

(define (append seq1 seq2)
  (foldr cons seq2 seq1))

(define (append seq1 seq2)
  (if (null? seq1)
    seq2
    (cons (car seq1) (append (cdr seq1) seq2))))

(append (list 1 2 3) (list 3 4 5))

(define (length sequence)
  (accumulate (lambda (x acc) (+ acc 1)) 0 sequence))

(length (list 1 2 3))

;; Ex 2.34

(define (horner-eval x coefficient-sequence)
  (accumulate 
   (lambda (this-coeff higher-terms)
     (+ this-coeff
        (* x higher-terms)))
   0
   coefficient-sequence))

(horner-eval 2 (list 1 3 0 5 0 1))

;; Ex 2.35

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(define (count-leaves t)
  (accumulate + 0 (map (lambda (x) 
                          (cond ((not (pair? x)) 1)
                                (else (count-leaves x))))
                       t)))

(count-leaves (list 1 3 0 (list 5 4) 0 1))

;; Ex 2.36

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define x (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))
(accumulate-n + 0 x)

; Ex 2.37

(define (dot-product v w)
  (accumulate + 0 (zipmap * v w)))

(define (zipmap f xs ys)
  (if (or (null? xs) (null? ys))
    '()
    (cons (f (car xs) (car ys))
          (zipmap f (cdr xs) (cdr ys)))))

(define v (list 1 2 3))
(define w (list 2 3 4))
(dot-product v w)

(define m (list v w v))

(define (matrix-*-vector m v)
  (map (lambda (r) (dot-product r v)) m))

(matrix-*-vector m v)

(accumulate cons '() (list 1 2 3))

(define (transpose mat)
  (accumulate-n cons '() mat))

(transpose m)

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (r) (matrix-*-vector cols r)) m)))

(matrix-*-matrix m m)

;; Ex 2.38

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op 
                      initial 
                      (cdr sequence)))))

(fold-right / 1 (list 1 2 3)) ;; 3/2
(fold-left  / 1 (list 1 2 3)) ;; 1/6
(fold-right list '() (list 1 2 3)) ;; (list 1 (list 2 (list 3 nil)))
(fold-left  list '() (list 1 2 3)) ;; (list (list (list (list nil 1) 2) 3) 4)

;; if (f x y) = (f y x), that is, f is commutative 
;; right fold         left fold
;; (f (f init x) y)  = (f (f init x) y)

;; Ex 2.39


(define (reverse xs)
  (fold-left (lambda (acc x) (cons x acc)) '() xs))

(define (reverse xs)
  (fold-right (lambda (x acc) (append acc (list x)))
    '() xs))

(define (reverse xs)
  (fold-right (lambda (x acc) (fold-right cons (list x) acc))
    '() xs))

(reverse (list 1 2 3))

;; Nested mappings

(define n 5)

(accumulate 
 append
 '()
 (map (lambda (i)
        (map (lambda (j) 
               (list i j))
             (enumerate-interval 1 (- i 1))))
      (enumerate-interval 1 n)))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (is-divisible-by n k)
  (= 0 (modulo n k)))

(is-divisible-by 25 5)

(define (prime? n)
  (define (loop k)
    (cond ((< (/ n 2) k) #t)
          ((is-divisible-by n k) #f)
          (else (loop (+ 1 k)))))
  (loop 2))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair)
        (cadr pair)
        (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum 
    (filter prime-sum?
      (flatmap (lambda (i) 
              (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
          (enumerate-interval 1 n)))))

(define (permutations s)
  (if (null? s)   ; empty set?
      (list '())  ; sequence containing empty set
      (flatmap (lambda (x)
                 (map (lambda (p) 
                        (cons x p))
                      (permutations 
                       (remove x s))))
               s)))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))

;; Ex 2.40


(define (unique-pairs n)
  (flatmap (lambda (i)
          (map (lambda (j)
                  (list i j))
               (enumerate-interval 1 i)))
    (enumerate-interval 1 n)))

(unique-pairs 5)
(prime-sum-pairs 5)

(define (prime-sum-pairs n)
  (map make-pair-sum 
    (filter prime-sum?
      (unique-pairs n))))

;; Ex 2.41

(define (unique-triples n)
  (flatmap (lambda (i)
            (flatmap (lambda (j)
                    (map (lambda (k)
                            (list i j k))
                         (enumerate-interval 1 (- j 1))))
                (enumerate-interval 1 (- i 1))))
    (enumerate-interval 1 n)))
(unique-triples 4)

(define (triple-sums n s)
  (filter (lambda (triple) (= s (+ (car triple)
                                (cadr triple)
                                (car (cdr (cdr triple))))))
         (unique-triples n)))

(triple-sums 5 10)

;; Ex 2.42

(define empty-board '())

(define (position row col)
  (list row col))

(define pos-row car)
(define pos-col cadr)

(define (safe-from-position a b)
  (not (or (= (pos-row a) (pos-row b))
           (= (abs (- (pos-row a) (pos-row b)))
              (abs (- (pos-col a) (pos-col b)))))))

(define (safe? k-col positions) ;; Weird because k is not needed
  (let ((kth-position (car positions)))
    (define (loop positions-left)
      (if (null? positions-left)
        #t
        (and (safe-from-position kth-position (car positions-left)) 
             (loop (cdr positions-left)))))
    (loop (cdr positions))))

(safe? 1 (list (position 1 1) (position 2 2)))

(define (adjoin-position new-row k-col rest-of-queens)
  (cons (position new-row k-col) rest-of-queens))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) 
           (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position 
                    new-row 
                    k 
                    rest-of-queens))
                 (enumerate-interval 
                  1 
                  board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(queens 8)

;; 

;; Ex 2.43

;; 8^8  / 8! = 416!

;; 2.2.4

;; Ex 2.44


(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter 
                                  (- n 1))))
        (beside painter 
                (below smaller smaller)))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter 
                               (n - 1))))
        (below (beside smaller smaller)
               painter))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter 
                                (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right 
                                   right))
              (corner (corner-split painter 
                                    (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right 
                         corner))))))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) 
                        quarter)))
      (below (flip-vert half) half))))

(define (flipped-pairs painter)
  (let ((painter2 
         (beside painter 
                 (flip-vert painter))))
    (below painter2 painter2)))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) 
                       (tr painter)))
          (bottom (beside (bl painter) 
                          (br painter))))
      (below bottom top))))

(define (flipped-pairs painter)
  (let ((combine4 
         (square-of-four identity 
                         flip-vert
                         identity 
                         flip-vert)))
    (combine4 painter)))

(define (square-limit painter n)
  (let ((combine4 
         (square-of-four flip-horiz 
                         identity
                         rotate180 
                         flip-vert)))
    (combine4 (corner-split painter n))))

;; Ex 2.45

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter 
                                  (- n 1))))
        (beside painter 
                (below smaller smaller)))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter 
                               (- n 1))))
        (below painter 
               (beside smaller smaller)))))

(define (split func-combine func-small)
  (define (helper painter n) 
    (if (= n 0)
        painter
        (let ((smaller (helper painter 
                               (- n 1))))
          (func-combine painter 
             (func-small smaller smaller)))))
  helper)

(define right-split (split beside below))
(define up-split (split below beside))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect 
      (scale-vect (xcor-vect v)
                  (edge1-frame frame))
      (scale-vect (ycor-vect v)
                  (edge2-frame frame))))))

;; Ex 2.46

(define (make-vect x y)
  (list x y))

(define xcor-vect car)
(define ycor-vect cadr)

(define ex (make-vect 1 2))
(ycor-vect ex)

(define (add-vect one two)
  (make-vect (+ (xcor-vect one)
                (xcor-vect two))
             (+ (ycor-vect one)
                (ycor-vect two))))

(add-vect ex ex)

(define (sub-vect one two)
  (make-vect (- (xcor-vect one)
                (xcor-vect two))
             (- (ycor-vect one)
                (ycor-vect two))))

(define (scale-vect vector s)
  (make-vect (* s (xcor-vect vector))
             (* s (ycor-vect vector))))

(scale-vect v 2)

;; Ex 2.47

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (cadr frame))

(define (edge2-frame frame)
  (car (cdr (cdr frame))))

(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (car (cdr frame)))

(define (edge2-frame frame)
  (cdr (cdr frame)))

(make-frame (make-vect 3 4) ex (make-vect 5 6))
(define fr (make-frame (make-vect 3 4) ex (make-vect 5 6)))

(origin-frame fr)
(edge1-frame fr)
(edge2-frame fr)

;; Painters

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) 
         (start-segment segment))
        ((frame-coord-map frame) 
         (end-segment segment))))
     segment-list)))

;; Ex 2.48

(define (make-segment start end)
  (list start end))

(define start-segment car)
(define end-segment cadr)

;; Ex 2.49

; 1. 

(define (outline-painter frame)
  (segments->painter (list (make-segment (make-vector 0 0) (make-vector 0 1))
                           (make-segment (make-vector 0 1) (make-vector 1 1))
                           (make-segment (make-vector 1 1) (make-vector 1 0))
                           (make-segment (make-vector 1 0) (make-vector 0 0)))))

(define (x-painter frame)
  (segments->painter (list (make-segment (make-vector 0 0) (make-vector 1 1))
                           (make-segment (make-vector 0 1) (make-vector 1 0)))))

(define (diamond-painter frame)
  (segments->painter (list (make-segment (make-vector 0 0.5) (make-vector 0.5 1))
                           (make-segment (make-vector 0.5 1) (make-vector 1 0.5))
                           (make-segment (make-vector 1 0.5) (make-vector 0.5 0))
                           (make-segment (make-vector 0.5 0) (make-vector 0 0.5)))))

(define (transform-painter 
         painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter (make-frame new-origin
                  (sub-vect (m corner1) 
                            new-origin)
                  (sub-vect (m corner2)
                            new-origin)))))))

(define (flip-vert painter)
  (transform-painter 
   painter
   (make-vect 0.0 1.0)   ; new origin
   (make-vect 1.0 1.0)   ; new end of edge1
   (make-vect 0.0 0.0))) ; new end of edge2

(define (shrink-to-upper-right painter)
  (transform-painter painter
                     (make-vect 0.5 0.5)
                     (make-vect 1.0 0.5)
                     (make-vect 0.5 1.0)))

(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (squash-inwards painter)
  (transform-painter painter
                     (make-vect 0.0 0.0)
                     (make-vect 0.65 0.35)
                     (make-vect 0.35 0.65)))

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left  (transform-painter 
                        painter1
                        (make-vect 0.0 0.0)
                        split-point
                        (make-vect 0.0 1.0)))
          (paint-right (transform-painter
                        painter2
                        split-point
                        (make-vect 1.0 0.0)
                        (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

(define (flip-horiz painter)
  (transform-painter painter 
                     (make-vect 1.0 0.0)   ; new origin
                     (make-vect 0.0 0.0)   ; new end of edge1
                     (make-vect 1.0 1.0))) ; new end of edge2

(define (rotate180 painter)
  (transform-painter painter 
                     (make-vect 1.0 1.0)   ; new origin
                     (make-vect 0.0 1.0)   ; new end of edge1
                     (make-vect 0.0 1.0))) ; new end of edge2

(define (rotate270 painter)
  (transform-painter painter 
                     (make-vect 1.0 0.0)   ; new origin
                     (make-vect 1.0 1.0)   ; new end of edge1
                     (make-vect 0.0 1.0))) ; new end of edge2

;; Ex 2.51

(define (below low high)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-low (transform-painter
                       low 
                       (make-vect 0.0 0.0)
                       (make-vect 1.0 0.0)
                       split-point))
          (paint-high (transform-painter
                       high
                       split-point
                       (make-vect 1.0 0.5)
                       (make-vect 1.0 1.0))))
      (lambda (frame)
        (paint-low frame)
        (paint-high frame)))))

