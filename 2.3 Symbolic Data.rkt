#lang sicp

; 2.3.1 
(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

(memq 'apple '(pear banana prune))

(memq 'apple '(x (apple sauce) y apple pear))

(list 'a 'b 'c) ;; 
(list (list 'george))
(cdr '((x1 x2) (y1 y2)))
(cadr '((x1 x2) (y1 y2)))
(pair? (car '(a short list)))
(memq 'red '((red shoes) (blue socks)))
(memq 'red '(red shoes blue socks))

;; Ex 2.54

(equal? '(this is a list) 
        '(this is a list))

(equal? '(1 2 3) '(1 2 3 4))
(eq? 'a 'a)


(define (equal-lists? list1 list2)
  (cond ((and (null? list1) (null? list2))
         #t)
        ((or (null? list1) (null? list2))
         #f)
        (else (and (eq? (car list1) (car list2))
                   (equal-lists? (cdr list1) (cdr list2))))))

(define (equal? a b)
  (if (or (list? a) (list? b))
       (equal-lists? a b)
       (eq? a b)))

(and #t #t #f)
(eq? 'a '(1 2 3))

(symbol? 'x)
(symbol? '(1))

(equal? 1 1)
(equal? 'a 'b)
(equal? 'a 'a)
(equal? '(1 2 3) '(1 2 3))
(equal? '(1 2 4) '(1 2 3))
(equal? '(1 2 4) '(1 2 4 1))

;; Ex 2.55

(car (quote (quote abc))) ; 'quote
(cdr (quote (quote abc))) ; '(abc)

; 2.3.2

;; (variable? e)          Is e a variable?
;; (same-variable? v1 v2) Are v1 and v2 the same variable?
;; (sum? e)               Is e a sum?
;; (addend e)             Addend of the sum e.
;; (augend e)             Augend of the sum e.
;; (make-sum a1 a2)       Construct the sum of a1 and a2.
;; (product? e)           Is e a product?
;; (multiplier e)         Multiplier of the product e.
;; (multiplicand e)       Multiplicand of the product e.
;; (make-product m1 m2)   Construct the product of m1 and m2.

(number? 1)

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product 
           (multiplier exp)
           (deriv (multiplicand exp) var))
          (make-product 
           (deriv (multiplier exp) var)
           (multiplicand exp))))
        (else (error "unknown expression 
                      type: DERIV" exp))))

;; Representing algebraic expressions

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

(define (make-sum a1 a2)
  (list '+ a1 a2))

(define (make-product m1 m2)
  (list '* m1 m2))

(define (sum? x)
  (and (pair? x) 
       (eq? (car x) '+)))

(define (addend s) (cadr s))
(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x)
       (eq? (car x) '*)))

(define (multiplicand p) (caddr p))
(define (multiplier p) (cadr p))

(deriv '(+ x 3) 'x)

(deriv '(* x y) 'x)

;; In order to simplify expressions, we can change make-sum to always return the most simplified form.

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list '+ a1 a2))))
    
(make-sum 'a 5)

(define (make-product m1 m2)
  (cond 
        ((=number? m1 0) 0)
        ((=number? m2 0) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2))
         (* m1 m2))
        (else (list '* m1 m2))))

(make-product 2 3)
(make-product 'a 3)
(make-product 'a 0)

(deriv '(+ x 3) 'x)
(deriv '(* x y) 'x)
(deriv '(* (* x y) (+ x 3)) 'x)

;; Ex 2.56

(define (make-expt b p)
  (cond ((=number? b 1) 1)
        ((=number? p 0) 1)
        ((=number? p 1) b)
        (else (list 'expt b p))))

(make-expt 1 2)

(define (expt? exp)
  (and (pair? exp)
       (eq? (car exp) 'expt)))

(expt? (make-expt 1 2))
(expt? '(expt 1 2))
(expt? '(* 1 2))

(define (base exp) (cadr exp))
(define (power exp) (caddr exp))

(base '(expt 1 2))
(power '(expt 1 2))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product 
           (multiplier exp)
           (deriv (multiplicand exp) var))
          (make-product 
           (deriv (multiplier exp) var)
           (multiplicand exp))))
        ((expt? exp)
         (make-product
            (power exp)
            (make-product 
              (make-expt (base exp) (- (power exp) 1))
              (deriv (base exp) var))))
        (else (error "unknown expression 
                      type: DERIV" exp))))

(deriv '(expt x 2) 'x)

;; Ex 2.57

(define (make-sum a1 a2)
  (cond ((null? a2) a1)
        ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list '+ a1 a2))))

(define (addend s) (cadr s))
(define (augend s) 
  (define (sum-terms terms)
    (if (null? (cdr terms))
        (car terms)
        (make-sum (car terms)
                  (sum-terms (cdr terms)))))
  (sum-terms (cddr s)))

(define (multiplicand p) (caddr p))

(define (multiplier p) 
  (define (multiply-terms terms)
    (if (null? (cdr terms))
        (car terms)
        (make-product (car terms) 
                  (multiply-terms (cdr terms)))))
  (multiply-terms (cddr p)))

(multiplier (list * 1 2 3 'x))

(deriv '(* x y (+ x 3)) 'x)

;; Ex 2.58

; part 1

(define (make-sum a1 a2)
  (cond ((null? a2) a1)
        ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list a1 '+ a2))))

(define (sum? s)
  (and (pair? s)
       (eq? (cadr s) '+)))
      
(sum? (make-sum 'x 'y))
(sum? '(x + y))

(define (make-product m1 m2)
  (cond 
        ((=number? m1 0) 0)
        ((=number? m2 0) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2))
         (* m1 m2))
        (else (list m1 '* m2))))

(define (product? s)
  (and (pair? s)
       (eq? (cadr s) '*)))

(product? (make-product 'x 'y))
(product? '(x * y))

(define (addend s) (car s))
(define (augend s) (caddr s))

(augend (make-sum 'x 'y))

(define (multiplicand s) (car s))
(define (multiplier s) (caddr s))

(deriv '(x * y) 'x)

; part 2

; (x + y * z) -> (x + (y * z))

(define (addend s) (car s))
(define (augend s) 
  (define (sum-terms terms)
    (if (null? (cdr terms))
        (car terms)
        terms))
  (sum-terms (cddr s)))

(augend '(x + y))
(augend '(x + y * z))

(define (multiplicand s) (car s))
(define (multiplier s) (caddr s))

(define (multiplier s) 
  (define (multiply-terms terms)
    (if (null? (cdr terms))
      (car terms)
      terms))
  (multiply-terms (cddr s)))

(multiplier '(x * y))
(multiplier '(x * y * z))

(deriv '(x * y) 'x)
(deriv '(x + 3 * (x + y + 2)) 'x)
(deriv '(x + (2 * (3 * x))) 'x)

; 2.3.3 representing sets

(define (element-of-set? x s)
  (cond ((null? s) #f)
        ((eq? x (car s)) #t)
        (else (element-of-set? x (cdr s)))))

(element-of-set? 'x '(x y z))
(element-of-set? 'x (cdr '(x y z)))

(define (adjoin-set x set)
  (if (element-of-set? x set)
    set
    (cons x set)))

(define (intersection-set set1 set2)
  (define (loop acc set1-remaining)
    (cond ((null? set1-remaining) acc)
          ((element-of-set? (car set1-remaining) acc) 
           (loop (adjoin-set (car set1-remaining) acc) 
                 (cdr set1-remaining)))
          (else 
           (loop acc (cdr set1-remaining)))))
  (loop set2 set1))

(intersection-set '(1 2 3) '(2 3))

;; Ex 2.59

(define (union-set set1 set2)
  (define (loop acc set1-remaining)
    (if (null? set1-remaining)
        acc
        (loop (adjoin-set (car set1-remaining) acc) 
              (cdr set1-remaining))))
  (loop set2 set1))

(union-set '(1 2 5) '(1 4 2 3))

;; Ex 2.60

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(element-of-set? 'x '(x y x z))

(define (adjoin-set x set)
  (cons x set))

(define (intersection-set set1 set2)
  (define (loop acc set1-remaining)
    (cond ((null? set1-remaining) acc)
          ((element-of-set? (car set1-remaining) acc) 
           (loop (adjoin-set (car set1-remaining) acc) 
                 (cdr set1-remaining)))
          (else 
           (loop acc (cdr set1-remaining)))))
  (loop set2 set1))

(define (union-set set1 set2)
  (define (loop acc set1-remaining)
    (if (null? set1-remaining)
        acc
        (loop (adjoin-set (car set1-remaining) acc) 
              (cdr set1-remaining))))
  (loop set2 set1))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-set 
                         (cdr set1)
                         (cdr set2))))
              ((< x1 x2) (intersection-set 
                          (cdr set1) 
                          set2))
              ((< x2 x1) (intersection-set 
                          set1 
                          (cdr set2)))))))

;; Ex 2.61

(define (fold-left f init coll)
  (if (null? coll)
      init
      (fold-left f (f init (car coll)) (cdr coll))))

(fold-left (lambda (acc x) (cons x acc)) '() '(1 2 3))

(define (adjoin-set x set)
  (define (loop searched remaining)
    (cond ((null? remaining) (fold-left (lambda (acc x) (cons x acc)) '(x) searched))
          ((> (car remaining) x) (fold-left (lambda (acc x) (cons x acc)) (cons x remaining) searched))
          ((< (car remaining) x) 
           (loop (cons (car remaining) searched) (cdr remaining)))
          ((= (car remaining) x) set)))
  (loop '() set))

(adjoin-set 0 '(1 2 4))

(define (union-set set1 set2)
  (define (loop acc set1-remaining)
    (if (null? set1-remaining)
        acc
        (loop (adjoin-set (car set1-remaining) acc) 
              (cdr set1-remaining))))
  (loop set2 set1))

(define (reverse xs)
  (fold-left (lambda (acc x) (cons x acc)) 
              '()
              xs))

(reverse '(1 2 3))

;; Tail recursive
(define (union-set set1 set2)
  (define (loop acc set1-remaining set2-remaining)
    (cond ((null? set1-remaining) (fold-left (lambda (acc x) (cons x acc)) acc set2-remaining))
          ((null? set2-remaining) (fold-left (lambda (acc x) (cons x acc)) acc set1-remaining))
          (else 
            (let ((x1 (car set1-remaining))
                  (x2 (car set2-remaining)))
              (cond 
                    ((= x1 x2)
                    (loop (cons x1 acc) (cdr set1-remaining) (cdr set2-remaining)))
                    ((< x1 x2)
                    (loop (cons x1 acc) (cdr set1-remaining) set2-remaining))
                    ((< x2 x1)
                    (loop (cons x2 acc) set1-remaining (cdr set2-remaining))))))))
  (reverse (loop '() set1 set2)))

;; Not tail recursive
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else 
          (let ((x1 (car set1))
                (x2 (car set2)))
            (cond 
                  ((= x1 x2)
                   (cons x1 (union-set (cdr set1) (cdr set2))))
                  ((< x1 x2)
                   (cons x1 (union-set (cdr set1) set2)))
                  ((< x2 x1)
                   (cons x2 (union-set set1 (cdr set2)))))))))

(union-set '(1 2 3) '(1 2 3 4))
(union-set '(1 2 3 4) '(1 2 3))

;; Ex 

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? 
          x 
          (left-branch set)))
        ((> x (entry set))
         (element-of-set? 
          x 
          (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree 
          (entry set)
          (adjoin-set x (left-branch set))
          (right-branch set)))
        ((> x (entry set))
         (make-tree
          (entry set)
          (left-branch set)
          (adjoin-set x (right-branch set))))))

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append 
       (tree->list-1 
        (left-branch tree))
       (cons (entry tree)
             (tree->list-1 
              (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list 
         (left-branch tree)
         (cons (entry tree)
               (copy-to-list 
                (right-branch tree)
                result-list)))))
  (copy-to-list tree '()))

(define (make-leaf x) (make-tree x '() '()))

(define a-tree 
  (make-tree 7 (make-tree 3 (make-leaf 1) (make-leaf 5))
               (make-tree 9 '() (make-leaf 11))))

(define b-tree 
  (make-tree 3 (make-leaf 1)
               (make-tree 7 (make-leaf 5) (make-tree 9 '() (make-leaf 11)))))

(tree->list-2 a-tree)
(tree->list-2 b-tree)

; part 2

; Ex 2.64

(define (list->tree elements)
  (car (partial-tree
        elements (length elements)))) ;; elements and gets the first n elements of the list

(define (partial-tree elts n)
  (if (= n 0) ;; base case: the partial tree is '(), and the elts are untouched.
      (cons '() elts)
      (let ((left-size 
             (quotient (- n 1) 2))) ;; Divide the number of elements to make into a tree in half.
        (let ((left-result 
               (partial-tree 
                elts left-size))) ;; Take the number of elements that should be in the left side and make a binary tree out of it.
          (let ((left-tree 
                 (car left-result))
                (non-left-elts 
                 (cdr left-result))
                (right-size 
                 (- n (+ left-size 1)))) ;; We know how many elements should be on the right side too (about half)
            (let ((this-entry 
                   (car non-left-elts)) ;; The entry is the first element that is not in the left side
                  (right-result 
                   (partial-tree 
                    (cdr non-left-elts) ;; Turn the right side elements into a tree
                    right-size)))
              (let ((right-tree 
                     (car right-result))
                    (remaining-elts 
                     (cdr right-result)))
                (cons (make-tree this-entry 
                                 left-tree  ;; Already sorted tree
                                 right-tree) ;; Already sorted tree
                      remaining-elts))))))))

(list->tree '(1 3 5 7 9 11))

;; Ex 2.65

(define (union-set-list set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else 
          (let ((x1 (car set1))
                (x2 (car set2)))
            (cond 
                  ((= x1 x2)
                   (cons x1 (union-set-list (cdr set1) (cdr set2))))
                  ((< x1 x2)
                   (cons x1 (union-set-list (cdr set1) set2)))
                  ((< x2 x1)
                   (cons x2 (union-set-list set1 (cdr set2)))))))))

(define (union-set set1 set2)
  (let ((list1 (tree->list-2 set1))
        (list2 (tree->list-2 set2)))
    (let ((unioned-list (union-set-list list1 list2)))
      (list->tree unioned-list))))

(define (adjoin-set-list x set)
  (define (loop searched remaining)
    (cond ((null? remaining) (fold-left (lambda (acc x) (cons x acc)) '(x) searched))
          ((> (car remaining) x) (fold-left (lambda (acc x) (cons x acc)) (cons x remaining) searched))
          ((< (car remaining) x) 
           (loop (cons (car remaining) searched) (cdr remaining)))
          ((= (car remaining) x) set)))
  (loop '() set))

(define (adjoin-set x set)
  (let ((set-list (tree->list-2 set)))
    (list->tree (adjoin-set-list x set-list))))

(define (intersection-set-list set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-set-list 
                         (cdr set1)
                         (cdr set2))))
              ((< x1 x2) (intersection-set-list 
                          (cdr set1) 
                          set2))
              ((< x2 x1) (intersection-set-list 
                          set1 
                          (cdr set2)))))))

(define (intersection-set set1 set2)
  (let ((list1 (tree->list-2 set1))
        (list2 (tree->list-2 set2)))
    (let ((intersection-list (intersection-set-list list1 list2)))
      (list->tree intersection-list))))
    
(define t (list->tree '(1 3 5 7 9 11)))

(define a-tree 
  (make-tree 7 (make-tree 3 '() (make-leaf 5))
               (make-tree 9 '() (make-leaf 11))))

(define b-tree 
  (make-tree 3 (make-leaf 1)
               (make-tree 7 (make-leaf 5) (make-tree 9 '() (make-leaf 11)))))

(list->tree '(1 3 5 6))
(adjoin-set-list 4 '(1 3 5 6))
(list->tree '(1 3 5 6))
(adjoin-set 4 b-tree)
a-tree
(adjoin-set 4 a-tree)
(adjoin-set 5 b-tree)

(union-set a-tree b-tree)
(intersection-set a-tree b-tree)
a-tree
b-tree

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((equal? given-key 
                 (key (car set-of-records)))
         (car set-of-records))
        (else 
         (lookup given-key 
                 (cdr set-of-records)))))

;; Ex 2.66

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) #f)
        ((- given-key (entry set-of-records))
         #t)
        ((< given-key (entry set-of-records))
         (lookup given-key (left-branch set-of-records)))
        ((> given-key (entry set-of-records))
         (lookup given-key (right-branch set-of-records)))))

;; Section 2.3.4: TODO practice

;; Representing Leaves

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

;; Representing Huffman trees

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) 
                (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

;; The decoding procedure

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch 
                (car bits) 
                current-branch)))
          (if (leaf? next-branch)
              (cons 
               (symbol-leaf next-branch)
               (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) 
                        next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: 
               CHOOSE-BRANCH" bit))))

;; Sets of weighted elements

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) 
         (cons x set))
        (else 
         (cons (car set)
               (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set 
         (make-leaf (car pair)    ; symbol
                    (cadr pair))  ; frequency
         (make-leaf-set (cdr pairs))))))

;; Ex 2.67

(define sample-tree
  (make-code-tree 
   (make-leaf 'A 4)
   (make-code-tree
    (make-leaf 'B 2)
    (make-code-tree 
     (make-leaf 'D 1)
     (make-leaf 'C 1)))))

(define sample-message 
  '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(decode sample-message sample-tree)

;; Ex 2.68

(define (encode message tree)
  (if (null? message)
      '()
      (append 
       (encode-symbol (car message) 
                      tree)
       (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (if (leaf? tree)
    (if (eq? symbol (symbol-leaf tree))
      '()
      #f)
    (let ((left-encoding (encode-symbol symbol (left-branch tree)))
          (right-encoding (encode-symbol symbol (right-branch tree))))
        (if left-encoding
            (cons 0 left-encoding)
            (if right-encoding
              (cons 1 right-encoding)
              #f)))))

(encode-symbol 'A sample-tree)
(encode-symbol 'B sample-tree)
(encode-symbol 'C sample-tree)
(encode-symbol 'D sample-tree)

;; Ex 2.69

(define (generate-huffman-tree pairs)
  (successive-merge 
   (make-leaf-set pairs)))

(make-leaf-set '((A 4) (B 2) (C 1) (D 1)))

(define (successive-merge wip)
  (if (null? (cdr wip))
    ;; there is only one node left, the tree
    (car wip)
    ;; remove the two smallest elements from the leaf-set
    (let ((next-small-tree (make-code-tree (car wip) (cadr wip))))
      ;; put the result back in the wip
      (successive-merge (adjoin-set next-small-tree (cddr wip))))))

(define sample-tree2 (successive-merge (make-leaf-set '((A 4) (B 2) (C 1) (D 1)))))

sample-tree
sample-tree2

(decode sample-message sample-tree2)
(decode sample-message sample-tree)

;; Ex 2.70

(define leaf-set 
  '((A 2)
    (BOOM 1)
    (GET 2)
    (JOB 2)
    (NA 16)
    (SHA 3)
    (YIP 9)
    (WAH 1)))

(define rock-huffman-tree (generate-huffman-tree leaf-set))

rock-huffman-tree
sample-tree

(define (apply fun args)
   (eval (cons fun args)))

(apply + (list (count (encode '(GET A JOB) rock-huffman-tree))
          (count (encode '(SHA NA NA NA NA NA NA NA NA) rock-huffman-tree))
          (count (encode '(GET A JOB) rock-huffman-tree))
          (count (encode '(SHA NA NA NA NA NA NA NA NA) rock-huffman-tree))
          (count (encode '(WAH YIP YIP YIP YIP) rock-huffman-tree))
          (count (encode '(YIP YIP YIP YIP YIP) rock-huffman-tree))
          (count (encode '(SHA BOOM) rock-huffman-tree))))
; => 84

(define (count xs)
  (if (null? xs)
    0
    (+ 1 (count (cdr xs)))))

;; Each word would be 3 bits

(* 3
(apply + 
  (map count 
    (list 
      '(GET A JOB)
      '(SHA NA NA NA NA NA NA NA NA)
      '(GET A JOB)
      '(SHA NA NA NA NA NA NA NA NA)
      '(WAH YIP YIP YIP YIP)
      '(YIP YIP YIP YIP YIP)
      '(SHA BOOM)))))
; => 108

;; Ex 2.71

;; Skipping for now.

;; Ex 2.72

;; Skipping for now.