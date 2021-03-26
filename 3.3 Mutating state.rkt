#lang sicp

;; First of all, let's create a cycle.

(define x (list 1 2 3))

(define (iterate f stopping-condition)
  (define (loop x)
    (if (stopping-condition x)
      '()
      (cons (f x) (loop (f x)))))
  loop)

(define (iterate-n f stopping-condition max-n)
  (define (loop max-n x)
    (if (or (stopping-condition x)
            (= 0 max-n))
      '()
      (cons (f x) (loop (- max-n 1) (f x)))))
  (lambda (x)
    (loop max-n x)))

(define x (list 1 2 3))

(define (last-pair x)
  (if (null? (cdr x))
    x
    (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define x (list 1 2 3))
(define cycle-x (make-cycle x))

(contains-cycle? x)

(cdr x)

(cdr x)

;; Will create an infinite loop
;; ((iterate cdr null?) x)
(null? (cdr (cdr (cdr x))))

((iterate-n cdr null? 10) x)

cycle-x

(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

(define v '(1 2 3 4 5))
(define w (mystery v))

;; Ex 3.17

(define (contains? set x)
  (if (null? set)
    #f
    (or (eq? (car set) x)
        (contains? (cdr set) x))))

(define x (cons 1 2))
(define y (cons 1 2))
(define z (cons 2 x))
(contains? (list x x x) x)
(contains? (list y x y) x)

(define (count-pairs x)
  (define (loop count visited stack)
    (if (null? stack)
      count
      (let ((popped (car stack)))
        (if (not (pair? popped))
          (loop count visited (cdr stack))
          (if (contains? visited popped)
            (loop count visited (cdr stack))
            (loop (+ 1 count) (cons popped visited) (cons (cdr popped) (cdr stack))))))))
  (loop 0 '() (list x)))

(count-pairs (list x y z))

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

(count-pairs (list x y z))

;; Ex 3.18

(define (contains-cycle? x)
  (define (loop remaining visited)
    (cond ((not (pair? remaining)) #f)
          ((memq remaining visited) #t)
          (else (or (loop (car remaining) (cons remaining visited))
                    (loop (cdr remaining) (cons remaining visited))))))
  (loop x '()))

(contains-cycle? (list x y z))
(contains-cycle? (list x y x))

 (define (cycle? l)
   (let ((n (count-pairs l)))
     (define (iter items i)
       (cond ((null? items) #f)
             ((> i n) #t)
             (else (iter (cdr items) (+ i 1)))))
     (iter l 0)))

(set-cdr! z x)

(cycle? (list x y z))
(cycle? (list x y x))

;; Ex 3.19

;; Turtoise and the hare solution

(define (has-cycle? x)
  (define (iter slow fast)
    (cond ((eq? slow fast) #t)
          ((null? (cdr fast)) #f) ;; we have iterated through the list
          ((null? (cddr fast)) #f) ;; we have iterated through the list
          (else (iter (cdr slow) (cddr fast)))))
  (iter x (cdr x)))

(has-cycle? x)
(has-cycle? (list 1 2 3))

;; Mutation is just assignment

;; 3.3.2 Representing Queues

;; Outline queue interface

(define (make-queue))
(define (empty-queue? queue))
(define (front-queue queue))
(define (insert-queue! queue item))
(define (delete-queue! queue))

;; Queue implementation with mutable pairs

(define (make-queue) (cons '() '()))
(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item)
  (set-car! queue item))
(define (set-rear-ptr! queue item)
  (set-cdr! queue item))

;; Implement the queue interface, without knowing that the queue is a list.

(define (empty-queue? queue)
  (null? (front-ptr queue)))

(define (front-queue queue)
  (if (empty-queue? queue)
    (error "FRONT called with empty queue" queue)
    (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (if (empty-queue? queue)
        (begin
          (set-front-ptr! queue new-pair)
          (set-rear-ptr! queue new-pair)
          queue)
        (begin
          (set-cdr! (rear-ptr queue) new-pair)
          (set-rear-ptr! queue new-pair)
          queue))))

(define (delete-queue! queue)
  (if (empty-queue? queue)
    (error "DELETE called with empty queue" queue)
    (begin
     (set-front-ptr! queue (cdr (front-ptr queue)))
     (if (null? (front-ptr queue))
      (set-rear-ptr! queue '()))
      queue)))

(define q (make-queue))
q
(empty-queue? q)
(insert-queue! q 1)
(insert-queue! q 2)
(delete-queue! q)
(delete-queue! q)

(cdr (front-ptr q))


;; Ex 3.21

(define (print-queue queue)
  (front-ptr queue))

(define q (make-queue))
q
(empty-queue? q)
(insert-queue! q 1)
(insert-queue! q 2)
(print-queue q)

;; Ex 3.22

(define (empty-queue? queue))
(define (front-queue queue))
(define (insert-queue! queue item))
(define (delete-queue! queue))

;; Queue implementation with mutable pairs

(define (make-queue) (cons '() '()))
(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item)
  (set-car! queue item))
(define (set-rear-ptr! queue item)
  (set-cdr! queue item))

  ;; Implementation starts here

(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (set-front-ptr! x) (set! front-ptr x))
    (define (set-rear-ptr! x) (set! rear-ptr x))
    (define (dispatch m)
      (cond ((eq? m 'set-front-ptr!) set-front-ptr!)
            ((eq? m 'set-rear-ptr!) set-rear-ptr!)
            ((eq? m 'front-ptr) front-ptr)
            ((eq? m 'rear-ptr) rear-ptr)
            (else (error "Undefined operation" m))))
    dispatch))

(define (front-ptr queue) (queue 'front-ptr))
(define (rear-ptr queue) (queue 'rear-ptr))
(define (set-front-ptr! queue item)
  ((queue 'set-front-ptr!) item))
(define (set-rear-ptr! queue item)
  ((queue 'set-rear-ptr!) item))

(define q (make-queue))
(front-ptr q)
(rear-ptr q)
(empty-queue? q)
(insert-queue! q 1)
(front-queue q)
(delete-queue! q)

(define (empty-queue? queue)
  (null? (front-ptr queue)))

(define (front-queue queue)
  (if (empty-queue? queue)
    (error "FRONT called with empty queue" queue)
    (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (if (empty-queue? queue)
        (begin
          (set-front-ptr! queue new-pair)
          (set-rear-ptr! queue new-pair)
          queue)
        (begin
          (set-cdr! (rear-ptr queue) new-pair)
          (set-rear-ptr! queue new-pair)
          queue))))

(define (delete-queue! queue)
  (if (empty-queue? queue)
    (error "DELETE called with empty queue" queue)
    (begin
     (set-front-ptr! queue (cdr (front-ptr queue)))
     (if (null? (front-ptr queue))
      (set-rear-ptr! queue '()))
      queue)))

;; Ex 3.23

;; From before
(define (make-deque) (cons '() '()))
(define (front-ptr deque) (car deque))
(define (rear-ptr deque) (cdr deque))
(define (set-front-ptr! deque item)
  (set-car! deque item))
(define (set-rear-ptr! deque item)
  (set-cdr! deque item))

(define (empty-deque? deque)
  (null? (front-ptr deque)))

(define (front-deque deque)
  (if (empty-deque? deque)
    (error "FRONT called on empty deque" deque))
    (car (front-ptr deque)))

(define (rear-deque deque)
  (if (empty-deque? deque)
    (error "REAR called on empty deque" deque))
    (car (rear-ptr deque)))

(cons 1 '())
(define t (cons 1 (cons '() '())))
t
(set-next! t '())
(cdr (cdr (list 1 '())))

(define (next triplet) (cadr triplet))
(define (previous triplet) (cddr triplet))

(define (set-next! triplet x)
  (set-car! (cdr triplet) x)
  triplet)

(define (set-previous! triplet x)
  (set-cdr! (cdr triplet) x)
  triplet)

(define (front-insert-deque! deque item)
  (let ((new-triplet (cons item (cons '() '()))))
    (if (empty-deque? deque)
      (begin
        (set-front-ptr! deque new-triplet)
        (set-rear-ptr! deque new-triplet)
        deque)
      (begin
        (set-previous! (front-ptr deque) new-triplet)
        (set-next! new-triplet (front-ptr deque))
        (set-front-ptr! deque new-triplet)
        deque))))

(define (rear-insert-deque! deque item)
  (let ((new-triplet (cons item (cons '() '()))))
    (if (empty-deque? deque)
      (begin
        (set-front-ptr! deque new-triplet)
        (set-rear-ptr! deque new-triplet)
        deque)
      (begin
        (set-next! (rear-ptr deque) new-triplet)
        (set-previous! new-triplet (rear-ptr deque))
        (set-rear-ptr! deque new-triplet)
        deque))))

(define (front-delete-deque! deque)
  (if (empty-deque? deque)
    (error "FRONT DELETE called on empty deque" deque)
    (begin
      (set-front-ptr! deque (next (front-ptr deque)))
      (if (null? (front-ptr deque))
        (set-rear-ptr! deque '())
        (set-previous! (front-ptr deque) '()))
      deque)))

(define (rear-delete-deque! deque)
  (if (empty-deque? deque)
    (error "FRONT DELETE called on empty deque" deque)
    (begin
      (set-rear-ptr! deque (previous (rear-ptr deque)))
      (if (null? (rear-ptr deque))
        (set-front-ptr! deque '())
        (set-next! (rear-ptr deque) '()))
      deque)))

(empty-deque? '())
(set-previous! '())

(define q (make-deque))
(front-ptr q)
(rear-ptr q)
(empty-queue? q)
(front-insert-deque! q 1)
(front-insert-deque! q 2)
(front-ptr q)
(front-deque q)
(front-delete-deque! q)
(rear-insert-deque! q 3)
q
(front-deque q)
(rear-deque q)
(rear-delete-deque! q)
(rear-delete-deque! q)

;; 3.3.3 Representing Tables

(define (make-table)
  (list '*table*))

;; Ex 3.24

(define (lookup same-key? key table)
  (let ((record (assoc-key same-key? key (cdr table))))
    (if record
        (cdr record)
        #f)))

(define (assoc-key same-key? key records)
  (cond ((null? records) #f)
        ((same-key? key (caar records))
         (car records))
        (else (assoc-key same-key? key (cdr records)))))

(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable
             (assoc-key same-key? key-1 (cdr local-table))))
        (if subtable
            (let ((record
                   (assoc-key same-key? key-2
                          (cdr subtable))))
              (if record (cdr record) #f))
            #f)))
    (define (insert! key-1 key-2 value)
      (let ((subtable
             (assoc-key same-key? key-1 (cdr local-table))))
        (if subtable
            (let ((record
                   (assoc-key same-key? key-2
                          (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr!
                   subtable
                   (cons (cons key-2 value)
                         (cdr subtable)))))
            (set-cdr!
             local-table
             (cons (list key-1
                         (cons key-2 value))
                   (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation:
                          TABLE" m))))
    dispatch))

;; Ex 3.25

(define t (make-table equal?))

((t 'insert-proc!) 'a 'b 1)
((t 'insert-proc!) 'a 'c 2)

((t 'lookup-proc) 'a 'b)
((t 'lookup-proc) 'a 'c)

(define (append! lst x)
  (if (null? (cdr lst))
    (set-cdr! lst (cons x '()))
    (append! (cdr lst) x)))

(define (append lst x)
  (if (null? lst)
    (cons x '())
    (cons (car lst)
          (append (cdr lst) x))))

(append (append (list 1 2 3) 4) 5)

(define x (list 1 2 3))
(append! x 4)

(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    (define (lookup keys)
      (define (loop subtable keys)
        (let ((next (assoc-key same-key? (car keys) (cdr subtable))))
          (if (null? (cdr keys))
            ;; if this is the last key, look up the record, and return
            (if next
                (cdr next)
                #f)
            ;; if we have multiple keys, look up the subtable, and loop
            (if next
                (loop next (cdr keys))
                #f))))
      (loop local-table keys))

    (define (gen-new-list keys value)
        (if (null? (cdr keys))
            (cons (car keys) value)
            (list (car keys) (gen-new-list (cdr keys) value))))

    (define (insert! keys value)
       (define (iter keys table)
         (let ((subtable (assoc-key same-key? (car keys) (cdr table))))
           (if subtable
               (cond ((null? (cdr keys))
                      (set-cdr! subtable value))
                     (else
                      (iter (cdr keys) subtable)))
               (set-cdr! table (cons (gen-new-list keys value) (cdr table)))))
         'ok)
       (iter keys local-table))

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            ((eq? m 'local-table) (cdr local-table))
            (else (error "Unknown operation:
                          TABLE" m))))
    dispatch))

(define t (make-table equal?))

((t 'insert-proc!) (list 'a 'b 'c) 1)
((t 'lookup-proc) (list 'a 'b 'c))
(define keys (list 'a 'b))

(cdr keys)

(t 'local-table)

(assoc-key equal? 'c (list (cdr (assoc-key equal? (car keys) (t 'local-table)))))

(define keys (list 'a 'b 'c))

(car (cdr keys))

(list 'a 'b)

;; Ex 3.26

(define (assoc-key-bintree same-key? key records)
  (cond ((null? records) #f)
        ((same-key? key (caar records))
         (car records))
        (else (assoc-key-bintree same-key? key (cdr records)))))

(define (make-tree entry left right)
  (list entry left right))
(define (entry tree) (car tree))
(define (left tree) (cadr tree))
(define (right tree) (caddr tree))

(define (record-key entry)
  (car entry))
(define (record-value entry)
  (cdr entry))
(define (make-record key value)
  (cons key value))
(define (set-value! record value)
  (set-cdr! record value))

(define (lookup tree key)
  (cond ((null? tree) #f)
        ((equal? (record-key (entry tree)) key) (record-value (entry tree)))
        ((< key (record-key (entry tree))) (lookup (left tree) key))
        ((> key (record-key (entry tree))) (lookup (right tree) key))))

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (insert! key value)
      (define (iter tree)
        (cond ((null? tree)
               (display "making emptry tree")
               (make-tree (make-record key value) '() '()))
              ((equal? key (record-key (entry tree)))
               (set-value! (entry tree) value)
               tree)
              ((< key (record-key (entry tree)))
               (make-tree (entry tree) (iter (left tree)) (right tree)))
              ((> key (record-key (entry tree)))
               (make-tree (entry tree) (left tree) (iter (right tree))))))
      (set-cdr! local-table (iter (cdr local-table))))
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) (lambda (item) (lookup (cdr local-table) item)))
            ((eq? m 'insert-proc!) insert!)
            ((eq? m 'local-table) (cdr local-table))
            (else (error "Unknown operation:
                          TABLE" m))))
    dispatch))

(define t (make-table))
((t 'insert-proc!) 1 'a)
((t 'insert-proc!) 2 'b)
((t 'lookup-proc) 1)
((t 'lookup-proc) 2)
(t 'local-table)
(record-key (entry (t 'local-table)))
(entry (right (t 'local-table)))
(left (t 'local-table))
(cdr (t 'local-table))

;; Ex 3.27

;; Fib needs to be memo-fib, otherwise the memoize procedure won't have the opportunity to shortcut the function call.

;; memo-fib only computes (fib n) one for each n, so the complexity grows linearly with n.

(define (make-wire)
...)

(define (get-signal wire)
...)
(define (set-signal! wire)
...)
(define (add-action! wire)
  (lambda () ...))

(define (after-delay time-delay proc))

(define (inverter input output)
  (define (invert-input)
    (let ((new-value
           (logical-not (get-signal input))))
      (after-delay
       inverter-delay
       (lambda ()
         (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

(define (and-gate in1 in2 output)
  (define (and-action-procedure)
    (let ((new-value
            (logical-and (get-signal in1)
                         (get-signal in2))))
      (after-delay
        and-gate-delay
        (lambda ()
          (set-signal! output new-value)))))
  (add-action! in1 and-action-procedure)
  (add-action! in2 and-action-procedure)
  'ok)

(define (logical-and s1 s2)
  (cond ((and (= 1 s1) (= 1 s2)) 1)
        (else 0)))

(define a (make-wire))
(define b (make-wire))
(define c (make-wire))
(define d (make-wire))
(define e (make-wire))
(define s (make-wire))

(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((c1 (make-wire))
        (c2 (make-wire))
        (s  (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

;; Ex 3.28

(define (or-gate in1 in2 output)
  (define (or-action-procedure)
    (let ((new-value
            (logical-or (get-signal in1)
                         (get-signal in2))))
      (after-delay
        or-gate-delay
        (lambda ()
          (set-signal! output new-value)))))
  (add-action! in1 or-action-procedure)
  (add-action! in2 or-action-procedure)
  'ok)

(define (logical-or s1 s2)
  (cond ((or (= 1 s1) (= 1 s2)) 1)
        (else 0)))

;; Ex 3.29

(define (or-gate in1 in2 output)
  (let ((a (make-wire))
        (b (make-wire))
        (c (make-wire)))
    (not-gate in1 a)
    (not-gate in2 b)
    (and-gate a b c)
    (not-gate c output)
    'ok))

;; Ex 3.30

(define (ripple-carry-adder as bs ss c)
  (define (loop as bs ss c)
    (if (null? as)
      c
      (let ((c-out (make-wire)))
        (full-adder (car as) (car bs) c (car ss)
                    (loop (cdr as) (cdr bs) (cdr ss) c)))))
  (loop as bs ss c)
  'ok)

;; Representing wires

(define (make-wire)
  (let ((signal-value 0)
        (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each
                  action-procedures))
          'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures
            (cons proc action-procedures))
      (proc))
    (define (dispatch m)
      (cond ((eq? m 'get-signal)
             signal-value)
            ((eq? m 'set-signal!)
             set-my-signal!)
            ((eq? m 'add-action!)
             accept-action-procedure!)
            (else (error "Unknown operation:
                          WIRE" m))))
    dispatch))

(define (get-signal wire)
  (wire 'get-signal))
(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))
(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))

;; The agenda

;; make, empty? first, remove first, add at a specified time, and current time

(define (after-delay delay action)
  (add-to-agenda!
   (+ delay (current-time the-agenda))
   action
   the-agenda))

;; Side effectful looping function that takes the first agenda item, calls it, then removes it from the agenda, then continues.
(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item
             (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

(define (probe name wire)
  (add-action!
   wire
   (lambda ()
     (newline)
     (display name)
     (display " ")
     (display (current-time the-agenda))
     (display "  New-value = ")
     (display (get-signal wire)))))

;; An agenda is a data abstraction
(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

;; Initialise the wires
(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))

;; Add probes

(probe 'sum sum)
(probe 'carry carry)

;; Wire them up
(half-adder input-1 input-2 sum carry)

;; Set the signal on the first input
(set-signal! input-1 1)
(propagate)
; sum 8  New-value = 1
; done

(set-signal! input-2 1)
(propagate)
; carry 11  New-value = 1
; sum 16  New-value = 0
; done

;; Implementing the agenda

(define (make-time-segment time queue)
  (cons time queue))
(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))

(define (make-agenda) (list 0))
(define (current-time agenda) (car agenda))
(define (set-current-time! agenda time)
  (set-car! agenda time))
(define (segments agenda) (cdr agenda))
(define (set-segments! agenda segments)
  (set-cdr! agenda segments))
(define (first-segment agenda)
  (car (segments agenda)))
(define (rest-segments agenda)
  (cdr (segments agenda)))

(define (empty-agenda? agenda)
  (null? (segments agenda)))

(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time
           (segment-time (car segments)))))
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
        (insert-queue!
         (segment-queue (car segments))
         action)
        (let ((rest (cdr segments)))
          (if (belongs-before? rest)
              (set-cdr!
               segments
               (cons (make-new-time-segment
                      time
                      action)
                     (cdr segments)))
              (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments!
         agenda
         (cons (make-new-time-segment
                time
                action)
               segments))
        (add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue
            (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
        (set-segments!
         agenda
         (rest-segments agenda)))))

;; Constraint propogation

(define (celsius-fahrenheit-converter c f)
  (let ((u (make-connector))
        (v (make-connector))
        (w (make-connector))
        (x (make-connector))
        (y (make-connector)))
    (multiplier c w u)
    (multiplier v x u)
    (adder v y f)
    (constant 9 w)
    (constant 5 x)
    (constant 32 y)
    'ok))

(define C (make-connector))
(define F (make-connector))
(celsius-fahrenheit-converter C F)

(probe "Celsius temp" C)
(probe "Fahrenheit temp" F)

(set-value! C 25 'user)
(set-value! F 212 'user)
(forget-value! C 'user) ;; Both F and C forget their values
(set-value! F 212 'user)

; (has-value? ⟨connector⟩) tells whether the connector has a value.
; (get-value ⟨connector⟩) returns the connector’s current value.
; (set-value! ⟨connector⟩ ⟨new-value⟩ ⟨informant⟩) indicates that the informant is requesting the connector to set its value to the new value.
; (forget-value! ⟨connector⟩ ⟨retractor⟩) tells the connector that the retractor is requesting it to forget its value.
; (connect ⟨connector⟩ ⟨new-constraint⟩) tells the connector to participate in the new constraint.

(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond ((and (has-value? a1)
                (has-value? a2)) ;; first checks
           (set-value! sum
                       (+ (get-value a1)
                          (get-value a2))
                       me))
          ((and (has-value? a1)
                (has-value? sum))
           (set-value! a2
                       (- (get-value sum)
                          (get-value a1))
                       me))
          ((and (has-value? a2)
                (has-value? sum))
           (set-value! a1
                       (- (get-value sum)
                          (get-value a2))
                       me))))
  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else (error "Unknown request:
                        ADDER" request))))
  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)

(define (inform-about-value constraint)
  (constraint 'I-have-a-value))
(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))

(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond ((or (and (has-value? m1)
                    (= (get-value m1) 0))
               (and (has-value? m2)
                    (= (get-value m2) 0)))
           (set-value! product 0 me))
          ((and (has-value? m1)
                (has-value? m2))
           (set-value! product
                       (* (get-value m1)
                          (get-value m2))
                       me))
          ((and (has-value? product)
                (has-value? m1))
           (set-value! m2
                       (/ (get-value product)
                          (get-value m1))
                       me))
          ((and (has-value? product)
                (has-value? m2))
           (set-value! m1
                       (/ (get-value product)
                          (get-value m2))
                       me))))
  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request:
                   MULTIPLIER"
                  request))))
  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me)

(define (constant value connector)
  (define (me request)
    (error "Unknown request: CONSTANT"
           request))
  (connect connector me)
  (set-value! connector value me)
  me)

(define (probe name connector)
  (define (print-probe value)
    (newline) (display "Probe: ")
    (display name) (display " = ")
    (display value))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value)
    (print-probe "?"))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else (error "Unknown request:
                        PROBE" request))))
  (connect connector me)
  me)

(define (make-connector)
  (let ((value false)
        (informant false) ;; The object that set the connector's value
        (constraints '())) ;; A list of constraints in which the
    (define (set-my-value newval setter)
      (cond ((not (has-value? me))
             (set! value newval)
             (set! informant setter)
             (for-each-except
              setter ;; skip the setter constraint
              inform-about-value ;; for each constraint, send a 'inform-about-value message
              constraints))
            ((not (= value newval))
             (error "Contradiction"
                    (list value newval)))
            (else 'ignored)))
    (define (forget-my-value retractor)
      (if (eq? retractor informant)
        (begin (set! informant false) ;; forget-my-value resets the informant
               (for-each-except
                retractor ;; skip the retractor
                inform-about-no-value
                constraints))
        'ignored))
    (define (connect new-constraint)
      (if (not (memq new-constraint
                     constraints))
        (set! constraints
              (cons new-constraint
                    constraints)))
      (if (has-value? me)
        (inform-about-value new-constraint))
      'done)
    (define (me request)
      (cond ((eq? request 'has-value?)
             (if informant true false))
            ((eq? request 'value) value)
            ((eq? request 'set-value!)
             set-my-value)
            ((eq? request 'forget)
             forget-my-value)
            ((eq? request 'connect) connect)
            (else (error "Unknown operation:
                         CONNECTOR")
                         request)))
    me))

(define (for-each-except exception
                         procedure
                         list)
  (define (loop items)
    (cond ((null? items) 'done)
          ((eq? (car items) exception)
           (loop (cdr items)))
          (else (procedure (car items))
                (loop (cdr items)))))
  (loop list))


;; interface for the connector
(define (has-value? connector)
  (connector 'has-value?))
(define (get-value connector)
  (connector 'value))
(define (set-value! connector
                    new-value
                    informant)
  ((connector 'set-value!)
   new-value
   informant))
(define (forget-value! connector retractor)
  ((connector 'forget) retractor))
(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))

;; Ex 3.33

(define (averager a b c)
  (let ((sum (make-connector))
        (two (make-connector)))
    (adder a b sum)
    (multiplier c two sum)
    (constant 2 two)
    'ok))

(define C (make-connector))
(define F (make-connector))

(celsius-fahrenheit-converter C F)

(probe "Celsius temp" C)
(probe "Fahrenheit temp" F)

(set-value! C 25 'user)
(set-value! F 212 'user)
(forget-value! C 'user)
(forget-value! F 'user)

(define A (make-connector))
(define B (make-connector))
(define C (make-connector))
(constant 10 C)

(averager A B C)

(probe "A" A)
(probe "B" B)

(set-value! A 5 'user)
(forget-value! A 'user)

(set-value! B 5 'user)

;; Ex 3.34

;; multiplier enforces a constraint between two values of three.
;; If you set b, then squarer should be fully constrained,
;; but multiplier will not be. So multiplier will not propagate changes

;; Ex 3.35

(inexact->exact (expt 4 0.5))

(define (squarer a b)
  (define (process-new-value)
    (if (has-value? b)
      (if (< (get-value b) 0)
        (error "square less than 0:
                    SQUARER"
               (get-value b))
        (set-value! a (expt (get-value b) 0.5) me))
      (if (has-value? a)
        (set-value! b (expt (get-value a) 2) me))))
  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! b me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request:
                   MULTIPLIER"
                  request))))
  (connect a me)
  (connect b me)
  me)

(define A (make-connector))
(define B (make-connector))

(squarer A B)

(probe "A" A)
(probe "B" B)

(set-value! B 16 'user)
(forget-value! A 'user)

;; Ex 3.36

(define a (make-connector))
(define b (make-connector))
(set-value! a 10 'user)

;; Ex 3.37

(define (celsius-fahrenheit-converter x)
  (c+ (c* (c/ (cv 9) (cv 5))
          x)
      (cv 32)))

(define C (make-connector))
(define F (celsius-fahrenheit-converter C))

(define (c+ x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))

(define (c- x y)
  (let ((z (make-connector)))
    (adder z y x)
    z))

(define (c* x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))

(define (c/ x y)
  (let ((z (make-connector)))
    (multiplier z y x)
    z))
