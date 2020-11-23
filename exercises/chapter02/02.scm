;; EXERCISE 17
(define (last-pair l)
  (if (null? (cdr l))
      l
      (last-pair (cdr l))))
(last-pair (list 23 72 149 34))

;; EXERCISE 18
(define (reverse l)
  (define (iter rem res)
    (if (null? rem)
        res
        (iter (cdr rem) (cons (car rem) res))))
  (iter l ()))
(reverse (list 1 4 9 16 25))

;; EXERCISE 19
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

(define first-denomination car)
(define except-first-denomination cdr)
(define no-more? null?)

(cc 100 us-coins)
;; the order does not effect the result, it only changes the "look" of the built tree
;; since it doesn't matter how you split the tree but rather how often.

;; EXERCISE 20
(define (same-parity first . rest)
  (define (iter rem res is-parity?)
    (cond ((null? rem) (reverse res))
          ((is-parity? (car rem))
           (iter (cdr rem) (cons (car rem) res) is-parity?))
          (else
           (iter (cdr rem) res is-parity?))))
  (let ((is-parity?
         (if (even? first)
             even?
             odd?)))
    (iter rest (list first) is-parity?)))

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)

;; EXERCISE 21
(define (square-list items)
  (if (null? items)
      '()
      (cons (square (car items)) (square-list (cdr items)))))
(square-list (list 1 2 3 4))

(define (square-list items)
  (map square items))
(square-list (list 1 2 3 4))

;; EXERCISE 22
;; a) because the `answer` list is always prepended with the current square of the car of things
;; hence, the list is built in reverse order.
;; b) because cons constructs a pair of its arguments. Lists are a pairs of an object and a pair, so if
;; the fist argument to cons is an object and the second is a list, it prepends the object to the list.
;; however, if the first argument is a list, it creates a pair of a list and its second argument.
;; thus, the second solution creates a nested pair where only the last element in the list is a number

;; EXERCISE 23
(define (for-each proc items)
  (cond ((not (null? items))
         (proc (car items))
         (for-each proc (cdr items)))))

(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))

;; EXERCISE 24
(list 1 (list 2 (list 3 4))) ;; (1 (2 (3 4)))

;;                     (1 (2 (3 4)))
;;                    /            \
;;                   1            (2 (3 4))
;;                               /    \
;;                              2     (3 4)
;;                                    /   \
;;                                    3    4


;; EXERCISE 25
(1 3 (5 7) 9) ;; (car (cdr (car (cdr (cdr l)))))
(define l (list 1 3 (list 5 7) 9))
(car (cdr (car (cdr (cdr l)))))

((7)) ;; (car (car l))

(1 (2 (3 (4 (5 (6 7)))))) ;; (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr ((car (cdr (car l))))))))))))))

;; EXERCISE 26
(define x (list 1 2 3))
(define y (list 4 5 6))

(append x y) ;;  (1 2 3 4 5 6)

(cons x y) ;; ((1 2 3) 4 5 6)

(list x y) ;; ((1 2 3) (4 5 6))

;; EXERCISE 27
(define (deep-reverse l)
  (if (not (pair? l))
      l
      (reverse (map deep-reverse l))))

(define x (list (list 1 2) (list 3 4)))
(reverse x)
(deep-reverse x)
(deep-reverse ())

;; EXERCISE 28
(define (fringe x)
  (cond ((null? x) ())
        ((number? (car a)) a)
        ((else (append (fringe (car x)) (fringe (cdr x)))))))

(define x (list (list 1 2) (list 3 4)))
(fringe x)
(fringe (list x x))

;; EXERCISE 29
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

;; a)
(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))

;; b)
(define (total-weight mobile)
  (define (branch-weight branch structure)
    (if (number? structure)
        structure
        (total-weight structure)))

  (if (number? mobile)
      mobile
      (let* ((lb (left-branch mobile))
             (rb (right-branch mobile))
             (lbs (branch-structure lb))
             (rbs (branch-structure rb)))
        (+ (branch-weight lb lbs)
           (branch-weight rb rbs)))))

;; c)
(define (balanced? mobile)
  (define (lowest-level? branch)
    (number? (branch-structure branch)))

  (define (sub-mobile-balanced? branch)
    (if (lowest-level? branch)
        true
        (balanced? (branch-structure branch))))
    
  (let* ((left (left-branch mobile))
         (right (right-branch mobile))
         (total-left-weight (total-weight (branch-structure left)))
         (left-length (branch-length left))
         (total-right-weight (total-weight (branch-structure right)))
         (right-length (branch-length right)))
    (and (= (* total-right-weight right-length)
            (* total-left-weight left-length))
         (sub-mobile-balanced? right)
         (sub-mobile-balanced? left))))

(define balanced-mobile
  (make-mobile (make-branch 3 6)
	       (make-branch 1
			    (make-mobile
			     (make-branch 1
					  (make-mobile
					   (make-branch 2 4)
					   (make-branch 1 8)))
			     (make-branch 2 6)))))

(total-weight balanced-mobile)

(balanced? balanced-mobile)

;; d) nothing bc I'm already treating the list like a pair.

;; EXERCISE 30
(define (square-tree tree)
  (cond ((null? tree) ())
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))

;; EXERCISE 31
(define (tree-map proc tree)
  (cond ((null? tree) ())
        ((not (pair? tree)) (proc tree))
        (else (cons (tree-map proc (car tree))
                    (tree-map proc (cdr tree))))))
(define (square-tree tree) (tree-map square tree))

;; EXERCISE 32
(define (subsets s)
  (if (null? s)
      (list ())
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))

(subsets (list 1 2 3))
;; this works bc the subsets of a set are always all the subsets of the set w/o one element (e.g. w/o (car s))
;; plus that element included in the subsets (TODO: better expl.)

;; EXERCISE 33
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define l1 (list 1 2 3))
(define l2 (list 4 5 6))
(cons (car l1) (cons (car

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) () sequence))
(map square (list 1 2 3 5))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))
(append (list 1 2 3) (list 4 5 6))

(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))
(length (list 1 2 3 4 5 6))


;; EXERCISE 34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

(horner-eval 2 (list 1 3 0 5 0 1))

;; EXERCISE 35
(define (enumerate-tree tree)
  (cond ((null? tree) ())
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))
(define (count-leaves t)
  (accumulate + 0 (map (lambda (x) 1) (enumerate-tree t))))

(define x (cons (list 1 2) (list 3 4)))
(count-leaves (list x x))

;; EXERCISE 36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      ()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define s (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))
(accumulate-n + 0 s)

;; EXERCISE 37
(define m (list (list 1 2 3 4) (list 5 6 7 8) (list 9 10 11 12)))
(define v (list 1 2 3 4)))
(define w (list 5 6 7 8)))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))
(dot-product v w)

(define (matrix-*-vector m v)
  (map (lambda (mi) (dot-product mi v)) m))
(matrix-*-vector m v)


(define (transpose mat)
  (accumulate-n cons () mat))
(transpose m)

;; TODO: currently allows for mismatched column multiplication
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (mi) (matrix-*-vector cols mi)) m)))

(matrix-*-matrix m (transpose m))

;; EXERCISE 38
(fold-right / 1 (list 1 2 3)) ;; 3/2
(fold-left / 1 (list 1 2 3)) ;; 1/6
(fold-right list () (list 1 2 3))
;; (list 1 (list 2 (list 3 ()))) --> (1 (2 (3 ())))
(fold-left list () (list 1 2 3))

(define (average a b)
  (/ (+ a b)
     2))
(fold-left average 0 (list 1 2 3))
;; (list (list (list () 1) 2) 3) --> (((() 1) 2) 3)

;; In order to be identical `op` needs to be commutative and associative,
;; i.e. the order in which elements are `accumulated` and the `nestedness` of the elements needs to be irrelevant.

;; EXERCISE 39
(define fold-right accumulate)

(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) () sequence))

(reverse (list 1 2 3 4))

(define (fold-left op initial sequence)

  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (reverse sequence)
  (fold-left (lambda (x y) (cons y x)) () sequence))

(reverse (list 1 2 3 4))

;; EXERCISE 40
(define (enumerate-interval x y)
  (define (enumerate-iter current res)
    (if (> current y)
        res
        (enumerate-iter (1+ current) (append res (list current)))))
  (enumerate-iter x ()))

(define (flatmap proc seq)
  (accumulate append () (map proc seq)))


(define (unique-pairs n)
  (flatmap
   (lambda (i)
     (map (lambda (j) (list i j))
          (enumerate-interval 1 (- i 1))))
   (enumerate-interval 1 n)))

(unique-pairs 5)


(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (filter prime-sum? (unique-pairs n)))

(prime-sum-pairs 6)

;; EXERCISE 41
(define (ordered-triplets n s)
  (define (valid-sum triplet)
    (>= s (+ (car triplet) (cadr triplet) (caddr triplet))))
  (filter valid-sum
          (flatmap
           (lambda (i)
             (flatmap (lambda (j)
                    (map (lambda (k) (list i j k))
                         (enumerate-interval 1 (-1+ j))))
                  (enumerate-interval 1 (-1+ i))))
           (enumerate-interval 1 n))))

(ordered-triplets 5 8)

;; EXERCISE 42
(define empty-board ())

(define (safe? k positions)
  (define (dist x y)
    (abs (- x y)))
  (let ((row (caar (filter (lambda (elem) (= (cadr elem) k)) positions))))
    (accumulate (lambda (x y) (and x y)) true
                (map (lambda (x)
                       (if (and (= k (cadr x))
                                (= row (car x)))
                           true ;; 'skip' current position
                           (and (not (= row (car x))) ;; can't be in the same row
                                (not (= (dist row (car x)) ;; can't be in same diagonal
                                        (dist k (cadr x)))))))
                     positions)))) 
    
  )

(define (adjoin-position new-row column rest-of-queens)
  (cons (list new-row column) rest-of-queens))

(define (queens board-size)
  (define (queen-cols k)  
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))
             

(length (queens 8))

;; EXERCISE 43
;; TODO
;; notes: due to the recursive call to queen-cols in inner-most map vs in outer flatmap
;; results in exponential growth, but how large?

;; EXERCISE 44
(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

;; EXERCISE 45
(define (split op1 op2)
  (define (split-iter painter n)
    (if (= n 0)
        painter
        (let ((smaller (split-iter painter (- n 1))))
          (op1 painter (op2 smaller smaller)))))
  (lambda (painter n)
    (split-iter painter n)))
        
;; EXERCISE 46
(define (make-vect x y)
  (cons x y))

(define (xcor-vect vect)
  (car vect))

(define (ycor-vect vect)
  (cdr vect))

(define (add-vect a b)
  (make-vect (+ (xcor-vect a)
                (xcor-vect b))
             (+ (ycor-vect a)
                (ycor-vect b))))

(define (sub-vect a b)
  (make-vect (- (xcor-vect a)
                (xcor-vect b))
             (- (ycor-vect a )
                (ycor-vect b))))

(define (scale-vect vect factor)
  (make-vect (* factor (xcor-vect vect))
             (* factor (ycor-vect vect))))

;; EXERCISE 47
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin frame)
  (car frame))

(define (edge1 frame)
  (cadr frame))

(define (edge2 frame)
  (caddr frame))

(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin frame)
  (car frame))

(define (edge1 frame)
  (cadr frame))

(define (edge2 frame)
  (cddr frame))

;; EXERCISE 48
(define (make-segment start-vect end-vect)
  (cons start-vect end-vect))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

;; EXERCISE 49
(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))

;; a) The painter that draws the outline of the designated frame.
(define outline-painter
  (segments->painter
   (list
    (make-segment (make-vect 0.0 0.0)
                  (make-vect 1.0 0.0))
    (make-segment (make-vect 0.0 0.0)
                  (make-vect 0.0 1.0))
    (make-segment (make-vect 1.0 0.0)
                  (make-vect 1.0 1.0))
    (make-segment (make-vect 0.0 1.0)
                  (make-vect 1.0 1.0)))))

;; b) The painter that draws an ``X'' by connecting opposite corners of the frame.
(define x-painter
  (segments->painter
   (list
    (make-segment (make-vect 0.0 0.0)
                  (make-vect 1.0 1.0))
    (make-segment (make-vect 1.0 0.0)
                  (make-vect 0.0 1.0)))))

;; c) The painter that draws a diamond shape by connecting the midpoints of the sides of the frame.
(define diamond-painter
  (segments->painter
   (list
    (make-segment (make-vect 0.0 0.5)
                  (make-vect 0.5 0.0))
    (make-segment (make-vect 0.0 0.5)
                  (make-vect 0.5 1.0))
    (make-segment (make-vect 0.5 1.0)
                  (make-vect 1.0 0.5))
    (make-segment (make-vect 1.0 0.5)
                  (make-vect 0.5 0.0)))))

;; d) The wave painter. --> too tedious
   
;; EXERCISE 50
(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0) ;; origin
                     (make-vect 0.0 0.0) ;; new end of edge 1
                     (make-vect 1.0 1.0))) ;; new end of edge 2

(define (rotate180-ccw painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))


(define (rotate270-ccw painter)
  (rotate90 painter))

(define (rotate270-cw painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

;; EXERCISE 51
(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              split-point
                              (make-vect 0.0 1.0)))
          (paint-right
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.0)
                              (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

;; Version 1
(define (below painter other-painter)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-top
           (transform-painter other-painter
                              split-point
                              (make-vect 1.0 0.5)
                              (make-vect 0.0 1.0)))
          (paint-bottom
           (transform-painter painter
                              (make-vect 0.0 0.0)
                              (make-vect 1.0 0.0)
                              split-point)))
      (lambda (frame)
        (paint-bottom frame)
        (paint-top frame)))))

;; Version 2 (use beside)
(define (below painter other-painter)
  ;; idea: rotate 90 degrees + beside + rotate -90 degrees
  (lambda (frame)
    ((rotate270-cw ;; rotate painter back into orig. orientation
      (beside (rotate90 other-painter) ;; apply beside (returns painter)
              (rotate90 painter))) ;; rotate painters by 90 degrees
     frame))) ;; apply frame on resulting painter


;; EXERCISE 52
;; a) too tedious
;; b) 
(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left up)
              (bottom-right right)
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

;; c)
(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-vert identity
                                  rotate90 flip-vert)))
    (combine4 (corner-split painter n))))

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)))
    (combine4 (corner-split painter n))))
