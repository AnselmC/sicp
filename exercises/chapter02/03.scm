;; EXERCISE 53
(list 'a 'b 'c) ;; (a b c)

(list (list 'george)) ;; ((george))
(cdr '((x1 x2) (y1 y2))) ;; ((y1 y2))

(cadr '((x1 x2) (y1 y2))) ;; (y1 y2)
(pair? (car '(a short list))) ;; false
(memq 'red '((red shoes) (blue socks))) ;; false

(memq 'red '(red shoes blue socks)) ;; (red shoes blue socks)

;; EXERCISE 54
(define (equal? a b)
  (or (and (symbol? a)
           (symbol? b)
           (eq? a b))
      (and (number? a)
           (number? b)
           (eq? a b))
      (and (list? a)
           (list? b)
           (or (and (null? a)
                    (null? b))
               (and (not (and (null? a)
                              (null? b)))
                    (equal? (car a) (car b))
                    (equal? (cdr a) (cdr b))))))) 

(equal? '(this is a list) '(this is a list))

(equal? '(this is a list) '(this (is a) list))

;; EXERCISE 55
(car ''abracadabra)
;; 'x expands to the expression (quote x). Hence, ''x becomes '(quote x). This is the list of the symbols `quote` and `x`.
;;  Thus, (car ''x) first evaluates to (car '(quote x)) and then to the symbol `quote`.

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        (else
         (error "unknown expression type -- DERIV" exp))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (caddr p))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))
(deriv '(+ x 3) 'x)
(deriv '(* x y) 'x)
(deriv '(* (* x y) (+ x 3)) 'x)

;; EXERCISE 56
(define (exponentiation? exp)
  (and (pair? exp) (eq? (car exp) '**))

(define (base exp)
  (cadr exp))

(define (exponent exp)
  (caddr exp))

(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        (else (list '** base exponent))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        ((exponentiation? exp)
         (make-product (make-product (exponent exp)
                                     (make-exponentiation (base exp)
                                                          (make-sum (exponential exp) -1)))
                       (deriv (base exp) var)))
        (else
         (error "unknown expression type -- DERIV" exp))))

;; EXERCISE 57
(define (make-exp symbol accumulator identity makes-zero? first . rest)
  (define (make-exp-iter remaining terms numbers)
    (if (null? remaining)
        (let ((accumulated (accumulate accumulator identity numbers)))
          (cond ((= (length terms) 0)
                 accumulated) ;; no non-number terms, return accumulated numbers
                ((= identity accumulated)
                 (if (= 1 (length terms)) ;; only one term left, return term without symbol
                     (car terms)
                     (cons symbol terms))) ;; return list of symbol and remaining terms
                (else
                 (append (list symbol accumulated) terms)))) ;; return list of symbol, accumulated numbers and remaining terms
        (let ((next (car remaining)) ;; next iteration
              (rest (cdr remaining)))
          (cond ((number? next)
                 (if (makes-zero? next) 
                     0
                     (make-exp-iter rest terms (cons next numbers)))) ;;
                (else
                 (make-exp-iter rest (cons next terms) numbers))))))
  (if (null? rest) 
      first
      (make-exp-iter (cons first rest) () ())))

(define (make-sum first . rest)
  (apply make-exp '+ + 0 (lambda (x) false) first rest))
    
(define (augend sum)
  (apply make-sum (cddr sum)))

(define (make-product first . rest)
  (apply make-exp '* * 1 (lambda (x) (= x 0)) first rest))

(define (multiplier p)
  (cadr p))

(define (multiplicand prod)
  (apply make-product (cddr prod)))

(deriv '(* x y (+ x 3)) 'x)

(deriv '(* x x) 'x)

;; EXERCISE 58
;; a)
(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))

(define (addend sum)
  (car sum))

(define (augend sum)
  (caddr sum))

(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier p)
  (car p))

(define (multiplicand p)
  (caddr p))

;; TODO: make-sum & make-product
(define example '(x + (3 * (x + (y + 2))))) ;; x + 3(x+(y+2)) = x + 3x + 3(y+2)
(define prod '(x * (y + 3)))
(define sum '(x + (y * 3)))

(sum? prod)
(product? prod)
(multiplier prod)
(multiplicand prod)

(product? sum)
(sum? sum)
(addend sum)
(augend sum)

(deriv '(x * y) 'x)
(deriv '(x + y) 'x)
(deriv sum 'x)
(deriv prod 'x)
(deriv example 'x)
(deriv '(x * (x + 3)) 'x)

;; b)
(define (list-or-value l)
  (if (list? l)
    (if (= 1 (length l))
        (car l)
        l)
    l))

(define (make-exp symbol makes-zero? accumulator identity first . rest)
  (if (null? rest)
      first ;; single value
      (let* ((elements (cons first rest))
             (number-elements (filter (lambda (x) (number? x)) elements))
             (other-elements (filter (lambda (x) (or (list? x) (variable? x))) elements))
             (accumulated (accumulate accumulator identity number-elements)))
        (cond ((makes-zero? accumulated) 0)
              ((null? other-elements) accumulated) ;; only numbers, return accumulated, problem if accumulated is identity
              (else
               (list-or-value
                (cons accumulated
                      (flatmap (lambda (x) (list symbol x)) ;; create a flatmap with every element preceded by a symbol
                               other-elements)))))))))

(define (make-sum first . rest)
  (apply make-exp '+ (lambda (x) false) + 0 first rest))

(define (make-product first . rest)
  (apply make-exp '* (lambda (x) (= x 0)) * 1 first rest))

(define (sum? exp)
  (list? (memq '+ exp)))

(define (addend sum)
  ;; first term of the sum
  ;; all the elements before the first '+'
  ;; could be either a single term or the multiplication of two or more terms
  (define (addend-iter remaining first-term)
    (let ((current (car remaining)))
      (if (eq? current '+) ;; reached end of first term
          (list-or-value first-term)
          (addend-iter (cdr remaining) (cons current first-term)))))
  (addend-iter sum ()))
  
(define (augend sum)
  ;; the rest of the terms
  (let ((rest (cdr (memq '+ sum)))) ;; guaranteed to return non-empty list
    (list-or-value rest)))

(define (product? exp)
  (and (not (sum? exp))
       (list? (memq '* exp))))

(define (multiplier prod)
  (list-or-value (car prod)))

(define (multiplicand prod)
  (list-or-value (cdr (memq '* prod))))

(define term '(x + 4 * (x + y + 2))) ;; 5x+4y+8 --> 5
(deriv term 'x)
(deriv '(x + y * (4 + x)) 'x) ;; identity problem


;; EXERCISE 59
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) ())
        ((element-of-set? (car set1) set2)        
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
         (else (union-set (cdr set1) (cons (car set1) set2)))))

(define set1 (adjoin-set 1 (adjoin-set 2 (adjoin-set 3 ()))))
(define set2 (adjoin-set 3 (adjoin-set 4 (adjoin-set 5 ()))))
(intersection-set set1 set2) ;; (3)
(union-set set1 set2) ;; (2 1 3 4 5)

;; EXERCISE 60
;; `element-of-set?` is identical, runtime is O(m) where m is the size of the set and m >= n where n are the number of distinct elements

(define (adjoin-set x set)
  (cons x set))
;; 0(1) instead of O(n)

(define (union-set set1 set2)
  (if (null? set1)
      set2
      (union-set (cdr set1) (cons (car set1) set2)))) ;; incrementally add elements from set1 to set2 until set1 is empty
      
;; 0(n) instead of O(n*m) with n size of set1 and m size of set2

;; `intersection-set` is identical w/ runtime O(n*m) (with n>=n_no-dupl and m>=m_no-dupl)

;; would use this if few duplicates are expected and operations consist of adding elements or computing unions


;; EXERCISE 61
(define (adjoin-set x set)
  (if (null? set)
      (list x)
      (let ((smallest (car set))
            (rest (cdr set)))
        (cond ((= x smallest) set) ;; already in set
              ((< x smallest) (cons x set)) ;; add to set
              (else (cons smallest (adjoin-set x rest))))))) ;; go to next

(adjoin-set 6 (adjoin-set 0 (adjoin-set 1 ())))

;; EXERCISE 62
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
         (let ((x1 (car set1))
               (x2 (car set2)))
           (cond ((< x1 x2)
                  (cons x1 (union-set (cdr set1) set2)))
                 ((< x2 x1)
                  (cons x2 (union-set (cdr set2) set1)))
                 (else
                  (cons x1 (union-set (cdr set1) (cdr set2)))))))))
  
(union-set (list 6 7) (list 3 4 5))
  
;; EXERCISE 63
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))


(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree) ;; traverses left
                      (cons (entry tree) ;; adds node to result list
                            (copy-to-list (right-branch tree) ;; then traverses right
                                          result-list)))))
  (copy-to-list tree '()))


;; a) Both result in an inorder traversal representation of the tree (i.e. traverses left, then node, then right).
;; I) [1, 3, 5, 7, 9, 11]
;; II) [1, 3, 5, 7, 9, 11]
;; III) [1, 3, 5, 7, 9, 11]
;; However, the second procedure really traverses the tree from right to left but the result-list is built in reverse order s.t. the result is an inorder traversal.

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define tree1
  (make-tree 7
             (make-tree 3
                        (make-tree 1 () ())
                        (make-tree 5 () ()))
             (make-tree 9
                        ()
                        (make-tree 11 () ()))))
(define tree2
  (make-tree 3
             (make-tree 1 () ())
             (make-tree 7
                        (make-tree 5 () ())
                        (make-tree 9
                                   ()
                                   (make-tree 11 () ())))))
(define tree3
  (make-tree 5
             (make-tree 3
                        (make-tree 1 () ())
                        ())
             (make-tree 9
                        (make-tree 7 () ())
                        (make-tree 11 () ()))))

(tree->list-1 tree1)
(tree->list-1 tree2)
(tree->list-1 tree3)

(tree->list-2 tree1)
(tree->list-2 tree2)
(tree->list-2 tree3)

;; b) WRONG: No, for the first procedure the recursion tree is balanced whereas for the right tree it is skewed?
;; Is it O(n logn) vs O(n**2)?
;;         ^   ^
;;         |   |
;;         |    --------from recursion tree?
;;          -- from append

;; EXERCISE 64
;; a)
;; if n is 0 (i.e. at least 0 elements), `partial-tree` returns a list of an empty list and the elements
;; else, it first determines the size of the left children and calls itself with the number of left children as n to compute the left subtree
;; (since elements is ordered it's guaranteed that all left children will be smaller than the root).
;; It then takes the result from the previous call and splits it into the left tree (the car) and the remaining elements.
;; The remaining elements are again split into the root (the car) and the right tree (the cdr).
;; It then applies itself again to the right tree in an equal manner as before and finally creates a balanced tree from the root, the left, and the right halves.
;; Since this is done recursively, all subtrees are balanced.

;; (1 3 5 7 9 11)

;;             5
;;            / \
;;           /   \
;;          1     9
;;           \   / \
;;           3  7  11
;; b)
;; O(n) since it visits each element exactly once?

;; EXERCISE 65
;; should return a new (balanced) tree
;; if runtime from b) is correct,
;; we can first create an ordered set from both trees in taking O(n) for each tree
;; then use O(n) implementations for `union-set` (Exercise 62) and `intersection-set` for ordered sets on result
;; and finally create a balanced tree in O(n)
;; overall this takes O(n) + 2*O(n) + O(n) = O(n) time
        
(define (union-set-* tree1 tree2)
  (let* ((set1 (tree->list-2 tree1))
         (set2 (tree->list-2 tree2))
         (union (union-set set1 set2)))
    (list->tree union)))
    
;; similar argument for intersection-set
(define (intersection-set-* tree1 tree2)
  (let* ((set1 (tree->list-2 tree1))
         (set2 (tree->list-2 tree2))
         (intersection (intersection-set set1 set2)))
    (list->tree intersection)))

;; EXERCISE 66
(define (lookup given-key tree-of-records)
  (if (null? tree-of-records)
      false
      (let* ((curr-record (entry (tree-of-records)))
             (curr-key (key curr-record)))
        (cond ((equal? given-key curr-key) curr-record)
              ((> given-key curr-key)
               (lookup given-key (right-branch tree-of-records)))
              (else
               (lookup given-key (left-branch tree-of-records)))))))

;; EXERCISE 67
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
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

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)    ; symbol
                               (cadr pair))  ; frequency
                    (make-leaf-set (cdr pairs))))))

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
(define decoded-message (decode sample-message sample-tree)) ;; (a d a b b c a)

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (if (not (memq symbol (symbols tree)))
      (error "bad symbol -- ENCODE-SYMBOL" symbol)
      (cond ((leaf? tree) '())
            ((memq symbol (symbols (left-branch tree)))
             (cons 0 (encode-symbol symbol (left-branch tree))))
            (else
             (cons 1 (encode-symbol symbol (right-branch tree)))))))


(equal? (encode decoded-message sample-tree) sample-message) ;; true

;; EXERCISE 69
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge leaf-pairs)
  (if (eq? (length leaf-pairs) 1) ;; all merges have been performed
      (car leaf-pairs) ;; return first and only element
      ;; at least two elements left
      (let* ((smallest-element (car leaf-pairs))
             (second-smallest-element (cadr leaf-pairs))
             (merged-elements (make-code-tree smallest-element second-smallest-element)) ;; merge two smallest elements
             (merged-leaf-pairs (adjoin-set merged-elements (cddr leaf-pairs)))) ;; insert merged elements into remaining set (in-order) 
        (successive-merge merged-leaf-pairs)))) ;; repeat
        
(define generated-tree (generate-huffman-tree '((A 4) (B 2) (C 1) (D 1))))
(equal? generated-tree sample-tree) ;; true

;; EXERCISE 70
(define rock-alphabet '((A 2) (NA 16) (BOOM 1) (SHA 3) (GET 2) (YIP 9) (JOB 2) (WAH 1)))
(define rock-tree (generate-huffman-tree rock-alphabet))
(define rock-song
  '(Get a job
    Sha na na na na na na na na
    Get a job
    Sha na na na na na na na na
    Wah yip yip yip yip yip yip yip yip yip
    Sha boom))
(define encoded-rock-song (encode rock-song rock-tree))
;; (1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 1 1 0 1 1 0 1 1)
(length encoded-rock-song) ;; 84 bits
(length rock-song)
;; for an 8 letter alphabet, one needs 3 bits per letter for a fixed-length encoding
;; the message contains 36 letters resulting in 36*3=108 bits. Hence, we have a ~22% space improvement.

;; EXERCISE 71
;; n=5: ((A 1) (B 2) (C 4) (D 8) (E 16)

;;            (ABCDE 31)
;;            /        \
;;        (E 16)     (ABCD 15)
;;                   /      \
;;                 (D 8)   (ABC 7)
;;                         /     \
;;                       (C 4)  (AB 3)
;;                              /   \
;;                            (B 2) (A 1)

;; n=10: ((A 1) (B 2) (C 4) (D 8) (E 16) (F 32) (G 64) (H 128) (I 264) (J 512))

;;                   (ABCDEFGHIJ 1023)
;;                    /            \
;;                  (J 512)     (ABCDEFGHI 511)
;;                               /          \
;;                             (I 264)     (ABCDEFGH 263)
;;                                          /         \
;;                                        (H 128)   (ABCDEFG 127)
;;                                                   /         \
;;                                                 (G 64)   (ABCDEF 63)
;;                                                           /       \
;;                                                         (F 32)   (ABCDE 31)
;;                      
;;                                                                    ...

;; One bit for most frequent symbol, n bit for (two) least frequent symbols

;; EXERCISE 72

;; 1. Searching for symbol in tree takes O(n)
;; 2. Checking whether symbol is in left branch takes O(1) since left branch always has size 1 for these relative frequencies.
;; 3. Also, going down the left branch always ends the recursion.
;; 4. At most (for the least frequent symbol), `encode-symbol` is called n times. Each time the symbol is searched in the current node symbols.
;; Thus, the procedure has a worst-case (least frequent symbol) runtime of O(n**2). Best-case (most-frequent symbol) is O(n).

