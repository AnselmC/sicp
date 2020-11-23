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
  (and (list? exp) (eq? (car exp) '**))

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
(define (make-exp symbol accumulator identity makes-zero first . rest)
  (define (make-exp-iter remaining terms numbers)
    (if (null? remaining)
        (let ((accumulated (accumulate accumulator identity numbers)))
          (cond ((= (length terms) 0)
                 accumulated) ;; no non-number terms, return accumulated numbers
                ((= identity accumulated)
                 (if (= 1 (length terms)) ;; only one term left, return term without symbol
                     (car terms)
                     (append (list symbol) terms))) ;; return list of symbol and remaining terms
                (else
                 (append (list symbol accumulated) terms)))) ;; return list of symbol, accumulated numbers and remaining terms
        (let ((next (car remaining)) ;; next iteration
              (rest (cdr remaining)))
          (cond ((number? next)
                 (if (makes-zero next) 
                     0
                     (make-exp-iter rest terms (cons next numbers)))) ;;
                (else
                 (make-exp-iter rest (cons next terms) numbers))))))
  (if (null? rest) 
      first
      (make-exp-iter (cons first rest) () ())))

(define (make-sum first . rest)
  (apply make-exp (append (list '+ + 0 (lambda (x) false)) (cons first rest))))
    
(define (augend sum)
  (apply make-sum (cddr sum)))

(define (make-product first . rest)
  (apply make-exp (append (list '* * 1 (lambda (x) (= x 0))) (cons first rest))))

(define (multiplier p) (cadr p))

(define (multiplicand prod)
  (apply make-product (cddr prod)))

(deriv '(* x y (+ x 3)) 'x)

(deriv '(* x 3) 'x)

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

(define example '(x + (3 * (x + (y + 2)))))
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
             (list-elements (filter (lambda (x) (list? x)) elements))
             (number-elements (filter (lambda (x) (number? x)) elements))
             (accumulated (accumulate accumulator identity number-elements)))
        (cond ((makes-zero? accumulated) 0)
              ((null? list-elements) accumulated) ;; only numbers, return accumulated, problem if accumulated is identity
              (else
               (list-or-value
                (cdr ;; discard superfluous plus at index 0
                 (flatmap (lambda (x) (list symbol x)) ;; create a flatmap with every element preceded by a symbol
                          (cons accumulated list-elements)))))))))

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
          (list-or-value (apply make-product first-term))
          (if (eq? current '*)
              (addend-iter (cdr remaining) first-term) ;; skip multipliers, product is created at end
              (addend-iter (cdr remaining) (cons current first-term))))))
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

(define term '(x + 4 * (x + y + 2))) ;; 4x+3y+6 --> 4
(deriv term 'x)
(deriv '(x + x * (4 + y)) 'x) ;; identity problem


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
      (union-set (cdr set1) (cons (car set1) set2))))
      
;; 0(n) instead of O(n*m) with n size of set1 and m size of set2

;; `intersection-set` is identical w/ runtime O(n*m) (with n>=n_no-dupl and m>=m_no-dupl)

;; would use this if few duplicates are expected and operations consist of adding elements or computing unions


;; EXERCISE 61
  
