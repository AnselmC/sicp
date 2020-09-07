;; EXERCISE 1.1
10
;; 10
(+ 5 4 3)
;;  12
(- 9 1)
;; 8
(/ 6 2)
;; 3
(+ (* 2 4) (- 4 6))
;; 6
(define a 3)
;; a
(define b (+ a 1))
;; b
(+ a b (* a b))
;; 19
(= a b)
;; #f
(if (and (> b a) (< b (* a b)))
    b
    a)
;; 4 (which is b)
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))
;; 16 (second cond)
(+ 2 (if (> b a) b a))
;; 6 (first cond)
(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))
;; 16 (second cond)


;; EXERCISE 1.2
(/ (+ 5 4
      (- 2
         (- 3
            (+ 6
               (/ 1 5)))))
   (* 3
      (* (- 6 2)
         (- 2 7))))
;; -71/300


;; EXERCISE 1.3
(define (sum-sq-2-largest a b c)
  ;; define sub-procedures
  (define (sum-sq x y)
    (define (sq z)
      (* z z))
    (+ (sq x)
       (sq y)))
  ;; actual logic
  (if (> a b)
      (if (> b c)
          (sum-sq a b)
          (sum-sq a c))
      (if (> a c)
          (sum-sq a b)
          (sum-sq b c))))

                          
;; EXERCISE 1.4
;; This procedure adds the absolute value of b to a.
;; It does so by checking whether b is positive
;; or non-positive as its first combination. This evaluates to either the `add` or `subtract` operator
;; which is then applied to a and b, hence subtracting be from a if b is negative and adding it to a
;; if it is positive.


;; EXERCISE 1.5
;; normal-order: evaluate operators first (until primitive operators are reached),
;; then evaluate operands 
;; applicative-order: evaluate operands and operators simultaneously and then apply resulting
;; operator to resulting operands
;; Code:
(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))
(test 0 (p))
;; Answer:
;; normal-order
;; Evaluates to 0 (first, the operators 0 and (p) are substituted into the body of test, then the predicate
;; in test is evaluated which results in the value 0

;; applicative-order
;; Never ending recursive loop since (p) is defined by itself and as it is an operator it is evaluated before
;; evaluating the procedure test on its operators


;; EXERCISE 1.6

;; Since the evaluater uses applicative order evaluation,
;; it will evaluate operands before applying operators.
;; Thus, using the procedure new-if in sqrt-iter will result in the evaluator first evaluating the
;; operands to new-if. The second operator is a recursion call to sqrt-iter, hence an infinite recursion
;; will occur.


;; EXERCISE 1.7
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1. x))
;; For small numbers the `good-enough?` test will be too imprecise.
;; As the square of `guess` only needs to be within +/-0.001 of x, for values of `x` <= 0.001,
;; `guesses` whose squares are less than 0.001 will be considered good enough,
;; e.g. a guess of 0.01 will be good enough although the true square root of x is approx. 0.031
(square (sqrt 0.001))
;; 0.0017 (1.7 times actual value)

;; For very large numbers, `guess` will not be guaranteed to ever be good enough
;; as it is improved via `improve` due to
;; floating point precision constraints during evaluation. Hence, the computation will recurse ad infinium.
(sqrt 1e13) ;; note that it sometimes does return.

;; Improved convergence test:
(define (alt-sqrt x)

  (define (good-enough? guess prev-guess)
    (< (/ (abs (- guess prev-guess)) guess) 0.001))

  (define (improve guess)
    (average guess (/ x guess)))

  (define (sqrt-iter guess prev-guess)
    (if (good-enough? guess prev-guess)
        guess
        (sqrt-iter (improve guess)
                    guess)))

    (sqrt-iter 1. 0))

(square (alt-sqrt 0.001))
(square (alt-sqrt 1e13))
  

;; EXERCISE 1.8
(define (cbrt x)
  (define (improve guess)
    (/ (+
        (/ x
           (square guess))
        (* 2 guess))
       3))
  (define (good-enough? guess prev-guess)
    ;; (< (abs (- guess prev-guess)) 0.0001))
    (< (/ (abs (- guess prev-guess)) guess) 0.001))

  (define (cbrt-iter guess prev-guess)
    (if (good-enough? guess prev-guess)
        guess
        (cbrt-iter (improve guess) guess))
)
  (cbrt-iter 1.0 0))

(cbrt 81)
