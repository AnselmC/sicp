;; Exercise 29
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (even? x)
  (= 0 (remainder x 2)))

(define (cube x) (* x x x))

(define (inc i)
  (+ i 1))

(define (dec i)
  (- i 1))

(define (identity x)
  x)

(define (double x)
  (* 2 x))

(define (integral-simpson f a b n)
    
  (define h
    (/ (- b a)
       n))

  (define (next k)
    (+ a
       (* h k)))


  (define (term k)
    (if (or (= k 0)
            (= k n))
        (f (next k))
        (if (even? k)
            (* 2
               (f (next k)))
            (* 4
               (f (next k))))))

  (* (/ h 3)
     (sum term 0 inc n)))
    

(integral-simpson cube 0 1 1000) ;; 1/4
(integral-simpson cube 0 1 10000) ;; 1/4


;; EXERCISE 30
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result
                          (term a)))))
  (iter a 0))


;; EXERCISE 31
;; a)
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result
                          (term a)))))
  (iter a 1))

(define (factorial a)
  (product identity 1 inc a))

(factorial 5)

(define (approx-pi n)

  (define (f x)
    (/ (square (double x))
       (*
        (inc (double x))
        (dec (double x)))))
  (* 2
     (product f 1 inc n)))

(approx-pi 30)


;; b)
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))


;; EXERCISE 32
;; a)
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

(define (product term a next b)
  (accumulate * 1 term a next b))

(define (sum term a next b)
  (accumulate + 0 term a next b))

;; b)
(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result
                                 (term a)))))
  (iter a null-value))

;; EXERCISE 33
(define (filtered-accumulate combine null-value term a next b filter)
  (define (iter a result)
    (if (> a b)
        result
        (if (filter a)
            (iter (next a) (combiner result
                                     (term a)))
            (iter (next a) result)))))

;; a)
(define (sum-square-primes a b)
  (filtered-accumulate + 0 square a inc b prime?))

;; b)
(define (product-relative-primes n)
  (define (filter x)
    (= 1
       (gcd x n)))
  (filtered-accumulate * 1 identity 0 inc (-1+ n) filter))


;; EXERCISE 34
(define (f g)
  (g 2))

;; (f f) --> (f 2) --> (2 2) --> error, can't apply 2 to 2

;; EXERCISE 35
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;; fixed-point: f(x) = x
;; show that golden ration is fixed-point of transformation x -> 1 + 1/x
;; golden ratio: x^2 = x + 1 --> x = 1 + 1/x
(fixed-point (lambda (x) (+ 1 (/ 1 x))) 2) ;; 987/610 = 1.618...


;; EXERCISE 36
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (newline)
      (display next)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;;  x -> log(1000)/log(x)
(fixed-point (lambda (x) (/ (log 1000)
                            (log x)))
             2)

(define (average x y)
  (/ (+ x y)
     2))
(fixed-point (lambda (x) (average x
                                  (/ (log 1000)
                                     (log x))))
             2)

;; average damping reduces the steps by factor of ~5

;; EXERCISE 37
;; a)
(define (cont-frac n d k)
  (define (iter i res)
    (if (= i 0)
        res
        (iter (- i 1)
              (/ (n i)
                 (+ (d i)
                    res)))))
  (iter k 0))


(define (compute-golden k)
  (/ 1
     (cont-frac (lambda (i) 1.0)
                (lambda (i) 1.0)
                k)))

(compute-golden 12) ;; 1.6180...

;; b)
(define (cont-frac n d k)
  (if (= k 1)
      (/ (n 1)
         (d 1))
      (/ (n 1)
         (+ (d 1)
         (cont-frac n d (- k 1))))))


;; EXERCISE 38
(define (approx-e k)
  (+ 2
     (cont-frac
      (lambda (i) 1.0)
      (lambda (i)
        (if (= 0 (remainder (+ i 1) 3))
            (* 2 (/ (+ i 1) 3))
            1.0))
      k)))
(approx-e 9)

;; EXERCISE 39
(define (cont-frac-gen n d k acc)
  (define (iter i res)
    (if (= i 0)
        res
        (iter (- i 1)
              (/ (n i)
                 (acc (d i)
                      res)))))
  (iter k 0))

(define (tan-cf x k)
  (cont-frac-gen
   (lambda (i)
     (if (= i 1)
         x
         (square x)))
   (lambda (i)
     (- (double i) 1.0))
   k
   -))

(tan-cf 5 20)


;; EXERCISE 40
(define dx 0.00001)

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (cubic a b c)
  (lambda (x)
    (+ (cube x)
       (* a (square x))
       (* b x)
       c)))

(newtons-method (cubic 1 1 4) 1)

;; EXERCISE 41
(define (double f)
  (lambda (x)
    (f (f x))))

((double inc) 1) ;; 3
(((double double) inc) 5) ;; 9
(((double (double double)) inc) 5) ;; 21

;; EXERCISE 42
(define (compose f g)
  (lambda (x)
    (f (g x))))

((compose square inc) 6) ;; 49

;; EXERCISE 43
(define (repeated f n)
  (define (iter i f-res)
    (if (= i n)
        f-res
        (iter (inc i) (compose f f-res))))
  (iter 1 f))

((repeated square 2) 5) ;; 625


;; EXERCISE 44
(define (smooth f)
  (lambda (x)
    (/ (+ (f x)
          (f (+ x dx))
          (f (- x dx)))
       3)))

(define (smooth-n-times f n)
  (repeated (smooth f) n)) 

;; EXERCISE 45
;; TODO

;; EXERCISE 46
(define (iterative-improve good-enough? improve-guess)
  (lambda (guess)
    (define (iter current-guess prev-guess)
      (display "iterating")
      (newline)
      (if (good-enough? current-guess prev-guess)
          current-guess
          (iter (improve-guess current-guess) current-guess)))
    (iter guess 1.)))
    

(define (sqrt x)
  (define (good-enough? guess prev-guess)
    (display guess)
    (newline)
    (display prev-guess)
    (newline)
    (< (/ (abs (- guess prev-guess)) guess) 0.001))

  (define (improve-guess guess)
    (display "improving")
    (average guess (/ x guess)))
    
  (iterative-improve good-enough? improve-guess) 4.)

(sqrt 4)
