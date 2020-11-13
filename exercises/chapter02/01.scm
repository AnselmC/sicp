(define (numer rat)
  (car rat))

(define (denom rat)
  (cdr rat))

(define (print-rat rat)
  (newline)
  (display (numer rat))
  (display "/")
  (display (denom rat)))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

;; EXERCISE 1
(define (make-rat n d)
  ;; if both are positive: (cons n d)
  ;; if n is negative and d is positive: (cons n d)

  ;; if both are negative: (cons (* -1 n) (*- 1 n))
  ;; if n is positive and d is negative: (cons (* -1 n) (* -1 n))
  (let ((g (gcd n d)))
    (if (> d 0)
        (cons (/ n g) (/ d g))
        (cons (* -1 (/ n g)) (* -1 (/ d g))))))
      
;; EXERCISE 2
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (make-segment p1 p2)
  (cons p1 p2))

(define (start-segment seg)
  (car seg))

(define (end-segment seg)
  (cdr seg))

(define (midpoint-segment seg)
  (define (avg a b)
    (/ (+ a b)
       2))
  (make-point
   (avg (x-point (start-segment seg))
        (x-point (end-segment seg)))
   (avg (y-point (start-segment seg))
        (y-point (end-segment seg)))))

;; EXERCISE 3
(define (add-points a b)
  (make-point (+ (x-point a) (x-point b))
              (+ (y-point a) (y-point b))))

(define (make-axis-aligned-rectangle p-bottom-left p-top-right)
  (cons (make-segment p-bottom-left (make-point  (+ (x-point p-bottom-left)
                                                    (- (x-point p-top-right)
                                                       (x-point p-bottom-left)))
                                                 (y-point p-bottom-left)))
        (make-segment p-bottom-left (make-point (x-point p-bottom-left)
                                                (+ (y-point p-bottom-left)
                                                   (- (y-point p-top-right)
                                                      (y-point p-bottom-left)))))))

(define (side-one aar)
  (car aar))

(define (side-two aar)
  (cdr aar))

(define (length seg)
  (sqrt (+
         (square (- (x-point (end-segment seg))
                    (x-point (start-segment seg))))
         (square (- (y-point (end-segment seg))
                    (y-point (start-segment seg)))))))

(define my-rect (make-axis-aligned-rectangle (make-point 3.0 2.5) (make-point 8.0 9.0)))
(area my-rect)
(perimeter my-rect)

(define (area aar)
  (* (length (side-one aar))
     (length (side-two aar))))

(define (perimeter aar)
  (+ (* 2 (length (side-one aar)))
     (* 2 (length (side-two aar)))))

;; alternative lazy impl.
(define (make-axis-aligned-rectangle p-bottom-left p-top-right)
  (cons p-bottom-left p-top-right))

(define (side-one aar)
  (let ((p-bottom-left (car aar))
        (p-top-right (cdr aar)))
    (make-segment p-bottom-left (make-point  (+ (x-point p-bottom-left)
                                                (- (x-point p-top-right)
                                                   (x-point p-bottom-left)))
                                             (y-point p-bottom-left)))))
(define (side-two aar)
  (let ((p-bottom-left (car aar))
        (p-top-right (cdr aar)))
    (make-segment p-bottom-left (make-point (x-point p-bottom-left)
                                            (+ (y-point p-bottom-left)
                                               (- (y-point p-top-right)
                                                  (y-point p-bottom-left)))))))



;; EXERCISE 4
(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))

(define pair (cons 4 5))
(car pair) ;; 4
(cdr pair) ;; 5

;; EXERCISE 5
(define (cons a b)
  (* (expt 2 a)
     (expt 3 b)))

(define (car z)
  (define (iter x)
    (if (= 0 (remainder x 3))
        (iter (/ x 3))
        (/ (log x)
           (log 2))))
  (iter z))

(define (cdr z)
  (define (iter x)
    (if (= 0 (remainder x 2))
        (iter (/ x 2))
        (/ (log x)
           (log 3))))
  (iter z))

(define pair (cons 13 12))
(car pair)
(cdr pair)

;; EXERCISE 6
(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(add-1 zero)
(lambda (f) (lambda (x) (f (((lambda (f2) (lambda (x2) x2)) f) x))))
(lambda (f) (lambda (x) (f (((lambda (x2) x2) x)))))
(lambda (f) (lambda (x) (f x)))

(define one (lambda (f) (lambda (x) (f x)))) ;; returns a procedure that takes f as argument, which in turn returns a procedure that takes x as argument and applies f to x

(add-1 one)
(lambda (f) (lambda (x) (f ((one f) x))))
(lambda (f) (lambda (x) (f (((lambda (f2) (lambda (x2) (f2 x2))) f) x))))
(lambda (f) (lambda (x) (f ((lambda (x2) (f x2)) x))))
(lambda (f) (lambda (x) (f (f x))))

(define two (lambda (f) (lambda (x) (f (f x)))))
((two (lambda(x) (1+ x))) 0)

(define (+-church n m)
  (lambda (f) (lambda (x) ((n f) ((m f) x)))))

(define three (+-church one two))
(define four (+-church three one))
(define seven (+-church four three))

;; EXERCISE 7
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x 
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

(define (make-interval a b) (cons a b))

(define (lower-bound int)
  (car int))

(define (upper-bound int)
  (cdr int))

;; EXERCISE 8
(define (sub-interval x y)
  (make-interval (- (lower-bound x)
                    (upper-bound y))
                 (- (upper-bound x)
                    (lower-bound y))))

;; EXERCISE 9
(define (width x)
  (/ (- (upper-bound x)
        (lower-bound x))
     2))
;; Addition + Subtraction
;; Given two intervals a = [i, j] and b = [k, l] with width a_w = (j-i)/2 and b_w = (l-k)/2
;; and their addition c = a + b = [i+k, j+l] with width c_w = (j+l - (i+k))/2 = (j+l-i-k)/2 = ((j-i)+(l-k))/2
;; we see that c_w can be expressed as: c_w = (2*a_w + 2*b_w)/2 = a_w + b_w.
;; The same holds true for subtraction where c = a - b = [i-l, j-k] and c_w = (j-k-(i-l))/2 = (j-k-i+l)/2 = a_w + b_w 
;; Multiplication
;; This doesn't hold true for multiplication due to the non-linearity of the min/max selectors, e.g.
;; for a = [0, 1] (a_w = 0.5) and b = [1, 3] (b_w = 2), c=a*b=[0, 3] and c_w=1.5
;; and a_2 = [0, 0] (a_2_w = 0) and b_2 = [1, 3] (b_2_w = 2), c_2 = a_2 * b_2=[0, 0] and c_2_w=0
;; and a_3 = [0, 0] (a_3_w = 0) and b_3 = [0, 0] (b_3_w = 0), c_3 = a_3 * b_3=[0, 0] and c_3_w=0.

;; EXERCISE 10
(define (div-interval-alt x y)
  (if (<= (* (upper-bound y) (lower-bound y)) 0)
      (error "Can't divide by interval which spans 0!")
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))


(define x (make-interval 1 1))
(define y (make-interval 1 5))
(define x+y (add-interval x y))
(define x-y (sub-interval x y))
(define x*y (mul-interval x y))
(define y/x (div-interval y x))
(define y/x-alt (div-interval-alt y x))

;; EXERCISE 11
(define (mul-interval-alt x y)
  (define (neg-int a)
    (and (< (upper-bound a) 0)
         (< (lower-bound a) 0)))
  (define (pos-int a)
    (and (> (lower-bound a) 0)
         (> (upper-bound a) 0)))

  (define (neg-to-pos-int a)
    (and (< (lower-bound a) 0)
         (> (upper-bound a) 0)))
  
  (cond ((pos-int x)
         (cond ((pos-int y)
                (make-interval (* (lower-bound x) (lower-bound y))
                               (* (upper-bound x) (upper-bound y))))
               ((neg-int y)
                (make-interval (* (upper-bound x) (lower-bound y))
                               (* (lower-bound x) (upper-bound y))))
               (else
                (make-interval (* (upper-bound x) (lower-bound y))
                               (* (upper-bound x) (upper-bound y))))))
         ((neg-int x)
          (cond ((pos-int y)
                 (make-interval (* (lower-bound x) (upper-bound y))
                                (* (upper-bound x) (lower-bound y))))
                ((neg-int y)
                 (make-interval (* (upper-bound x) (upper-bound y))
                                (* (lower-bound x) (lower-bound y))))
                (else
                 (make-interval (* (lower-bound x) (upper-bound y))
                                (* (lower-bound x) (lower-bound y))))))
         (else
          (cond ((pos-int y)
                 (make-interval (* (lower-bound x) (upper-bound y))
                                (* (upper-bound x) (upper-bound y))))
                ((neg-int y)
                 (make-interval (* (upper-bound x) (lower-bound y))
                                (* (lower-bound x) (lower-bound y))))
                (else ;; TODO: fix
                 (make-interval (* (lower-bound x) (upper-bound y))
                                (* (upper-bound x) (upper-bound y))))))))

;; EXERCISE 12
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (make-center-percent c pct)
  (let* ((tolerance (* c (/ pct 100)))
         (lower (- c (/ tolerance 2)))
         (upper (+ c (/ tolerance 2))))
    (make-interval lower upper)))

(define (percent int)
  (* 100
     (/ (width int)
     (center int))))

;; EXERCISE 13
;; Given an interval a with center c and tolerance t, we write a = [c; t]
;; the width of a is given as w(c, t) = c * t / 2 and hence the bounds are [c-c*t/2, c+c*t/2]
;; or [c(1-t/2), c(1+t/2)].
;; As previously shown, multiplying two intervals b = [i, j] and c = [k, l] where i, j, k, l > 0
;; results in the interval d = [i*k, j*l].
;; Re-writing d in center-tolerance form, we get d = [(i*k + j*l)/2; 4*(jl-ik)/(jl+ik)]
;; with t_d = 4*(jl-ik)/(jl+ik)
;; For b and c tolerances are t_b=
;; TODO

;; EXERCISE 14
(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1))) 
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

(define r1 (make-center-percent 3 0.0))
(define r2 (make-center-percent 5 0.05))
(div-interval r1 r1) ;; only [1, 1] if uncertainty is 0, else uncertainties will diverge the interval
(div-interval r1 r2)

(define res1 (par1 r1 r2))
(define res2 (par2 r1 r2))

(center res1)
(percent res1) ;; larger than for res2

(center res2)
(percent res2)

;; EXERCISE 15
;; Yes, she is correct.
;; Every uncertainty that is introduced in an equation adds multiplicatively to the resulting uncertainty.
;; Hence, replacing the `one` defintion by R1/R1 will also introduce more uncertainty,
;; although argebraically its obviously identical.


;; EXERCISE 16
;; TODO
