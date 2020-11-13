;; EXERCISE 1.9
(define (+ a b)
  (if (= a 0)
      b
      (inc (+ (dec a) b))))
;; (+ 4 5)
;; (inc (+ (dec 4) 5)
;; (inc (+ 3 5)
;; (inc (inc (+ (dec 3) 5)))
;; (inc (inc (+ 2 5)))
;; (inc (inc (inc (+ (dec 2) 5))))
;; (inc (inc (inc (+ 1 5))))
;; (inc (inc (inc (inc (+ (dec 1) 5)))))
;; (inc (inc (inc (inc (+ 0 5)))))
;; (inc (inc (inc (inc 5))))
;; (inc (inc (inc 6)))
;; (inc (inc 7))
;; (inc 8)
;; 9
;; --> Recursive process

(define (+ a b)
  (if (= a 0)
      b
      (+ (dec a) (inc b))))
;; (+ 4 5)
;; (+ (dec 4) (inc 5))
;; (+ 3 6)
;; (+ (dec 3) (inc 6))
;; (+ 2 7)
;; (+ (dec 2) (inc 7))
;; (+ 1 8)
;; (+ (dec 1) (inc 8))
;; (+ 0 9)
;; 9
;; --> Iterative process


;; EXERCISE 1.10
(define (A x y)
  (cond ((= y 0) 0) ;; returns 0 if y is 0
        ((= x 0) (* 2 y)) ;; returns 2y if x is 0
        ((= y 1) 2) ;; returns 2 if y is 1
        (else (A (- x 1) ;; else returns A of ((dec x) and A of (x and (dec y)))
                 (A x (- y 1))))))

(A 1 10)
;; (A 0 (A 1 9))
;; (* 2 (A 0 (A 1 8)))
;; (* 2 (* 2 (A 0 (A 1 7))))
;; (* 2 (* 2 (* 2 (A 0 (A 1 6)))))
;; (* 2 (* 2 (* 2 (* 2 (A 0 (A 1 5))))))
;; (* 2 (* 2 (* 2 (* 2 (* 2 (A 0 (A 1 4)))))))
;; (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (A 0 (A 1 3))))))))
;; (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (A 0 (A 1 2)))))))))
;; (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (A 0 (A 1 1))))))))))
;; (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 2)))))))))
;; (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 4))))))))
;; (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 8)))))))
;; (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 16))))))
;; (* 2 (* 2 (* 2 (* 2 (* 2 32)))))
;; (* 2 (* 2 (* 2 (* 2 64))))
;; (* 2 (* 2 (* 2 128)))
;; (* 2 (* 2 256))
;; (* 2 512)
;; 1024

(A 2 4)
;; (A 1 (A 2 3))
;; (A 1 (A 1 (A 2 2)))
;; (A 1 (A 1 (A 1 (A 2 1))))
;; (A 1 (A 1 (A 1 2)))
;; (A 1 (A 1 (A 0 (A 1 1))))
;; (A 1 (A 1 (A 0 2)))
;; (A 1 (A 1 (* 2 2)))
;; (A 1 (A 1 4))
;; (A 1 (A 0 (A 1 3)))
;; (A 1 (A 0 (A 0 (A 1 2))))
;; (A 1 (A 0 (A 0 (A 0 (A 1 1)))))
;; (A 1 (A 0 (A 0 (A 0 2))))
;; (A 1 (A 0 (A 0 (* 2 2))))
;; (A 1 (A 0 (A 0 4)))
;; (A 1 (A 0 (* 2 4)))
;; (A 1 (A 0 8))
;; (A 1 (* 2 8))
;; (A 1 16)
;; (A 0 (A 1 15))
;; ...
;; (A 0 32768)
;; (* 2 32768)
;; 65536

(A 3 3)
;; (A 2 (A 3 2))
;; (A 2 (A 2 (A 3 1)))
;; (A 2 (A 2 2))
;; (A 2 (A 1 (A 2 1)))
;; (A 2 (A 1 2))
;; (A 2 (A 0 (A 1 1)))
;; (A 2 (A 0 2))
;; (A 2 (* 2 2))
;; (A 2 4)
;; ...
;; 65536

(define (f n) (A 0 n))
;; f(n) = 2n

(define (g n) (A 1 n))
;; g(n) = 2^n

(define (h n) (A 2 n))
;; h(n) = n_2 (tetration)
;; h(0) = 0
;; h(1) = 2^1 = 2
;; h(2) = 2^(2^1) = 4
;; h(3) = 2^(2^(2^1)) = 16
;; h(4) = 2^(2^(2^(2^1))) = 65356


;; EXERCISE 1.11
;; f(n) = n if n<3 and f(n) = f(n - 1) + 2f(n - 2) + 3f(n - 3) if n> 3
;; f(0) = 0
;; f(1) = 1
;; f(2) = 2
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(define (fib n)
  (fib-iter 0 1 n))

(define (fib-iter a b count)
  (if (= count 0)
      a
      (fib-iter b (+ a b) (- count 1))))
  
(define (f-rec n)
  (if (< n 3)
      n
      (+ (f-rec (- n 1))
         (* 2
            (f-rec (- n 2)))
         (* 3
            (f-rec (- n 3))))))

(define (f-iter n)
  (define (f-iter-helper a b c counter)
    (if (= counter 0)
        a
        (f-iter-helper b
                       c
                       (+ (* 3 a)
                          (* 2 b)
                          c)
                       (- counter 1))))
    (f-iter-helper 0 1 2 n))


;; EXERCISE 1.12
(define (pascal row col)
  (cond ((= col 0) 1)
        ((= col row) 1)
        (else (+ (pascal (- row 1) col)
                 (pascal (- row 1) (- col 1))))))


;; EXERCISE 1.13
;; Prove that Fib(n) = (theta^n - psi^n)/sqrt(5) where theta = (1 + sqrt(5)) / 2 and psi = (1 - sqrt(5)) / 2
;; INIT
;; n = 0:
;; Fib(0) = 0
;; (theta^0 - psi^0)/sqrt(5) = (1-1)/... = 0
;; n = 1
;; Fib(1) = 1
;; (theta^1 - psi^1)/sqrt(5) = ... = sqrt(5)/sqrt(5) = 1
;; INDUCTION STEP
;; Assuming for some n and m = n+1 the claim holds, then
;; Fib(n+2) = Fib(n) + Fib(m) = ... = (theta^(n+1) + theta^n - (psi^(n+1) + psi^n))/sqrt(5)
;; Since theta^(n+1) + theta^n = theta^n * (1+theta)
;; and (1+theta) = (3+sqrt(5)/2) = theta^n
;; then theta^(n+1) + theta^n = theta^(n) * theta^2 = theta^(n+2)
;; and by similar equalities for psi
;; psi^(n+1) + psi^n = psi^(n) * psi^2 = psi^(n+2)
;; we get Fib(n+2) = (theta^(n+2) - psi^(n+2))/sqrt(5)
;; q.e.d.

;; EXERCISE 1.14
(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

;; Space Complexity: O(n^2) --> always about max depth
;; Time Complexity: O(n) 


;; EXERCISE 15
(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
   (if (not (> (abs angle) 0.1))
       angle
       (p (sine (/ angle 3.0)))))

;; a)
;; (sine 12.15)
;; (p (sine 4.05))
;; (p (p (sine 1.35)))
;; (p (p (p (sine 0.45))))
;; (p (p (p (p (sine 0.15)))))
;; (p (p (p (p (p (sine 0.05))))))
;; (p (p (p (p (p 0.05)))))
;; --> 5 times
;; b)
;; (sine a)
;; Order of growth: O(log(a)/log(3))


;; EXERCISE 16
(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (even? n)
  (= (remainder n 2) 0))

(define (fast-expt-iter b n)
  (define (fei-helper b n a)
    (if (= n 0)
        a
        (if (even? n)
            (fei-helper (* b b) (/ n 2) a)
            (fei-helper b (- n 1) (* a b)))))
  (fei-helper b n 1))

;; EXERCISE 17
(define (double x)
  (+ x x))

(define (halve x)
  (/ x 2))

(define (fast-mult a b)
  (cond ((= b 0) 0)
        (else
         (if (even? b)
             (if (= 1 (halve b))
                 (double a)
                 (+ (double a) (fast-mult a (halve b))))
          (+ a (fast-mult a (- b 1)))))))
          

;; EXERCISE 18
;; simplify
(define (fast-mult-iter a b)
  (define (fast-mult-helper a b c)
    (if (= b 0)
        c
        (if (even? b)
            (fast-mult-helper (double a)
                              (halve b)
                              c)
            (fast-mult-helper a
                              (- b 1)
                              (+ c a)))))
  (fast-mult-helper a b 0))


;; EXERCISE 19
(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (* p p)
                      (* q q)) ;; compute p'
                   (+ (* q
                         (+ q p)) ;; compute q'
                      (* q p))
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))


;; EXERCISE 20
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(gcd 206 40)
;; applicative-order eval: evaluate operands first, then apply operators
(gcd 40 (remainder 206 40))
(gcd 40 6)
(gcd 6 (remainder 40 6))
(gcd 6 4)
(gcd 4 (remainder 6 4))
(gcd 4 2)
(gcd 2 (remainder 4 2))
(gcd 2 0)
(2)
;; --> remainder is valled four times

;; normal-order eval: fully expand, then evaluate
(gcd 206 40) ;; 0 eval in if

(gcd 40
     (remainder 206 40));; 1 eval in if --> false

(gcd (remainder 206 40)
     (remainder 40 (remainder 206 40))) ;; 2 eval in if --> false

(gcd (remainder 40 (remainder 206 40))
     (remainder (remainder 206 40)
                (remainder 40 (remainder 206 40)))) ;; 4 eval in if --> false

(gcd (remainder (remainder 206 40)
                (remainder 40 (remainder 206 40)))
     (remainder (remainder 40 (remainder 206 40))
                (remainder (remainder 206 40)
                           (remainder 40 (remainder 206 40))))) ;;  7 eval in if --> true
(remainder (remainder 206 40)
           (remainder 40 (remainder 206 40)));; --> 4 eval in else

;; total: 1 + 2 + 4 + 7 + 4 = 18


;; EXERCISE 21
(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(smallest-divisor 199);; 199
(smallest-divisor 1999);; 1999
(smallest-divisor 19999);; 7


;; EXERCISE 22
(define (prime? n)
  (= n (smallest-divisor n)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (real-time-clock)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (real-time-clock) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time)
  (display "ms"))

(define (search-for-primes min nprimes)
  (define (found-prime)
    (timed-prime-test min)
    (search-for-primes (+ min 2)
                       (- nprimes 1)))
  (define (not-found-prime)
    (search-for-primes (+ min 2)
                       nprimes))
  (if (> nprimes 0)
    (if (even? min)
        (search-for-primes (+ min 1) nprimes)
        (if (prime? min)
            (found-prime)
            (not-found-prime)))))
(search-for-primes 10000000000 3) ;; around 60 ms per prime
;; 10000000019
;; 10000000033
;; 10000000061
(search-for-primes 100000000000 3) ;; around 200 ms per prime ~ factor 3
;; 100000000003
;; 100000000019
;; 100000000057
(search-for-primes 1000000000000 3) ;; around 600 ms per prime ~ factor 3
;; 1000000000039
;; 1000000000061 
;; 1000000000063
;; --> does support expected runtime (sqrt(10) ~ 3)

;; EXERCISE 23
(define (next test-divisor)
  (if (= test-divisor 2)
      3
      (+ test-divisor 2)))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(search-for-primes 10000000000 3) ;; around 40 ms per prime ~ 30% decrease
(search-for-primes 100000000000 3) ;; around 130 ms per prime ~ 30% decrease
(search-for-primes 1000000000000 3) ;; around 410 ms per prime ~ 30% decrease
;; --> does not confirm factor of 2 reduction / 50% decrease in runtime but rather only 30% decrease
;; TODO: why? is some other procedure not constant?

;; EXERCISE 24
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 10)
      (report-prime (- (real-time-clock) start-time))))


(search-for-primes 10000000000 3) ;; less than 1ms
(search-for-primes 100000000000 3) ;; less than 1ms
(search-for-primes 1000000000000 3) ;; less than 1ms
;; Expected 2-fold increase as log(100) = 2
;; However, the time for exponentiation is possibly dwarfed by random
;; TODO: use even larger numbers

(define (time-it func)
  (define (run-func start-time)
    (func)
    (display (- (real-time-clock) start-time))
    (display "ms")
    )
  (run-func (real-time-clock)))


;; EXERCISE 25
(define (expmod base exp m)
  (remainder (fast-expt base exp) m))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

;; never terminates because  ...

;; EXERCISE 26
;; unsure...

;; EXERCISE 27
;; Numbers that fool the Fermat test are called Carmichael numbers,
;; and little is known about them other than that they are extremely rare.
;; There are 255 Carmichael numbers below 100,000,000.
;; The smallest few are 561, 1105, 1729, 2465, 2821, and 6601.
;; In testing primality of very large numbers chosen at random,
;; the chance of stumbling upon a value that fools the Fermat test is
;; less than the chance that cosmic radiation will cause the computer to
;; make an error in carrying out a ``correct'' algorithm.
;; Considering an algorithm to be inadequate for the first reason
;; but not for the second illustrates the difference between mathematics and engineering.

(define (check-congruent n)
  (define (try-it a)
    (= (expmod a n n) a))
  (define (check-congruent-helper iter)
    (if (try-it iter)
        (if (= iter (- n 1))
            true ;; last value reach, return true
            (check-congruent-helper (+ iter 1))) ;; try next
        false))
  (check-congruent-helper 0))
    
(check-congruent 561)
(check-congruent 1105)
(check-congruent 1729)
(check-congruent 2465)
(check-congruent 2821)
(check-congruent 6601)

;; EXERCISE 28
;; TODO
;; Hint: two conds

  
