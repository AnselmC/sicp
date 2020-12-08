;; EXERCISE 73
;; a)
;; `(get 'deriv operator) gets the appropriate derivation procedure for a given expression type from the table
;; This is then applied to the operands w.r.t. the variable.
;; In principle they could, but they would need an operator, say 'number and 'var.
;; This would however be very impractical and, e.g., one would have to write
;; '(+ (number 3) (var x)) instead of '(+ 3 x). It would also require the overhead of having these types
;; (for which primitive predicates exist) in the table with convoluted procedures for e.g. `same-variable`.
;;
;; b)
(define (deriv-sum exp var)
  (define (tag x) (attach-tag '+ x))
  (tag
   (make-sum (deriv (addend exp) var)
             (deriv (augend exp) var))))

(define (deriv-prod exp var)
  (define (tag-sum x) (attach-tag '+ x))
  (define (tag-prod x) (attach-tag '* x))
  (tag-sum
   (make-sum
    (tag-prod
     (make-product (multiplier exp)
                   (deriv (multiplicand exp) var))
    (tag-prod
     (make-product (deriv (multiplier exp) var)
                   (multiplicand exp)))))))

(put 'deriv '(+) deriv-sum)
(put 'deriv '(*) deriv-prod)

;; c)
(define (deriv-expt exp var)
  (define (tag-prod x) (attach-tag '* x))
  (define (tag-expt x) (attach-tag '** x))
  (define (tag-sum x) (attach-tag '+ x))
  (tag-prod
   (make-product
    (tag-prod
     (make-product (exponent exp)
                   (tag-expt
                    (make-exponentiation (base exp)
                                         (tag-sum (make-sum (exponential exp) -1)))))
     (deriv (base exp) var)))))
(put 'deriv '(**) deriv-expt)

;; d)
;; The `put` statements would have to change to (put '(operator) 'deriv proc)

;; EXERCISE 74
;; a)
;; need a table with personnel-file as columns and procedure names as rows
;;
;;                                      divisions
;;
;;                 |        | division-a | division-b |  
;;                 |--------|------------|------------|
;;   procedures    | record | proc-rec-a | proc-rec-b |
;;                 | salary | proc-sal-a | proc-sal-b |
;;
;; the personnel-files should be a pair such as '(division-name file)
;; the stored procedure should take the personnel file and employee name and return the employee record or false if record is not present
(define (division-name personnel-file)
  (car personnel-file))

(define (get-info op employee-name personnel-file)
  (let* ((div-name (division-name personnel-file))
         (file (file personnel-file))
         (proc (get op div-name)))
    (proc employee-name file)))

(define (get-record employee-name personnel-file)
  (get-info 'record employee-name personnel-file))

;; b)
(define (get-salary employee-name personnel-file)
  (get-info 'salary employee-name personnel-file))

;; c)
(define (find-employee-record employee-name personnel-files)
  (define (find-employee-record-iter remaining-files)
    (if (null? remaining-files)
        false ;; didn't find employee-record anywhere
        (let* ((next-file (car remaining-files))
               (possible-record (get-record employee-name next-file)))
          (if (eq? possible-record false) ;; didn't find record
              (find-employee-record-iter (cdr remaining-files)) ;; search in remaining files
              possible-record)))) ;; found record, return it
  (find-employee-record-iter personnel-files))

;; d)
;; New procedures for the division for getting the salary or employee record must be `put` into the table/installed.
        

;; EXERCISE 75
(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude)
           (sqrt (+ (square x) (square y))))
          ((eq? op 'angle) (atan y x))
          (else
           (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
  dispatch)

(define (make-from-mag-ang mag ang)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* (mag z) (cos (ang z))))
          ((eq? op 'imag-part) (* (mag z) (sin (ang z))))
          ((eq? op 'magnitude) mag)
          ((eq? op 'angle) ang)
          (else
           (error "Unknown op -- MAKE-FROM-MAG-ANG" op))))
  dispatch)

;; EXERCISE 76

;; 1) generic operations with explicit dispatch: each generic operations has to be changed to account for a new type,
;; i.e. a condition checking for it and then dispatching appropriately has to be added. Conversely, the operations have to be implemented for the new type.

;; 2) data-directed style: the operations have to be implemented for the new type and installed into the table for reference; the generic procedure do not need to be touched.

;; 3) message-passing style: the operations need to be implemented in the internal dispatch method of the new type.

;; --> all three procedures entail the effort (obviously) of implementing the procedures for the new type. However, only the message-passing style has no more work to do after this.
;; Hence, this seems to be the most appropriate for a system in which new types are added frequently. 
;; However, if new operations are added frequently, the data-directed style makes more sense, as the new procedures have to only be installed into the table whereas for the message-passing style
;; each object type needs to be re-installed with the included procedures.


