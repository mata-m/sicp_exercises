#lang sicp
(define (cube x) (* x x x))
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))
(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))
(integral cube 0 1 0.01)


; Ex: 1.31
; recursive product of function values in range
(define (prod a b func)
  (if (> a b)
      1
      (* (func a)
         (prod (+ 1 a) b func))))
; Iterative product of function values in range
(define (prod-iter a b func)
  (define (p-iter a b func result)
    (if (> a b)
       result
       (p-iter (+ 1 a) b func (* result (func a)))))
  (p-iter a b func 1)
)

;(prod 1 4 cube)
;(prod-iter 1 5 cube)

; 1.32

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner
                            null-value
                            term
                            (next a)
                            next
                            b))))


(define (accumulate-iter combiner null-value term a next b)
  (define (a-iter a result)
    (if (> a b)
      result
      (a-iter   (next a) (combiner result (term a)))))
  (a-iter a null-value)
)
(define (inc a)
  (+ a 1))
(accumulate-iter * 1 cube 1 inc 4)

; 1.33
  (define (filtered-accumulate combiner null-value term a next b filter) 
  (if (> a b) null-value 
      (if (filter a) 
          (combiner (term a) (filtered-accumulate combiner null-value term (next a) next b filter)) 
          (combiner null-value (filtered-accumulate combiner null-value term (next a) next b filter)))))



(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

; Rational number implemenation using pairs and abstraction examples
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
(define (abs x)
  (if (> x 0)
      x
      (* -1 x)))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n (abs g)) (/ d (abs g)))))

; Added pretty printing for negative fractions
(define (better-make-rat n d)
  (let ((g (gcd (abs n) (abs d))))
    (cond ((and (< n 0) (< d 0)) (cons (/ (abs n)
                                          g)
                                       (/ (abs d)
                                          g)))
          ((and (> n 0) (< d 0)) (cons (/ (* -1 n)
                                          g)
                                       (/ (abs d)
                                          g)))
          (else (cons (/ n g) (/ d g))))))

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (display (numer x))
  (display '/)
  (display (denom x))
  (newline))

(define one-half (better-make-rat 3 9))
(print-rat one-half)
