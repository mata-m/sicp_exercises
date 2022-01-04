#lang sicp
; Rational number implementation using pairs
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
  (if (< x 0)
      (* x -1)
      x))

(abs -1)

(define (make-rat n d)
  (let ((g (gcd n d)))
   (cond ((and (< n 0)
               (< d 0)) (cons (/ (abs n) g) (/ (abs d) g)))
         ((and (> n 0)
               (< d 0)) (cons (* (/ n g) -1) (/ (abs d) g)))
         (else (cons (/ n g) (/ d g)))
         )))
(+ -1 2)

(define (numer x) (car x))
(define (denom x) (cdr x))
(define (print-rat x)
  (display (numer x))
  (display '/)
  (display (denom x))
  (newline))
(define test (make-rat -1 -2))
(define test-two (make-rat 2 1))
;(equal-rat? test test-two)
(print-rat test)


; Rectangle representation built on point and segment abstractions
(define (y-point point)
  (cdr point))
(define (x-point point)
  (car point))
(define (make-point x-point y-point)
  (cons x-point y-point))

(define (end-segment seg)
  (cdr seg))
(define (start-segment seg)
  (car seg))
(define (make-segment start-point end-point)
  (cons start-point end-point))

(define (midpoint-segment seg)
  (define (average a b) (/ (+ a b) 2.0))
  (let ((a (start-segment seg))
        (b (end-segment seg)))
    (cons (average (x-point a)
                   (x-point b))
          (average (y-point a)
                   (y-point b)))))

(define startPoint (make-point 1 1))
(define endPoint   (make-point 3 3))

(define mySeg (make-segment startPoint endPoint))
(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  (newline))

(print-point (midpoint-segment mySeg))

(define top-segment cdr)
(define side-segment car)

(define (make-rectangle side-seg top-seg)
  (cons side-seg top-seg))

(define (get-top-bottom-length rect)
  (let ((seg-start (top-segment rect))
        (seg-end   (top-segment rect)))
    (abs (- (x-point seg-start)
            (x-point seg-end)))))

(define (get-left-right-length rect)
  (let ((seg-start (side-segment rect))
        (seg-end   (side-segment rect)))
    (abs (- (y-point seg-start)
            (y-point seg-end)))))

(define (rectangle-perimeter rect)
  (+ (* 2 (get-top-bottom-length rect))
     (* 2 (get-left-right-length rect))))

(define (rectangle-area rect)
  (* (get-top-bottom-length rect)
     (get-left-right-length rect)))

; Other ways to represent are as: a corner + width + height
                                ; bottom-left + top-right
(define (make-rectangle-from-points top-left top-right bottom-left)
  (cons (make-segment top-left bottom-left)
        (make-segment top-left top-right)))

; Ex 2.4
(define (cons x y)
  (lambda (m) (m y x ))) 

(define (car z)
  (z (lambda (p q) p))) ; The inner lambda acts as a selector. Which then gets passed to the lambda
                        ; returned by cons so that the selector can act on the pair saved within
(define (cdr z)
  (z (lambda (p q) q))) ; Same as cdr

(define test-diff-cons (cons 1 2))
(cdr test)


; Ex 2.5

; 2 * 2 * 2 * 2 * 3 * 3 * 3        2 * 2 * 2 * 3 * 3 * 3
; -------------------------   ->>  ---------------------
;          2                                1
; The idea is that you repeat the above process until the remainder is not 0
; In other words to find how many 2's you had, divide by 2 until you've essentially
; divided all the 2's out of the original product. At that point you'll try to divide
; an odd by an even which whil give non-zero remainder
; For the inverse (when dividing out the 3's) you've eventually get to dividing an
; even by an odd which is also always going to give a non-zero remainder

; Ex 2.7
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
   (if (= (upper-bound y) (lower-bound y)) 
     (error "division by zero interval") 
     (mul-interval x  
       (make-interval (/ 1.0 (upper-bound y)) 
                       (/ 1.0 (lower-bound y)))))) 
; Ex 2.8
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(define (make-interval a b) (cons a b))





