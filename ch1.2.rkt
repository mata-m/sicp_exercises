#lang sicp
(#%require (lib "27.ss" "srfi"))
(define (exp num ex)
  (exp-iter 1 num ex 1))
(define (exp-iter sum num max counter)
  (if (< max counter)
      sum
      (exp-iter (* sum num) num max (+ counter 1))))
;(exp 2 16)


; Iterative version (secret here was that you realized that you need to work bottom up)
; Then you realized that to avoid recomputing stuff, you just move around the states and keep one that is the main
; This is more obvious when you work out f(5)'s recursive tree by hand

(define (f n)
  (define (f-iter one two three count)
    (if (< count 3)
      one
      (f-iter (+ one
                 (* 2 two)
                 (* 3 three))
              one
              two
              (- count 1))))
  (if (< n 3)
    n
    (f-iter 2 1 0 n)))
   
  ;n
  ;(+ (f (- n 1))
  ;   (* (f (- n 2)) 2)
  ;   (* (f (- n 3)) 3))))
;(f 7)

;1.12
(define (pascal r c)
  (cond ((or (< c 0) (> c (+ r 1))) 0)
        ((or (= c 0) (= r c) )1)
        ((and (= r 0) (or (= c 1) (= c 0))) 1)
        (else (+ (pascal (- r 1) (- c 1))
                 (pascal (- r 1) c)))))

;(pascal 6 4)
(define (even? n)
  (= (remainder n 2) 0))
(define (square x) (* x x))

; a * b^n is the equivalency you're trying to keep
; (1 * 2^5) == (2 * 2^4) == (2 * 4^2) == (2 * 16^1) == (32 * (16^0)) 
 (define (fast-expt b n) 
   (define (iter N B A) 
     (cond ((= 0 N) A) 
           ((even? N) (iter (/ N 2) (square B) A)) 
           (else (iter (- N 1) B (* B A))))) 
   (iter n b 1)) 

;(fast-expt 2 4 )

; 1.17
;(define (* a b)
;  (if (= b 0)
;      0
;      (+ a (* a (- b 1)))))

(define (double x) (* 2 x))
(define (half x) (/ x 2))
(define (mul b n)
  (define (mul-iter n b a)
    (cond ((= n 0) a)
          ((even? n) (mul-iter (half n) (double b) a))
          (else (mul-iter (- n 1) b (+ a b)))))
  (mul-iter n b 0))
(mul 2 4 )

(define (next test-divisor)
  (if (= test-divisor 2)
      3
      (+ test-divisor 2)))

(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))
(define (divides? a b)
  (= (remainder b a) 0))
;(smallest-divisor 19999)




; 1.22

; Computes the exponential of a number modulo another number
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))


(define (prime? n) 
  ;(= n (smallest-divisor n))
  (fast-prime? n 100)
  )

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n a) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime n (- (runtime) start-time))
      #f))

(define (report-prime n elapsed-time)
  (newline)
  (display n)
  (display " *** ")
  (display elapsed-time))
  (newline)

;(timed-prime-test 1009)

(define (search-for-primes larger-than)
  (define (start-search start-time num-to-find curr)
    (if (> num-to-find 0)
        (if (timed-prime-test curr)
            (start-search start-time (- num-to-find 1) (+ curr 2))
            (start-search start-time num-to-find (+ curr 2)))))
  (start-search (runtime) 3 larger-than) 
 ) 
; want to find 3 smallest primes larger than input
; need to track the # of primes found so far
; need to track the time it takes to find them
; need to track the number of primes found so we stop once we hit 3


(search-for-primes 1001)

