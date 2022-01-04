#lang sicp
(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))
(define (length items)
  (define (length-iter a count)
    (if (null? a)
        count
        (length-iter (cdr a) (+ 1 count))))
  (length-iter items 0))

(define squares (list 1 4 9 16 25))
(define odds (list 1 3 5 7))
;(length odds)
;(append squares odds)

; Ex 2.17
(define (last-pair items)
  (let ((rest (cdr items)))
    (if (null? rest)
        items
        (last-pair rest))))

;(last-pair (list 23 72 149 34 1223))

; Ex 2.18
(define (reverse li)
  (if (null? (cdr li))
      (cons (car li) nil)
      (append (reverse (cdr li)) (list (car li)))))

;(reverse (list 1 4 9 16 25))


; Ex 2.19
(define us-coins (list 50 25 10 5 1))

(define uk-coins (list 100 50 20 10 5 2 1 0.5))

 (define (cc amount denominations)
   (define (first-denomination values)
       (car values))

   (define (except-first-denom values)
       (cdr values))

   (define (no-more? values)
       (null? values))
   (cond  
    ;; If there's no change left, we have a solution 
    ((= amount 0) 1) 
     
    ;; If we're gone -ve amount, or there are no more kinds of coins 
    ;; to play with, we don't have a solution. 
    ((or (< amount 0) (no-more? denominations)) 0) 
     
    (else 
     ;; number of ways to make change without the current coin type 
     ;; plus the number of ways after subtracting the amount of the 
     ;; current coin. 
     (+ (cc amount (except-first-denom denominations)) 
        (cc (- amount  
               (first-denomination denominations))  
            denominations))))) 



 ;(cc 100 (list 50 25 10 5 1))

; Ex 2.20

(define (odd? x)
  (if (= 0 (remainder x 2))
      #f
      #t))


 (define (same-parity . L) 
   (define (filter x) 
     (if (even? x) 
         even? 
         odd?)) 
   (define (construct f L) 
     (cond ((null? L) nil) ; Reached end of list so append null to denote end
           ((f (car L)) (cons (car L)  
                                     (construct f (cdr L)))) ; Current is same parity so return it cons'd with following of same parity
           (else (construct f (cdr L))))) ; Current num is not same parity 
   (construct (filter (car L)) L))

;(same-parity 1 2 3 4 5 6 7)
;(same-parity 2 3 4 5 6 7)

;(append  (list 2 3 4) (list nil))

; Ex: 2.21
(define (square x) 
  (* x x))
(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items)) (square-list (cdr items)))))

(define (square-list-map items)
  (map square items))
;(square-list-map (list 1 2 3 4))

; Ex 2.22
(define (square-list-rev items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (if (null? answer)
              (list (square (car things)))
              (append answer
                      (list (square (car things))))))))
  (iter items nil))

; (square-list-rev (list 2 3 4))

; Ex 2.23

(define (for-each proc list) 
   (cond 
    ((null? list) #t) 
    (else (proc (car list)) 
          (for-each proc (cdr list))))) 

(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))


; Ex 2.24

; (1 (2 (3 4)))
(list 1 (list 2 (list 3 4)))


 ;           (1 (2 (3 4)))
;             |    |
;             1  (2 (3 4))
;                 |    |
;                 2  (3 4)
;                    |   |
;                    3   4


; Ex 2.25

(define l (list 1 3 (list 5 7) 9))
(define l1 (list (list 7)))
(define l2 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

(car (cdr (car (cdr (cdr l)))))
(car (car l1))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr l2))))))))))))

; Ex 2.26
;(define x (list 1 2 3))
;(define y (list 4 5 6))


; Ex 2.27
; Reverse's sublists as well as main list
(define (deep-reverse li)
  
  (cond ((null? li) nil) 
         ((pair? (car li)) ; Either we have a sublist  
          (append 
           (deep-reverse (cdr li)) 
           (list (deep-reverse (car li))))) ; Reverse the sublist in addition to rest of the parent list
         (else             ; Or we have a non-list
          (append 
           (deep-reverse (cdr li)) 
           (list (car li)))))) ; Only reverse the parent list as this element is not a sublist

(define x (list (list 1 2) (list 3 4) 5))
x
(deep-reverse x)