#lang sicp
; Problem Set 2

; BlackJack
; Two players
; Objective: Get as close to 21 as possible without going over
; Values:
        ;- Face Cards:  10
        ;- Number Cards:Whatever # they are
;

; Each player's strategy is represented as a function
; with 2 args (current hand, opponent's face-up card)
; Returns True if wants to hit, False if staying
; Example of stupid strategy

;;; Scheme code for Twenty-One Simulator 

(define (twenty-one player-strategy house-strategy)
  (let ((house-initial-hand (make-new-hand (deal)))) ; set up house hand
     ; The reason we use nested let's instead of doing all of them in one
     ; is that we use house-initial-hand in the definition of the nested let
     ; This would not be valid if all was in the same let since it wouldnt be in scope
    (let ((player-hand ; setup initial hand and play out
           (play-hand player-strategy
                      (make-new-hand (deal))
                      (hand-up-card house-initial-hand))))
      (if (> (hand-total player-hand) 21)
          0                                ; ``bust'': player loses
          (let ((house-hand 
                 (play-hand house-strategy
                            house-initial-hand
                            (hand-up-card player-hand))))
            (cond ((> (hand-total house-hand) 21)
                   1)                      ; ``bust'': house loses
                  ((> (hand-total player-hand)
                      (hand-total house-hand))
                   1)                      ; house loses
                  (else 0)))))))           ; player loses

(define (play-hand strategy my-hand opponent-up-card)
  (cond ((> (hand-total my-hand) 21) my-hand) ; I lose... give up
        ((strategy my-hand opponent-up-card) ; hit?
         (play-hand strategy
                    (hand-add-card my-hand (deal))
                    opponent-up-card))
        (else my-hand)))                ; stay


(define (deal) (+ 1 (random 10)))

(define (make-new-hand first-card)
  (make-hand first-card first-card))

(define (make-hand up-card total)
  (cons up-card total))

(define (hand-up-card hand)
  (car hand))

(define (hand-total hand)
  (cdr hand))

(define (hand-add-card hand new-card)
  (make-hand (hand-up-card hand)
             (+ new-card (hand-total hand))))

(define (hit? your-hand opponent-up-card)
  (newline)
  (display "Opponent up card ")
  (display opponent-up-card)
  (newline)
  (display "Your Total: ")
  (display (hand-total your-hand))
  (newline)
  (display "Hit? ")
  (user-says-y?))


; Problem 2
; returns a procedure that stops saying you should hit if you've reached the stop number
(define (stop-at stop-number)
  (lambda (hand up-card)
    (if (< (hand-total hand) stop-number)
        true
        false)))

(define (print-test-game-results p1 p2)
  (newline)
  (display "Player 1: ")
  (display p1)
  (display "    Player 2:")
  (display p2))

; Problem 3
(define (test-strategy strat-one strat-two game-count)
  (define (start-test player-games-won house-games-won)
    ; notice how start test is solely responsible for keeping track of games won, and playing games
    (if (= game-count (+ player-games-won
                         house-games-won))
    (print-test-game-results player-games-won house-games-won); Quit testing if played allotted games
    (if (< 0  (twenty-one strat-one strat-two))
        (start-test (inc player-games-won) house-games-won)
        (start-test player-games-won (inc house-games-won)))))
  ; Notice how test-strategy is solely an interface for the inner function
  (start-test 0 0)
)

; Problem 4
(define (watch-player strategy)
  (lambda (my-hand opponent-upcard)
    (let ( (result (strategy my-hand opponent-upcard)) )
    ; print my-handtotal
    (newline)
    (display "Player's hand-total: ")
    (display (hand-total my-hand))
    ; print the result of the strategy
    (display " Strat says: ")
    (display result)
    ; return the result of the strategy
    result)))

; Problem 5
(define (louis my-hand opponent-upcard)
  (cond ((< (hand-total my-hand) 12) true)
        ((> (hand-total my-hand) 16) false)
        ((= (hand-total my-hand) 12) (if (< opponent-upcard 4)
                                     true
                                     false))
        ((= (hand-total my-hand) 16) (if (= opponent-upcard 10)
                                         false
                                         true))
        ((> opponent-upcard 6) true)
        (else false)))

; Problem 6
(define (both strat-one strat-two)
  (lambda (my-hand opponent-upcard)
    (let ((strat-one-result (strat-one my-hand opponent-upcard))
          (strat-two-result (strat-two my-hand opponent-upcard)))
      (if (and strat-one-result
               strat-two-result true)
          true
          false))))

; Tut exercise 1

; Card
(define (make-card value suit) (cons value suit))
(define (get-card-value card) (car card))
(define (get-card-suit card) (cdr card))

;Card-link
(define (make-card-link next-card card) (cons next-card card))
(define (select-next-card card-link) (car card-link))
(define (select-card card-link) (cdr card-link))

;Card-set
(define (make-card-set head-card-link total-value) (cons head-card-link total-value))
(define (select-head-card-link card-set) (car card-set))
(define (select-total-value card-set) (cdr card-set))

;Hand
(define (make-new-hand-new upcard) (make-hand-new (make-card-set (make-card-link "empty" upcard)
                                                                 (get-card-value upcard))
                                                  upcard))
(define (make-hand-new set upcard) (cons set upcard))

(define (hand-upcard-new hand) (cdr hand)) ; remember will need to pull out the value at some point, could be here or in the algo
(define (select-hand-set hand) (car hand))
(define (hand-total-new hand) (select-total-value (select-hand-set hand)))
(define (hand-add-card-new hand new-card) (make-hand-new (insert-card-in-set new-card (select-hand-set))
                                                         (hand-upcard-new  hand)))


; Tut ex 2 a)
; How to implement a procedure that generates a fresh deck of cards that look like (1 1 1 1 2 2 2 2 3 3 3 ... 10 10 10 10)
; Procedure must insert certain amt of cards of certain values into the deck

(define (make-new-deck max-value-card max-cards-per-value)
    (define (make-empty-deck) (cons -1 -1))
    (define (deck-add-card deck card) (cons deck card))
    (define (make-deck current-card amt-of-card deck)
      (cond ((and (= current-card max-value-card) ; Finished adding cards
                  (= amt-of-card max-cards-per-value)) deck ) ; Return finished deck
            ((= amt-of-card max-cards-per-value) (make-deck (inc current-card) 0 deck)) ; Finished adding cards of current value
            (else (make-deck current-card (inc amt-of-card) (deck-add-card deck current-card))) ; Need to add cards of current value
       ))
  (make-deck 1 0 (make-empty-deck)) ; Braces around "make-empty-deck" because otherwise it will just pass the procedure instead of the pair made by the procedure
)

(define (print-deck deck)
  (cond ((= (cdr deck) -1) (display " Done"))
        (else (newline)
              (display (cdr deck))
              (print-deck (car deck)))))

(define new-deck (make-new-deck 10 4))
(print-deck new-deck)

(define new-card (make-card 1 "queen"))

(define (insert-card-in-set card set) (make-card-set (make-card-link (select-head-card-link set) card)
                                                     (+ (get-card-value card)
                                                        (select-total-value set))))

; Tut ex 2 b)
; Find length of the deck
; Split deck in 2
; Recombine into new deck, alternately picking elements from each half and forming a new deck

; Tut ex 2 c)
; Find length of the deck
; Split deck in 2
; Recombine into new deck, but instead grab a random amount of elements (1-5) at a time
   ; Obviously needs some sort of check for whether or not you've reached end before grabbing them

; Tut ex 2 d)
; You could split up the fetching of the top card and the removal of it from the deck
; Or you could return top card and new deck as a pair and then handle that from the calling function



(test-strategy (watch-player (stop-at 16)) (watch-player (stop-at 15)) 2)

(test-strategy louis (stop-at 15) 10)
(test-strategy louis (stop-at 16) 10)
(test-strategy (both louis (stop-at 18)) (stop-at 17) 10)

(define (user-says-y?) (eq? (read) 'y))

;(twenty-one hit? hit?)
