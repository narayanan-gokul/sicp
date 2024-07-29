

(define (twenty-one strategy)
  (define (play-dealer customer-hand dealer-hand-so-far rest-of-deck)
    (cond ((> (best-total dealer-hand-so-far) 21) 1)
	  ((< (best-total dealer-hand-so-far) 17)
	   (play-dealer customer-hand
			(se dealer-hand-so-far (first rest-of-deck))
			(bf rest-of-deck)))
	  ((< (best-total customer-hand) (best-total dealer-hand-so-far)) -1)
	  ((= (best-total customer-hand) (best-total dealer-hand-so-far)) 0)
	  (else 1)))

  (define (play-customer customer-hand-so-far dealer-up-card rest-of-deck)
    (cond ((> (best-total customer-hand-so-far) 21) -1)
	  ((strategy customer-hand-so-far dealer-up-card)
	   (play-customer (se customer-hand-so-far (first rest-of-deck))
			  dealer-up-card
			  (bf rest-of-deck)))
	  (else
	   (play-dealer customer-hand-so-far
			(se dealer-up-card (first rest-of-deck))
			(bf rest-of-deck)))))

  (let ((deck (make-deck)))
    (play-customer (se (first deck) (first (bf deck)))
		   (first (bf (bf deck)))
		   (bf (bf (bf deck))))) )

(define (make-ordered-deck)
  (define (make-suit s)
    (every (lambda (rank) (word rank s)) '(A 2 3 4 5 6 7 8 9 10 J Q K)) )
  (se (make-suit 'H) (make-suit 'S) (make-suit 'D) (make-suit 'C)) )

(define (make-deck)
  (define (shuffle deck size)
    (define (move-card in out which)
      (if (= which 0)
	  (se (first in) (shuffle (se (bf in) out) (- size 1)))
	  (move-card (bf in) (se (first in) out) (- which 1)) ))
    (if (= size 0)
	deck
    	(move-card deck '() (random size)) ))
  (shuffle (make-ordered-deck) 52) )

(define (test-strategy name strategy test-cases)
  (if (empty? test-cases)
    ((lambda () 
       (display "Testing strategy: ") 
       (display name) 
       (display " complete") 
       (newline)))
    ((lambda () (let
      ((hand (first (first test-cases)))
       (dealer (first (bf (first test-cases))))
       (output (last (first test-cases))))
      (let ((result (strategy hand dealer)))
	(if (equal? result output)
	  ((lambda () 
	     (display "Test case passed!")
	     (newline)))
	  ((lambda ()
	     (display "Test case failed:")
	     (newline)
	     (display "\tHand           : ")
	     (display hand)
	     (newline)
	     (display "\tDealer         : ")
	     (display dealer)
	     (newline)
	     (display "\tExpected output: ")
	     (display output)
	     (newline)
	     (display "\tActual output  : ")
	     (display result)
	     (newline))))))
       (test-strategy name strategy (bf test-cases))))))

(define (best-total cards)
  (define (card-value card)
    (if (member? (first card) '(J Q K))
      10
      (bl card)))

  (define (sum-sans-aces cards)
    (cond ((empty? cards) 0)
	  ((equal? (first (first cards)) 'A) (sum-sans-aces (bf cards)))
	  (else (+ (card-value (first cards)) (sum-sans-aces (bf cards))))))

  (define (sum-aces sans-aces cards)
    (cond ((empty? cards) sans-aces)
	  ((equal? (first (first cards)) 'A) 
	   (let ((is-eleven (sum-aces (+ sans-aces 11) (bf cards)))) 
	     (if (> is-eleven 21)
	       (sum-aces (+ sans-aces 1) (bf cards))
	       is-eleven)))
	  (else (sum-aces sans-aces (bf cards)))))

  (sum-aces (sum-sans-aces cards) cards))

(define (stop-at n)
  (lambda (cards dealer) 
    (if (< (best-total cards) n)
      #t
      #f)))

(define stop-at-17 (stop-at 17))

(define (play-n strategy n)
  (if (= 0 n)
    0
    (+ (twenty-one strategy) (play-n strategy (- n 1)))))

(define (dealer-sensitive hand dealer)
  (let ((total (best-total hand)))
    (cond ((and 
	     (< total 17)
	     (member? (bl dealer) 
		      '(A 7 8 9 10 K Q J)))
	   #t)
	  ((and 
	     (< total 12)
	     (member? (bl dealer) '(2 3 4 5 6)))
	   #t)
	  (else #f))))

(test-strategy "dealer-sensitive" dealer-sensitive '(((AS AD 6C) 9H #f)
				  ((5H 6S) 9H #t)
				  ((AC 9H) 2H #f)
				  ((5H 6S) 2H #t)
				  ((7H 6S) 9H #t)
				  ((10H 6S) 2H #f)))

(define (suit-strategy suit includes excludes)
  (lambda 
    (hand dealer)
    (define (includes? cards)
      (cond
	((empty? cards) #f)
	((equal? (last (first cards)) suit) #t)
	(else (includes? (bf cards)))))
    (if (includes? hand)
      (includes hand dealer)
      (excludes hand dealer))))

(define valentine (suit-strategy 'H (stop-at 19) (stop-at 17)))

(test-strategy "valentine" valentine '(((3H 5S AS 9C) 9S #t)
			   ((2H 5S AS 9C) 9S #t)
			   ((2C 5S AS 7C) 9S #t)
			   ((3C 5S AS 9C) 9S #f)))

(define (majority strategy1 strategy2 strategy3)
  (lambda (hand dealer) 
    (let ((res1 (strategy1 hand dealer))
	  (res2 (strategy2 hand dealer))
	  (res3 (strategy3 hand dealer)))
      (if res1
	(or res2 res3)
	(and res2 res3)))))

(define (reckless strategy)
  (lambda (hand dealer) (strategy (bl hand) dealer)))
