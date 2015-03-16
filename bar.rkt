(require racket/gui/base) ; to have TV
(require plot/no-gui) ; to have plot/dc
(require racket/draw) ; to draw
(require racket/math) ; to draw arc
(require math) ; to have mean
(plot-new-window? #t)

;; support function

(define (shuffle a-vector)
  (do
      ([n (vector-length a-vector) (- n 1)])
      ((zero? n) a-vector)
    (let* ([r (random n)]
	   [t (vector-ref a-vector r)])
      (vector-set! a-vector r (vector-ref a-vector (- n 1)))
      (vector-set! a-vector (- n 1) t))))

(define (vector-first a-vector)
  (vector-ref a-vector 0))
(define (vector-second a-vector)
  (vector-ref a-vector 1))
(define (vector-third a-vector)
  (vector-ref a-vector 2))
(define (vector-last a-vector)
  (vector-ref a-vector (- (vector-length a-vector) 1)))
(define (vector-rest a-vector)
  (vector-drop a-vector 1))

;; initial automata
(define (h) (vector 1 (vector 1 1 1 8)))
(define (m) (vector 1 (vector 1 1 1 5)))
(define (l) (vector 1 (vector 1 1 1 2)))
(define (a) (vector 1 (vector 3 1 2 5) (vector 3 1 2 8) (vector 3 1 2 2)))
(define 4-type-list (list (h) (m) (l) (a)))

;; support function for automata
(define (morph an-auto)
  (let ([listed (vector->list an-auto)])
    (append (take listed 1)
	    (map vector->list
		 (rest listed)))))
(define (clone an-auto)
  (let* ([morphed (morph an-auto)]
	 [vectored (list->vector morphed)])
    (vector-append (vector-take vectored 1)
		   (vector-map list->vector
			       (vector-rest vectored)))))

;; create a world of p slots
(define (create-world p)
  (for/vector ([i p]) 0))
(define N 1000)
(define A (create-world N)) ; A is population
(define B (create-world N)) ; B is payoff book
(define B+ (create-world N)) ; B+ is the positive payoff book
(define T (create-world 4)) ; C is the type counting
(define F (create-world 4)) ; D is the type fitness
(define S (vector 0)) ; S is the payoff total

;; create population to fill in the world (all positive numbers)
(define (create-population world high medium low accom) ; (h m l a) (900 50 1 49)
  (let ([auto-population
         (shuffle
          (list->vector
           (append
            (for/list
                ([i high])
              (clone (h)))
            (for/list
                ([j medium])
              (clone (m)))
            (for/list
                ([k low])
              (clone (l)))
            (for/list
                ([m accom])
              (clone (a))))))])
    (begin
      (set! series (list (vector high medium)))
      (for ([n (vector-length world)])
        (vector-set! world n (vector-ref auto-population n))))))

(define (match-strat a b)
  (if (<= (+ a b) 10)
      (list a b)
      '(0 0)))

(define (posn? opponent-strat)
  (cond [(equal? opponent-strat 8) vector-first]
	[(equal? opponent-strat 5) vector-second]
	[else vector-third]))

(define (match-auto! auto1 auto2 r)
  (let ([pay-list
         (for/list ([n r])
           (let* ([n1 (vector-first auto1)]
                  [n2 (vector-first auto2)]
                  [current-state1 (vector-ref auto1 n1)]
                  [current-state2 (vector-ref auto2 n2)]
                  [current-strat1 (vector-last current-state1)]
                  [current-strat2 (vector-last current-state2)])
             (vector-set! auto1 0
                          ((posn? current-strat2) current-state1))
             (vector-set! auto2 0
                          ((posn? current-strat1) current-state2))
             (match-strat current-strat1 current-strat2))
           )]) pay-list))

(define (reset-state auto)
  (vector-set! auto 0 1))

(define (mean-pay posn pay-list)
  (mean (map posn pay-list)))

(define (accum a-list)
  (for/list ([n (length a-list)])
    (sum (take a-list (+ n 1)))))

;; payoff book
(define (set-payoff! population i1 i2 r pay-book)
  (let ([payoff (match-auto! (vector-ref population i1)
                             (vector-ref population i2)
                             r)])
    (vector-set! pay-book i1  (mean-pay first payoff))
    (vector-set! pay-book i2  (mean-pay second payoff))))

(define (match-population! population r pay-book)
  (begin
    (for ([n (/ (vector-length population) 2)])
      (set-payoff! population (* 2 n) (add1 (* 2 n)) r pay-book))
    (vector-map reset-state population)))

(define (add1! pay-book posi-book)
  (for ([i (vector-length pay-book)])
    (vector-set! posi-book i
                 (add1
                  (vector-ref
                   pay-book i)))))

(define (v-sum! posi-book sum-book)
  (vector-set! sum-book 0
	       (sum (vector->list posi-book))))

(define (reset-book! book)
  (for ([n (vector-length book)])
    (vector-set! book n 0)))

(define (identify-auto auto)
  (cond [(equal? auto #(1 #(1 1 1 8))) 0] ; all-high
        [(equal? auto #(1 #(1 1 1 5))) 1] ; all-medium
        [(equal? auto #(1 #(1 1 1 2))) 2] ; all-low
        [else 3])) ; accommodator

(define (high? auto)
  (equal? auto #(1 #(1 1 1 8))))
(define (medium? auto)
  (equal? auto #(1 #(1 1 1 5))))
(define (low? auto)
  (equal? auto #(1 #(1 1 1 2))))
(define (accom? auto)
  (equal? auto #(1 #(3 1 2 5) #(3 1 2 8) #(3 1 2 2))))

(define (count-types population type-book)
  (begin
    (vector-set! type-book 0 (vector-count high? population))
    (vector-set! type-book 1 (vector-count medium? population))
    (vector-set! type-book 2 (vector-count low? population))
    (vector-set! type-book 3 (vector-count accom? population))))

(define (extract-payoff type? population posi-book)
  (for/list ([i (vector-length population)])
    (and
     (type? (vector-ref population i))
     (vector-ref posi-book i))))

(define (true? x)
  (not (false? x)))

(define (extract-fit type? population posi-book sum-book)
    (/ (sum (filter true? (extract-payoff type? population posi-book)))
       (vector-first sum-book)))

(define (calculate-type-fitness population posi-book fitness-book sum-book)
  (begin
    (vector-set! fitness-book 0 (extract-fit high? population posi-book sum-book))
    (vector-set! fitness-book 1 (extract-fit medium? population posi-book sum-book))
    (vector-set! fitness-book 2 (extract-fit low? population posi-book sum-book))
    (vector-set! fitness-book 3 (extract-fit accom? population posi-book sum-book))))

(define (extract-average type? population posi-book)
  (if (zero? (vector-count type? population))
      0
      (/ (extract-sum type? population posi-book)
         (vector-count type? population))))
(define (type-average population posi-book)
  (vector (extract-average high? population posi-book)
          (extract-average medium? population posi-book)
          (extract-average low? population posi-book)
          (extract-average accom? population posi-book)))

(define (do-cal! population pay-book posi-book sum-book type-book fitness-book)
  (begin
    (add1! pay-book posi-book)
    (v-sum! posi-book sum-book)
    (count-types population type-book)
    (calculate-type-fitness population posi-book fitness-book sum-book)))

(define (abridged-report type-book fitness-book)
  (list
   (vector->list type-book)
   (accum (vector->list fitness-book))))

(define (regenerate population speed type-book fitness-book)
  (let ([accum-fitness (second (abridged-report type-book fitness-book))])
    (for ([i speed])
     (vector-set! population i
                  (let ([r (random)])
                    (cond [(< r (first accum-fitness)) (clone (h))]
                          [(and (>= r (first accum-fitness))
                                (< r (second accum-fitness))) (clone (m))]
                          [(and (>= r (second accum-fitness))
                                (< r (third accum-fitness))) (clone (l))]
                          [else (clone (a))]))))))


(define (export-data path txt)
  (call-with-output-file path
    (lambda (output-port)
      (write txt output-port))
    #:exists 'append))

;; TV
(define dynamic-frame (new frame% [label "replicator dynamic"]
                           [width 400]
                           [height 400]))
(define dynamic-canvas (new canvas% [parent dynamic-frame]))
(define dc-dynamic (send dynamic-canvas get-dc))
(define series
  (list (vector 0 0)))

(define (add-pair! type-book)
  (set! series (append series (list (vector-take type-book 2)))))

(define (plot-dynamic type-book)
  (begin
    (add-pair! type-book)
    (plot/dc (lines series
                    #:x-min 0 #:x-max 1000
                    #:y-min 0 #:y-max 1000)
             dc-dynamic
             0 0
             400 400)))

(define (shuffle! population)
  (let ([new-popu (shuffle population)])
    (for ([i (vector-length population)])
      (vector-set! population i (vector-ref new-popu i)))))

;;create population A at ratio...
(define (evolve-population cycles speed pause)
  (for/and ([n cycles])
    (match-population! A 100 B)
    (sleep pause)
    (do-cal! A B B+ S T F)
    (plot-dynamic T)
    (sleep pause)
    (regenerate A speed T F)
    (shuffle! A)
   ; (export-data "report.rkt"
   ;              (vector-take (count-types A T) 2))
    ))

 (send dynamic-frame show #t)
