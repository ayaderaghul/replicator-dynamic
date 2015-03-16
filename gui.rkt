(require racket/gui/base) ; to have TV
(require plot/no-gui) ; to have plot/dc
(require racket/draw) ; to draw
(require racket/math) ; to draw arc
(require math) ; to have mean
(plot-new-window? #t)


(define dynamic-frame (new frame% [label "replicator dynamic"]
                           [width 400]
                           [height 400]))

(define dynamic-canvas (new canvas% [parent dynamic-frame]))
(define dc-dynamic (send dynamic-canvas get-dc))

(define (plot-traject series)
 (plot/dc 
		(lines series #:x-min 0 #:x-max 1000 #:y-min 0 #:y-max 1000) 
dc-dynamic 0 0 400 400)
)

(define (plot-trajects)
 (plot/dc 
	(list
	(lines X1 #:x-min 0 #:x-max 1000 #:y-min 0 #:y-max 1000) 
	 (lines X2 #:x-min 0 #:x-max 1000 #:y-min 0 #:y-max 1000)
(lines X3 #:x-min 0 #:x-max 1000 #:y-min 0 #:y-max 1000)
(lines X4 #:x-min 0 #:x-max 1000 #:y-min 0 #:y-max 1000)
(lines X5 #:x-min 0 #:x-max 1000 #:y-min 0 #:y-max 1000)
(lines X6 #:x-min 0 #:x-max 1000 #:y-min 0 #:y-max 1000)
(lines X7 #:x-min 0 #:x-max 1000 #:y-min 0 #:y-max 1000)
(lines X8 #:x-min 0 #:x-max 1000 #:y-min 0 #:y-max 1000)
)
dc-dynamic 0 0 400 400 #:x-label "high" #:y-label "medium")
)


 


(send dynamic-frame show #t)
