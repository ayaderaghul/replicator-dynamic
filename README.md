# replicator dynamic

this is to draw the dynamic of a population playing bargaining game over time.
there are 4 types of machines: High, Medium, Low, Accommodator.

# how to run

open racket

```
(load "bar.rkt")

(create-population A 900 50 1 49) ; high medium low accommodator

(evolve-population 150 50 0) ; run 150 cycles, speed 50, pause 0s
```
