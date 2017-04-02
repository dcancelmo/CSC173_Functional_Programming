#lang racket

;In case of possible need and for uniformity all functions have a helper function

;List functions

(define (myappendH firstElem restElem)
  (if (empty? firstElem)
      restElem
      (cons (first firstElem) (myappendH (rest firstElem) restElem))
      )
  )
(define (myappend list1 list2)
  (myappendH list1 list2)
  )

(define (indexofH item listP counter)
  (cond
    [(empty? listP) #f]
    [(eq? item (car listP)) counter]
    [else (indexofH item (cdr listP) (+ counter 1))]
    )
  )
(define (indexof item listP)
  (indexofH item listP 0)
  )

(define (addtoendH listP item)
  (cond
    [(empty? listP) item]
    [else (cons (car listP)(addtoendH (cdr listP) item))]
    )
  )
(define (addtoend listP item)
  (addtoendH listP (cons item '()))
  )



;Set functions

(define (cardinalityH listP counter)
  (cond
    [(empty? listP) counter]
    [else (cardinalityH (cdr listP) (+ counter 1))]
    )
  )

(define (cardinality listP)
  (cardinalityH listP 0)
  )

(define (memberH item listP counter)
    (cond
      [(empty? listP) #f]
      [(eq? item (car listP)) #t]
      [else (memberH item (cdr listP) (+ counter 1))]
    )
  )

(define (member item listP)
  (memberH item listP 0)
  )