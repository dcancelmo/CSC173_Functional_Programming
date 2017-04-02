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

(define (cardinalityH setP counter)
  (cond
    [(empty? setP) counter]
    [else (cardinalityH (cdr setP) (+ counter 1))]
    )
  )
(define (cardinality setP)
  (cardinalityH setP 0)
  )

(define (memberH item setP counter)
    (cond
      [(empty? setP) #f]
      [(eq? item (car setP)) #t]
      [else (memberH item (cdr setP) (+ counter 1))]
    )
  )
(define (member item setP)
  (memberH item setP 0)
  )

(define (unionH set1 set2)
    (cond
      [(empty? set1) set2]
      [(empty? set2) set1]
      [(eq? set1 set2) set1]
      [(eq? (car set1) (car set2)) (cons (car set1) (unionH (cdr set1) (cdr set2)))]
      [else (cons (car set1) (cons (car set2)(unionH (cdr set1) (cdr set2))))]
    )
  )
(define (union set1 set2)
  (unionH set1 set2)
  )