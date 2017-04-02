#lang racket

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