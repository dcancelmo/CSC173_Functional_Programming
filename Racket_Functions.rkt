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


;Math functions

(define (absH num)
  (cond
    [(> num 0) num]
    [else (* num -1)]
    )
  )
(define (abs num)
  (absH num)
  )

(define (factorialH num)
  (cond
    [(eq? num 0) 1]
    [(eq? num 1) num]
    [else (* num (factorialH (- num 1)))]
    )
  )
(define (factorial num)
  (factorialH num)
  )

(define (gcdH num1 num2)
  (cond
    [(eq? num2 0) num1]
    [else (gcdH num2 (remainder num1 num2))]
    )
  )
(define (gcd num1 num2)
  (gcdH num1 num2)
  )

;Extra credit function
(define (lcmH num1 num2)
  (/ (* num1 num2) (gcd num1 num2))
  )
(define (lcm num1 num2)
  (lcmH num1 num2)
  )


;Required functions

(define (perfectH num sumP counter)
  (cond
    [(> counter (/ num 2)) sumP]
    [(eq? (remainder num counter) 0) (+ sumP (+ counter (perfectH num sumP (+ counter 1))))]
    [else (perfectH num sumP (+ counter 1))]
    )
  )
(define (perfect? num)
  (cond
    [(eq? (perfectH num 0 1) num) #t]
    [else #f]
    )
  )

(define (abundantH num)
  (cond
    [(> (perfectH num 0 1) num) #t]
    [else #f]
    )
  )
(define (abundant? num)
  (abundantH num)
  )