;Daniel Cancelmo

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

(define (deficientH num)
  (cond
    [(< (perfectH num 0 1) num) #t]
    [else #f]
    )
  )
(define (deficient? num)
  (deficientH num)
  )

;Testing list functions
(display "Enter a list for use on list functions: ")
(define list1 (read))
(display "\n")
(display "Enter a second list for use on list functions: ")
(define list2 (read))
(display "\n")
(display "Testing myAppend with these two lists:\n")
(myappend list1 list2)
(display "Enter an item to return its index if in list1: ")
(define itemIndex (read))
(display "\n")
(display "Testing indexof with list1 and your item:\n")
(indexof itemIndex list1)
(display "Enter an item to add to the end of list1: ")
(define newItem (read))
(display "\n")
(display "Testing addtoend with list1 and your item:\n")
(addtoend list1 newItem)

;Testing set functions
(display "Enter a set (as a list) for use on set functions: ")
(define set1 (read))
(display "\n")
(display "Testing cardinality with this set:\n")
(cardinality set1)
(display "Enter an item to test if it is in the set: ")
(define itemTest (read))
(display "\n")
(display "Testing member with set1 and your item:\n")
(member itemTest set1)
(display "Enter a second set (as a list) to union with set1: ")
(define set2 (read))
(display "\n")
(display "Testing union with set1 and set2\n")
(union set1 set2)

;Testing math functions
(display "Enter a number to find its absolute value: ")
(define absValNum (read))
(display "\n")
(display "Testing absolute value with your input:\n")
(abs absValNum)
(display "Enter a number to find its factorial: ")
(define factNum (read))
(display "\n")
(display "Testing factorial with your input:\n")
(factorial factNum)
(display "Enter a number (for GCD): ")
(define gcdNum1 (read))
(display "\n")
(display "Enter a second number (for GCD): ")
(define gcdNum2 (read))
(display "\n")
(display "Testing GCD with your input\n")
(gcd gcdNum1 gcdNum2)

;Extra credit function
(display "Enter a number (for LCM): ")
(define lcmNum1 (read))
(display "\n")
(display "Enter a second number (for LCM): ")
(define lcmNum2 (read))
(display "\n")
(display "Testing LCM with your input\n")
(lcm lcmNum1 lcmNum2)

;Required functions
(display "Enter a number to find if it is 'perfect': ")
(define perfectNum (read))
(display "\n")
(display "Testing perfect number with your input:\n")
(perfect? perfectNum)
(display "Enter a number to find if it is 'abundant': ")
(define abundantNum (read))
(display "\n")
(display "Testing abundant number with your input:\n")
(abundant? abundantNum)
(display "Enter a number to find if it is 'deficient': ")
(define deficientNum (read))
(display "\n")
(display "Testing deficient number with your input:\n")
(deficient? deficientNum)