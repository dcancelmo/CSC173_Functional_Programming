#lang racket

(define (myAppend list1 list2)
  ;(let (newList [list first(list1) rest(list1) first(list2) rest(list2)]))
  ;newList)
  (list (car list1) (cdr list1) (car list2) (cdr list2)))