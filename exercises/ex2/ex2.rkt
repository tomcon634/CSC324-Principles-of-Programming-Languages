#lang racket #| CSC324 Fall 2018: Exercise 2 |#
#|
★ Before starting, please review the exercise guidelines at
https://www.cs.toronto.edu/~david/csc324/homework.html ★
|#
;-------------------------------------------------------------------------------
; This expression exports functions so they can be imported into other files.
; Don't change it!
(provide tail-calls)


;-------------------------------------------------------------------------------
; ★ Task 2: Detecting tail calls ★
;-------------------------------------------------------------------------------

#|
(tail-calls expr)
  expr: A racket datum with the structure defined on the exercise handout.

  Returns a *list of function call expressions* that are in tail call position
  with respect to the input expression.

  Feel free to change the define into define/match to use pattern-matching instead!
|#
(define (tail-calls expr)
  (if (or (integer? expr) (boolean? expr) (string? expr)) '()
      (if (or (equal? (first expr) 'and) (equal? (first expr) 'or)) (tail-calls (last expr))
           (if (equal? (first expr) 'if) (append (tail-calls (third expr)) (tail-calls (last expr)))
               (cons expr '())))))


(module+ test
  (require rackunit)
  ; We've provided test cases for the first three syntactic forms described
  ; in the handout. Please add additional test cases, including ones for the
  ; other forms!
  (test-equal? "Atomic value" (tail-calls 3) empty)
  (test-equal? "Simple call" (tail-calls '(+ 1 2)) (list '(+ 1 2)))
  (test-equal? "Nested call"
               (tail-calls '(+ (* 3 4) 2))
               ; NOTE: the outermost expression is in tail-call position,
               ; and it should just be output directly. Don't try to evaluate
               ; the inner '(* 3 4) -- this is harder to do!
               (list '(+ (* 3 4) 2)))
  (test-equal? "Simple if" (tail-calls '(if (> x 0) (- x 2) (+ x 3)))
               (list '(- x 2)
                     '(+ x 3)))
  (test-equal? "Simple and" (tail-calls '(or (> x 0) (- x 2) (+ x 3)))
               (list '(+ x 3))))
