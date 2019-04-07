#lang racket #| Choices and backtracking, Version 3 |#
; This version of the "choices" library uses a *stack* to store choice points
; (thunks containing a continuation called on a choice expression).

#| Usage note

  In the DrRacket menu, go to Language -> Choose Language...
  Click on Show Details, and make sure "Enforce constant definitions"
  is *NOT* checked.
|#
; We import `prompt` from this library, for our implementation of `get-choices`.
(require racket/control)

(provide -<
         next
         choices
         get-choices

         ; We didn't talk about these in lecture, but are sometimes useful exports.
         done?
         reset-choices!)


; A constant representing the state of having *no more choices*.
(define DONE 'done)

; The stack of choice points, represented as a list.
(define choices empty)

#|
(-< expr ...)
 expr ... : Any expression.

 Evaluates and returns the first expression, but creates a "choice point"
 that can be returned to later with a different expression used.
|#
(define-syntax -<
  (syntax-rules ()
    ; Given no options, return done.
    [(-<) DONE]

    ; Given one option, return the value.
    [(-< <expr1>) <expr1>]

    ; If there are two or more values, return the first one and
    ; store the others in a thunk,
    ; but *also stores the continuation of the -< expression*.
    [(-< <expr1> <expr2> ...)
     (let/cc cont
       ; 1. Store the current continuation and the remaining choices in a thunk.
       (add-choice! (thunk (cont (-< <expr2> ...))))

       ; 2. Evaluate and return the first expression.
       <expr1>)]))


#|
(next)

  Returns the next choice, or `done` if there are no more choices.

  Sample usage (with -<):

  > (+ 10 (-< 1 2 3))
  11
  > (next)
  12
  > (next)
  13
  > (next)
  'done
  > (next)
  'done
|#
(define (next)
  (if (null? choices)
      ; abort is imported from racket/control. It removes the current
      ; continuation (just like calling another continuation), except
      ; all it does is return its argument; in this case, DONE.
      (abort DONE)
      ; Note that (get-choice!) returns a thunk, which we need to call.
      ((get-choice!))))


; Sample usage, in test form.
(module+ test
  (require rackunit)
  
  ; -< as a top-level expression.
  (test-case
   "-< as a top-level expression"
   (check-equal? (prompt (-< 1 2 3)) 1)
   (check-equal? (prompt (next)) 2)
   (check-equal? (prompt (next)) 3)
   (check-equal? (prompt (next)) DONE))

  (test-case
   "Illustrating the use of continuations! The continuation stored is (+ 10 _)."
   (check-equal? (prompt (+ 10 (-< 1 2 3))) 11)
   (check-equal? (prompt (next)) 12)
   (check-equal? (prompt (next)) 13)
   (check-equal? (prompt (next)) DONE))

  (test-case
   "Illustrating the use of multiple choice points!"
   (check-equal? (prompt (+ (-< 1 2) (-< 10 20))) 11)
   (check-equal? (prompt (next)) 21)
   (check-equal? (prompt (next)) 12)
   (check-equal? (prompt (next)) 22)
   (check-equal? (prompt (next)) DONE)))


;-------------------------------------------------------------------------------
; Stack helpers
;-------------------------------------------------------------------------------
; Wrapper functions around the push! and pop! macros.
; This is necessary for exporting purposes, as Racket doesn't allow
; mutation of module identifiers from outside of the module.
; See https://docs.racket-lang.org/guide/module-set.html.
(define (add-choice! c) (push! choices c))
(define (get-choice!) (pop! choices))

(define-syntax push!
  (syntax-rules ()
    [(push! <id> obj)
     (set! <id>
           (cons obj <id>))]))

(define-syntax pop!
  (syntax-rules ()
    [(pop! <id>)
     (let ([obj (first <id>)])
       (set! <id> (rest <id>))
       obj)]))


;-------------------------------------------------------------------------------
; Other utilities
;-------------------------------------------------------------------------------
#|
(get-choices n)
  n: a non-negative integer

  Returns a list of the next n choices (stored in choice).
  If there are fewer than n choices remaining, returns all of them.
|#
(define (get-choices n)
  (if (equal? n 0)
      empty
      (let* ([v (prompt (next))])
        (if (equal? v DONE)
            empty
            (cons v (get-choices (- n 1)))))))


(define (reset-choices!) (set! choices empty))
(define (done? v) (equal? v DONE))
