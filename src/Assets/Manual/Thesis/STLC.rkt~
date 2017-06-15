#lang racket
(require redex)

(define-language L
   ( e (e e)
       (λ (x t) e)
       x
       (amb e ...)
       number
       (+ e ...)
       (if0 e e e)
       (fix e))
    (t (→ t t) num)
    (x variable-not-otherwise-mentioned))

