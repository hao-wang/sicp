;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname chap-1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define (square x) (* x x))
(define (sum-of-squares x y) (+ (square x) (square y)))
(sum-of-squares 3 4)
; f(a)
(define (f a) (sum-of-squares (+ a 1) (+ a 2)))
(f 2)
(define (myabs x)
  (cond ((< x 0) (- x))
        (else x)))
(myabs -3)
(myabs 4)
; any always-true predicates can replace "else"
(define rawpi 3.1415926)
(define (myabs2 x)
  (cond ((< x 0) (- x))
        (#t x))) ; the same as #true
(myabs2 -10)
(myabs2 12)
(define (myabs3 x)
  (if (< x 0)
      (- x)
      x))
(myabs3 -3)
; logical composition operations
(define (myabs4 x)
  (if (or (< x 0) (= x 0))
      (- x)
      x))
(myabs4 -5)
(myabs4 0)
; Ex 1.2
(/ (+ 5
      4
      (- 2
         (- 3
            (+ 6 4/5))
         )
      )
   (* 3 (- 6 2) (- 2 7)))
; Ex. 1.3
(define (sum-square x y)
  (+ (* x x) (* y y)))

(define (le x y)
  (or (< x y) (= x y)))
;(sum-square 3  4)
(define (two-larger x y z)
  (cond ((and (le x y) (le x z)) (sum-square y z))
        ((and (le y x) (le y z)) (sum-square z x))
        ((and (le z x) (le z y)) (sum-square x y))
  ))

(two-larger 3 4 4)
(two-larger 3 3 4)
; Ex. 1.4; it won't work -- Racket doesn't allow for operator reload?
;(define (a_plus_abs_b a b)
;  ((if (> b 0) + -) a b)
;  )
;(define (a-plus-abs-b a b)
;  ((if (> b 0) + -) a b))
;(a_plus_abs_b 3 4)
;(a_plus_abs_b 3 -4)

; Ex. 1.5: normal-order or applicative-order
; normal-order: graph-mode, to reduce until primitive operators
; applicative-order: eager-mode, or try to evaluate whenever possible
(define (p x) x)
(define (test x y) (if (= x 0) 0 y))
;(test 0 (p))
; 0 if applicative, because it won't try to evaluate y when x=0
; undefined error, if normal, as p is not defined but evaluated.

; Newton's method for square root
(define (newton_sqrt x guess)
  (if (goodenough? guess x)
      guess
      (newton_sqrt x (improve_guess guess x)))
  )

(define (goodenough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (improve_guess guess x)
  (/ (+ guess (/ x guess)) 2.0)
  )

(newton_sqrt 2.0 1.0)
;(goodenough? 2 2.1)
;(improve_guess 1.416 2.0)