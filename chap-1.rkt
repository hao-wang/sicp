(define func_square "square---") func_square
(define (square x) (* x x))
(define (sum-of-squares x y) (+ (square x) (square y)))
(sum-of-squares 3 4)
; f(a)
(define (f a) (sum-of-squares (+ a 1) (+ a 2)))
(f 2)
(define func_abs "abs---") func_abs
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
(define func_twolarger "two larger---") func_twolarger
(define (two-larger x y z)
  (cond ((and (le x y) (le x z)) (sum-square y z))
        ((and (le y x) (le y z)) (sum-square z x))
        ((and (le z x) (le z y)) (sum-square x y))
  ))

(two-larger 3 4 4)
(two-larger 3 3 4)
; Ex. 1.4; it won't work -- Racket doesn't allow for operator reload?
(define (a_plus_abs_b a b)
  ((if (> b 0) + -) a b)
  )
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
(a_plus_abs_b 3 4)
(a_plus_abs_b 3 -4)

; Ex. 1.5: normal-order or applicative-order
; normal-order: graph-mode, to reduce until primitive operators
; applicative-order: eager-mode, do whatever can be done (walk-and-see)
; racket-scheme is in the applicative order
; http://community.schemewiki.org/?sicp-ex-1.5
(define ex_order "Ex.1.5---") ex_order
(define (p) (p))
(define (test x y) (if (= x 0) 0 y))
;(test 0 (p))
; non-stop if applicative, because it will try to evaluate (p) but will never succeed
; 0 if normal, (if (= 0 0) 0 (p)) -> 0

; Newton's method for square root
(define func_sqrt "sqrt---") func_sqrt
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
(define ex_1.6 "E.x. 1.6---") ex_1.6
(define (new_if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))
(new_if (= 2 3) 0 5)
(define (newton_sqrt_new x guess)
  (new_if (goodenough? guess x)
      guess
      (newton_sqrt_new x (improve_guess guess x)))
  )
; it won't work as new_if causes sqrt never stop calling itself
;(newton_sqrt_new 2.0 1.0)

(define ex_1.7 "ex. 1.7--") ex_1.7
(define (new_goodenough? guess new_guess)
  (< (abs (/ (- new_guess guess) guess)) 0.01))

(define (new_sqrt x guess new_guess)
  (if (new_goodenough? guess new_guess)
      new_guess
      (new_sqrt x new_guess (improve_guess new_guess x))))

(new_sqrt 2.0 0.0 1.0)
(new_sqrt 0.0001 0.0 0.1)  ; correct
(newton_sqrt 0.0001 1)  ; wrong
(new_sqrt 1e50 0.0 1.0)
;(newton_sqrt 1e50 1.0)  ; hard to converge
;(new_goodenough? 2 2.01)

(define ex_1.8 "ex. 1.8--") ex_1.8
(define (improve_guess_cuberoot guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))
(define (cuberoot x guess new_guess)
  (if (new_goodenough? guess new_guess)
      new_guess
      (cuberoot x new_guess (improve_guess_cuberoot new_guess x))))

(cuberoot 8  1 1.5)
(cuberoot 125  1 1.5)
(cuberoot 125000  1 1.5)

; scopes
(define (scope_sqrt x)
  (define (sqrt_iter guess)
    (if (goodenough? guess)
        guess
        (sqrt_iter (improve_guess guess))))
  (define (goodenough? guess)
    (< (abs (- x (square guess))) 0.001))
  (define (improve_guess guess)
    (/ (+ guess (/ x guess)) 2.0))
  (sqrt_iter 1.0)
  )
(scope_sqrt 4.0)

(define ex_1.9 "ex. 1.9--") ex_1.9
;(define (+ a b) (if (= a 0) b (inc (+ (dec a) b)))) -->(1)
;(define (+ a b) (if (= a 0) b (+ (dec a) (inc b)))) -->(2)
;(+ 4 5)
;(1)
;(inc (+ 3 5))
;(inc (inc (+ 2 5)))
;(inc (inc (inc (+ 1 5))))
;(inc (inc (inc (inc (+ 0 5)))))
;(inc (inc (inc (inc 5))))
;(inc (inc (inc 6)))
;(inc (inc 7))
;(inc 8)
;9
;(2)
;(+ 3 6)
;(+ 2 7)
;(+ 1 8)
;(+ 0 9)
;9
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))
(A 1 10)
(A 2 4)
(A 3 3)

(define ex_1.10 "ex. 1.10--") ex_1.10

