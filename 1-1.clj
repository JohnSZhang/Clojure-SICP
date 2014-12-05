; Exercise 1.1

10
(+ 5 3 4)
(- 9 1)
(/ 6 2)
(+ (* 2 4) (- 4 6))
(define a 3) ; clojure version uses def
(define b (+ a 1)) ; clojure version uses def
(+ a b (* a b))
(= a b)
(if (and (> b a) (< b (* a b)))
b
a)
(cond ((= a 4) 6)
  ((= b 4) (+ 6 7 a))
  (else 25)) ; clojure version uses :else
(+ 2 (if (> b a) b a))
(* (cond ((> a b) a)
  ((< a b) b)
  (else -1)) ; clojure version uses def
(+ a 1))

; becomes
10
12
8
3
6
a = 3
b = 4
19
false
4
16
6
4
4

; exercise 1.2

(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7)))

; exercise 1.3
( def square (fn[a] (* a a)))
( def sum_square (fn[a, b] (+ (square a) (square b))))
( def sum_of_largest_two (fn[a, b, c] (
  cond (and (< c b) (< c a)) (sum_square a b)
    (and (< b c) (< b a)) (sum_square c a)
    :else (sum_square b c)
)))

; exercise 1.4
(define (a-plus-abs-b a b)
((if (> b 0) + -) a b))

; clojure version
(def a-plus-abs-b (fn[a, b] ((if (> b 0) + -) a b)))
; woah lisp! bacause combinations operators can also be compound expressions, we use a if statement to select whether the operator should be + or -

; exercise 1.5
(define (p) (p))

(define (test x y)
(if (= x 0)
0
y))

(test 0 (p))

; clojure version
(def (p) (p)); cannot find clojure equal

(def special_test (fn[x, y] (if (= x 0) 0 y)))

(test 0 (p))

; With applicative-order evaluation, the (p) argument would be expanded out indifinitely and crash the program as (p) will continously be replaced with another version of (p), ad infiniteum ie. (test 0 (p)) becomes (test 0 (p)) and so one

; With normal-order evaluation, the expression terminates on the very first round of evaluation as the (= x 0) part is evaluationed and the rest are skipped. This is because normal order does not require the the evluation of the operands until they are needed, and in this case the second part of the if conditional is never evluated as it is never needed.

; Newtons Method
(define (square x) (* x x))

(define (abs x) (if (< x 0) (- x) x))

(define (average x y)
(/ (+ x y) 2))

(define (improve guess x)
(average guess (/ x guess)))

(define (good-enough? guess x)
(< (abs (- (square guess) x)) 0.001))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x)
        x)))

(define (sqrt x) (sqrt-iter 1.0 x))

; clojure version

(def square
  (fn[a]
    (* a a)))

(def abs
  (fn[x]
    (if (< x 0)
    (* -1 x)
    x)))

(def average
  (fn[x, y]
    (/ (+ x y) 2)))

(def improve
  (fn[guess,x]
    (average guess
      (/ x guess))))

(def good_enough?
  (fn[guess, x]
    (< (abs
      (- (square guess) x))
      0.001)))

(def sqrt_iter
  (fn[guess,x]
    (if (good_enough? guess x)
      guess
      (sqrt_iter (improve guess x)
        x ))))

(def sqrt
  (fn[x]
    (sqrt_iter 1.0 x)))

; exercise 1.6
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
  (else else-clause)))

; new-if for sqrt_root iter
(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x)
    x)))
; another applicative order problem, because of the applicative order nature of cond operator, the sqrt_iter method will be expanded until we run out of memory, while the built in if special form will not evaluate the second part of the if statement once the predicate value is false. (maximum recursion depth exceeded!)

; exercise 1.7
; When answers are too small, the good enough's 0.001 parameter will not be percise enough as the solution itself can be as small as 0.00001, on the other hand, call stack will be exceeded if the solution is too large, as 10000000000 will be two large for the 0.001 to make a dent on. (might lead to stackoverflow or float math issues). Classic example of edge cases.
; example of too small
(sqrt 0.00000009)
; example of too large
(sqrt 90000000000000000000000000000000000000000) ; needs to be sufficiently large
(def good_enough?
  (fn[guess, x]
    (< (abs
      (- (square guess) x))
      (/ x 10000))))
; works for sufficiently small and large

; exercise 1.8
(def cube_rt
  (fn[x]
    (cube_iter 1.0 x)))

(def cube_iter
  (fn[guess, x]
    (if (cube_good_enough? guess x)
      guess
      (cube_iter (cube_improve guess x)
      x ))))

(def cube
  (fn[x]
    (* x x x)))

(def cube_good_enough?
  (fn[guess, x]
    (< (abs
      (- (cube guess) x))
      0.001)))

(def cube_improve
  (fn[guess, x]
    (/ (+ (/ x (* guess guess)) (* 2 guess))
      3))
)

;1.8 example
(define (sqrt x)
  (define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess x)
    (average guess (/ x guess)))
  (define (sqrt-iter guess x)
      (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x)))
  (sqrt-iter 1.0 x))

(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
      guess
    (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

;Clojure
(def sqrt
  (fn[x]
    (def square
      (fn[x]
        (* x x)))
    (def abs
      (fn[x]
        (if (< x 0)
          (- x)
          x)))
    (def average
      (fn[x,y]
        (/ (+ x y) 2)))
    (def good_enough?
      (fn[guess]
        (< (abs (- (square guess) x)) 0.001)))
    (def improve
      (fn[guess]
        (average guess (/ x guess))))
    (def sqrt_iter
      (fn[guess]
        (if (good_enough? guess)
          guess
          (sqrt_iter (improve guess)))))
    (sqrt_iter 1.0)))
