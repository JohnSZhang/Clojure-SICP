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

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x)
        x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

; clojure version
(def average
  (fn[x, y])
  (/ (+ x y) 2))

(def improve
  (fn[guess,x]
    (average guess
      (/ x guess))))
      
(def good_enough?
  (fn[guess, x])
  (< (abs (- (square guess) x) 0.001)))

(def sqrt_iter
  (fn[guess,x]
    (if (good-enough? guess x)
      guess
      (sqrt_iter (improve guess x)
        x ))))
