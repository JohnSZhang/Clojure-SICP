; factorial fn
(def factorial (
  fn[n] (
    if (= n 1)
    1
    (* n (factorial (- n 1)))
    )
  )
)

(def factorial (
  fn[n] (
    factorial-iter n
    )
  )
)

(def factorial-iter (
  fn[n, product, max] (
    if (= n max)
      product
      (factorial-iter (+ n 1)
      (* product n)
      max)
    )
  )
)

; Exercise 1.9
(define (+ a b)
(if (= a 0)
b
(inc (+ (dec a) b))))

(define (+ a b)
(if (= a 0)
b
(+ (dec a) (inc b))))

(def plus
  (fn[a,b] (
    if (= a 0)
      b
      (inc (plus (dec a) b))
    )
  )
)

(def plus
  (fn[a,b] (
    if (= a 0)
    b
    (plus (dec a) (inc b))
  ))
)
; Does not matter as Clojure does not have tail recurion
; However, with the first process, the memory allocation grows as the size of a grows and is a linear recursive process while the second is an linear iterative process as the number of arguments being called by plus stays consistent at 2.

; exercise 1.10

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(def A
  (fn[x y]
    (cond (= y 0) 0
          (= x 0) (* 2 y)
          (= y 1) 2
          :else (A (- x 1)
                   (A x (- y 1))))))
(A 1 10) 1024

(A 2 4) 65536

(A 3 3) 65536

(define (f n) (A 0 n))

(def f
  (fn[x]
    (A 0 x)))

; equal to 2 * N

(define (g n) (A 1 n))

(def g
  (fn[x]
    (A 1 x)))

; equal to 2 ** N

(define (h n) (A 2 n))

(def h
  (fn[x]
    (A 2 x)))

; equal to 2 ** 2 ** n - 1
(def count-change
  (fn[amount]
    (cc amount 5)))

(def first-denom
  (fn[kinds-of-coins]
    (cond (= kinds-of-coins 1) 1
      (= kinds-of-coins 2) 5
      (= kinds-of-coins 3) 10
      (= kinds-of-coins 4) 25
      (= kinds-of-coins 5) 50)))

(def cc
  (fn[amount, kind]
    (cond (= amount 0) 1
      (or (< amount 0) (= kind 0)) 0
      :else (+ (cc amount
                  (- kind 1))
                (cc (- amount
                    (first-denom kind))
                    kind)))))

; exercise 1.11
(def f
  (fn[n]
    (cond (< n 3) n
      :else (+ (f (- n 1))
                (* 2 (f (- n 2)))
                (* 3 (f (- n 3)))
                ))))
(def f2
  (fn[n]
    (cond (= n 0) 0
      (= n 1) 1
      (= n 2) 2
      :else (f-iter 3 n 2 1 0))))

(def f-iter
  (fn[cur, n, a, b, c]
    (cond (= cur n) (+ a
                  (* 2 b)
                  (* 3 c))
      :else (f-iter (+ cur 1)
              n
              (+ a
                (* 2 b)
                (* 3 c))
              a
              b))))

; exercise 1.12
(def pascal
  (fn[row, column]
    (cond (> column row) 0
      (< column 1) 0
      (= row 1) 1
      :else (+ (pascal (- row 1) (- column 1))
              (pascal (- row 1) column)))))

; exercise 1.13 Skipping until later late for proof

; exercise 1.14
; 11 cents -> 50
;          -> 25
;          -> 10  -> 1
;          -> 5 -> 5 -> 1
;          -> 5 -> 1
;          -> 5 -> 1 -> 1
;          -> 5 -> 1 -> 1 -> 1
;          -> 5 -> 1 -> 1 -> 1
;          -> 5 -> 1 -> 1 -> 1 -> 1
;          -> 5 -> 1 -> 1 -> 1 -> 1 -> 1
;          -> 5 -> 1 -> 1 -> 1 -> 1 -> 1 -> 1
;          -> 1 (all other ones... etc)
;          ...

; order of growth for space is also linear as it takes about the most depth of the tree (all single pennies )
; order of growth for steps is exponential as each additional needs to be checked via a n ^ 5th number of subtress (one per type of coin)

; exercise 1.15

(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
(if (not (> (abs angle) 0.1))
angle
(p (sine (/ angle 3.0)))))

; a. when (sine 12.15) is evaluated we divide it by 3 each time before angle gets less than .1 so about 5 times.

;b. the number of space grows in log of a as angle increases while the the number of steps increases also in a log of a speed

; exercise 1.16
(def fast_exp
  (fn[b, n]
    (fast_exp_iter b n 1)))

(def is_even
  (fn[a]
    (cond (= 0 (mod a 2)) true
    :else false)))

(def fast_exp_iter
  (fn[b, n, a]
    (cond (= n 1) a
      (is_even n) (fast_exp_iter b
                      (/ n 2)
                      (* a (* b b)))
      :else (fast_exp_iter b
              (/ (- n 1) 2)
              (* a (* b b) b)))))

; exercise 1.17
(def two_times
  (fn[a]
    (* a 2)))

(def half
  (fn[a]
    (/ a 2)))

(def multi
  (fn[a, b]
    (cond (= b 1) a
      (is_even b) (multi (double a) (half b))
      :else (multi (+ a (double a)) (half (- b 1))))))

; exercise 1.18

(def fast_multi
  (fn[a, b]
    (fast_multi_iter a b 0)))

(def fast_multi_iter
  (fn[a, b, s]
    (cond (= b 1) s
    (is_even b) (fast_multi_iter a
      (half b)
      (double (+ s a)))
      :else (fast_multi_iter a
        (half (- b 1))
        (+ a (double (+ s a)))))))

; exercise 1.19

(def fib
  (fn[n]
    (fib-iter 1 0 0 1 n)))

(def fib-iter
  (fn[a, b, p, q, count]
    (cond (= count 0) b
      (is_even count) (fib-iter a
                                b
                                (+ (* p p) (* q q))
                                (+ (* 2 p q) (* q q))
                                (/ count 2))
      :else (fib-iter (+ (* b q) (* a q) (* a p))
                      (+ (* b p) (* a q))
                      p
                      q
                      (- count 1)))))

; exercise 1.20

(define (gcd a b)
(if (= b 0)
a
(gcd b (remainder a b))))

; with normal order operations, the entire gcd gets expanded until only primatives are remaining which means at every point the (remainder a b) must be written out as remainder(a b) while in applicative order arguments are evaluated step by step, therefore with normal order each gcd operations after the first are expanded out completely as we subsitute both a and b with (remainder a b) until all we are left with are remainder operations.

; with applicative order opreations, each expression of the if then statement clauses are fully evaluated before before application the top most level operation, therefore we do not have to expand out each argument of the remainder operation completely. this significantly reduces the amount of memory and step it takes to calculate the final result when compared to the normal order version.

; exercise 1.21

(def smallest-divisor
  (fn[n]
    (find-divisor n 2)))

(def find-divisor
  (fn[n, test]
    (cond (> (square test) n) n
      (divides test n) test
      :else (find-divisor n (+ test 1)))))

(def divides
  (fn[a, b]
    (= (mod b a) 0)))

(def square
  (fn[n]
    (* n n)))

(def is-prime
  (fn[n]
    (= n (smallest-divisor n))))

; smallest divisors for 199 199, 1999 1999, 19999 7

; exercise 1.22 couldnt figure out how to calculate time and scheme implementation seems to be wonky
(define (smallest-divisor n)
(find-divisor n 2))

(define (find-divisor n test-divisor)
(cond ((> (square test-divisor) n) n)
((divides? test-divisor n) test-divisor)
(else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
(= (remainder b a) 0))

(define (prime? n)
(= n (smallest-divisor n)))

(define (timed-prime-test n)
(newline)
(display n)
(start-prime-test n (runtime)))

(define (start-prime-test n start-time)
(if (prime? n)
(report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
(display " *** ")
(display elapsed-time))

(define (is-even n)
  (cond ((= (remainder n 2) 0) true)
        (else false)))

(define (search-for-primes start end)
  (cond ((< end start) false)
        ((is-even start)
            (search-for-primes (+ start 1) end))
        (else (timed-prime-test start)
                (search-for-primes (+ start 1)
                                    end))))))

; exercise 1.23 once again cannot test for time, cannot find a good time measuring function

(def smallest-divisor
  (fn[n]
    (find-divisor n 2)))


(def find-divisor
  (fn[n, test]
    (cond (> (square test) n) n
    (divides test n) test
    :else (find-divisor n (next-num test)))))

(def divides
  (fn[a, b]
    (= (mod b a) 0)))

(def square
  (fn[n]
    (* n n)))

(def is-prime
  (fn[n]
    (= n (smallest-divisor n))))

(defn next-num [n]
  (cond (= n 2) 3
        :else n + 2))

; exercise 1.24
(define (expmod base exp m)
(cond ((= exp 0) 1)
((even? exp)
(remainder (square (expmod base (/ exp 2) m))
m))
(else
  (remainder (* base (expmod base (- exp 1) m))
  m))))

(define (fermat-test n)
(define (try-it a)
(= (expmod a n n) a))
(try-it (+ 1 (random (- n 1)))))


(define (fast-prime? n times)
(cond ((= times 0) true)
((fermat-test n) (fast-prime? n (- times 1)))
(else false)))

(define (start-prime-test n start-time)
(if (fast-prime? n 2)
(report-prime (- (runtime) start-time))))

; exercise 1.25

; with our version of expmod we are never having to actualy deal with a ver large number, at most we are dealing with the remainder of (some number smaller than m) and m. If we were to actually use our fast exp then the remainder operation will take close to forever as doing large interger divisions is very costly

; exercise 1.26

; because of the multiplcation, each number has to actually be calculated twice and therefore the function grows at 2n of m while the original is at log n. (theres an additional call to expomod per 2 increases in the value of n) it also transform the memory footprint of the function into that of a tree recursion, calcuating 2 extra branches per 2 increase in n and therefore slows thing down greatly. 
