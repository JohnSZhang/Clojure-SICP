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

; exercise 1.9

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
