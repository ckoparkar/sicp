(ns sicp.core
  (:use [clojure.repl :as repl]))

(defn square [x]
  (* x x))

(defn average [x y]
  (/ (+ x y) 2))

(defn abs [x]
  (cond 
   (< x 0) (- x)
   :else x))

(defn improve [guess x]
  (average guess (/ x guess)))

;; (defn good-enough? [guess x]
;;   (< (abs (- (square guess) x)) 0.001))


(defn good-enough? [guess x]
  (< (/ (abs (- guess (improve guess x))) guess) 0.001))


(defn sqrt-iter [guess x]
  (if  (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) x)))

(defn sqrt [x]
  (sqrt-iter 1.0 x))

(sqrt 10000000000000)
(sqrt 0.000005)


(defn factorial-r [n]
  (cond 
   (= n 0) 1
   :else (* n (factorial-r (- n 1)))))

(factorial-r 20)

(defn fact-iter [n product]
  (if
      (= n 0) product
      (fact-iter (- n 1) (* product n))))

(defn factorial-i [n]
  (fact-iter n 1 ))

(factorial-i 20)


(defn fib [n]
  (cond 
   (= n 0) 0
   (= n 1) 1
   :else (+ (fib (- n 1)) (fib (- n 2)))))

(fib 6)


(defn fib-iter [first second n]
  (cond 
   (= n 0) 0
   (= n 1) second
   :else (fib-iter second (+ first second) (- n 1))
   ))

(defn fibo [n]
  (fib-iter 0 1 n))

(fibo 4)


(defn f-n [n]
  (if (< n 3) n
      (+ (f-n (- n 1)) (* 2 (f-n (- n 2))) (* 3 (f-n (- n 3))) )))

(f-n 4)

;; make this iterative. remaining

(defn cube [x]
  (* x x x))

(defn p [x]
  (- (* 3 x) (* 4 (cube x))))

(defn sine [angle]
  (if
      (<= (abs angle) 0.01) angle
      (p (sine (/ angle 3.0)))))

(sine 90)

(defn expt [b n]
  (if
      (= n 0) 1
      (* b (expt b (- n 1)))))

(expt 2 3)


(defn expt-iter [b n product]
  (if 
      (= n 0) product
      (expt-iter b (- n 1) (* b product))))

(defn expt-i [b n]
  (expt-iter b n 1))

(expt-i 2 4)


(defn fast-expt [b n]
  (cond
   (= n 0) 1
   (even? n) (square (fast-expt b (/ n 2)))
   :else (* b (fast-expt b (- n 1)))))

(fast-expt 2.0 1000)


(defn fast-expt-iter [b n a]
  (cond
   (= n 0) a
   (even? n) (fast-expt-iter (square b) (/ n 2) a)
   :else (fast-expt-iter b (- n 1) (* a b))
   ))

(defn fast-expt-i [b n]
  (fast-expt-iter b n 1))

(fast-expt-i 2.0 1000)


(defn gcd [a b]
  (if
      (= 0 b) a
      (gcd b (rem a b))
      ))

(gcd 6 3)


(defn divides? [a b]
  (= 0 (rem a b)))


(defn next-divisor [n]
  (if 
      (= n 2) 3
      (+ n 2)))

(defn find-divisor [n a]
  (cond
   (divides? n a) a
   :else (find-divisor n (next-divisor a))))

(defn smallest-divisor [n]
  (find-divisor n 2))

(smallest-divisor 19)

(defn prime? [n]
  (= (smallest-divisor n) n))

(prime? 134)
(prime? 13)

;;debug
;; (defn expmod [base exp m]
;;   (cond
;;    (= exp 0) 1
;;    (even? exp) (rem (square (expmod base (/ exp 2) m)) m)
;;    :else (rem (* base (expmod base (- exp 1) m)) m)
;;    ))

;; (defn fermat-test [n]
;;   (defn try [a]
;;     (= a (expmod a n n))
;;     )
;;   (try (+ 1 (rand-int (- n 1))))
;;   )

;; (defn fast-prime? [n times]
;;   (cond
;;    (= times 0) true
;;    (fermat-test n) (fast-prime? n (- times 1))
;;    :else false))

;; (fast-prime? 10 100)

(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)

(defn sum [a b term next]
  (if 
      (> a b) 0
      (+ (term a) (sum (next a) b term next))
      ))

(sum 1 10 (fn [x] x) inc)

(sum 1 10 cube inc)

;; gives approximation of pi. 1/1.3 + 1/5.7 + 1/9.11 ...
(* 8 (sum 1 1000 (fn [x] (/ 1.0 (* x (+ x 2)))) (fn [x] (+ x 4))))

(defn integral [f a b dx]
  (* dx (sum a b f (fn [x] (+ x dx)))))

(integral cube 0 1 0.001)

(defn simpson-integral [f a b n]
  (let [h (/ (- b a) n) k 0]

    (defn yn [k]
      (f (+ a (* k h)))
      )

    (defn actual-yn [k]
      (cond
       (or (= k 0) (= k n)) (yn k)
       (even? k) (* 2 (yn k))
       :else (* 4 (yn k))
       )
      )

    (* (/ h 3)(sum 0 n actual-yn inc))
    )
  )

(simpson-integral cube 0 1.0 1000)

(defn sum-i [a b term next]
  (defn iter [a result]
    (if 
        (> a b) result
        (iter (next a) (+ result (term a)))
        )
    )
  (iter a 0)
  )

(sum-i 1 10 cube inc)

(defn product [a b term next]
  (if
      (> a b) 1
      (* (term a) (product (next a) b term next))))

;; gives factorial
(product 1 10 (fn [x] x) inc)


(defn product-i [a b term next]
  (defn iter [a result]
    (if
        (> a b) result
        (iter (next a) (* result (term a)))
        ))
  (iter a 1)
  )

(product-i 1 10 (fn [x] x) inc)

;; (accumulate (combiner a b) base-value term a next b)

(defn accumulate-i [combiner base-value term a next b]
  (defn iter [a result]
    (if
        (> a b) result
        (iter (next a) (combiner result (term a)))
        ))
  (iter a base-value)
  )

(accumulate-i + 0 (fn [x] x) 0 inc 10)
(accumulate-i * 1 (fn [x] x) 1 inc 10)

(defn accumulate [combiner base-value term a next b]
  (cond
   (> a b) base-value
   :else (combiner (term a) (accumulate combiner base-value term (next a) next b))
   )
  )

(accumulate-i + 0 (fn [x] x) 0 inc 10)
(accumulate * 1 (fn [x] x) 1 inc 10)


;; debug
;; (defn filtered-accumulate-i [combiner base-value term a next b pred]
;;   (defn iter [a result pred]
;;     (cond
;;      (> a b) result
;;      (pred a) (iter (next a) (combiner result (term a)) pred)
;;      :else (iter (next a) result pred)
;;      )
;;     )
;;   (iter a base-value pred)
;;   )

;; (filtered-accumulate-i + 0 (fn [x] x) 0 inc 10 prime?)
;; (filtered-accumulate-i * 1 (fn [x] x) 1 inc 10 prime?)


;; (defn filtered-accumulate [combiner base-value term a next b pred]
;;   (cond
;;    (> a b) base-value
;;    (pred a) (combiner (term a) (filtered-accumulate combiner base-value term (next a) next b pred))
;;    :else (filtered-accumulate combiner base-value term (next a) next b pred)
;;    ))

;; (filtered-accumulate + 0 (fn [x] x) 0 inc 10 prime?)
;; (filtered-accumulate * 1 (fn [x] x) 1 inc 10 prime?)


(defn search [f neg pos]
  (let [mid (average neg pos)
        close-enough? (fn [x y] (< (abs (- x y)) 0.001))]
    (cond
     (close-enough? neg pos) mid
     (neg? (f mid)) (search f mid pos)
     (pos? (f mid)) (search f neg mid)
     :else mid
     )
    ))

(defn half-interval [f a b]
  (let [a-val (f a)
        b-val (f b)]
    (cond
     (and (pos? a-val) (neg? b-val)) (search f b a)
     (and (neg? a-val) (pos? b-val)) (search f a b)
     :else :incorrect-values
     )))

(half-interval sine 2.0 4.0)

(half-interval (fn [x] (- (cube x) (* 2 x) 3)) 1.0 2)



(defn fixed-point [f guess]
  (let [tolerance 0.001
        close-enough? (fn [x y] (< (abs (- x y)) tolerance))]
    (defn try-guess [f guess]
      (println guess)
      (let [next (f guess)]
        (if
            (close-enough? guess next) guess
            (try-guess f next)
            ))
      )
    (try-guess f guess)
    ))

(fixed-point #(Math/cos %) 1.0)

(defn square-root [x]
  (fixed-point #(average % (/ x %)) 1)
  )

(square-root 4.0)


;; golden ratio maps to (x -> 1 + 1/x)

(fixed-point #(+ 1 (/ 1 %)) 1.0)

(fixed-point #(average % (/ (Math/log 1000) (Math/log %))) 10.0)
