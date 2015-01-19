(ns sicp.c1
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(defn abs [x]
  (cond
   (< x 0) (- x)
   :else x))

(defn square [x] (* x x))

(defn sum-of-sqaures [x y]
  (+ (square x) (square y)))

(defn max2 [args]
  (let [sorted (reverse (sort args))]
    (+ (first sorted) (first (rest sorted)))
    ))

(defn average [x y]
  (/ (+ x y) 2))

(defn improve [guess x]
  (average guess (/ x guess)))

(defn good-enough? [guess x]
  (< (abs (- (improve guess x) guess)) (* 0.001 guess))
  )

(defn sqrt-iter [guess x]
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) x)))

(defn sqrt [x]
  (sqrt-iter 1.0 x))

(defn improve-cube [guess x]
  (/ (+
      (/ x (square guess))
      (* 2 guess))
     3))

(defn good-enough-cube? [guess x]
  (< (abs (- (improve-cube guess x) guess)) (* 0.001 guess))
  )

(defn cube-iter [guess x]
  (if (good-enough-cube? guess x)
    guess
    (cube-iter (improve-cube guess x) x)))

(defn cube-root [x]
  (cube-iter 1.0 x))

(defn factorial [n]
  (if (= n 1)
    n
    (* n (factorial (- n 1)))
    ))

(defn fact-iter [c p n]
  (if (> c n)
    p
    (fact-iter (+ c 1) (* p c) n)
    ))

(defn factorial [n]
  (fact-iter 1 1 n))

(defn fib [n]
  (cond
   (= n 0) 0
   (= n 1) 1
   :else (+ (fib (- n 1)) (fib (- n 2))
            )))

(defn fib-iter [a b n]
  (cond
   (= n 0) 0
   (= n 1) b
   :else (fib-iter b (+ a b) (- n 1))
   ))

(defn fib [n]
  (fib-iter 0 1 n))

(defn gcd [a b]
  (if (= b 0)
    a
    (gcd b (mod a b))))

(defn divides? [a b]
  (= (mod a b) 0))

(defn next' [test-divisor]
  (if (= test-divisor 2)
    3
    (+ test-divisor 2)))

(defn find-divisor [n test-divisor]
  (cond
   (> (square test-divisor) n) n
   (divides? n test-divisor) test-divisor
   :else (find-divisor n (next' test-divisor))
   ))

(defn smallest-divisor [n]
  (find-divisor n 2))

(defn prime? [n]
  (= (smallest-divisor n) n))

;; Fermat test
;; n is prime if a**n mod n == a

(defn exp-mod [base exp m]
  "Returns base**exp mod m"
  (cond 
   (= exp 0) 1
   (even? exp) (mod (square
                     (exp-mod base (/ exp 2) m))
                    m)
   :else (mod (* base (exp-mod base (dec exp) m)) m)
   ))

(defn try-it [n a]
  (= a (exp-mod a n n)))

(defn fast-prime? [n times]
  (cond
   (= times 0) true
   (try-it n (rand-int n)) (fast-prime? n (dec times))
   :else false))

(defn prime? [n]
  (fast-prime? n 10))

;; 3 primes > 1000
(take 3 (filter prime? (range 1001 1100 2)))

;; 3 primes > 10000
(take 3 (filter prime? (range 10001 11000 2)))

;; 3 primes > 100000
(time
 (dotimes [_ 100]
   (doall
    (take 3 (filter prime? (range 100001 110000 2)))
    )))

(defn sum [term a next b]
  (if (> a b)
    0
    (+ (term a) (sum term (next a) next b))))

;; iterative version of sum
(defn sum [term a next b]
  (defn sum-iter [a s]
    (if (> a b)
      s
      (sum-iter (next a) (+ s (term a)))))
  (sum-iter a 0))

(defn cube [x]
  (* x x x))

(defn integral [f a b dx]
  (defn add-dx [x] (+ x dx))
  (* (sum f (+ a (/ dx 2)) add-dx b) dx))

(time (dotimes [_ 100] (integral cube 0 1 0.00091)))

(defn simpson-integral [f a b n]
  (def h (/ (- b a) n))
  (defn y [k]
    (f (+ a (* k h))))
  (defn term [k]
    (* (cond
        (zero? (mod k n)) 1
        (odd? k) 4
        :else 2)
       (y k)))
  (/ (* h (sum term 0 inc n)) 3.0))

(time (dotimes [_ 100] (simpson-integral cube 0 1 10)))

(defn product [term a next b]
  (defn iter [a p]
    (if (> a b)
      p
      (iter (next a) (* p (term a)))))
  (iter a 1)
  )

(defn factorial [n]
  (product identity 1 inc n))

;; approximate pi using
;; pi/4 = 2.4.4.6.6.8.../3.3.5.5.7.7

(defn approximate-pi [n]
  (* 4
   (/
    (product (fn [a] (* a (+ a 2.0))) 2 (fn [a] (+ a 2)) n)
    (product square 3 (fn [a] (+ a 2.0)) (inc n)))))

(approximate-pi 160)


(defn accumulate [combiner null-value term a next b]
  (defn iter [a c]
    (if (> a b)
      c
      (iter (next a) (combiner c (term a)))))
  (iter a null-value)
  )

(accumulate * 1 identity 1 inc 4)

(defn filtered-accumulate [combiner null-value term a next b pred]
  (defn iter [a c]
    (if (> a b)
      c
      (iter (next a) (if (pred a)
                       (combiner c (term a))
                       c)
            )))
  (iter a null-value))

;; sum of squares of prime numbers in a to b
;; 2..4 -> 4 + 9
(filtered-accumulate + 0 square 2 inc 4 prime?)

;; product of all ints less than n that are relatively prime to n
;; all i such that GCD(i,n) = 1
;; 1..5 ->  * 1 2 3 4
(filtered-accumulate * 1 identity 1 inc 5 (fn [a] (= 1 (gcd a 5))))
