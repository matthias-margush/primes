(ns primes.gen
  (:require [clojure.math.numeric-tower :refer [sqrt]]))


(defn prime?
  "Checks whether a number is prime."
  [p?]
  (if (< p? 2) false
    (let [end (sqrt p?)]
      (loop [n 2]
        (cond
          (> n end) true
          (= 0 (rem p? n)) false
          :else (recur (inc n)))))))


(defn next-prime
  "Returns the next highest prime, given an initial prime number."
  [p]
  (loop [n (inc p)]
    (if (prime? n) n
      (recur (inc n)))))


(defn primes
  "Returns a lazy seq of primes."
  ([] (primes 2))
  ([p] (lazy-seq (cons p (primes (next-prime p))))))

