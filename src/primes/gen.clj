(ns primes.gen
    (:require [clojure.string :refer [join]]
              [clojure.math.numeric-tower :refer [sqrt]]))


(defn- prime?
  "Checks whether a number is prime."
  [p?]
  (let [end (sqrt p?)]
    (loop [n 2]
      (cond
        (> n end) true
        (= 0 (rem p? n)) false
        :else (recur (inc n))))))


(defn- next-prime
  "Returns the next highest prime, given an initial prime number"
  [p]
  (loop [n (inc p)]
    (if (prime? n) n
      (recur (inc n)))))


(defn primes
  "Returns a lazy seq of primes."
  ([] (primes 1))
  ([p] (lazy-seq (cons p (primes (next-prime p))))))


(defn products
  "Applies f to each item in the cartesian product of rows and cols."
  [f rows cols]
  (partition (count cols) (for [x rows y cols] (f x y))))


(defn transpose
  "Transposes a list of lists."
  [table]
  (apply map list table))


(defn max-width
  "Returns the item in the sequence with the longest printed length."
  [seq]
  (apply max (map (comp count str) seq)))


(defn fmt
  "Prepends val with enough spaces so it fits in width, and appends vals"
  [width val & vals]
  (let [fmt-str (str "%" width "s")]
    (apply str (format fmt-str val) vals)))


(defn spacer
  "Creates a spacer by repeating ch of the given width"
  [width s]
  (join "" (take width (repeat s))))


(defn print-table
  "Prints a multiplication table of primes."
  []
  (let [f *
        v-div " |"
        h-div "-"

        primes (take 10 (primes))

        ; label data
        row-headers primes
        col-headers primes

        ; table data
        rows (products f row-headers col-headers)
        cols (transpose rows)

        ; layout widths
        col-widths (map max-width cols)
        row-lbl-width (first col-widths)

        ; labels
        topleft-lbl (spacer (+ (count v-div) row-lbl-width) " ")
        row-lbls (map #(fmt row-lbl-width % v-div) row-headers)
        col-lbls (cons topleft-lbl (map fmt col-widths col-headers))
        row-vals (for [row rows] (map fmt col-widths row))

        ; render
        header-str (join " " col-lbls)
        divider-str (spacer (count header-str) "-")
        rows-strs (map #(str %1 " " (join " " %2)) row-lbls row-vals)]
    
    (println header-str)
    (println divider-str)
    (doseq [row-str rows-strs]
           (println row-str))))

