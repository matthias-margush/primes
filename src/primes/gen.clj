(ns primes.gen
    (:require [clojure.string :refer [join]]))


(defn primes
  "Returns a lazy seq of primes."
  []
  (rest (range)))


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
  "Returns the seq with each item formatted for printing."
  [width v]
  (let [fmt-str (str "%" width "s")]
    (format fmt-str v)))


(defn print-table
  "Prints a multiplication table of primes."
  []
  (let [f *
        primes (take 10 (primes))

        row-headers primes
        col-headers primes

        rows (products f row-headers col-headers)
        cols (transpose rows)

        col-widths (map max-width cols)
        row-lbl-width (first col-widths)
        
        col-labels (map fmt col-widths col-headers)
        row-labels (map fmt (repeat row-lbl-width) row-headers)
        row-values (for [row rows] (map fmt col-widths row))]
    
    ; column header row
    (println (fmt row-lbl-width " ")(join " " col-labels))

    ; each row with labels
    (dorun
      (map #(println %1 (join " " %2)) row-labels row-values))))



