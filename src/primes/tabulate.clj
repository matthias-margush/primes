(ns primes.tabulate
    (:require [clojure.string :refer [join]]))


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
  "Prepends str of val with enough spaces so its count is width, then appends vals."
  [width val & vals]
  (let [fmt-str (str "%" width "s")]
    (apply str (format fmt-str val) vals)))


(defn spacer
  "Creates a spacer of the given width by repeating s."
  [width s]
  (join "" (take width (repeat s))))


(defn tabulate
  "Renders a table of values."
  [xs ys table-data]
  (let [v-div " |"
        h-div "-"

        ; table data
        rows table-data
        cols (transpose rows)

        ; label data
        row-headers ys
        col-headers xs

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
    
    ; header, divider, then each row
    (join "\n" [header-str divider-str (join "\n" rows-strs)])))

