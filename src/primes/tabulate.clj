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
  "Prepends val with enough spaces so it fits in width, and appends vals."
  [width val & vals]
  (let [fmt-str (str "%" width "s")]
    (apply str (format fmt-str val) vals)))


(defn spacer
  "Creates a spacer by repeating ch of the given width."
  [width s]
  (join "" (take width (repeat s))))


(defn tabulate
  "Renders a table of values resulting from applying f to each item in x-seq and y-seq"
  [f x-seq y-seq]
  (let [v-div " |"
        h-div "-"

        ; label data
        row-headers y-seq
        col-headers x-seq

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
    
    ; header, divider, each row
    rows
    #_(join "\n" [header-str divider-str (join "\n" rows-strs)])))
