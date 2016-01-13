(ns primes.core
  (:require [primes.tabulate :refer [tabulate products]]
            [primes.gen :refer [primes]])
  (:gen-class))

(defn -main
  "Prints a table of the products of the first 10 prime numbers."
  [& args]

  ; TODO: parse command line
  (let [ps (take 10 (primes))]
    (println (tabulate ps ps (products * ps ps)))) )
