(ns primes.core
  (:require [primes.tabulate :refer [tabulate products]]
            [primes.gen :refer [primes]])
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]

  (let [ps (take 10 (primes))]
    (println (tabulate ps ps (products * ps ps)))) )
