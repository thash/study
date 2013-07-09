;; 10.3 test.generative
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ns clojurebreaker.game-test
  (:use [clojure.test.generative :only (defspec) :as test])
  (:require [clojure.test.generative.generators :as gen]
            [clojurebreaker.game :as game]
            [clojure.math.combinatorics :as comb]))

(defn matches
  "Given a score, returns total number of exact plus
   unordered matches."
  [score]
  (+ (:exact score) (:unordered score)))

(defn scoring-is-symmetric
  [secret guess score]
  (= score (game/score guess secret)))

(defn scoring-is-bounded-by-number-of-pegs
  [secret guess score]
  (< 0 (matches score) (count secret)))

(defn reordering-the-guess-does-not-change-matches
  [secret guess score]
  (= #{(matches score)}
     (into #{} (map
                 #(matches (game/score secret %))
                 (comb/permutations guess)))))

