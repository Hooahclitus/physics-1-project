(ns physics-1.convert
  #_{:clj-kondo/ignore [:refer-all]}
  (:require [physics-1.utils :refer :all]))


;; START CONVERSION FUNCTIONS
(defn km [dist]
  (round (* dist 1000)))

(defn toKm [dist]
  (round (double (/ dist 1000))))

(defn toMin [rev]
  (* 60 rev))

;; END CONVERSION FUNCTIONS
