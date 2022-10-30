(ns physics-1.energy
  #_{:clj-kondo/ignore [:refer-all]}
  (:require [physics-1.utils :refer :all]))


;; Kinetic Energy
(defn kinetic [sym & binds]
  (let [{:keys [k m v] :as coll}
        (if (map? (first binds)) (first binds) binds)]
    (cond
      (= sym 'kinetic)
      (cond
        (every? coll [:m :v]) {:k (* 1/2 m (sqr v))})
      (= sym 'mass)
      (cond
        (every? coll [:k :v]) {:m (/ (* 2 k) (sqr v))})
      (= sym 'velocity)
      (cond
        (every? coll [:k :m]) {:v (sqrt (/ (* 2 k) m))}))))


;; Work-Energy
(defn work-energy [sym & binds]
  (let [{:keys [k1 k2] :as coll}
        (if (map? (first binds)) (first binds) binds)]
    (if (= sym 'netWork)
      (every? coll [:k1 :k2]) {:wNet (- k2 k1)})))


;; Difference in Potential Energy
(defn potentialDifference [& binds]
  (let [{:keys [ua ub N] :as coll}
        (if (map? (first binds)) (first binds) binds)]
    (cond
      (every? coll [:ua :ub :N]) (* N (- ub ua)))))
