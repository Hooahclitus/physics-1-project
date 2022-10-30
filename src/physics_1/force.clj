(ns physics-1.force
  #_{:clj-kondo/ignore [:refer-all]}
  (:require [physics-1.utils :refer :all]))

;; Weight
(defn weight [sym & binds]
  (let [{:keys [m g] :as coll}
        (if (map? (first binds)) (first binds) binds)]
    (cond
      (= sym 'newton)
      (cond
        (every? coll [:m :g]) {:N (round (* m g))}))))


;; Newton's Second Law & Derivations
(defn newton [sym & binds]
  (let [{:keys [N a m] :as coll}
        (if (map? (first binds)) (first binds) binds)]
    (cond
      (= sym 'force)
      (cond
        (every? coll [:a :m]) {:N (round (* a m))})
      (= sym 'acceleration)
      (cond
        (every? coll [:m :N]) {:a (round (/ N m))})
      (= sym 'mass)
      (cond
        (every? coll [:a :N]) {:m (round (/ N a))}))))


;; Static/Kinetic Friction & Derivations
(defn friction [sym & binds]
    (let [{:keys [staticFrictionMax kineticFriction COFstatic COFkinetic mass] :as coll}
          (if (map? (first binds)) (first binds) binds)]
    (cond
      ;; Max Static Friction
      (= sym 'staticFrictionMax)
      (cond
        (every? coll [:COFstatic :mass]) {:Fstatic (* COFstatic mass 9.8)})

      ;; Coefficient of Static Friction
      (= sym 'COFstatic)
      (cond
        (every? coll [:Fs :mass]) {:COFstatic (/ staticFrictionMax (* mass 9.8))})

      ;; Kinetic Friction
      (= sym 'kineticFriction)
      (cond
        (every? coll [:COFkinetic :mass]) {:Fkinetic (* COFkinetic mass 9.8)})

      ;; Coefficient of Kinetic Friction
      (= sym 'COFkinetic)
      (cond
        (every? coll [:Fk :mass]) {:COFkinetic (/ kineticFriction (* mass 9.8))})

      ;; Mass from Static or Kinetic Friction
      (= sym 'mass)
      (cond
        ;; Mass from Static Friction
        (every? coll [:Fs :COFstatic]) {:mass (/ staticFrictionMax (* COFstatic 9.8))}

        ;; Mass from Kinetic Friction
        (every? coll [:Fk :COFkinetic]) {:mass (/ kineticFriction (* COFkinetic 9.8))}))))
