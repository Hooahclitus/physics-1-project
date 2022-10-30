(ns physics-1.vector
  #_{:clj-kondo/ignore [:refer-all]}
  (:require [physics-1.utils :refer :all]))


;; Takes n vectors => Sum of vectors
(def sum (partial mapv +))

;; Takes n vectors => Difference of vectors
(def diff (partial mapv -))

;; Takes n vectors => Dot product of vectors
(def dot (partial mapv *))

;; Takes scalar and vector => Scaled vector
(defn scale [u v]
  (cond
    (vector? u) ; u is a vector, v is a scalar
    (mapv (partial * v) u)
    (vector? v) ; v is a vector, u is a scalar
    (mapv (partial * u) v)))

;; Takes two vectors of three elements => Cross of vectors
(defn cross [u v]
  (let [i (- (* (nth u 1) (nth v 2))
             (* (nth u 2) (nth v 1)))
        j (- (* (nth u 2) (nth v 0))
             (* (nth u 0) (nth v 2)))
        k (- (* (nth u 0) (nth v 1))
             (* (nth u 1) (nth v 0)))]
    [i j k]))

;; Takes x & y components => Magnitude
(defn mag [& binds]
  (cond
    (vector? (first binds)) (round (sqrt (reduce + (map sqr (first binds)))))
    :else (round (sqrt (reduce + (map sqr binds))))))

;; Given magnitude & angle => Map of magnitude, angle, x & y components
;; Given x & y components  => Map of magnitude, angle, x & y components
(defn resolveVector [& binds]
  (let [{:keys [x1 x2 y1 y2 x-comp y-comp mag theta] :as coll}
        (if (map? (first binds)) (first binds) binds)]
    (cond
      (every? coll [:mag :theta])
      (let [theta (if (ratio? theta) (deg theta) theta)]
        {:deg (round theta)
         :mag (round mag)
         :x-comp (round (* mag (cos theta)))
         :y-comp (round (* mag (sin theta)))})
      (every? coll [:x-comp :y-comp])
      {:deg (round (deg (Math/acos (/ x-comp
                                      (sqrt (+ (sqr x-comp)
                                               (sqr y-comp)))))))
       :mag (round (sqrt (+ (sqr x-comp)
                            (sqr y-comp))))
       :x-comp (round x-comp)
       :y-comp (round y-comp)}
      (every? coll [:x1 :x2 :y1 :y2])
      {:deg (round (deg (Math/acos (/ (- x2 x1)
                                      (sqrt (+ (sqr (- x2 x1))
                                               (sqr (- y2 y1))))))))
       :mag (round (sqrt (+ (sqr (- x2 x1))
                            (sqr (- y2 y1)))))
       :x-comp (round (- x2 x1))
       :y-comp (round (- y2 y1))})))