(ns physics-1.motion
  #_{:clj-kondo/ignore [:refer-all]}
  (:require [physics-1.utils :refer :all]))


(defn linear [sym & binds]
  (let [{:keys [a t vi vf xi xf] :as coll}
        (if (map? (first binds)) (first binds) binds)]
    (cond
      (= sym 'initialPosition)
      (cond
        (every? coll [:a :vi :vf :xf]) {:xi (round (- xf (/ (- (sqr vf) (sqr vi)) (* 2 a))))}
        (every? coll [:a :t :vi :xf]) {:xi (round (- xf (* vi t) (* 1/2 a (sqr t))))}
        (every? coll [:t :vi :vf :xf]) {:xi (round (- xf (/ (* t (+ vf vi)) 2)))})
      (= sym 'finalPosition)
      (cond
        (every? coll [:a :vi :vf :xi]) {:xf (round (+ xi (/ (- (sqr vf) (sqr vi)) (* 2 a))))}
        (every? coll [:a :t :vi :xi]) {:xf (round (+ xi (* 1/2 (sqr t) a) (* vi t)))}
        (every? coll [:t :vi :vf :xi]) {:xf (round (+ xi (* (+ vf vi) 1/2 t)))})
      (= sym 'initialVelocity)
      (cond
        (every? coll [:a :t :xi :xf]) {:vi (round (/ (- (* 2 (- xf xi)) (* a (sqr t))) (* 2 t)))}
        (every? coll [:a :vf :xi :xf]) {:vi (round (sqrt (- (sqr vf) (* 2 a (- xf xi)))))}
        (every? coll [:t :vf :xi :xf]) {:vi (round (- (/ (* 2 (- xf xi)) t) vf))}
        (every? coll [:a :t :vf]) {:vi (round (- vf (* a t)))})
      (= sym 'finalVelocity)
      (cond
        (every? coll [:a :vi :xi :xf]) {:vf (round (sqrt (+ (sqr vi) (* 2 a (- xf xi)))))}
        (every? coll [:t :vi :xi :xf]) {:vf (round (- (/ (* 2 (- xf xi)) t) vi))}
        (every? coll [:a :t :vi]) {:vf (round (+ (* a t) vi))})
      (= sym 'acceleration)
      (cond
        (every? coll [:vi :vf :xi :xf]) {:a (round (/ (- (sqr vf) (sqr vi)) (* 2 (- xf xi))))}
        (every? coll [:t :vi :xi :xf]) {:a (round (/ (* 2 (- xf xi (* vi t))) (sqr t)))}
        (every? coll [:t :vi :vf]) {:a (round (/ (- vf vi) t))})
      (= sym 'time)
      (cond
        (every? coll [:vi :vf :xi :xf]) {:t (/ (* 2 (- xf xi)) (+ vi vf))}
        (every? coll [:a :vi :vf]) {:t (/ (- vf vi) a)}))))


(defn circular [sym & binds]
  (let [{:keys [per rad radAccel angVel tanVel] :as coll}
        (if (map? (first binds)) (first binds) binds)]
    (cond
      (= sym 'radialAcceleration)
      (cond
       (every? coll [:tanVel :rad]) {:radAccel (round (/ (sqr tanVel) rad))})
      (= sym 'tangentialVelocity)
      (cond
       (every? coll [:radAccel :rad]) {:tanVel (round (sqrt (* radAccel rad)))}
       (every? coll [:angVel :rad]) {:tanVel (round (* angVel rad))})
      (= sym 'radius)
      (cond
       (every? coll [:radAccel :tanVel]) {:rad (round (/ (sqr tanVel) radAccel))}
       (every? coll [:angVel :tanVel]) {:rad (round (/ tanVel angVel))})
      (= sym 'period)
      (cond
        (every? coll [:tanVel :rad]) {:per (round (/ (* 2 pi rad) tanVel))})
      (= sym 'frequency)
      (cond
        (every? coll [:per]) {:freq (/ 1 per)}))))
