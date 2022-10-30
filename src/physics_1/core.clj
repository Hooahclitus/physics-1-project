(ns physics-1.core
  #_{:clj-kondo/ignore [:refer-all]}
  (:require [physics-1.utils :refer :all]
            [physics-1.convert :as convert]
            [physics-1.vector :as vector]
            [physics-1.motion :as motion]
            [physics-1.force :as force]
            [physics-1.energy :as energy]))


;; Vector operations given two 1x3 matrices
(def v1 [1 2 3])
(def v2 [4 5 6])

(vector/sum v1 v2)   ;; => [5 7 9]
(vector/diff v1 v2)  ;; => [-3 -3 -3]
(vector/dot v1 v2)   ;; => [4 10 18]
(vector/cross v1 v2) ;; => [-3 6 -3]

;; Vector operations given scalar and one 1x3 matrix
(def scl 2)
(vector/scale scl v1) ;; => [2 4 6]

;; Note: The following four operations work on any
;;       number of 1xn matrices. If there are not
;;       an equal number of elements in a vector
;;       then the returned value is equal to the
;;       vector with the lowest element count.
(def v3 (vec (take 20 (repeatedly #(rand-int 30)))))
(def v4 (vec (take 20 (repeatedly #(rand-int 30)))))

(vector/sum v3 v4)
(vector/diff v3 v4)
(vector/scale scl v3)
(vector/dot v3 v4)


;; Ch.2 Vectors
;; Problem 39:
;; You drive 7.50 km in a straight line in a direction 15° east of north.
;;      a) Find the distances you would have to drive straight east
;;         and then straight north to arrive at the same point.
;;      b) Show that you still arrive at the same point if the east
;;         and north legs are reversed in order.
;; Ans: a) 1.94 km, 7.24 km
;;      b) line 28

;; Definitions
(def p39-mag (convert/km 7.5))
(def p39-theta (- 90 15))
(def p39-sol (vector/resolveVector :mag p39-mag :theta p39-theta))

;; Solutions: a)
(convert/toKm (get p39-sol :x-comp)) ;; => 1.94
(convert/toKm (get p39-sol :y-comp)) ;; => 7.24

;; Solutions: b)
(convert/toKm (vector/mag (get p39-sol :x-comp)   ;; => 7.5
                          (get p39-sol :y-comp)))
(convert/toKm (vector/mag (get p39-sol :y-comp)   ;; => 7.5
                          (get p39-sol :x-comp)))


;; Problem 49:
;; Given two displacement vectors:
;;      A = (3.00i − 4.00j + 4.00k)m
;;      B = (2.00i + 3.00j − 7.00k)m
;; Find the displacements and their magnitudes for:
;;      a) C = A + B
;;      b) D = 2A - B
;; Ans: a) C = (5.0i − 1.0j − 3.0k)m
;;         C = 5.92m
;;      b) D = (4.0i − 11.0j + 15.0k)m
;;         D = 19.03m

;; Definitions
(def vec-A [3 -4 4])
(def vec-B [2 3 -7])

;; Solution: a)
(vector/sum vec-A vec-B)              ;; => [5 -1 -3]
(vector/mag (vector/sum vec-A vec-B)) ;; => 5.92

;; Solution: b)
(vector/diff (vector/scale 2 vec-A) vec-B)              ;; => [4 -11 15]
(vector/mag (vector/diff (vector/scale 2 vec-A) vec-B)) ;; => 19.03


;; Ch.3 Motion with Constant Acceleration:
;; Problem 45:
;; A particle moves in a straight line with an initial
;; velocity of 30 m/s and constant acceleration 30 m/s2.
;;      a) What is its displacement at t = 5 s?
;;      b) What is its velocity at this same time?
;; Ans: a) 525m
;;      b) vf = 180 m/s

;; Definition
(def p45-known {:a 30 :t 5 :vi 30 :xi 0})

;; Solution: a)
(motion/linear 'finalPosition p45-known) ;; => {:xf 525.0}

;; Solution: b)
(motion/linear 'finalVelocity p45-known) ;; => {:vf 180.0}


;; Problem 51:
;; A bullet in a gun is accelerated from the firing chamber to the end of
;; the barrel at an average rate of 6.20E5 m/s2 for 8.10E−4 seconds.
;; Q: What is its muzzle velocity (that is, its final velocity)?
;; Ans: v = 502.20 m/s

;; Definitions
(def p51-known {:a 6.2E5 :t 8.10E-4 :vi 0})

;; Solution
(motion/linear 'finalVelocity p51-known) ;; => {:vf 502.2}


;; Problem 67:
;; Calculate the displacement and velocity at times of a, b, c, d, and e
;; for a rock thrown straight down with an initial velocity of 14.0 m/s
;; from the Verrazano Narrows Bridge in New York City.
;; The roadway of this bridge is 70.0 m above the water.
;;      a) t = 0.50s
;;      b) t = 1.00s
;;      c) t = 1.50s
;;      d) t = 2.00s
;;      e) t = 2.50s
;; Ans: a) xf = −8.23m, vf = −18.9m/s
;;      b) xf = -18.9m, vf = −23.8m/s
;;      c) xf = −32.0m, vf = −28.7m/s
;;      d) xf = −47.6m, vf = −33.6m/s
;;      e) xf = −65.6m, vf = −38.5m/s

;; Definitions
(def p67-known {:a -9.8 :vi -14 :xi 0})
(def p67-intervals [{:t 0.5} {:t 1.0} {:t 1.5} {:t 2.0} {:t 2.5}])
(def p67-known-intervals (map #(merge % p67-known) p67-intervals))

;; Solution
(map #(merge %1 %2)
     (map #(motion/linear 'finalPosition %) p67-known-intervals)
     (map #(motion/linear 'finalVelocity %) p67-known-intervals))

;; The above solution produces a collection of maps, each containing
;; a key value pair corrosponding with the solutions for a through e
;; => ({:xf -8.23,  :vf -18.9}  a)
;;     {:xf -18.9,  :vf -23.8}  b)
;;     {:xf -32.03, :vf -28.7}  c)
;;     {:xf -47.6,  :vf -33.6}  d)
;;     {:xf -65.63, :vf -38.5}) e)


;; Ch.4 Motion in Two and Three Dimensions
;; Problem 19:
;; The 18th hole at Pebble Beach Golf Course is a dogleg to the left of length 496.0 m.
;; The fairway off the tee is taken to be the x direction. A golfer hits his tee shot
;; a distance of 300.0 m, corresponding to a displacement delta-r1 = 300.0mi,
;; and hits his second shot 189.0 m with a displacement  delta-r2 = 172.0mi + 80.3mj.
;; Q: What is the final displacement of the golf ball from where it started?
;; Ans: final displacement = 472.0mi + 80.3mj

;; Definitions
(def p19-r1 [300 0])
(def p19-r2 [172.0 80.3])

;; Solution
(vector/sum p19-r1 p19-r2) ;; => [472.0 80.3]


;; Problem 33:
;; A bullet is shot horizontally from shoulder height (1.5 m) with an initial speed 200 m/s.
;;      a) How much time elapses before the bullet hits the ground?
;;      b) How far does the bullet travel horizontally?
;; Ans: a) t = 0.55s
;;      b) x = 110m

;; Definitions
(def p33-x-known {:a 0 :vi 200 :xi 0})
(def p33-y-known {:a -9.8 :vi 0 :xi 0 :xf -1.5})

;; Solution for y
(def p33-y-vf (motion/linear 'finalVelocity p33-y-known))         ;; => {:vf 5.42}
(def p33-y-update (merge p33-y-vf p33-y-known))                   ;; => {:vf 5.42, :a -9.8, :vi 0, :xi 0, :xf -1.5}
(def p33-time {:t (flat-val (motion/linear 'time p33-y-update))}) ;; => {:t 0.55}

;; Solution for x
(def p33-x-update (merge p33-time p33-x-known)) ;; => {:t 0.55, :a 0, :vi 200, :xi 0}
(motion/linear 'finalPosition p33-x-update) ;; => {:xf 110.0}


;; Problem 61:
;; A particle travels in a circle of radius 10 m at a constant speed of 20 m/s.
;; Q: What is the magnitude of the acceleration?
;; Ans: radAccel = 40m/s2

;; Definitions
(def p61-known {:tanVel 20 :rad 10})

;; Solution
(motion/circular 'radialAcceleration p61-known) ;; => {:radAccel 40.0}


;; Problem 63:
;; A fairground ride spins its occupants inside a flying saucer-shaped container.
;; If the horizontal circular path the riders follow has an 8.00-m radius,
;; Q: at how many revolutions per minute are the riders subjected to a centripetal
;;    acceleration equal to that of gravity?
;; Ans: 10.6 rev/min

;; Definitions
(def p63-known {:radAccel 9.8 :rad 8})

;; Solution
(def p63-tanVel (motion/circular 'tangentialVelocity p63-known)) ;; => {:tanVel 8.85}
(def p63-known-update (merge p63-tanVel p63-known))              ;; => {:tanVel 8.85, :radAccel 9.8, :rad 8}
(def p63-period (motion/circular 'period p63-known-update))      ;; => {:per 5.68}
{:freq (round (convert/toMin                                     ;; => {:freq 10.56}
               (:freq (motion/circular 'frequency p63-period))))}


;; Ch. 5 Newton's Laws of Motion
;; Problem 19:
;; Two ropes are attached to a tree, and forces of F1 = 2.0i + 4.0jN
;; and F2 = 3.0i + 6.0jN are applied. The forces are coplanar.
;;      a) What is the resultant (net force) of these two force vectors?
;;      b) What is the magnitude and direction of this net force?
;; Ans: a) Fnet = 5.0i + 10.0jN
;;      b) Magnitude = 11N
;;         Direction = 63 degrees

;; Definitions
(def p19-F1 [2.0 4.0])
(def p19-F2 [3.0 6.0])

;; Solutions
(def p19-resultant (zipmap [:x-comp :y-comp]            ;; => {:x-comp 5.0, :y-comp 10.0}
                           (vector/sum p19-F1 p19-F2)))
(def p19-resolved (vector/resolveVector p19-resultant)) ;; => {:deg 63.43, :mag 11.18, :x-comp 5.0, :y-comp 10.0}
(def p19-magnitude (:mag p19-resolved))                 ;; => 11.18
(def p19-direction (:deg p19-resolved))                 ;; => 63.43


;; Problem 23:
;; While sliding a couch across a floor, Andrea and Jennifer exert forces FA and FJ on
;; the couch. Andrea’s force is due north with a magnitude of 130.0N and Jennifer’s force
;; is 32° east of north with a magnitude of 180.0N.
;;      a) Find the net force in component form. (
;;      b) Find the magnitude and direction of the net force.
;;      c) If Andrea and Jennifer’s housemates, David and Stephanie, disagree with the move
;;         and want to prevent its relocation, with what combined force F-DS should they push
;;         so that the couch does not move?
;; Ans: a) Fnet = 95.0i + 283jN
;;      b) 299N at 71° north of east
;;      c) F-DS = −(95.0i + 283j)N

;; Definitions
(def p23-FA {:mag 130 :theta 90})
(def p23-FJ {:mag 180 :theta (- 90 32)})

;; Solutions
(def p23-FA-resolved (vector/resolveVector p23-FA))
(def p23-FJ-resolved (vector/resolveVector p23-FJ))

(def p23-FA-components [(:x-comp p23-FA-resolved) (:y-comp p23-FA-resolved)]) ;; => [0.0 130.0]
(def p23-FJ-components [(:x-comp p23-FJ-resolved) (:y-comp p23-FJ-resolved)]) ;; => [95.39 152.65]
(def p23-sumForces (vector/sum p23-FA-components p23-FJ-components))          ;; a) => [95.39 282.65]

(def p23-netForce-resolved (vector/resolveVector
                            (zipmap [:x-comp :y-comp] p23-sumForces)))        ;; => {:deg 71.35, :mag 298.31, :x-comp 95.39, :y-comp 282.65}
(def p23-netForce-magnitude (:mag p23-netForce-resolved))                     ;; b) => 298.31
(def p23-netForce-direction (:deg p23-netForce-resolved))                     ;; b) => 71.35

(def p23-houseMate-Maneuver (vector/diff p23-sumForces p23-sumForces))        ;; c) => [0.0 0.0]
                                                                              ;;    => -p23-sumForces


;; Problem 29:
;; The rocket sled shown below accelerates opposite to the motion at a rate of 196m/s2.
;; Q: What force is necessary to produce this acceleration opposite to the motion?
;;    Assume that the rockets are off. The mass of the system is 2.10E3 kg.
;; Ans: Fnet = 4.12E5N

;; Definitions
(def p29-known {:a 196 :m 2.10E3})

;; Solution
(def p29-force (force/newton 'force p29-known)) ;; => {:f 411600.0}


;; Problem 33:
;; A powerful motorcycle can produce an acceleration of 3.50m/s2 while traveling at 90.0 km/h.
;; At that speed, the forces resisting motion, including friction and air resistance, total 400.0N.
;; Q: What is the magnitude of the force that motorcycle exerts backward on the ground
;;    to produce its acceleration if the mass of the motorcycle with rider is 245 kg?
;; Ans: Fnet = 1.26E3N

;; Definitions
(def p33-motorcycle-known {:a 3.5 :m 245})
(def p33-neg-force 400)

;; Solution
(def p33-pos-force (force/newton 'force p33-motorcycle-known)) ;; => {:f 857.5}
(def p33-pos-netForce (+ (:f p33-pos-force) p33-neg-force))    ;; => 1257.5


;; Problem 41:
;; The weight of an astronaut plus his space suit on the Moon is only 250 N.
;;      a) How much does the suited astronaut weigh on Earth?
;;      b) What is the mass on the Moon? On Earth?
;; Ans: a) Weight on Earth = 150kg
;;      b) Earth = 1.5E3N
;;         Moon = 1.5E3N

;; Definitions
(def p41-known-moon {:a 1.625 :N 250})

;; Solutions
(def p41-mass-earth (force/newton 'mass p41-known-moon)) ;; a) => {:m 153.85}
(def p41-weight-earth (force/weight 'newton {:m 153.85 :g 9.8})) ;; b) => {:N 1507.73}


;; Problem 51:
;; a) What net external force is exerted on a 1100.0-kg artillery shell fired
;;    from a battleship if the shell is accelerated at 2.40E4m/s2?
;; b) What is the magnitude of the force exerted on the ship by the artillery shell?
;; Ans: a) Fnet = 2.64E7N
;;      b) Same as a, but in the opposite direction.

;; Definitions
(def p51_2-known {:a 2.4E4 :m 1100})

;; Solution
(def p51_2-force (force/newton 'force p51_2-known)) ;; a) => {:N 2.64E7}


;; Chapter 6: Applications of Newton's Laws
;; Problem 25:
;; A 30.0-kg girl in a swing is pushed to one side and held at rest by a horizontal
;; force F so that the swing ropes are 30.0° with respect to the vertical.
;;      a) Calculate the tension in each of the two ropes supporting
;;         the swing under these conditions.
;;      b) Calculate the magnitude of F.
;; Ans: a) 170N
;;      b) 170N

;; Definitions
(def p25-known {:a 9.8 :m 30})

;; Solution
(def p25-force (force/newton 'force p25-known))
(def p25-tension (round (/ (:N p25-force) (* 2 (cos 30))))) ;; a) => 169.74


;; Problem 61:
;; Consider the 52.0-kg mountain climber shown below.
;; a) Find the tension in the rope and the force that the mountain climber
;;    must exert with her feet on the vertical rock face to remain stationary.
;; b) What is the minimum coefficient of friction between her shoes and the cliff?
;; Ans: a) 272 N, 512 N
;;      b) 0.268

;; Definitions
(def p61_2-known {:g 9.8 :m 52})
(def theta_1 31)
(def theta_2 15)

;; Solutions
;; T = (/ mg (+ costheta (* sintheta tantheta2)))
;; F = (/ (* T sintheta) costheta2)
;; COFs = tantheta2
(def p61_2-weight (force/weight 'newton p61_2-known)) ;; => {:N 509.6}
(def p61_2-theta-total (+ (cos theta_1)               ;; => 0.9951713369455414
                          (* (sin theta_1)
                             (tan theta_2))))
(def p61_2-tension (round (/ (:N p61_2-weight) p61_2-theta-total)))                 ;; a) => 512.07
(def p61_2-climber-force (round (/ (* p61_2-tension (sin theta_1)) (cos theta_2)))) ;; a) => 273.04
(def p61_2-min-friction-coeff (round (tan theta_2)))                                ;; b) => 0.27


;; Chapter 7: Work and Kinetic Energy
;; Problem 29:
;; A shopper pushes a grocery cart 20.0m at constant speed on level ground,
;; against a 35.0N frictional force. He pushes in a direction 25.0° below the horizontal.
;;      a) What is the work done on the cart by friction?
;;      b) What is the work done on the cart by the gravitational force?
;;      c) What is the work done on the cart by the shopper?
;;      d) Find the force the shopper exerts, using energy considerations.
;;      e) What is the total work done on the cart?
;; Ans: a) –700 J
;;      b) 0 J
;;      c) 700 J
;;      d) 38.6 N
;;      e) 0 J

;; Solutions
(def p29_2-friction-work (* -35 20))              ;; a) => -700
(def p29_2-gravity-work)                          ;; b) => 0 No work is done along the vertical
(def p29_2-shopper-work (* 35 20))                ;; c) => 700
(def p29_2-shopper-force (round (/ 35 (cos 25)))) ;; d) => 38.62
(def p29_2-cart-work)                             ;; e) => 0 No work done by cart


;; Problem 45:
;;      a) How fast must a 3000-kg elephant move to have the same kinetic energy
;;         as a 65.0-kg sprinter running at 10.0 m/s?
;; Ans: a) 1.47 m/s

;; Definitions
(def p29_2-elephant-mass 3000)
(def p29_2-sprint-mass 65)
(def p29_2-sprinter-velocity 10)

;; Solution
;; (* 1/2 massElephant (sqr velocityElephant)) = (* 1/2 massSprinter (sqr velocitySprinter))
;; elephantVelocity = (sqrt (* (sqr velocitySprinter) (/ massSprinter massElephant)))
(def p45_2-elephant-velocity (round (sqrt (* (sqr p29_2-sprinter-velocity) ;; a) => 1.47
                                             (/ p29_2-sprint-mass
                                                p29_2-elephant-mass)))))


;; Chapter 8: Potential Energy and Conservation of Energy
;; Problem 21:
;; A camera weighing 10N falls from a small drone hovering 20m overhead and enters free fall.
;; What is the gravitational potential energy change of the camera from the drone to the
;; ground if you take a reference point of
;;      a) the ground being zero gravitational potential energy?
;;      b) The drone being zero gravitational potential energy?
;;      c) What is the gravitational potential energy of the camera before it falls from the drone?
;;      d) What is the gravitational potential energy of the camera after the camera lands on the
;;         ground if the reference point of zero gravitational potential energy is
;;         taken to be a second person looking out of a building 30m from the ground?
;; Ans: a) −200J
;;      b) −200J
;;      c) −100J
;;      d) −300J

;; Definitions
(def p21-weight {:N 10})
(def p21-ub (map #(merge {:ub %}) [0 0 20 0]))
(def p21-ua (map #(merge {:ua %}) [20 20 30 30]))
(def p21-ua-ub-weight (map #(merge % p21-weight) (map #(merge %1 %2) p21-ua p21-ub)))
;; Above line => ({:ua 20, :ub 0, :N 10} {:ua 20, :ub 0, :N 10} {:ua 30, :ub 20, :N 10} {:ua 30, :ub 0, :N 10})
(def p21-grav-potent-energy (map energy/potentialDifference p21-ua-ub-weight)) ;; a-d => (-200 -200 -100 -300)
