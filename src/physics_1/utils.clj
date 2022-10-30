(ns physics-1.utils)


;; START UTIL FUNCTIONS
(defn round [arg]
  (Double/parseDouble (format "%.2f" (double arg))))

(defn abs [arg]
  (Math/abs arg))

(def flat-val (comp (partial apply #(round (abs %))) vals))

;; END UTIL FUNCTIONS


;; START TRIG FUNCTIONS
(def pi Math/PI)

(defn rad [theta]
  (Math/toRadians theta))

(defn deg [theta]
  (Math/toDegrees theta))

(defn cos [theta]
  (if (seq? theta)
    ;; SEQ? TRUE
    (if (ratio? (first theta))           ;; RADIANS?
      (Math/cos (* pi (first theta)))    ;; -> TRIG FUNC
      (Math/cos (rad (first theta))))    ;; CAST -> TRIG FUNC

    ;; SEQ? FALSE
    (if (ratio? theta)                   ;; RADIANS?
      (Math/cos (* pi theta))            ;; -> TRIG FUNC
      (Math/cos (rad theta)))))          ;; CAST -> TRIG FUNC

(defn sin [theta]
  (if (seq? theta)
    ;; SEQ? TRUE
    (if (ratio? (first theta))           ;; RADIANS?
      (Math/sin (* pi (first theta)))    ;; -> TRIG FUNC
      (Math/sin (rad (first theta))))    ;; CAST -> TRIG FUNC

    ;; SEQ? FALSE
    (if (ratio? theta)                   ;; RADIANS?
      (Math/sin (* pi theta))            ;; -> TRIG FUNC
      (Math/sin (rad theta)))))          ;; CAST -> TRIG FUNC

(defn tan [theta]
  (if (seq? theta)
    ;; SEQ? TRUE
    (if (ratio? (first theta))           ;; RADIANS?
      (Math/tan (* pi (first theta)))    ;; -> TRIG FUNC
      (Math/tan (rad (first theta))))    ;; CAST -> TRIG FUNC

    ;; SEQ? FALSE
    (if (ratio? theta)                   ;; RADIANS?
      (Math/tan (* pi theta))            ;; -> TRIG FUNC
      (Math/tan (rad theta)))))          ;; CAST -> TRIG FUNC

;; END TRIG FUNCTIONS


;; START SECOND ORDER OPERATION FUNCTIONS
(defn sqrt [x]
  (Math/sqrt x))

(defn sqr [x]
  (Math/pow x 2))

(defn sqr+ [x & y]
  (reduce + (map sqr (cons x y))))

(defn sqr* [x & y]
  (reduce * (map sqr (cons x y))))

;; END SECOND ORDER OPERATION FUNCTIONS