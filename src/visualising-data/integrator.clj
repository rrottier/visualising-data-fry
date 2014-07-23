(ns visualising-data.integrator)


(def damping 0.5)
(def attraction 0.2)
(def mass 1)
(def target 0)
(def targeting false)

(defn update []
  (if targeting
    (let [force (+ force (* attraction (- target current)))
          accel (/ force mass)
          vel (* (+ vel accel) damping)
          current (+ current vel)
          force 0])))
