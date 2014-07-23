(ns visualising-data.Ch2
  (:use quil.core))

(defn setup []
  (smooth)
  (frame-rate 100)
  (background 192 64 0)
  (stroke 10 7 80)
  (stroke-weight 4))

(defn draw []
  (line 150 25 (mouse-x) (mouse-y)))

(defn clear []
  (background 192 64 0))

(defn saver []
  (save-frame "test.png"))

(defn mypic []
  (let [img (load-image "test.png")]
  (image img 500 500)))

(defsketch simple
  :title "Simple sketch"
  :setup setup
  :draw draw
  :mouse-clicked mypic
;  :mouse-entered clear
;  :mouse-exited saver
  :size [400 400]
  :rendered :opengl
  )

