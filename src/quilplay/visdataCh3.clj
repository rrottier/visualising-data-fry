(ns quilplay.visdataCh3
  (:use quil.core)
  (:use clojure-csv.core)
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def mapImage (ref nil))
(def data (ref nil))

(defn normalise [x xmax xmin]
  (let [xmm (- xmax xmin)]
    (float (/ (- x xmin) xmm))))

(defn normalise2D [x xmax xmin]
  (float
   (if (>= x 0)
    (/ x xmax)
    (- 0 (/ x xmin)))))

(defn minmax [series]
  (let [xs (vals series)
        xmax (reduce max xs)
        xmin (reduce min xs)]
    [xmax xmin])
  )

(def small-key (comp keyword str/lower-case))

(defn read-data [file]
  (reduce (fn [m [nm v]] (assoc m (small-key nm) (read-string v)))
          {}
          (parse-csv (slurp file) :delimiter \tab)))

(minmax (read-data "random.tsv"))

(do
  (dosync (ref-set data (read-data "random.tsv")))
  ;(let [[mx mn] (minmax data)
  ;      v (normalise (data "AL") mx mn)]
  (map #(let [[a b] %] b) (deref data)))

(defn data-to-map [line]
  (let [[nm v] line]
  (hash-map nm (read-string v))))

(defn setup []
  (background 255)
  (dosync (ref-set mapImage (load-image "map.png")))
  (dosync (ref-set data (read-data "random.tsv")))
  (text-font (create-font "CenturyGothic" 12)))

(defn getdata [state normfunc]
  (let [[mx mn] (minmax (deref data))]
    (normfunc (data state) mx mn)
    ))

(defn plotpointwithsize [state x y]
  (let [v (getdata state normalise)
        r (+ 2 (* v 38))]
   (ellipse (read-string x) (read-string y) r r)))

(defn plotpointwithcolour [nm x y]
  (color-mode :hsb)
  (let [v (getdata nm normalise)
        c1 (color 41 111 52)
        c2 (color 97 226 240)
        col (lerp-color c1 c2 v)]
    (fill col)
  (ellipse (read-string x) (read-string y) 15 15)))

(defn plotpointwithcolourandsize [nm x y]
  (let [raw (data nm)
        v (getdata nm normalise2D)
        x (read-string x)
        y (read-string y)
        c1 (color 51 51 102)
        c2 (color 236 81 102)
        r (abs (+ 1.5 (* v 13.5)))]
    (if (>= v 0)
      (fill c1)
      (fill c2))
    (ellipse-mode :radius)
    (ellipse x y r r)
    (if (< (dist x y (mouse-x) (mouse-y)) (+ r 2))
      (do
        (fill 0)
        (text-align :center)
        (text (str raw " (" nm ")") x (- y r 4)))
      )))

(defn plotpointwithcolourandtranparency [nm x y]
  (let [[mx mn] (minmax data)
        v (normalise2D (data nm) mx mn)
        c1 (color 51 51 102)
        c2 (color 236 81 102)
        tr (abs (* v 255))]
    (if (>= v 0)
      (fill c1 tr)
      (fill c2 tr))
  (ellipse (read-string x) (read-string y) 15 15)))

(defn plot [file]
  (with-open [rdr (io/reader file)]
  (doseq [[nm x y] (parse-csv rdr :delimiter \tab)]
    (plotpointwithcolourandsize (small-key nm) x y))))

(defn draw []
  (smooth)
  (fill 192 0 0)
  (no-stroke)
  (background 255)
  (image @mapImage 0 0)
  (plot "locations.tsv"))

(defsketch simple
  :title "Simple sketch"
  :setup setup
  :draw draw
  :size [640 400]
  )
