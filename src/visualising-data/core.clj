(ns visualising-data.core
  (:use quil.core)
  (:require [quilplay.dynamic :as dynamic]))

(defsketch example
  :title "Oh so many grey circles"
  :setup dynamic/setup
  :draw dynamic/draw
  :size [323 200])


