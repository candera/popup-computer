(ns user
  (:require [incanter.core :as incanter]
            [incanter.charts :as charts]
            [org.craigandera.popup-computer :as pc]))

(defn plot
  [data]
  (doto (charts/scatter-plot)
    (charts/add-lines [0
                       (-> data :release pc/x)
                       (-> data :roll-out pc/x)
                       (-> data :pull-down pc/x)
                       (-> data :climb pc/x)
                       (-> data :pull-up pc/x)
                       (-> data :pup pc/x)
                       (-> data :vrp pc/x)]
                      [0
                       (-> data :release pc/y)
                       (-> data :roll-out pc/y)
                       (-> data :pull-down pc/y)
                       (-> data :climb pc/y)
                       (-> data :pull-up pc/y)
                       (-> data :pup pc/y)
                       (-> data :vrp pc/y)]
                      :points true
                      :auto-sort false)
    (incanter/view)))

