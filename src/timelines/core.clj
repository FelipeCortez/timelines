(ns timelines.core
  (:require [timelines.little-history :as d]
            [hiccup.core :refer [html]]
            [hiccup.page :refer [html5 doctype include-css include-js]]
            [tick.alpha.api :as t]))

(defn decades [first last]
  (loop [coll    []
         current first]
    (if (< current last)
      (recur (conj coll current)
             (+ current (- 10 (mod current 10))))
      (conj coll last))))

(defn remap [[from-1 to-1] [from-2 to-2] n]
  (let [range-1 (- to-1 from-1)
        range-2 (- to-2 from-2)
        percentage (/ (- n from-1) range-1)]
    (+ from-2 (* range-2 percentage))))

(defn together [& xs] (into [] (sequence cat xs)))

(defn timeline [data]
  (let [initial-year 1945
        end-year     (t/int (t/year (t/now)))
        year-range   [initial-year end-year]]
    (together
     [:svg {:viewBox "0 0 840 360"
            :width   "840px"
            :height  "360px"
            :xlmns   "http://www.w3.org/2000/svg"}]
     (let [strong (set (decades initial-year end-year))]
       (reduce (fn [coll year]
                 (let [x    (remap year-range [16.5 800.5] year)
                       snap (+ 0.5 (int x))]
                   (if (strong year)
                     (conj coll
                           [:line {:x1     snap :x2 snap
                                   :y1     0    :y2 300
                                   :stroke "gray"}]
                           [:text {:transform (format "translate(%s, 320) rotate(45)"
                                                      (int x))}
                            year])
                     (conj coll
                           [:line {:x1     snap :x2 snap
                                   :y1     0    :y2 300
                                   :stroke "lightgray"}]))))
               []
               (range initial-year (inc end-year))))
     (reduce (fn [coll {:keys [event year]}]
               (println "y" year)
               (let [x (remap year-range [16.5 800.5] year)]
                 (conj coll
                       [:circle {:fill "#DD0000" :cx x :cy 150 :r 5}]
                       [:text {:transform (format "translate(%s, 170) rotate(45)" x)}
                        event])))
             []
             data))))


(defn document []
  (str (:html5 doctype)
       (html [:html {:lang "en"}
              [:body (timeline d/events)]])))

(spit (str (System/getenv "PWD") "/index.html") (document))
