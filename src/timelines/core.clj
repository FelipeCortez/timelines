(ns timelines.core
  (:require [timelines.little-history :as d]
            [garden.core :refer [css]]
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

(def hinc (partial + 0.5))

(defn timeline [data]
  (let [initial-year 1945
        end-year     (t/int (t/year (t/now)))
        year-range   [initial-year end-year]

        pad 40
        w   1200
        h   360]
    (together
     [:svg {:viewBox (str "0 0 " (+ pad w) " 360")
            :width   (str (+ pad w) "px")
            :height  (str h "px")
            :xlmns   "http://www.w3.org/2000/svg"}]
     (let [strong (set (decades initial-year end-year))]
       (reduce (fn [coll year]
                 (let [x    (remap year-range [(hinc pad) (hinc w)] year)
                       snap (hinc (int x))]
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
     (loop [svg [], events data, year->events {}]
       (if (seq events)
         (let [{:keys [event year]} (first events)
               vertical-margin      30
               x                    (remap year-range [(hinc pad) (hinc w)] year)
               y                    (+ 150 (* vertical-margin (get year->events year 0)))]
               (recur (conj svg
                            [:circle {:fill "#DD0000" :cx x :cy y :r 4}]
                            [:text {:transform (format "translate(%s, %s) rotate(45)" x (+ 10 y))}
                             event])
                      (rest events)
                      (update year->events year (fnil inc 0))))
         svg)))))


(defn document []
  (str (:html5 doctype)
       (html [:html {:lang "en"}
              [:head
               [:style (css [:svg {:font-family   "Triplicate T4c"
                                   :font-size     "12px"
                                   :letter-spacing "-0.75px"}])]]
              [:body (timeline d/events)]])))

(spit (str (System/getenv "PWD") "/index.html") (document))
