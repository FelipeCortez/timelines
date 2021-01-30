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

(defn event->svg [year-range pad w h year->events {:keys [event year years]}]
  (if year
    (let [vertical-margin 32
          x               (remap year-range [(hinc pad) (hinc w)] year)
          y               (+ 8 (* vertical-margin (get year->events year 0)))]
      [[:circle {:fill "#DD0000" :cx x :cy y :r 4}]
       [:text {:transform (format "translate(%s, %s) rotate(45)" x (+ 10 y))}
        event]])
    (let [vertical-margin 32
          x1              (remap year-range [(hinc pad) (hinc w)] (:from years))
          x2              (remap year-range [(hinc pad) (hinc w)] (:to years))
          y               (+ 8 (* vertical-margin (get year->events year 0)))]
      [[:circle {:fill "#DD0000" :cx x1 :cy y :r 4}]
       [:circle {:fill "#DD0000" :cx x2 :cy y :r 4}]
       [:rect {:fill "#DD0000" :x x1 :y (- y 2) :width (- x2 x1) :height 4}]
       [:text {:transform (format "translate(%s, %s) rotate(45)" x1 (+ 10 y))}
        event]])))

(defn timeline [events]
  (let [initial-year (:year (first events))
        end-year     (:year (last events))
        year-range   [initial-year end-year]

        pad 40
        w   1200
        h   300]
    (together
     [:svg {:viewBox (str "0 0 " (+ pad w) " " h)
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
                                   :y1     0    :y2 (- h 60)
                                   :stroke "gray"}]
                           [:text {:transform   (format "translate(%s, %s)"
                                                        (int x)
                                                        (-  h 40))
                                   :text-anchor "middle"}
                            year])
                     (conj coll
                           [:line {:x1     snap :x2 snap
                                   :y1     0    :y2 (- h 60)
                                   :stroke "lightgray"}]))))
               []
               (range initial-year (inc end-year))))
     (loop [svg [], events events, year->events {}]
       (if (seq events)
         (let [{:keys [year years]} (first events)]
           (recur (apply conj svg (event->svg year-range pad w h year->events (first events)))
                  (rest events)
                  (if year
                    (update year->events year (fnil inc 0))
                    (reduce (fn [year->events year] (update year->events year (fnil inc 0)))
                            year->events
                            (range (:from years) (inc (:to years)))))))
         svg)))))


(defn document []
  (str (:html5 doctype)
       (html [:html {:lang "en"}
              [:head
               [:style (css [:body {:padding "42px 0"
                                    :margin "0"}
                             :svg {:font-family   "Triplicate T4c"
                                   :font-size     "12px"
                                   :letter-spacing "-0.75px"}])]]
              [:body (timeline d/events)]])))

(spit (str (System/getenv "PWD") "/index.html") (document))
