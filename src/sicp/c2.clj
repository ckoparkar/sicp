(ns sicp.c2
  (:require [sicp.c1 :refer :all]
            [clojure.repl :refer :all])
  (:gen-class))

;; Ex 2.2

(defrecord Point [x y])

(defn make-point [x y]
  (->Point x y))

(defn x-point [p]
  (:x p))

(defn y-point [p]
  (:y p))

(defrecord Segment [start end])

(defn make-segment [start end]
  (->Segment start end))

(defn start-segment [s]
  (:start s))

(defn end-segment [s]
  (:end s))

(defn midpoint-segment [s]
  (let [start (start-segment s)
        end (end-segment s)
        sx (x-point start)
        sy (y-point start)
        ex (x-point end)
        ey (y-point end)]
    (make-point (average sx ex)
                (average sy ey))))
