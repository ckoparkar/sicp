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

(defn segment-points [s]
  (let [start (start-segment s)
        end (end-segment s)
        sx (x-point start)
        sy (y-point start)
        ex (x-point end)
        ey (y-point end)]
    {:xs [sx ex] :ys [sy ey]}))

(defn midpoint-segment [s]
  (let [{xs :xs ys :ys} (segment-points s)]
    (make-point (average (first xs) (second xs))
                (average (first ys) (second ys)))))

(defn length-segment [s]
  (let [{xs :xs ys :ys} (segment-points s)]
    (sqrt (+ (square (abs (- (first xs) (second xs))))
          (square (abs (- (first ys) (second ys))))
          ))))

;; Ex 2.3

(defrecord Rectangle [top-left bottom-right])

(defn make-rectangle [top-left bottom-right]
  (->Rectangle top-left bottom-right))

(defn top-left [r]
  (:top-left r))

(defn bottom-right [r]
  (:bottom-right r))

(defn width-rectangle [p1 p2]
  (- (x-point p2) (x-point p1)))

(defn height-rectangle [p1 p2]
  (- (y-point p2) (y-point p1)))

(defn area-rectangle [r]
  (* (width-rectangle r)
     (height-rectangle r)))

(defn perimeter-rect [width height]
  (+ (* 2 width) (* 2 height)))

(defrecord Rect [side parallel-side])

(defn make-rect [side parallel-side]
  (->Rect side parallel-side))

(defn side [r]
  (:side r))

(defn parallel-side [r]
  (:parallel-side r))

(defn width-rect [r]
  (length-segment (side r)))

(defn length-rect [r]
  (length-segment
   (make-segment (-> r side start-segment)
                 (-> r parallel-side start-segment))))

(defn area-rect [r]
  (* (width-rect r)
     (length-rect r)))
