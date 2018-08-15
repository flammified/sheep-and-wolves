(ns sheep-and-wolves.game
  (:require [clojure.math.combinatorics :as combo]))

(defn new-game []
  {:sheep [{:id 0 :location [1 0] :dragging false}
           {:id 1 :location [3 0] :dragging false}
           {:id 2 :location [5 0] :dragging false}
           {:id 3 :location [7 0] :dragging false}]
   :wolf {:location [4 7]}})

(defonce state (atom {}))

(defn add-vec [[x1 y1] [x2 y2]]
  [(+ x1 x2) (+ y1 y2)])

(defn vec-equals? [[x1 y1] [x2 y2]]
  (and (= x1 x2) (= y1 y2)))


(def moves {:sheep [[-1 1] [1 1]] :wolf [[-1 -1] [1 -1] [-1 1] [1 1]]})

(defn valid-move? [state [rx ry]]
  (if (or (< rx 0) (> rx 7) (< ry 0) (> ry 7))
    false
    (not
      (or
        (some (fn [sheep] (vec-equals? [rx ry] (:location sheep))) (:sheep state))
        (vec-equals? [rx ry] (:location (:wolf state)))))))

(defn possible-moves-for [state position type]
  (let [all-moves (combo/cartesian-product [position] (type moves))
        applicated-moves (map
                            (fn [[from delta]]
                              [from (add-vec from delta)])
                            all-moves)
        filtered (filter
                   (fn
                     [[from to]]
                     (valid-move? state to))
                   applicated-moves)]
    filtered))

(defn all-possible-moves [state type]
  (case type
    :sheep (mapcat (fn [sheep] (possible-moves-for state (:location sheep) :sheep)) (:sheep state))
    :wolf (possible-moves-for state (:location (:wolf state)) :wolf)))

(println (all-possible-moves (new-game) :wolf))

(defn id-of-location [state [sx sy]]
  (let [identical-vectors (filter
                            (fn [index sheep] (vec-equals? [sx sy] (:location sheep)))
                            (:sheep state))]
    (case identical-vectors
      nil nil
      identical-vectors)))

(defn handle-square-click [state coordinate]
  (let [clicked-sheep (id-of-location state coordinate)]
    (println clicked-sheep)
    (assoc clicked-sheep :dragging true)))


(defn minimax [])
