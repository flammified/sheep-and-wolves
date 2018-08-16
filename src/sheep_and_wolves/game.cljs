;; Stuff to fix:
;; dragging -> index in game
;; possible moves -> multimethod
;; possible moves -> object instead of [from to]

(ns sheep-and-wolves.game
  (:require [clojure.math.combinatorics :as combo]))

(defn new-game []
  {:sheep [{:index 0 :location [1 0] :dragging false}
           {:index 1 :location [3 0] :dragging false}
           {:index 2 :location [5 0] :dragging false}
           {:index 3 :location [7 0] :dragging false}]
   :wolf {:location [4 7]}
   :mousex 0
   :mousey 0
   :result :ongoing})

(defonce state (atom {}))

(defn add-vec [[x1 y1] [x2 y2]]
  [(+ x1 x2) (+ y1 y2)])

(defn vec-equals? [[x1 y1] [x2 y2]]
  (and (= x1 x2) (= y1 y2)))

(defn equal-vector-in-list? [vectors to-find]
  (some (fn [vector] (vec-equals? vector to-find)) vectors))

(defn sheep-on-location? [sheep location]
  (equal-vector-in-list? (map :location sheep) location))

(def moves {:sheep [[-1 1] [1 1]] :wolf [[-1 -1] [1 -1] [-1 1] [1 1]]})

(defn can-move? [state [rx ry]]
  (if (or (< rx 0) (> rx 7) (< ry 0) (> ry 7))
    false
    (not
      (or
        (sheep-on-location? (:sheep state) [rx ry])
        (vec-equals? [rx ry] (:location (:wolf state)))))))

(defn possible-moves-for [state object-to-move type]
  (println object-to-move)
  (let [all-moves (combo/cartesian-product [object-to-move] (type moves))
        applicated-moves (map
                            (fn [[from delta]]
                              [object-to-move (add-vec (:location object-to-move) delta)])
                            all-moves)
        filtered (filter
                   (fn
                     [[object to]]
                     (can-move? state to))
                   applicated-moves)]
    filtered))

(defn wolf-behind-sheep? [state]
  (let [[wolf-x wolf-y] (-> state :wolf :location)]
    (= 0 (count
           (filter (fn [sheep]
                     (let [[sheep-x sheep-y] (:location sheep)]
                       (> wolf-y sheep-y)))
             (:sheep state))))))

(defn game-result [state]
  (let [sheep-location (map :location (:sheep state))
        wolf-moves (possible-moves-for state (:wolf state) :wolf)]
    (cond
      (wolf-behind-sheep? state) :lost
      (= 0 (count wolf-moves)) :won
      :else :ongoing)))

(defn apply-move-sheep [state sheep-to-move to]
  (assoc-in state [:sheep (:index sheep-to-move) :location]  to))

(defn apply-move-wolf [state to]
  (assoc-in state [:wolf :location] to))

(defn find-dragging [state]
  (first (filter
           (fn [sheep] (= (:dragging sheep) true))
           (:sheep state))))

(defn id-of-location [state [sx sy]]
  (let [identical-vectors (filter
                            (fn [sheep] (vec-equals? [sx sy] (:location sheep)))
                            (:sheep state))]
    (case identical-vectors
      '() nil
      (first identical-vectors))))

(defn check-move [state coordinate]
  (let [dragging-sheep (get (:sheep state) (:dragging state))
        possible-places-for-sheep (map second (possible-moves-for state dragging-sheep :sheep))]
    (case dragging-sheep
       nil state
       (let [valid-move (equal-vector-in-list? possible-places-for-sheep coordinate)
             stopped-dragging-state (-> state
                                      (assoc-in ,,, [:sheep (:index dragging-sheep)] (assoc dragging-sheep :dragging false))
                                      (assoc ,,, :dragging nil))]
        (if valid-move
             (apply-move-sheep stopped-dragging-state dragging-sheep coordinate)
             stopped-dragging-state)))))

(defn handle-release-click [state coordinate]
  (let [after-move (check-move state coordinate)
        result (game-result after-move)]
    (assoc after-move :result result)))

(defn handle-square-click [state coordinate]
  (case (:result state)
    :ongoing (let [clicked-sheep (id-of-location state coordinate)]
               (case clicked-sheep
                 nil state
                 (-> state
                   (assoc-in ,,, [:sheep (:index clicked-sheep)] (assoc clicked-sheep :dragging true))
                   (assoc ,,, :dragging (:index clicked-sheep)))))
    state))


(def wolf-won-score 10)
(def wolf-lost-score -10)

(defn value-of-sheep-move [state [sheep to]]
  (let [applied-move (apply-move-sheep state sheep to)]
    (case game-result applied-move
      :won -10
      :lost 10
      :ongoing (let [wolf-moves (map #(possible-moves-for applied-move % :wolf) (:wolf applied-move))
                     move-scores (map #(value-of-wolf-move applied-move %) wolf-moves)]
                (reduce + move-scores)))))



(defn value-of-wolf-move [state move]
  (let [applied-move (apply-move-wolf state (second move))]
    (case game-result applied-move
      :won -10
      :lost 10
      :ongoing (let [sheep-moves (map #(possible-moves-for applied-move % :sheep) (:sheep applied-move))
                     move-scores (map #(value-of-sheep-score applied-move %) sheep-moves)]
                (reduce + move-scores)))))



(defn minimax [state]

  (let [wolf-moves (possible-moves-for state (:wolf state) :wolf)
        _ (println wolf-moves)
        values-of-moves (map (fn [move] ([move (value-of-wolf-move state move)])) wolf-moves)]))






(print (minimax (new-game)))
