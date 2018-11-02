;; Stuff to fix:
;; possible moves -> object instead of [from to]
;; Remove sheep from possible moves
;; clear up check-moves
;; invert sheep and wolf

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
  (let [all-moves (type moves)]
    (reduce
      (fn [valid-moves delta]
        ; (println delta object-to-move)
        (let [applied-move (add-vec (:location object-to-move) delta)]
          ; (println applied-move)
          (if (can-move? state applied-move)
            (conj valid-moves applied-move)
            valid-moves)))
      []
      all-moves)))


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
      (= 0 (second (:location (:wolf state)))) :lost
      (wolf-behind-sheep? state) :lost
      (= 0 (count wolf-moves)) :won
      :else :ongoing)))


(defmulti apply-move (fn [type state move] type))

(defmethod apply-move :sheep [type state to sheep-to-move]
  ; (println "applymove" sheep-to-move state)
  (assoc-in state [:sheep (:index sheep-to-move) :location] to))

(defmethod apply-move :wolf [type state to]
  ; (println to)
  (assoc-in state [:wolf :location] to))

(defn id-of-location [state [sx sy]]
  (let [identical-vectors (filter
                            (fn [sheep] (vec-equals? [sx sy] (:location sheep)))
                            (:sheep state))]
    (case identical-vectors
      '() nil
      (first identical-vectors))))

(defn check-move [state coordinate]
  (let [dragging-sheep (get (:sheep state) (:dragging state))
        possible-places-for-sheep (possible-moves-for state dragging-sheep :sheep)]
    (case dragging-sheep
       nil [false state]
       (let [valid-move (equal-vector-in-list? possible-places-for-sheep coordinate)
             stopped-dragging-state (-> state
                                      (assoc-in ,,, [:sheep (:index dragging-sheep)] (assoc dragging-sheep :dragging false))
                                      (assoc ,,, :dragging nil))]
        (if valid-move
             [true (apply-move :sheep stopped-dragging-state coordinate dragging-sheep)]
             [false stopped-dragging-state])))))


(defn value-of-board [state]
  (let [initial-score (- 100 (* 10 (-> state (:wolf) (:location) (get 1))))]
    (+ initial-score (case (game-result state)
                       :won -100
                       :lost 100
                       :ongoing 0))))

(defn prefix-sequence [prefix sequence]
  (map
    #(vector prefix %)
    sequence))

(defn print-and-return [v]
  (println v)
  v)

(defn value-of-move [type state object to depth]
  (if (= depth 0)
    (value-of-board state)
    (let [applied-move (case type
                         :sheep (apply-move :sheep state to object)
                         :wolf (apply-move :wolf state to))]

      (case (game-result applied-move)
        :won 100
        :lost -100
        :ongoing (case type

                   :sheep (let [wolf-moves (possible-moves-for applied-move (:wolf applied-move) :wolf)
                                move-scores (map
                                              #(value-of-move :wolf applied-move (:wolf applied-move) % (dec depth))
                                              wolf-moves)
                                ; _ (println move-scores)
                                best-wolf-move (apply max move-scores)]
                           best-wolf-move)

                   :wolf (let [sheep-moves (mapcat
                                             #(prefix-sequence % (possible-moves-for applied-move % :sheep))
                                             (:sheep applied-move))
                               ; _2 (println sheep-moves)
                               move-scores (map
                                             (fn [[sheep-to-move move]] (value-of-move :sheep applied-move sheep-to-move move (dec depth)))
                                             sheep-moves)
                               ; _ (println "SSS" move-scores)
                               best-sheep-move (apply min move-scores)]
                            best-sheep-move))))))


(defn minimax [state]
  (let [wolf-moves (possible-moves-for state (:wolf state) :wolf)
        values-of-moves (map
                          (fn [move] (vector move (value-of-move :wolf state (:wolf state) move 6)))
                          wolf-moves)
        _3 (println values-of-moves)
        best-move (apply max-key #(get 1 %) values-of-moves)]
    best-move))

(defn handle-release-click [state coordinate]
  (let [[move-done after-move] (check-move state coordinate)
         result (game-result after-move)
         [best-move-for-wolf score] (minimax after-move)]
    (print "Minimax" best-move-for-wolf score)
    (case result
      :won (assoc after-move :result :won)
      :lost (assoc after-move :result :lost)
      (if move-done (apply-move :wolf after-move best-move-for-wolf) after-move))))


(defn handle-square-click [state coordinate]
  (case (:result state)
    :ongoing (let [clicked-sheep (id-of-location state coordinate)]
               (case clicked-sheep
                 nil state
                 (-> state
                   (assoc-in [:sheep (:index clicked-sheep)] (assoc clicked-sheep :dragging true))
                   (assoc :dragging (:index clicked-sheep)))))
    state))
