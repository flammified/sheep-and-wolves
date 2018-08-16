(ns sheep-and-wolves.display
  (:require [sheep-and-wolves.game :as game]))

(def square_size 50)
(def circle_size (/ square_size 2))

(defn in?
  "true if coll contains elm"
  [coll elm]
  (some #(= elm %) coll))


(defn render-chessboard [color]
  (let [indices (for [y (range 0 8)
                      x (range 0 8)]
                     [x y])]
       (reduce
         (fn [rectangles coordinate]
           (let [[x y] coordinate
                 row-mod (mod x 2)
                 column-mod (mod y 2)
                 append-to (if (= row-mod column-mod) 0 1)
                 new-rectangle [:rect {:x (* square_size x) :y (* square_size y) :width square_size :height square_size}]]
               (assoc rectangles append-to (conj (get rectangles append-to) new-rectangle))))
         [[:fill {:color "white"}] [:fill {:color color}]]
         indices)))

(defn render-sheep [state sheep]
  (let [[x y] (:location sheep)
        dragging (:dragging sheep)]
    (if dragging
      [[:image {:name "wolf-512.png", :x (- (:mousex state) 24), :y (- (:mousey state) 24), :width 48, :height 48}]]
      [[:image {:name "wolf-512.png", :x (* x square_size), :y (* y square_size), :width 48, :height 48}]])))

(defn render-wolf [wolf]
  (let [[x y] (:location wolf)]
    [[:image {:name "sheep-512.png", :x (* x square_size), :y (* y square_size), :width 48, :height 48}]]))

(defn render-square [[x y] color]
  [[:fill {:color color} [:rect {:x (* square_size x) :y (* square_size y) :width square_size :height square_size}]]])

(defn render-game [state]
  (vector (vec (concat
                 (render-chessboard (case (:result state) :ongoing "#526F35" :won "#009999" :lost "#990000"))
                 (if (some? (:dragging state)) (vector (map #(render-square % "#279f27") (map second (game/possible-moves-for state (get (:sheep state) (:dragging state)) :sheep)))))
                 (vector (map #(render-sheep state %)(:sheep state)))
                 (render-wolf (:wolf state))))))
