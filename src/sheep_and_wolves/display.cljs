(ns sheep-and-wolves.display)

(def square_size 50)
(def circle_size (/ square_size 2))

(defn in?
  "true if coll contains elm"
  [coll elm]
  (some #(= elm %) coll))

(defn chessboard []
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
         [[:fill {:color "white"}] [:fill {:color "green"}]]
         indices)))

(defn sheep [sheep-information]
  (let [[x y] (:location sheep-information)]
    [[:image {:name "sheep-512.png", :x (* x square_size), :y (* y square_size), :width 48, :height 48}]]))

(defn wolf [wolf-information]
  (let [[x y] (:location wolf-information)]
    [[:image {:name "wolf-512.png", :x (* x square_size), :y (* y square_size), :width 48, :height 48}]]))

(defn square [[x y]]
  [[:fill {:color "blue"} [:rect {:x (* square_size x) :y (* square_size y) :width square_size :height square_size}]]])
