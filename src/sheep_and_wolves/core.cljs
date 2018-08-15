(ns sheep-and-wolves.core
  (:require [play-cljs.core :as p]
            [goog.events :as events]
            [sheep-and-wolves.game :as game]
            [sheep-and-wolves.display :as display])
  (:require-macros [sheep-and-wolves.music :refer [build-for-cljs]]))

(defonce game (p/create-game 800 800))

(def main-screen
  (reify p/Screen
    (on-show [this]
      (reset! state (game/new-game)))
    (on-hide [this])
    (on-render [this]

      (p/render game (display/chessboard))
      (doall (map #(p/render game %) (map display/sheep (:sheep @state))))
      (p/render game (display/wolf (:wolf @state))))))

(defn screen-to-square [screen-loc]
  (Math/floor (/ screen-loc 50)))

(events/listen js/window "mousedown"
  (fn [event]
    (let [coordinate [(screen-to-square (.-clientX event)) (screen-to-square (.-clientY event))]
          clicked-square (game/handle-square-click @state coordinate)]
      ;
      (case clicked-square
        nil nil
        (let [_ (println clicked-square)
              index (:id clicked-square)
              (!swap state assoc-in [:sheep index] clicked-square)])))))
      ; (println @state))))



(events/listen js/window "mouseup"
  (fn [event]
    (println "up")))

(events/listen js/window "mousemove"
  (fn [event]))
    ; (swap! state assoc :text-x (.-clientX event) :text-y (.-clientY event))))

(events/listen js/window "resize"
  (fn [event]
    (p/set-size game js/window.innerWidth js/window.innerHeight)))

(doto game
  (p/start)
  (p/set-screen main-screen))

; uncomment to generate a song and play it!

;(defonce audio (js/document.createElement "audio"))
;(set! (.-src audio) (build-for-cljs))
;(.play audio)
