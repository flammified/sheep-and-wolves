(ns sheep-and-wolves.core
  (:require [play-cljs.core :as p]
            [goog.events :as events]
            [sheep-and-wolves.game :as game]
            [sheep-and-wolves.display :as display])
  (:require-macros [sheep-and-wolves.music :refer [build-for-cljs]]))

(defonce canvas (p/create-game 401 401))
(println canvas)
(def main-screen
  (reify p/Screen
    (on-show [this]
      (reset! game/state (game/new-game)))
    (on-hide [this])
    (on-render [this]
      (p/render canvas (display/render-game @game/state)))))

(defn screen-to-square [screen-loc]
  (Math/floor (/ screen-loc 50)))

(defn convert-to-relative [screenx screeny]
  (println))

(events/listen js/window "mousedown"
  (fn [event]
    (let [coordinate [(screen-to-square (.-clientX event)) (screen-to-square (.-clientY event))]]
      (swap! game/state game/handle-square-click coordinate))))

(events/listen js/window "keypress"
  (fn [event]
    (if (= 32 (.-charCode event))
      (reset! game/state (game/new-game)))))

(events/listen js/window "mouseup"
  (fn [event]
    (let [coordinate [(screen-to-square (.-clientX event)) (screen-to-square (.-clientY event))]]
       (swap! game/state game/handle-release-click coordinate))))


(events/listen js/window "mousemove"
  (fn [event]
    (swap! game/state assoc :mousex (.-clientX event) :mousey (.-clientY event))))

(events/listen js/window "resize"
  (fn [event]
    (p/set-size canvas js/window.innerWidth js/window.innerHeight)))

(doto canvas
  (p/start)
  (p/set-screen main-screen))

; uncomment to generate a song and play it!
