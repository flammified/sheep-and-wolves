(ns sheep-and-wolves.core
  (:require [play-cljs.core :as p]
            [goog.events :as events]
            [sheep-and-wolves.game :as game]
            [sheep-and-wolves.display :as display])
  (:require-macros [sheep-and-wolves.music :refer [build-for-cljs]]))

(defonce parent (.getElementById js/document "game"))
(defonce canvas (p/create-game 401 401 {:debug? false :parent (.getElementById js/document "game")}))

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

(events/listen js/window "resize"
  (fn [event]
    (p/set-size canvas js/window.innerWidth js/window.innerHeight)))

(doto canvas
  (p/start)
  (p/set-screen main-screen)
  (p/listen "mousedown"
    (fn [event]
      (let [bounds (.getBoundingClientRect parent)
            x (- (.-clientX event) (.-left bounds))
            y (- (.-clientY event) (.-top bounds))
            coordinate [(screen-to-square x) (screen-to-square y)]]
        (swap! game/state game/handle-square-click coordinate))))

  (p/listen "keypress"
    (fn [event]
      (if (= 32 (.-charCode event))
        (reset! game/state (game/new-game)))))

  (p/listen "mouseup"
    (fn [event]
      (let [bounds (.getBoundingClientRect parent)
            x (- (.-clientX event) (.-left bounds))
            y (- (.-clientY event) (.-top bounds))
            coordinate [(screen-to-square x) (screen-to-square y)]]
        (swap! game/state game/handle-release-click coordinate))))

  (p/listen "mousemove"
    (fn [event]
      (let [bounds (.getBoundingClientRect parent)
            x (- (.-clientX event) (.-left bounds))
            y (- (.-clientY event) (.-top bounds))]
        (swap! game/state assoc :mousex x :mousey y)))))


; uncomment to generate a song and play it!
