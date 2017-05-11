(ns quick-chess.core
  (:require [reagent.core :as r]))

(enable-console-print!)

(println (str "Reloaded: " (.toLocaleTimeString (js/Date.))))

(def init-pieces
  [[\u265C \u265E \u265D \u265B \u265A \u265D \u265E \u265C]
   [\u265F \u265F \u265F \u265F \u265F \u265F \u265F \u265F]
   ["" "" "" "" "" "" "" ""]
   ["" "" "" "" "" "" "" ""]
   ["" "" "" "" "" "" "" ""]
   ["" "" "" "" "" "" "" ""]
   [\u2659 \u2659 \u2659 \u2659 \u2659 \u2659 \u2659 \u2659]
   [\u2656 \u2658 \u2657 \u2655 \u2654 \u2657 \u2658 \u2656]])

(def init-colors
  (vec
   (take 8 (cycle [(vec (take 8 (cycle '("#a3854b" "#d3d2cf"))))
                   (vec (take 8 (cycle '("#d3d2cf" "#a3854b"))))]))))

(defonce init-state
  {:width 500
   :height 500
   :board
   (mapv
    (fn [row-pieces row-colors]
      (mapv
       (fn [piece color]
         {:piece piece :color color})
       row-pieces row-colors))
    init-pieces init-colors)})

(defonce app-state (r/atom init-state))
(swap! app-state assoc-in [:board 7 4 :color] "#4286f4")
#_(reset! app-state init-state)

(defn board-style
  [data]
  {:border "1px solid black"
   :width (:width data)
   :height (:height data)})

(defn tr-style
  [data]
  {:width (:width data)
   :height (/ (:height data) (count (:board data)))})

(defn td-style
  [data color]
  {:border "1px solid black"
   :width (/ (:width data) (count (first (:board data))))
   :height (/ (:height data) (count (:board data)))
   :background color})

(defn piece-style
  [data]
  {:text-align "center"
   :font-size (*
               (min (:height data) (:width data))
               0.07)})

(defn board-view
  "the board"
  [state]
  [:table {:style (board-style state)}
   (map-indexed
    (fn [row-idx row]
      ^{:key row-idx} [:tr {:style (tr-style state)}
                   (map-indexed
                    (fn [col-idx elem]
                      ^{:key col-idx} [:td {:style (td-style state (:color elem))}
                                       [:div {:style (piece-style state)} (:piece elem)]])
                    row)])
    (:board state))])

(defn app-view
  "main component of our app"
  []
  [:div
   [board-view @app-state]])

;; -------------------------
;; Initialize app

(defn mount-root []
  (r/render [app-view] (.getElementById js/document "app")))

(defn init! []
  (mount-root))
