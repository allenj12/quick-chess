(ns quick-chess.core
  (:require [reagent.core :as r]
            [clojure.set :as s]))

(enable-console-print!)

(println (str "Reloaded: " (.toLocaleTimeString (js/Date.))))

(defonce init-pieces
  [[\u265C \u265E \u265D \u265B \u265A \u265D \u265E \u265C]
   [\u265F \u265F \u265F \u265F \u265F \u265F \u265F \u265F]
   ["" "" "" "" "" "" "" ""]
   ["" "" "" "" "" "" "" ""]
   ["" "" "" "" "" "" "" ""]
   ["" "" "" "" "" "" "" ""]
   [\u2659 \u2659 \u2659 \u2659 \u2659 \u2659 \u2659 \u2659]
   [\u2656 \u2658 \u2657 \u2655 \u2654 \u2657 \u2658 \u2656]])

(defonce init-colors
  (vec
   (take 8 (cycle [(vec (take 8 (cycle '("#a3854b" "#d3d2cf"))))
                   (vec (take 8 (cycle '("#d3d2cf" "#a3854b"))))]))))

(defonce highlight-color "#538ab5")

(defn string-mapping
  "given a list of rows and cols and a string list
  creates the string mapping"
  [rows cols strings]
  (zipmap
   strings
   (for [y rows
         x cols]
     [y x])))

(def init-state
  (let [init-white-bindings
        (string-mapping
         (range 6 8)
         (range 8)
         (map char (range 97 113)))
        init-black-bindings
        (string-mapping
         (reverse (range 2))
         (range 8)
         (map char (range 97 113)))]
      {:width 500
       :height 500
       :board
       (mapv
        (fn [row-pieces row-colors]
          (mapv
           (fn [piece color]
             {:piece piece :color color})
           row-pieces row-colors))
        init-pieces init-colors)     
       :string-map
       {:white {:str->pos
                init-white-bindings
                :pos->str
                (s/map-invert init-white-bindings)}
        :black {:str->pos
                init-black-bindings
                :pos->str
                (s/map-invert init-black-bindings)}}
       :turn :white}))

(defonce black #{\u265F \u265C \u265E \u265D \u265B \u265A})

(defonce white #{\u2659 \u2656 \u2658 \u2657 \u2655 \u2654})

(defonce app-state (r/atom init-state))
#_(swap! app-state assoc :height 500)
#_(swap! app-state assoc-in [:board 7 4 :color] "#4286f4")
#_(reset! app-state init-state)
#_(swap! app-state assoc :turn :black)

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
   :height (/ (:width data) (count (first (:board data))) 2)
   :font-size (*
               (min (:height data) (:width data))
               0.06)})

(defn selection-char-style
  [data]
  {:text-align "center"
   :height (/ (:width data) (count (first (:board data))) 3)
   :font-size (*
               (min (:height data) (:width data))
               0.03)})

(defn slider-view
  "adjusts the size of the board"
  [state]
  [:input {:type "range" :value (:width state) :min 250 :max 700
           :style {:width "200px"}
           :on-change (fn [e]
                        (let [val (js/parseInt (.-target.value e))]
                          (swap! app-state assoc :width val :height val)))}])

(defn deselect-piece
  "changes the board it its normal no-piece selected
  state were the board colors are default"
  [state old]
  (let [[row col] (get-in
                   state
                   [:string-map (:turn state) :str->pos old])
        color (get-in init-colors [row col])]
    (swap! app-state assoc-in [:board row col :color] color)))

(defn deselected?
  "returns true if the input signifies the user deselected a piece"
  [state old new] 
  (and
   (get-in state
           [:string-map (:turn state) :str->pos old])
   (> (count old) (count new))))

(defn out-of-board?
  "returns all possible locations for a given piece
  can assume piece is white or black depending on the
  turn"
  [[row col]]
  (or
   (< row 0)
   (>= row 8)
   (< col 0)
   (>= col 8)))

(defn possible-placements
  [[row col :as pos] turn]
  (if (= :black turn)
    (filter #(not (out-of-board? %)) [[(+ row 2) col]])
    (filter #(not (out-of-board? %)) [[(- row 2) col]])))

(defn highlight-piece
  [[row col] color]
  (fn [state]
    (assoc-in state [:board row col :color] color)))

(defn highlight-possible-positions
  [positions color]
  (fn [state]
    (let [pos->str
          (map #(vector %1 %2)
               positions
               (map char
                    (range 97 123)))]
      (reduce
       (fn [hmap [[row col] char]]
         (assoc-in hmap [:board row col]
                   {:piece char :color color}))
       state
       pos->str))))

(defn handle-selection
  [pos turn]
  (comp
   (highlight-possible-positions
    (possible-placements pos turn) highlight-color)
   (highlight-piece pos highlight-color)))

(defn select-piece
  [state text-state [row col :as pos]]
  (swap! text-state
         assoc :move-to (vec (possible-placements pos (:turn state))))
  (swap! app-state (handle-selection pos (:turn state))))

(defn handle-text-input
  "handles the text input given an event"
  [state text-state]
  (fn [e]
    (let [str (-> e .-target .-value)
          old (:text @text-state)
          piece (get-in
                 state
                 [:string-map (:turn state) :str->pos str])]
      (swap! text-state assoc :text str)
      (cond
        piece (select-piece state text-state piece)
        (deselected? state old str) (deselect-piece state old)))))

(defn input-view
  "input of commands through strings"
  []
  (let [text-state (r/atom {:text ""
                            :move-to []})]
    (fn [state]
      [:input {:type "text"
               :value (:text @text-state)
               :on-change (handle-text-input state text-state)}])))

(defn cell-view
  [state row-idx]
  (fn [col-idx elem]
    ^{:key col-idx}
    [:td {:style (td-style state (:color elem))}
     ;;merge implementation isnt that great create our own
     ;;or reorganize init-state.
     [:div {:style (piece-style state)} (:piece elem)]
     (let [binding (get (merge
                         (get-in state [:string-map :white :pos->str])
                         (get-in state [:string-map :black :pos->str]))
                        [row-idx col-idx] "")]
       [:div {:style (selection-char-style state)} binding])]))

(defn row-view
  [state]
  (fn [row-idx row]
    ^{:key row-idx}
    [:tr {:style (tr-style state)}
     (map-indexed (cell-view state row-idx) row)]))

(defn board-view
  "the board"
  [state]
  [:table {:style (board-style state)}
   [:tbody
    (map-indexed (row-view state) (:board state))]])

(defn app-view
  "main component of our app"
  []
  [:div
   [:div "Size:"]
   [slider-view @app-state]
   [board-view @app-state]
   [input-view @app-state]])

;; -------------------------
;; Initialize app

(defn mount-root []
  (r/render [app-view] (.getElementById js/document "app")))

(defn init! []
  (mount-root))
