(ns quick-chess.core
  (:require [reagent.core :as r]
            [clojure.set :as s]))

(enable-console-print!)

(println (str "Reloaded: " (.toLocaleTimeString (js/Date.))))

;;-----------------------------------
;;--INIT STATE AND GLOBAL DEFINITIONS
(defonce highlight-color "#538ab5")

(defonce default-keybindings (map char (range 97 123)))

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

(defonce init-selection-text
  (let [keys (take 16 default-keybindings)
        default-mapping (map vec
                             (split-at
                              (/ (count keys) 2) keys))]
    (vec
     (concat
      (reverse
       default-mapping)
      (take 4 (cycle [(vec (take 8 (cycle '(""))))]))
      default-mapping))))

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
         default-keybindings)
        init-black-bindings
        (string-mapping
         (reverse (range 2))
         (range 8)
         default-keybindings)]
      {:width 500
       :height 500
       :board
       (mapv
        (fn [row-pieces row-colors row-text]
          (mapv
           (fn [piece color text]
             {:piece piece
              :color color
              :text text})
           row-pieces row-colors row-text))
        init-pieces
        init-colors
        init-selection-text)     
       :string-map
       {:white init-white-bindings
        :black init-black-bindings}
       :turn :white}))

(defonce black #{\u265F \u265C \u265E \u265D \u265B \u265A})

(defonce white #{\u2659 \u2656 \u2658 \u2657 \u2655 \u2654})

(defonce app-state (r/atom init-state))
#_(swap! app-state assoc :height 500)
#_(swap! app-state assoc-in [:board 7 4 :color] "#4286f4")
#_(reset! app-state init-state)
#_(swap! app-state assoc :turn :black)

;;-------------
;;--DYNAMIC CSS
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

;;--------------------------------------------------------
;;--REAGENT/REACT COMPONENTS AND HELPER FUNCTIONS TO THOSE
(defn slider-view
  "adjusts the size of the board"
  [state]
  [:input {:type "range" :value (:width state) :min 250 :max 700
           :style {:width "200px"}
           :on-change (fn [e]
                        (let [val (js/parseInt (.-target.value e))]
                          (swap! app-state assoc :width val :height val)))}])

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

(defn change-color
  [[row col] color]
  (fn [state]
    (assoc-in state [:board row col :color] color)))

(defn change-positions-to-color
  [positions color]
  (fn [state]
    (let [pos->str
          (map #(vector %1 %2)
               positions
               default-keybindings)]
      (reduce
       (fn [hmap [[row col] char]]
         (update-in hmap [:board row col] assoc :color color :text char))
       state
       pos->str))))

(defn positions-to-default-color
  [positions]
  (fn [state]
    (reduce
     (fn [hmap [row col]]
       (let [color (get-in init-colors [row col])]
         (update-in hmap [:board row col] assoc :color color :text "")))
     state
     positions)))

(defn handle-deselection
  [[row col :as pos] positions]
  (comp
   (change-color pos (get-in init-colors [row col]))
   (positions-to-default-color positions)))

;;can almost certainly be abstracted more
;;so highlighting and unhighling can use
;;majority of the same functions
(defn handle-selection
  [pos turn]
  (comp
   (change-positions-to-color
    (possible-placements pos turn) highlight-color)
   (change-color pos highlight-color)))

(defn deselect-piece
  "changes the board it its normal no-piece selected
  state were the board colors are default"
  [state text-state old positions]
  (let [[row col :as pos]
        (get-in
         state
         [:string-map (:turn state) old])
        color (get-in init-colors [row col])
        pos-colors (map
                    (fn [[row col]]
                      (get-in
                       state
                       [:board row col :color]))
                    positions)]
    (swap! app-state (handle-deselection pos positions))
    (swap! text-state assoc :move-to {})))

(defn deselected?
  "returns true if the input signifies the user deselected a piece"
  [state old new] 
  (and
   (get-in state
           [:string-map (:turn state) old])
   (> (count old) (count new))))

(defn select-piece
  [state text-state [row col :as pos]]
  (swap! text-state
         assoc :move-to (zipmap
                         default-keybindings
                         (vec (possible-placements pos (:turn state)))))
  (swap! app-state (handle-selection pos (:turn state))))

(defn handle-text-input
  "handles the text input given an event"
  [state text-state]
  (fn [e]
    (let [str (-> e .-target .-value)
          old (:text @text-state)
          piece (get-in
                 state
                 [:string-map (:turn state) str])]
      (swap! text-state assoc :text str)
      (cond
        piece (select-piece state text-state piece)
        (deselected? state old str) (deselect-piece
                                     state
                                     text-state
                                     old
                                     (vals (:move-to @text-state)))))))

(defn input-view
  "input of commands through strings"
  []
  (let [text-state (r/atom {:text ""
                            :move-to {}})]
    (fn [state]
      [:input {:type "text"
               :value (:text @text-state)
               :on-change (handle-text-input state text-state)}])))

(defn cell-view
  [state row-idx]
  (fn [col-idx elem]
    ^{:key col-idx}
    [:td {:style (td-style state (:color elem))}
     [:div {:style (piece-style state)} (:piece elem)]
     [:div {:style (selection-char-style state)} (:text elem)]]))

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
