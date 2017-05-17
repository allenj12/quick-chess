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

(defonce black? #{\u265F \u265C \u265E \u265D \u265B \u265A})

(defonce white? #{\u2659 \u2656 \u2658 \u2657 \u2655 \u2654})

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

;;---------------------------------------------------
;;--NORMAL HELPER FUNCTIONS TO HELP COMPUTE THE STATE
(defn color-of-piece
  [board [row col]]
  (if (black? (get-in board [row col :piece]))
    :black
    :white))

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
         (update-in hmap
                    [:board row col]
                    assoc :color color :text "")))
     state
     positions)))

(defn move
  [[from-row from-col] [to-row to-col]]
  (fn [state]
    (let [from-map (get-in state
                           [:board from-row from-col])]
      ((comp
         #(update-in % [:board from-row from-col]
                     assoc
                     :text ""
                     :piece "")
         #(update-in % [:board to-row to-col]
                     assoc
                     :text (:text from-map)
                     :piece (:piece from-map)))
       state))))

(defn update-string-map
  [str pos]
  (fn [state]
    (assoc-in state [:string-map (:turn state) str] pos)))

(defn handle-deselection
  [pos positions]
  (comp
   (change-color pos (get-in init-colors pos))
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
  "changes the board to normal where no-piece is
  selected state were the board colors are default"
  [state text-state positions]
  (let [pos (get-in
             state
             [:string-map (:turn state) (:piece-str @text-state)])]
    (swap! app-state (handle-deselection pos positions))
    (swap! text-state assoc
           :move-to {}
           :piece-str "")))

(defn deselected?
  "returns true if the input signifies the user deselected a piece"
  [state {:keys [piece-str]} new] 
  (and
   (get-in state
           [:string-map (:turn state) piece-str])
   (> (count piece-str) (count new))))

(defn select-piece
  [{:keys [turn]} text-state pos str]
  (swap! text-state
         assoc
         :move-to (zipmap
                   default-keybindings
                   (vec
                    (possible-placements pos turn)))
         :piece-str str)
  (swap! app-state (handle-selection pos turn)))

(defn handle-move
  "deselects the pieces, and moves the piece"
  [state text-state positions]
  (let [{:keys [move-to piece-str text]} @text-state
        from-pos (get-in
                  state
                  [:string-map (:turn state) piece-str])
        to-pos (get move-to
                    (apply str
                           (drop
                            (count piece-str)
                            text)))]
    (swap! app-state
           (comp
            (update-string-map piece-str to-pos)
            (move from-pos to-pos)
            (handle-deselection from-pos positions)))
    (swap! text-state assoc
           :move-to {}
           :piece-str ""
           :text "")))

(defn move-command?
  [{:keys [piece-str move-to text]}]
  (and
   (not (empty? piece-str))
   (get move-to
           (apply str
                  (drop
                   (count piece-str)
                   text)))))

(defn handle-text-input
  "handles the text input given an event"
  [text-state]
  (fn [e]
    (let [str (-> e .-target .-value)
          state @app-state
          piece-pos (get-in
                     state
                     [:string-map (:turn state) str])
          positions (vals (:move-to @text-state))]
      (swap! text-state assoc :text str)
      (cond
        (move-command? @text-state) (handle-move
                                     state
                                     text-state
                                     positions)
        piece-pos (select-piece
                   state text-
                   state
                   piece-pos
                   str)
        (deselected? state @text-state str) (deselect-piece
                                             state
                                             text-state
                                             (vals (:move-to @text-state)))))))
;;-----------------------
;;--REAGENT/REACT COMPONENTS
(defn slider-view
  "adjusts the size of the board"
  []
  [:input {:type "range" :value (:width @app-state) :min 250 :max 700
           :style {:width "200px"}
           :on-change (fn [e]
                        (let [val (js/parseInt (.-target.value e))]
                          (swap! app-state assoc :width val :height val)))}])

(defn input-view
  "input of commands through strings"
  []
  (let [text-state (r/atom {:text ""
                            :move-to {}
                            :piece-str ""})]
    (fn []
      [:input {:type "text"
               :value (:text @text-state)
               :on-change (handle-text-input text-state)}])))

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
   [slider-view]
   [board-view @app-state]
   [input-view]])

;; --------------------
;; --INITIALIZE THE APP
(defn mount-root []
  (r/render [app-view] (.getElementById js/document "app")))

(defn init! []
  (mount-root))
