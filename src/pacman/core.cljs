(ns pacman.core
  (:require [pacman.ai :as a]
            [hiccups.runtime :as hiccupsrt])
  (:require-macros [hiccups.core :as hiccups :refer [html]]))

(enable-console-print!)

(println "This text is printed from src/pacman/core.cljs. Go ahead and edit it and see reloading in action.")

;; define your app data so that it doesn't get over-written on reload

(defonce app-state (atom {:text "Hello world boo!"}))

(defn cell-id [x y]
  (str "cell-" x "-" y))

(defn prep-pac-table [w h]
  (html [:table
         (map (fn [y]
                (html [:tr
                      (map (fn [x] (html
                                     [:td {:id (cell-id x y)
                                           :style "width: 30px; height: 30px;"}]
                                     )) (range w))
                      ])) (range h))
         ]))

(defn css-class-for-type [el-type]
  ({
   :wall "wall"
   :bean "bean"
   :pacman "pacman"
   :ghost "ghost"
   :space "space"
   } el-type))

(defn set-cell-for-type [el-type x y]
  (set! (.-className (.getElementById js/document (cell-id x y)))
        (css-class-for-type el-type)))

(defn set-class-for-cell [the-class x y]
  (set! (.-className (.getElementById js/document (cell-id x y)))
        the-class))

(defn paint-pacman-and-ghost-on-model [the-data [px py] ghost-pos]
  (loop [post-pac
         (a/replace-node the-data
            px py {:type :pacman})
         rem-ghost ghost-pos]
    (if (seq rem-ghost)
      (let [[gx gy] (first rem-ghost)]
        (recur
          (a/replace-node post-pac gx gy {:type :ghost})
          (rest rem-ghost)))
      post-pac)))

(defn get-pos-from-data [the-data]
  (let [the-map (a/str-map-2-data (:repr the-data)
                                  (:width the-data))
        pac-pos (or (:pacpos the-data)
                    (first (a/pacman-pos the-map)))
        ghost-pos (or (:ghostpos the-data)
                      (into [] (a/ghost-pos the-map)))
        repl-repr (clojure.string/replace (:repr the-data) #"[pg]" " ")
        painted (paint-pacman-and-ghost-on-model
                  the-map pac-pos ghost-pos)]
    (-> the-data
        (assoc :pacpos pac-pos)
        (assoc :ghostpos ghost-pos)
        (assoc :repr repl-repr)
        (assoc :topaint painted)
        )))

(def map-repr (atom (get-pos-from-data {:width 19 :repr a/sample-map})))
(def move-stack (atom []))
(def auto-next (atom false))

(defn pacman-class-by-direction [prev-stack]
  (let [[prev curr] (take-last 2 prev-stack)]
    (if (and prev curr)
      (nth ["pacman-left" "pacman-up" "pacman-right" "pacman-bot"]
        (a/coords-2-vector-pos prev curr))
      "pacman")))

(defn render-pacman-table [dat width prev-stack]
  (let [pac-pos (first (a/pacman-pos dat))]
    (dorun
     (map-indexed
      (fn [y row]
        (dorun
          (map-indexed
            (fn [x elem]
              (if (not= (:type elem) :pacman)
                (set-cell-for-type (:type elem) x y)
                (set-class-for-cell
                  (pacman-class-by-direction
                    (conj prev-stack pac-pos))
                  x y)
                ))
            row)))
      dat))))

(defn reset-map-repr []
  (reset! map-repr {:width 19 :repr a/sample-map}))

(defn render-map-repr []
  (let [curr @map-repr]
    (render-pacman-table (:topaint curr)
                         (:width curr)
                         (:prevstack curr))))

(defn hook-button [the-id the-fn]
  (set! (.-onclick (.getElementById js/document the-id))
        the-fn))

(defn conj-trim [the-vec elem lim]
  (let [res (conj the-vec elem)]
    (if (> (count res) lim)
      (subvec res (- (count res) lim))
      res)))

(defn next-pacman-turn []
  (let [curr @map-repr
        the-data (a/str-map-2-data (:repr curr) (:width curr))
        prev-stack (or (:prevstack curr) [])
        my-pos (:pacpos curr)
        [cx cy] my-pos
        ghost-pos (:ghostpos curr)
        painted-repr (paint-pacman-and-ghost-on-model the-data my-pos ghost-pos)
        scores (a/score-turn-for-pacman painted-repr prev-stack)
        best-score-pos (first (apply max-key second (map-indexed vector scores)))
        [bx by] (nth [[-1 0] [0 -1] [1 0] [0 1]] best-score-pos)
        npos [(+ cx bx) (+ cy by)]
        [nx ny] npos
        after-move (a/map-after-pacman-move the-data nx ny)
        post-move-paint (paint-pacman-and-ghost-on-model after-move npos ghost-pos)
        ]
    (swap! move-stack conj curr)
    (reset! map-repr
      (assoc
        (a/map-2-str after-move)
        :prevstack (conj-trim prev-stack my-pos 16)
        :pacpos npos
        :ghostpos ghost-pos
        :topaint post-move-paint)) ; 8 for second iteration
    (println "chosen move:" [bx by])
    (render-map-repr)))

(defn pop-pacman-turn []
  (let [head (last @move-stack)]
    (swap! move-stack pop)
    (reset! map-repr head)
    (render-map-repr)))

(defonce autoupd
  (js/setInterval #(if @auto-next (next-pacman-turn))
                  200))

(defn prep-pac-table-reset []
  (set! (.-innerHTML (.getElementById js/document "viz"))
        (prep-pac-table 19 21)))

(declare reset-all)

(defn hook-all-buttons []
  (hook-button "next" next-pacman-turn)
  (hook-button "prev" pop-pacman-turn)
  (hook-button "auto" #(swap! auto-next not))
  (hook-button "reset" reset-all)
  )

(defn reset-all []
  (prep-pac-table-reset)
  (reset! map-repr
    (get-pos-from-data
      {:width 19 :repr a/sample-map}))
  (render-map-repr)
  (hook-all-buttons))

(set! (.-onload js/window) reset-all)

(defn on-js-reload []
  (prep-pac-table-reset)
  (render-map-repr)
  (hook-all-buttons)
  
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
