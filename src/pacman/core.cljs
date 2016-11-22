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
  (set! (.-className (.getElementById js/document (cell-id x y))) (css-class-for-type el-type)))

(def map-repr (atom {:width 19 :repr a/sample-map}))
(def move-stack (atom []))
(def auto-next (atom false))

(defn render-pacman-table [the-str width]
  (let [dat (a/str-map-2-data the-str width)]
    (dorun
     (map-indexed
      (fn [y row]
        (dorun
          (map-indexed
            (fn [x elem]
              (set-cell-for-type (:type elem) x y))
            row)))
      dat))))

(defn reset-map-repr []
  (reset! map-repr {:width 19 :repr a/sample-map}))

(defn render-map-repr []
  (let [curr @map-repr]
    (render-pacman-table (:repr curr) (:width curr))))

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
        my-pos (first (a/pacman-pos the-data))
        scores (a/score-turn-for-pacman the-data prev-stack)
        best-score-pos (first (apply max-key second (map-indexed vector scores)))
        [bx by] (nth [[-1 0] [0 -1] [1 0] [0 1]] best-score-pos)]
    (swap! move-stack conj curr)
    (reset! map-repr
      (assoc
        (a/map-2-str (a/map-after-pacman-move the-data bx by))
        :prevstack (conj-trim prev-stack my-pos 16))) ; 8 for second iteration
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

(defn on-js-reload []
  (set! (.-innerHTML (.getElementById js/document "viz"))
        (prep-pac-table 19 21))
  (render-map-repr)
  (hook-button "next" next-pacman-turn)
  (hook-button "prev" pop-pacman-turn)
  (hook-button "auto" #(swap! auto-next not))
  
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
