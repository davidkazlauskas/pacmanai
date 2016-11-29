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

(defn hook-checkbox [the-id truefn falsefn]
  (set! (.-onclick (.getElementById js/document the-id))
        (fn []
          (if (.-checked (.getElementById js/document the-id))
            (truefn)
            (falsefn))
          nil)))

(defn hook-checkbox-to-atom [the-id the-atom]
  (hook-checkbox the-id
                 #(reset! the-atom true)
                 #(reset! the-atom false)))

(defn conj-trim [the-vec elem lim]
  (let [res (conj the-vec elem)]
    (if (> (count res) lim)
      (subvec res (- (count res) lim))
      res)))

(defn weights-symbols []
  [
  {:symbol "wallnext"
   :description "Is actor next to the wall"}
  {:symbol "ghostnext"
   :description "Is actor next to the ghost"}
  {:symbol "pacmannext"
   :description "Is actor next to the pacman"}
  {:symbol "beannext"
   :description "Is actor next to the bean"}
  {:symbol "ghostcount"
   :description "Ghost count in territory weight"}
  {:symbol "beancount"
   :description "Bean count in territory weight"}
  {:symbol "posvisited"
   :description "Was position last visited"}
  {:symbol "walkghostscore"
   :description "Weight for ghosts found in following paths"}
  {:symbol "walkpacmanscore"
   :description "Weight for pacman found in following paths"}
  {:symbol "walkbeanscore"
   :description "Weight for beans found in following paths"}
  {:symbol "walkspacescore"
   :description "Weight for space found in following paths"}
  ])

(defn dimensions []
  ["pacman" "ghosts"])

(def atom-cache
  (into {}
    (map #(vector %
      (into {}
      (map
        (fn [i] (vector (:symbol i)
                 (atom (a/get-val (:symbol i)))))
        (weights-symbols)))
      ) (dimensions))))

(defn set-dim-val [dim the-symb the-val]
  (reset! (get-in atom-cache [dim the-symb]) the-val))

(defn get-dim-val [dim the-symb]
  (deref (get-in atom-cache [dim the-symb])))

(defn apply-weight-map [dim the-map]
  (doseq [[k v] the-map]
    (set-dim-val dim k v)))

(apply-weight-map
  "pacman"
  {"wallnext" -1000
   "ghostnext" -500
   "pacmannext" 0
   "beannext" 100
   "ghostcount" -20
   "beancount" 10
   "posvisited" -15
   "walkghostscore" -300
   "walkpacmanscore" 0
   "walkbeanscore" 5
   "walkspacescore" -1})

(apply-weight-map
  "ghosts"
  {"wallnext" -1000
   "ghostnext" -100
   "pacmannext" 1000
   "beannext" 0
   "ghostcount" -20
   "beancount" 0
   "posvisited" 0
   "walkghostscore" -100
   "walkpacmanscore" 300
   "walkbeanscore" 5
   "walkspacescore" 0})

(defn get-float-val-by-id [the-id]
  (js/parseInt (.-value (.getElementById js/document the-id))))

(defn weight-id [dimension symb]
  (str
    "weight-input-"
    dimension "-"
    (clojure.string/lower-case (str symb))))

(defn set-dimension-values [the-dim]
  (doseq [i (weights-symbols)]
    (a/set-val (:symbol i)
      (get-dim-val the-dim (:symbol i)))))

(defn flush-inputs-to-cache []
  (doseq [i (dimensions)
          j (weights-symbols)]
    (set-dim-val i (:symbol j)
      (get-float-val-by-id (weight-id i (:symbol j))))))

(defn generate-table []
  (html
    [:table
     [:tr [:th "Description"] (map #(html [:th %])
                                   (dimensions))]
     (map
       (fn [weight]
         (html
           [:tr
            [:td (:description weight)]
            (map (fn [dim]
                   [:td
                    [:input {:id (weight-id dim (:symbol weight))
                             :type "text"
                             :value (get-dim-val dim (:symbol weight))}]
                    ]
                   ) (dimensions))
            ])
         )
        (weights-symbols))]))

(defn pop-pacman-turn []
  (let [head (last @move-stack)]
    (swap! move-stack pop)
    (reset! map-repr head)
    (render-map-repr)))

(defn score-to-coords [[cx cy] scores]
  (let [all-vals
        (take 2 (sort-by second > (map-indexed vector scores)))
        choose-vec [[-1 0] [0 -1] [1 0] [0 1]]
        [dx dy]
        (nth choose-vec (first (first all-vals)))]
    [[(+ cx dx) (+ cy dy)] [dx dy] [cx cy]]))

(defn next-pacman-turn []
  (set-dimension-values "pacman")
  (let [curr @map-repr
        the-data (a/str-map-2-data (:repr curr) (:width curr))
        prev-stack (or (:prevstack curr) [])
        my-pos (:pacpos curr)
        ghost-pos (:ghostpos curr)
        painted-repr (paint-pacman-and-ghost-on-model the-data my-pos ghost-pos)
        scores (a/score-turn-for-pacman painted-repr prev-stack)
        [npos ndelt] (score-to-coords my-pos scores)
        [nx ny] npos
        after-move (a/map-after-pacman-move the-data nx ny)
        post-move-paint (paint-pacman-and-ghost-on-model after-move npos ghost-pos)]
    (swap! move-stack conj curr)
    (reset! map-repr
      (assoc
        (a/map-2-str after-move)
        :prevstack (conj-trim prev-stack my-pos 16)
        :pacpos npos
        :ghostpos ghost-pos
        :topaint post-move-paint)) ; 8 for second iteration
    (println "chosen move:" ndelt)
    (render-map-repr)))

(defn ghosts-separate-if-needed [ncoords]
  (println "spike" ncoords)
  (loop [the-coords []
         the-set #{}
         the-rem ncoords]
    (if (seq the-rem)
      (let [f (first the-rem)
            wcur (conj the-set (first f))
            rst (rest the-rem)]
        (if (the-set (first f))
          (recur (conj the-coords (last f)) wcur rst)
          (recur (conj the-coords (first f)) wcur rst)))
      the-coords)))

(defn next-ghost-turn []
  (set-dimension-values "ghosts")
  (let [curr @map-repr
        the-data (a/str-map-2-data (:repr curr) (:width curr))
        prev-stack (or (:prevstack curr) [])
        my-pos (:pacpos curr)
        ghost-pos (:ghostpos curr)
        painted-repr (paint-pacman-and-ghost-on-model the-data my-pos ghost-pos)
        scores (a/score-turn-ghosts painted-repr ghost-pos)
        the-turns (mapv #(score-to-coords %1 %2) ghost-pos scores)
        nghost-pos (ghosts-separate-if-needed the-turns)
        nghost-delt (mapv second the-turns)
        post-move-paint (paint-pacman-and-ghost-on-model the-data my-pos nghost-pos)]
    (swap! move-stack conj curr)
    (reset! map-repr
      (assoc
        (a/map-2-str the-data)
        :prevstack prev-stack
        :pacpos my-pos
        :ghostpos nghost-pos
        :topaint post-move-paint)) ; 8 for second iteration
    (println "chosen moves:" nghost-delt)
    (render-map-repr)))

(defn wrap-func-on-atom-cond [the-fn the-atom the-key]
  (fn
    ([]
    (let [res @the-atom]
      (when res
        (the-fn))
      res))
    ([_] the-key)))

(def pacman-move (atom true))
(def ghosts-move (atom true))

(defn n-game-continuation []
  (cycle [(wrap-func-on-atom-cond next-pacman-turn pacman-move :pac)
          (wrap-func-on-atom-cond next-ghost-turn ghosts-move :ghost)]))

(def game-continuation (atom (n-game-continuation)))

(defn is-game-over []
  (let [cgame @map-repr]
    (contains? (into #{} (:ghostpos cgame)) (:pacpos cgame))))

(defn game-advance []
  (swap! game-continuation
         (fn [curr]
           (if-not (is-game-over)
             (loop [exec-set #{} next-iter curr]
               (let [now (first next-iter)
                     tail (rest next-iter)
                     the-key (now 0)
                     wkey (conj exec-set the-key)]
                 (if (or (exec-set the-key) (now))
                   tail
                   (recur wkey tail))))
             curr))))

(defonce autoupd
  (js/setInterval #(if @auto-next (game-advance))
                  200))

(defn prep-pac-table-reset []
  (set! (.-innerHTML (.getElementById js/document "viz"))
        (prep-pac-table 19 21))
  (set! (.-innerHTML (.getElementById js/document "weight-table"))
        (generate-table)))

(declare reset-all)

(defn hook-all-buttons []
  (hook-button "next" game-advance)
  (hook-button "prev" pop-pacman-turn)
  (hook-button "auto" #(swap! auto-next not))
  (hook-button "reset" reset-all)
  (hook-button "update" flush-inputs-to-cache)
  (hook-checkbox-to-atom "pacmoves" pacman-move)
  (hook-checkbox-to-atom "ghostmoves" ghosts-move))

(defn reset-all []
  (reset! game-continuation (n-game-continuation))
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
