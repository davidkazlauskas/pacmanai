(ns pacman.ai)

; should dominate
(def WEIGHT-WALL-NEXT (atom -1000))
(def WEIGHT-GHOST-NEXT (atom -100))
; prefer going to beans
(def WEIGHT-BEAN-NEXT (atom 10))
(def WEIGHT-GHOST-COUNT (atom -1))
(def WEIGHT-BEAN-COUNT (atom 1))

(def WEIGHT-POS-VISITED (atom -5))

(def WALK-GHOST-SCORE (atom -25))
(def WALK-BEAN-SCORE (atom 15))

(def sample-map
  (str
    "wwwwwwwwwwwwwwwwwww"
    "w........w........w"
    "w.ww.www.w.www.ww.w"
    "w.................w"
    "w.ww.w.wwwww.w.ww.w"
    "w....w...w...w....w"
    "wwww.www w www.wwww"
    "   w.w   g   w.w   "
    "wwww.w ww ww w.wwww"
    "    .  wgggw  .    "
    "wwww.w wwwww w.wwww"
    "   w.w       w.w   "
    "wwww.w wwwww w.wwww"
    "w........w........w"
    "w.ww.www.w.www.ww.w"
    "w..w.....p.....w..w"
    "ww.w.w.wwwww.w.w.ww"
    "w....w...w...w....w"
    "w.wwwwww.w.wwwwww.w"
    "w.................w"
    "wwwwwwwwwwwwwwwwwww"
    )
  )

(def sample-map-next-to-ghost
  (str
    "wwwwwwwwwwwwwwwwwww"
    "w........w........w"
    "w.ww.www.w.www.ww.w"
    "w.................w"
    "w.ww.w.wwwww.w.ww.w"
    "w....w...w...w....w"
    "wwww.www w www.wwww"
    "   w.w  pg   w.w   "
    "wwww.w ww ww w.wwww"
    "    .  wgggw  .    "
    "wwww.w wwwww w.wwww"
    "   w.w       w.w   "
    "wwww.w wwwww w.wwww"
    "w........w........w"
    "w.ww.www.w.www.ww.w"
    "w..w...........w..w"
    "ww.w.w.wwwww.w.w.ww"
    "w....w...w...w....w"
    "w.wwwwww.w.wwwwww.w"
    "w.................w"
    "wwwwwwwwwwwwwwwwwww"
    )
  )

(defn letter-to-node [character]
  (case character
    \p {:type :pacman}
    \w {:type :wall}
    \space {:type :space}
    \g {:type :ghost}
    \. {:type :bean}))

(defn str-map-2-data [strmap mapwidth]
  (->> strmap
       (partition mapwidth)
       (mapv #(mapv letter-to-node %))))

(defn generic-find-pos [filterfunc full-map-vec]
  (let [the-res (atom [])]
    (doall
      (map-indexed
        #(doall
           (map-indexed
           (fn [idx i]
             (if (filterfunc [i idx %1])
               (swap! the-res conj [idx %1])))
           %2))
        full-map-vec))
    @the-res))

(def pacman-pos (partial generic-find-pos #(= :pacman (:type (first %)))))
(def wall-pos (partial generic-find-pos #(= :wall (:type (first %)))))
(def ghost-pos (partial generic-find-pos #(= :ghost (:type (first %)))))
(def bean-pos (partial generic-find-pos #(= :bean (:type (first %)))))
(def space-pos (partial generic-find-pos #(= :space (:type (first %)))))

(def sample-bot-right [18 20])

;
; split directions into four teritories
; | |
; |p|
; | |
(defn parition-teritories [[pcord-x pcord-y]
                           [b-right-x b-right-y]]
  ; first is left,
  ; second is top,
  ; third is right,
  ; fourth is bot
  [
   [[0 0] [pcord-x b-right-y]]
   [[pcord-x 0] [b-right-x b-right-y]]
   [[0 0] [b-right-x pcord-y]]
   [[0 pcord-y] [b-right-x b-right-y]]
   ]
  )

(defn sub-map [original [[tlx tly] [brx bry]]]
  (mapv
    #(subvec % tlx brx)
    (subvec original tly bry)))

(defn evaluate-territory-ghost-score [original]
  (let [ghosts (count (ghost-pos original))]
    (cond
      ; TODO: weight here or not here?
      (= ghosts 0) 0.0
      (= ghosts 1) 0.25
      (= ghosts 2) 0.50
      (= ghosts 3) 0.75
      (>= ghosts 4) 1.0)))

(defn total-nodes [original]
  (* (count original) (count (first original))))

(defn bean-score [original divisor]
  (/ (float (count (bean-pos original))) divisor))

(defn elem-or-nil [original x y]
  (if-let [yel (get original y)]
    (get yel x)))

(defn replace-node [original x y nv]
  (assoc-in original [y x] nv))

(defn is-something-next-to-something
  [condition original [inpx inpy] [delx dely]]
  (let [inp (elem-or-nil original (or inpx 0) (or inpy 0))
        del (elem-or-nil original (+ (or inpx 0) (or delx 0)) (+ (or inpy 0) (or dely 0)))]
    (if (and inp del)
      (condition inp del)
      false)))

(def is-bean-next-to-pacman
  (partial is-something-next-to-something
           #(and
              (= :pacman (:type %1))
              (= :bean (:type %2)))))

(def is-ghost-next-to-pacman
  (partial is-something-next-to-something
           #(and
              (= :pacman (:type %1))
              (= :ghost (:type %2)))))

(def is-wall-next-to-pacman
  (partial is-something-next-to-something
           #(and
              (= :pacman (:type %1))
              (= :wall (:type %2)))))

(def is-wall-next-to-ghost
  (partial is-something-next-to-something
           #(and
              (= :ghost (:type %1))
              (= :wall (:type %2)))))

(defn last-coords-for-orig [original]
  [(dec (count (first original))) (dec (count original))])

(defn bool-2-num [the-value]
  (if the-value 1.0 0.0))


(defn round-and-weight-functions [original weight bool-func]
  (let [pac-pos (first (pacman-pos original))]
    [
     (* weight
      (bool-2-num
       (bool-func original pac-pos [-1 0])))
     (* weight
      (bool-2-num
       (bool-func original pac-pos [0 -1])))
     (* weight
      (bool-2-num
       (bool-func original pac-pos [1 0])))
     (* weight
      (bool-2-num
       (bool-func original pac-pos [0 1])))
     ]))

(defn round-and-weight-function-gradient [original weight grad-func]
  (let [pac-pos (first (pacman-pos original))]
    [
     (* weight
      (grad-func original pac-pos [-1 0]))
     (* weight
      (grad-func original pac-pos [0 -1]))
     (* weight
      (grad-func original pac-pos [1 0]))
     (* weight
      (grad-func original pac-pos [0 1]))
     ]))

; score when ghost is immediately to pacman
(defn score-immediate-danger [original]
  (round-and-weight-functions
    original @WEIGHT-GHOST-NEXT is-ghost-next-to-pacman))

; score when ghost is immediately to pacman
(defn score-immediate-bean [original]
  (round-and-weight-functions
    original @WEIGHT-BEAN-NEXT is-bean-next-to-pacman))

; score when ghost is immediately to pacman
(defn score-wall [original]
  (round-and-weight-functions
    original @WEIGHT-WALL-NEXT is-wall-next-to-pacman))

(defn territory-score-func [original weight eval-func]
  (let [pac-pos (first (pacman-pos original))
        [left-part top-part right-part bot-part]
        (parition-teritories pac-pos (last-coords-for-orig original))]
    (mapv #(* weight %)
     [(eval-func
       (sub-map original left-part))
     (eval-func
       (sub-map original top-part))
     (eval-func
       (sub-map original right-part))
      (eval-func
       (sub-map original bot-part))])))

(defn score-ghost-teritories [original]
  (territory-score-func original
                        @WEIGHT-GHOST-COUNT
                        evaluate-territory-ghost-score))

(defn score-bean-teritories [original]
  (territory-score-func original
                        @WEIGHT-BEAN-COUNT
                        (fn [i]
                          (bean-score i (count original)))))

(defn print-positional [pref [l t r b]]
  (println pref "left:" l "top:" t "right:" r "bot:" b))

(defn score-prev-pos-existance [original pos-stack]
  (round-and-weight-function-gradient
    original @WEIGHT-POS-VISITED
    (fn [_ [cx cy] [dx dy]]
      (let [to-visit [(+ cx dx) (+ cy dy)]]
        (count (filter #(= % to-visit) pos-stack))))))

(defn surround-coords [x y]
  [
   [(dec x) y]
   [x (dec y)]
   [(inc x) y]
   [x (inc y)]
   ]
  )

(defn try-advance [original used-set
                   path-valid [x y] iter limit]
  (if (< iter limit)
    (let [all (surround-coords x y)
          filtered
          (filterv some?
            (map (fn [[cx cy]]
                   (if-let [el (elem-or-nil original cx cy)]
                     (if (and (path-valid el)
                              (not (used-set [cx cy])))
                             {:coords [cx cy]})))
                 all))
          new-set (into used-set (map :coords filtered))]
      {:coords [x y]
       :leaves (mapv
                 #(try-advance original new-set
                               path-valid
                               (:coords %)
                               (inc iter) limit)
                 filtered)})
   {:coords [x y]
    :leaves []}))

(defn try-advance-root [original path-valid
                        coords limit]
  (try-advance original #{coords} path-valid
               coords 0 limit))

(defn score-walk-tree [original
                       walk-tree
                       current-res
                       current-iter
                       scoring-function]
  (let [[x y] (:coords walk-tree)]
   (if-let [elem (elem-or-nil original x y)]
          (let [[curr-score keep-going]
                (scoring-function current-iter elem)]
           (+ current-res
              curr-score
              (if keep-going
                (reduce +
                      (map
                        #(score-walk-tree
                           original
                           % 0 (inc current-iter)
                           scoring-function)
                        (:leaves walk-tree)))
                0)))
           current-res)))

(defn score-walk-tree-root [original walk-tree scoring-function]
  (score-walk-tree original walk-tree 0 0 scoring-function))

(defn coords-2-vector-pos [[x1 y1] [x2 y2]]
  (case [(- x2 x1) (- y2 y1)]
    [-1 0] 0 ; left
    [0 -1] 1 ; top
    [1 0] 2 ; right
    [0 1] 3 ; bottom
    ))

(defn score-four-directions-walk-tree
  [original walk-tree scoring-function]
  (let [orig-coords (:coords walk-tree)
        leaves-with-pos (map
                          #(vector
                             (coords-2-vector-pos
                               orig-coords
                               (:coords %))
                             %)
                          (:leaves walk-tree))
        prelim [0 0 0 0]]
    (loop [all leaves-with-pos our-vec prelim]
      (if (seq all)
        (let [[pos tree] (first all)
              cont (next all)]
          (recur
            cont
            (assoc our-vec
                   pos
                   (score-walk-tree-root
                     original tree scoring-function))))
        our-vec))))

(defn not-wall [node]
  (not= (:type node) :wall))

(defn walking-tree-scoring [distance the-node]
  (let [the-type (:type the-node)
        dist-norm (float (/ 1 (inc distance)))]
    (cond
      (= the-type :ghost)
      [(* @WALK-GHOST-SCORE dist-norm) false]
      (= the-type :bean)
      [(* @WALK-BEAN-SCORE dist-norm) true]
      :else [0 true])))

(defn surround-map-sides-with-walls [original]
  (mapv
    (fn [row]
        (-> row
          (assoc 0 {:type :wall})
          (assoc (dec (count row)) {:type :wall})))
    original))

; TODO: add creep data
(defn score-turn-for-pacman [original pacman-prev-positions]
  (let [surrounded (surround-map-sides-with-walls original)
        dvec (score-immediate-danger surrounded)
        imbean (score-immediate-bean surrounded)
        wvec (score-wall surrounded)
        gvec (score-ghost-teritories surrounded)
        bvec (score-bean-teritories surrounded)
        prev-pos (score-prev-pos-existance surrounded pacman-prev-positions)
        walk-tree (try-advance-root surrounded not-wall
                                    (last pacman-prev-positions)
                                    16)
        walk-scoring (score-four-directions-walk-tree
                       surrounded walk-tree walking-tree-scoring)
        final-vec (apply mapv + [dvec imbean wvec gvec
                                 bvec prev-pos walk-scoring])
        ]
    (print-positional "GHOST NEXT:" dvec)
    (print-positional "WALL NEXT:" wvec)
    (print-positional "BEAN NEXT:" imbean)
    (print-positional "GHOST TERRITORY:" gvec)
    (print-positional "BEAN TERRITORY:" bvec)
    (print-positional "PREV POS SCORE:" prev-pos)
    (print-positional "WALK SCORE:" walk-scoring)
    (print-positional "FINAL SCORE: " final-vec)
    final-vec
    ))

; expand map on both sides

(defn turn-api-coords-to-accepted [api-coords]
  (mapv
    #(mapv (fn [i] (if i
                     (cond
                       (= :cookie (:type i))
                       (assoc i :type :bean)
                       (= :tecman (:type i))
                       (assoc i :type :pacman)
                       :else i)
                     {:type :space})) %)
    api-coords))

(defn moves-for-turn [original step [x y]]
  (let [moves 
         (filterv #(or
                     (= (:type %) :space)
                     (= (:type %) :bean))
           (filter some?
             (map (fn [xi yi] (elem-or-nil original xi yi))
                 (surround-coords x y))))]
  (if (= 0 (count moves))
    nil))
  )

(defn is-ghost-close [original move-stack]
  
  )

(defn random-direction [original [x y]]
  (rand-nth
   (filter some?
   (map
    (fn [[xi yi]]
      (if-let [el (elem-or-nil original xi yi)]
        (if (not= :wall (:type el))
          [xi yi]
          )
        ))
   [
     [(dec x) y]
     [x (dec y)]
     [(inc x) y]
     [x (inc y)]
   ]))))

(defn hitting-wall-turn [original
                         [[ygprev xgprev]
                          [ycurr xcurr]]]
  (let [[mx my] [(- xcurr xgprev)
                 (- ycurr ygprev)]
        next-turn [(+ xcurr mx)
                   (+ ycurr my)]]
    (if (or
          (is-wall-next-to-ghost
            original
            [ycurr xcurr]
            [mx my])
          (= [0 0] [mx my]))
      (let [[xn yn] (random-direction original [xcurr ycurr])]
        [yn xn])
      next-turn)))

(defn node-to-char [the-node]
  (case (:type the-node)
    :wall "w"
    :pacman "p"
    :ghost "g"
    :bean "."
    :space " "))

(defn map-2-str [the-map]
  {:width (count (first the-map))
   :repr (apply str
            (map #(apply str (map node-to-char %))
              the-map))})

(defn map-after-pacman-move [the-data xpos ypos]
  (replace-node the-data xpos ypos {:type :space}))

(defn external-advice-turn [api-map]
  (println api-map)
  (case (:mode api-map)
    :tecman
    (let [[tecy tecx] (:tecman-position api-map)
          conv-map (turn-api-coords-to-accepted (:map api-map))
          [left top bot right]
          (score-turn-for-pacman conv-map [])
          svec [
           {:dir :left :score left}
           {:dir :top :score top}
           {:dir :right :score right}
           {:dir :bot :score bot}
          ]
          the-dir (:dir (first (sort-by :score > svec)))
          ]
      (case the-dir
        :left [[tecy (dec tecx)]]
        :top [[(dec tecy) tecx]]
        :right [[tecy (inc tecx)]]
        :bot [[(inc tecy) tecx]])
      )
    :ghost
    (let [orig-map (turn-api-coords-to-accepted (:map api-map))]
       (mapv
       #(hitting-wall-turn orig-map [%1 %2])
       (:ghost-previous-positions api-map)
       (:ghost-positions api-map)))
))
