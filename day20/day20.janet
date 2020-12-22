(def tile-peg
  (peg/compile
    ~{:num (/ ':d+ ,scan-number)
      :row (* '(10 (set "#.")) (? :s))
      :main (* "Tile " :num ":" :s+ (/ (10 :row) ,tuple))}))

(defn rotate-edges [edges]
  (tuple ;(drop 1 edges) (first edges)))

(defn extract-edges [image]
  (let [top (first image)
        bottom (string/reverse (last image))
        right (string/join (seq [y :range-to [0 9]] (slice (image y) -2)))
        left (string/reverse
               (string/join (seq [y :down-to [9 0]] (slice (image y) 0 1))))]
    {:edges
     {:starting {:top top :right right
                 :bottom bottom :left left}

      :all (table
             ;(interpose true [top right bottom left
                               (string/reverse bottom)
                               (string/reverse left)
                               (string/reverse top)
                               (string/reverse right)]) true)}}))
(defn intersection [a b]
  (seq [k :keys a
        :when (b k)] k))

(defn parse-tiles [str]
  (def tiles-str (string/split "\n\n" (string/trim str)))

  (reduce
    (fn [tiles [id image]]
      (put tiles id (merge {:image image} (extract-edges image))))
    @{}
    (map (partial peg/match tile-peg) tiles-str)))

(defn find-corners [tiles]
  (def matches @{})

  (loop [[id tile] :pairs tiles
         [other-id other] :pairs tiles
         :when (and (not= id other-id))
                    #(not (matches id)))
         :let [tile-edges (get-in tile [:edges :all])
               other-edges (get-in other [:edges :all])
               common-edges (intersection tile-edges other-edges)]
         :when (< 0 (length common-edges))]
    (update matches id |(if (nil? $) 1 (inc $))))

  (seq [[id n] :pairs matches
        :when (= 2 n)] id))

(defn find-top-left-corner [tiles corners])


(def test-tiles (parse-tiles (slurp "test")))
(def input (parse-tiles (slurp "input")))
#(def corners (find-corners input))

(def test-corners @[1951 3079 2971 1171])


# (print "Part 1: " (product corners))
