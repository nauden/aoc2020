(def neighbors [[-1 -1] [0 -1] [1 -1]
                [-1  0]        [1  0]
                [-1  1] [0  1] [1  1]])

(defn occupied-neighbors [grid x y]
  (def h (length grid))
  (def w (length (grid 0)))
  (var o 0)
  (loop [[dx dy] :in neighbors
         :let [nx (+ x dx)
               ny (+ y dy)]
         :when (and (<= 0 nx) (<= 0 ny)
                    (< ny h) (< nx w)
                    (= (chr "#") (get-in grid [ny nx])))] (++ o))
  o)

(defn visible-occupied [grid x y]
  (def h (length grid))
  (def w (length (grid 0)))
  (var o 0)

  (loop [[dx dy] :in neighbors]
    (var nx (+ x dx))
    (var ny (+ y dy))
    (while (and (<= 0 nx) (<= 0 ny)
                (< ny h) (< nx w))
      (case ((grid ny) nx)
        (chr "#") (do (++ o) (break))
        (chr "L") (break))

      (+= nx dx)
      (+= ny dy)))
  o)

(defn count-occupied [grid]
  (sum (map (partial count (partial = (chr "#"))) grid)))

(defn stabilize [grid cnt countfn]
  (def h (length grid))
  (def w (length (grid 0)))
  (def next-grid (seq [y :range [0 h]] (buffer (grid y))))
  (var changed false)

  (loop [y :range [0 h]
         x :range [0 w]
         :let [c ((grid y) x)
               o (countfn grid x y)]
         :when (not= c (chr "."))]
    (cond
      (and (= c (chr "L")) (zero? o))
      (do
        (put-in next-grid [y x] (chr "#"))
        (set changed true))

      (and (= c (chr "#")) (>= o cnt))
      (do
        (put-in next-grid [y x] (chr "L"))
        (set changed true))))

  (if changed
    (stabilize next-grid cnt countfn)
    (count-occupied next-grid)))

(def input
  (->> (slurp "input")
       (string/trim)
       (string/split "\n")
       (map buffer)))

(print "Part 1: " (stabilize input 4 occupied-neighbors))
(print "Part 2: " (stabilize input 5 visible-occupied))
