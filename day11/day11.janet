(def diff [[-1 -1] [0 -1] [1 -1]
           [-1  0]        [1  0]
           [-1  1] [0  1] [1  1]])

(defmacro xy-to-i [x y]
  ~(+ ,x (* w ,y)))

(defn occupied-neighbors [{:w w :h h :grid grid} x y]
  (var o 0)
  (loop [[dx dy] :in diff
         :let [nx (+ x dx)
               ny (+ y dy)]
         :when (and (<= 0 nx) (<= 0 ny)
                    (< ny h) (< nx w)
                    (= (chr "#") (grid (xy-to-i nx ny))))] (++ o))
  o)

(defn visible-occupied [{:w w :h h :grid grid} x y]
  (var o 0)

  (loop [[dx dy] :in diff]
    (var nx (+ x dx))
    (var ny (+ y dy))
    (while (and (<= 0 nx) (<= 0 ny)
                (< ny h) (< nx w))
      (case (grid (xy-to-i nx ny))
        (chr "#") (do (++ o) (break))
        (chr "L") (break))

      (+= nx dx)
      (+= ny dy)))
  o)

(defn stabilize [{:w w :h h :grid inital} cnt countfn]
  (def grid (buffer/blit @"" inital))
  (def next-grid (buffer/blit @"" grid))
  (var changed true)

  (while changed
    (set changed false)

    (loop [y :range [0 h]
           x :range [0 w]
           :let [i (xy-to-i x y)
                 c (grid i)
                 o (countfn {:w w :h h :grid grid} x y)]
           :when (not= c (chr "."))]
      (cond
        (and (= c (chr "L")) (zero? o))
        (do
          (put next-grid (xy-to-i x y) (chr "#"))
          (set changed true))

        (and (= c (chr "#")) (>= o cnt))
        (do
          (put next-grid (xy-to-i x y) (chr "L"))
          (set changed true))))

    (buffer/blit grid next-grid))

  (count (partial = (chr "#")) grid))


(defn parse-grid [str]
  (def lines (string/split "\n" str))
  {:h (length lines)
   :w (length (lines 0))
   :grid (string/replace-all "\n" "" str)})

(def input
  (->> (slurp "input")
       (string/trim)
       (parse-grid)))

(print "Part 1: " (stabilize input 4 occupied-neighbors))
(print "Part 2: " (stabilize input 5 visible-occupied))
