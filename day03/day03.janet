(def input
  (->> (slurp "input")
       (string/trim)
       (string/split "\n")))

(defn count-trees [grid &opt slope]
  (default slope [3 1])
  (def [dx dy] (if (= :number (type slope))
                 [slope 1]
                 slope))

  (def l (length (grid 0)))

  (var x 0)
  (length
    (seq [y :range [dy (length grid) dy]
          :before (set x (mod (+ x dx) l))
          :when (= ((grid y) x) (chr "#"))] 1)))

(print "Part 1: " (count-trees input))
(print "Part 2: " (product (map (partial count-trees input) [1 3 5 7 [1 2]])))
