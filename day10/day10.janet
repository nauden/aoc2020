(defn test-all-adapters [adapters]
  (product
    (drop 1
      (reduce
        (fn [[rating one three] adapter]
          (case (- adapter rating)
            1 [adapter (inc one) three]
            3 [adapter one (inc three)]))
        [0 0 1]
        adapters))))

(defn distinct-combinations [adapters &opt i rating seen]
  (default i 0)
  (default rating 0)
  (default seen @{})

  (cond
    (= i (length adapters)) 1
    (get seen i false) (seen i)
    (let [result (sum (seq [j :range [i (length adapters)]
                            :let [adapter (adapters j)]
                            :while (<= adapter (+ rating 3))]
                        (distinct-combinations adapters (inc j)
                                               adapter seen)))]

      (put seen i result)
      result)))

(def input
  (->> (slurp "input")
       (string/trim)
       (string/split "\n")
       (map scan-number)
       (sorted)))

(print "Part 1: " (test-all-adapters input))
(print "Part 2: " (distinct-combinations input))
