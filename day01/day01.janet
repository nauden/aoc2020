(def input
  (->> (slurp "input")
       (string/trim)
       (string/split "\n")
       (map scan-number)))

(defn find-entries [ns]
  (first
    (seq [[i a] :pairs ns
           b :in (drop (inc i) ns)
          :when (= 2020 (+ a b))]
      (* a b))))

(defn find-entries2 [ns]
  (first
    (seq [[i a] :pairs ns
          [j b] :pairs (drop (inc i) ns)
          :when (> 2019 (+ a b))
           c :in (drop (+ i j 1) ns)
          :when (= 2020 (+ a b c))]
      (* a b c))))


(print "Part 1: " (find-entries input))
(print "Part 2: " (find-entries2 input))
