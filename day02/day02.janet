(def entry
  (peg/compile
    ~{:num (cmt ':d+ ,scan-number)
      :main (* :num "-" :num :s (cmt ':a ,first) ":" :s ':w+)}))

(def input
  (->> (slurp "input")
       (string/trim)
       (string/split "\n")
       (map (partial peg/match entry))))

(defn valid? [[lo hi c pw]]
  (<= lo (count (partial = c) pw) hi))

(defn valid2? [[lo hi c pw]]
  (not=
    (= c (pw (dec lo)))
    (= c (pw (dec hi)))))

(print "Part 1: " (count valid? input))
(print "Part 2: " (count valid2? input))
