(defn seat-id [pass]
  (def n (->> (string/replace-all "F" "0" pass)
              (string/replace-all "B" "1")
              (string/replace-all "L" "0")
              (string/replace-all "R" "1")))

  (+ (* 8 (scan-number (string "2r" (string/slice n 0 7))))
     (scan-number (string "2r" (string/slice n 7)))))

(def input
  (->> (slurp "input")
       (string/trim)
       (string/split "\n")
       (map seat-id)
       (sort)))

(print "Part 1: " (last input))

(defn find-my-seat [ids]
  (def m (table ;(array ;(interpose true ids) true)))
  (first
    (seq [id :range [1 (dec (last ids))]
          :when (and (not (get m id false))
                     (m (dec id))
                     (m (inc id)))] id)))

(print "Part 2: " (find-my-seat input))
