(defn find-loop-size [pubkey]
  (var value 1)
  (var loop-size 0)

  (while (not= value pubkey)
    (*= value 7)
    (%= value 20201227)
    (++ loop-size))

  loop-size)

(defn find-encryption-key [pubkey loop-size]
  (var value 1)

  (repeat loop-size
    (*= value pubkey)
    (%= value 20201227))

  value)

(def [card-pub door-pub]
  (->> (slurp "input")
       (string/trim)
       (string/split "\n")
       (map scan-number)))

(print "Part 1: " (find-encryption-key door-pub (find-loop-size card-pub)))
