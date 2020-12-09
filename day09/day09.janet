(defn xmas [ns &opt preamble]
  (default preamble 25)

  (first
    (seq [[i n] :pairs (slice ns preamble)
          :when (empty?
                  (seq [a :in (slice ns i (+ i preamble))
                        b :in (slice ns (inc i) (+ i preamble))
                        :when (= n (+ a b))] n))] n)))

(defn break-encryption [ns n]
  (var s 0)
  (var i 0)
  (var j 0)
  (var mi 0)
  (var ma 0)

  (forever
    (set s (ns i))
    (set j i)
    (set mi s)
    (set ma s)
    (while (< s n)
      (++ j)
      (+= s (ns j))
      (set mi (min mi (ns j)))
      (set ma (max ma (ns j))))

    (++ i)
    (when (= s n)
      (set s (+ ma mi))
      (break)))

  s)

(def input
  (->> (slurp "input")
       (string/trim)
       (string/split "\n")
       (map scan-number)))

(let [invalid (xmas input)]
  (print "Part 1: " invalid)
  (print "Part 2: " (break-encryption input invalid)))
