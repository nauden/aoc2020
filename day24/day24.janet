(defn split-instructions [line]
  (if (empty? line)
    []
    (case (take 1 line)
      "e" ["e" ;(split-instructions (drop 1 line))]
      "w" ["w" ;(split-instructions (drop 1 line))]
      [(take 2 line) ;(split-instructions (drop 2 line))])))

(defn follow-steps [line]
  (var x 0)
  (var y 0)

  (each ins (split-instructions line)
    (case ins
      "e"  (++ x)
      "w"  (-- x)
      "se" (do (-- y) (+= x .5))
      "sw" (do (-- y) (-= x .5))
      "ne" (do (++ y) (+= x .5))
      "nw" (do (++ y) (-= x .5))
      (error ins)))

  (string/format "%.1f %.1f" x y))

(defn flip-tiles [str]
  (reduce
    (fn [floor line]
      (def pos (follow-steps line))
      (if (floor pos)
        (put floor pos nil)
        (put floor pos true)))
    @{}
    (string/split "\n" str)))

(defn neighbors [str]
  (def [x y] (map scan-number (string/split " " str)))
  (map (fn [[nx ny]] (string/format "%.1f %.1f" nx ny))
      [[(inc x) y]          # e
       [(dec x) y]          # w
       [(+ x .5) (inc y)]   # se
       [(- x .5) (inc y)]   # sw
       [(+ x .5) (dec y)]   # ne
       [(- x .5) (dec y)]])) # nw

(defn art-exhibit [floor &opt day]
  (default day 1)

  (prin "\rDay " day)
  (flush)

  (if (= 101 day)
    (do (prin "\r          \r") (flush) (length floor))
    (let [next-floor @{}
          white-tiles @{}]
      (eachk tile floor
        (var ns 0)
        (each n (neighbors tile)
          (if (floor n)
            (++ ns)
            (put white-tiles n true)))

        (when (or (one? ns) (= 2 ns))
          (put next-floor tile true)))

      (eachk tile white-tiles
        (var ns 0)
        (each n (neighbors tile)
          (when (floor n) (++ ns)))

        (when (= 2 ns)
          (put next-floor tile true)))

      (art-exhibit next-floor (inc day)))))


(def tiles
  (->> (slurp "input")
       (string/trim)
       (flip-tiles)))

(print "Part 1: " (length tiles))
(print "Part 2: " (art-exhibit tiles))
