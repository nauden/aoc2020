(defn make-cups [str]
  (map (partial + -48) (string/bytes str)))

(defn destination [d mi ma c1 c2 c3]
  (cond
    (< d mi) (destination ma mi ma c1 c2 c3)
    (or (= d c1) (= d c2) (= d c3)) (destination (dec d) mi ma c1 c2 c3)
    d))

(defn cup-game [str]
  # @{ cup => next cup }
  (let [xs (make-cups str)
        mi (min ;xs)
        ma (max ;xs)
        cups (table ;(interleave xs (tuple ;(drop 1 xs) (first xs))))]

    (var current (first xs))

    (for move 0 100
      (let [cup1 (cups current)
            cup2 (cups cup1)
            cup3 (cups cup2)
            dest-cup (destination (dec current) mi ma cup1 cup2 cup3)
            next-cup (cups cup3)]
        (put cups cup3 (cups dest-cup))
        (put cups current next-cup)
        (put cups dest-cup cup1)
        (set current next-cup)))

    (set current 1)

    (string/from-bytes
      ;(seq [cup :iterate (cups current)
             :until (= cup 1)]
         (set current cup)
         (+ cup 48)))))

(defn cup-game2 [str]
  # @{ cup => next cup }
  (let [xs (make-cups str)
        mi (min ;xs)
        ma 1_000_000
        cups (table/new ma)]

    (each [cup next-cup] (partition 2 (interleave xs (drop 1 xs)))
      (put cups cup next-cup))
    (put cups (last xs) (inc (max ;xs)))

    (for cup (inc (max ;xs)) ma
      (put cups cup (inc cup)))
    (put cups ma (first xs))

    (var current (first xs))

    (for move 1 10_000_001
      (when (zero? (mod move 100_000))
        (prinf "\r%.f%%" (* 100 (/ move 10_000_000)))
        (flush))

      (let [cup1 (cups current)
            cup2 (cups cup1)
            cup3 (cups cup2)
            dest-cup (destination (dec current) mi ma cup1 cup2 cup3)
            next-cup (cups cup3)]
        (put cups cup3 (cups dest-cup))
        (put cups current next-cup)
        (put cups dest-cup cup1)
        (set current next-cup)))

    (prin "\r     \r")
    (flush)
    (* (cups 1) (cups (cups 1)))))


(def input
  (->> (slurp "input")
       (string/trim)))

(print "Part 1: " (cup-game input))
(print "Part 2: " (cup-game2 input))
