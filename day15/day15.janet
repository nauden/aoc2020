(def input
  (->> (slurp "input")
       (string/trim)
       (string/split ",")
       (map scan-number)))

(defn memory-game [initial &opt turns]
  (default turns 2020)
  (var gap false)
  (var n 0)
  (def last-seen @{})
  (def l (length initial))

  (loop [turn :range [0 turns]]
    (when (zero? (mod (inc turn) 100_000))
      (prinf "\r%.2f%" (* 100 (/ turn 30_000_000)))
      (flush))

    (set n (cond
             (< turn l) (initial turn)
             (number? gap) gap
             0))
    (set gap
         (if (get last-seen n false)
            (- turn (last-seen n))
            false))

    (put last-seen n turn))

  (prinf "\r                    \r")
  (flush)
  n)

(print "Part 1: " (memory-game input))
(print "Part 2: " (memory-game input 30_000_000))
