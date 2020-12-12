(def input
  (->> (slurp "input")
       (string/trim)
       (string/split "\n")))

(defmacro n []
  ~(math/trunc (/ value 90)))

(defn run [instructions]
  (var x 0)
  (var y 0)
  (var facing 1) # north, east, south, west

  (loop [instruction :in instructions
         :let [action (instruction 0)
               value (scan-number (string/slice instruction 1))]]
    (case action
      (chr "N") (-= y value)
      (chr "S") (+= y value)
      (chr "E") (+= x value)
      (chr "W") (-= x value)
      (chr "L") (set facing (mod (- facing (n)) 4))
      (chr "R") (set facing (mod (+ facing (n)) 4))
      (chr "F") (case facing
                  0 (-= y value) # north
                  1 (+= x value) # east
                  2 (+= y value) # south
                  3 (-= x value) # west
                  (error value))
      (error instruction)))

  (+ (math/abs x) (math/abs y)))

(defmacro swap []
  ~(do (set wx (bxor wx wy))
       (set wy (bxor wy wx))
       (set wx (bxor wx wy))))

(defn run2 [instructions]
  (var x 0)
  (var y 0)
  (var wx 10)
  (var wy -1)

  (loop [instruction :in instructions
         :let [action (instruction 0)
               value (scan-number (string/slice instruction 1))]]
    (case action
      (chr "N") (-= wy value)
      (chr "S") (+= wy value)
      (chr "E") (+= wx value)
      (chr "W") (-= wx value)
      (chr "L") (repeat (n) (swap) (*= wy -1))
      (chr "R") (repeat (n) (swap) (*= wx -1))
      (chr "F") (do (+= x (* wx value)) (+= y (* wy value)))
      (error instruction)))

  (+ (math/abs x) (math/abs y)))

(print "Part 1: " (run input))
(print "Part 2: " (run2 input))
