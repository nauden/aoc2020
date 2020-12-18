(defn parse3 [str]
  (def grid @{})

  (loop [[y line] :pairs (string/split "\n" str)
         [x c] :pairs line
         :let [p (string/format "%d %d 0" x y)]
         :when (= c (chr "#"))]
    (put grid p true))
  grid)

(defn neighbors3 [[x y z]]
  (seq [dx :range-to [-1 1]
        dy :range-to [-1 1]
        dz :range-to [-1 1]
        :when (not= 0 dx dy dz)]
    (string/format "%d %d %d" (+ x dx) (+ y dy) (+ z dz))))


(defn step3 [grid &opt step]
  (default step 6)
  (if (zero? step)
     (length grid)
     (let [next-grid @{}
           inactive @{}]

       # 1. active with 2 or 3 neighbors
       (loop [s :keys grid
              :let [p (map scan-number (string/split " " s))
                    ns (neighbors3 p)]]

         (var nsc 0)
         (each n ns
           (if (grid n)
             (++ nsc)
             (put inactive n true)))

         (when (or (= nsc 2) (= nsc 3))
           (put next-grid s true)))

       # 2. inactive with 3 neighbors
       (loop [s :keys inactive
              :let [p (map scan-number (string/split " " s))
                    ns (neighbors3 p)]]

         (var nsc 0)
         (each n ns
           (when (grid n)
             (++ nsc)))

         (when (= nsc 3)
           (put next-grid s true)))

       (step3 next-grid (dec step)))))

(defn parse4 [str]
  (def grid @{})

  (loop [[y line] :pairs (string/split "\n" str)
         [x c] :pairs line
         :let [p (string/format "%d %d 0 0" x y)]
         :when (= c (chr "#"))]
    (put grid p true))
  grid)

(defn neighbors4 [[x y z w]]
  (seq [dx :range-to [-1 1]
        dy :range-to [-1 1]
        dz :range-to [-1 1]
        dw :range-to [-1 1]
        :when (not= 0 dx dy dz dw)]
    (string/format "%d %d %d %d" (+ x dx) (+ y dy) (+ z dz) (+ w dw))))


(defn step4 [grid &opt step]
  (default step 6)
  (prinf "\r%d" step)
  (flush)
  (if (zero? step)
     (do (prin "\r      \r") (flush) (length grid))
     (let [next-grid @{}
           inactive @{}]

       # 1. active with 2 or 3 neighbors
       (loop [s :keys grid
              :let [p (map scan-number (string/split " " s))
                    ns (neighbors4 p)]]

         (var nsc 0)
         (each n ns
           (if (grid n)
             (++ nsc)
             (put inactive n true)))

         (when (or (= nsc 2) (= nsc 3))
           (put next-grid s true)))

       # 2. inactive with 3 neighbors
       (loop [s :keys inactive
              :let [p (map scan-number (string/split " " s))
                    ns (neighbors4 p)]]

         (var nsc 0)
         (each n ns
           (when (grid n)
             (++ nsc)))

         (when (= nsc 3)
           (put next-grid s true)))

       (step4 next-grid (dec step)))))

(def input
  (->> (slurp "input")
       (string/trim)))

(def test `.#.
..#
###`)

(print "Part 1: " (step3 (parse3 input)))
(print "Part 2: " (step4 (parse4 input)))
