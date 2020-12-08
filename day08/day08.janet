(def instruction
  (peg/compile
    ~{:num (cmt '(* (set "-+") :d+) ,scan-number)
      :ins (cmt '(+ "acc" "jmp" "nop") ,keyword)
      :main (* :ins :s :num)}))

(def input
  (->> (slurp "input")
       (string/trim)
       (string/split "\n")
       (map (partial peg/match instruction))))

(defn run-program [program]
  (var ip 0)
  (var acc 0)
  (var did-finish false)
  (def seen @{})

  (while (not (seen ip))
    (put seen ip true)
    (when (>= ip (length program))
      (set did-finish true)
      (break))

    (def [op v] (program ip))
    (case op
      :acc (do (+= acc v) (++ ip))
      :jmp (+= ip v)
      :nop (++ ip)))

  [acc did-finish])

(defn fix-program [program]
  (var a 0)
  (def swap {:jmp :nop :nop :jmp})

  (for i 0 (length program)
    (def [op v] (program i))
    (when (not= op :acc)
        (let [[acc did-finish] (run-program (put program i [(swap op) v]))]
          (put program i [op v])
          (when did-finish
            (set a acc)
            (break)))))
  a)

(print "Part 1: " (first (run-program input)))
(print "Part 2: " (fix-program input))
