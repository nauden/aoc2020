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

  (loop [_ :iterate (not (seen ip))]
    (when (>= ip (length program))
      (set did-finish true)
      (break))

    (def [op v] (program ip))
    (put seen ip true)
    (case op
      :acc (do (+= acc v) (++ ip))
      :jmp (+= ip v)
      :nop (++ ip)))

  [acc did-finish])

(defn fix-program [program]
  (var done false)

  (last
    (seq [i :range (0 (length program))
          :until done
          :let [[op v] (program i)]]
      (case op
        :jmp (let [copy (array/concat @[] program)
                   [acc did-finish] (-> (put copy i [:nop v]) run-program)]
                (when did-finish
                  (set done true)
                  acc))

        :nop (let [copy (array/concat @[] program)
                   [acc did-finish] (-> (put copy i [:jmp v]) run-program)]
                (when did-finish
                  (set done true)
                  acc))))))

(print "Part 1: " (first (run-program input)))
(print "Part 2: " (fix-program input))
