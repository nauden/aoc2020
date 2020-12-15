(def input (string/trim (slurp "input")))

(def mem-peg
  (peg/compile
    ~{:num (cmt ':d+ ,scan-number)
      :main (* "mem[" :num "] = " :num)}))

(defn initialize [str]
  (var and-mask nil)
  (var or-mask nil)
  (def mem @{})

  (loop [line :in (string/split "\n" str)]
    (if (string/has-prefix? "mask" line)
      (let [mask-str (string "2r" (string/slice line 7))]
         (set and-mask (int/u64 (scan-number (string/replace-all "X" "1" mask-str))))
         (set or-mask (int/u64 (scan-number (string/replace-all "X" "0" mask-str)))))

      (let [[adr val] (peg/match mem-peg line)
             v (-> (band val and-mask) (bor or-mask))]
        (put mem adr v))))

  (sum (values mem)))

(defn addresses [address floating-bits]
  (if (empty? floating-bits)
    [address]
    (let [bit (first floating-bits)
          adr1 (bor address (blshift (int/u64 1) bit)) # set bit to 1
          adr0 (bxor adr1 (blshift (int/u64 1) bit))]  # flip bit (set to zero)
      (array
        ;(addresses adr0 (drop 1 floating-bits))
        ;(addresses adr1 (drop 1 floating-bits))))))


(defn initialize2 [str]
  (var floating-bits nil)
  (var mask nil)
  (def mem @{})

  (loop [line :in (string/split "\n" str)]
    (if (string/has-prefix? "mask" line)
      (let [mask-str (string/slice line 7)]
        (set floating-bits (string/find-all "X" (string/reverse mask-str)))
        (set mask (int/u64
                    (scan-number
                     (string "2r" (string/replace-all "X" "0" mask-str))))))

      (let [[adr val] (peg/match mem-peg line)]
        (reduce
          (fn [m a]
            (put m (string a) val))
          mem
          (addresses (bor (int/u64 adr) mask) floating-bits)))))

  (sum (values mem)))


(print "Part 1: " (initialize input))
(print "Part 2: " (initialize2 input))
