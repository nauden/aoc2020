(defn evaluate-inner [exp]
  (while (< 1 (length exp))
    (def l (scan-number (exp 0)))
    (def r (scan-number (exp 2)))
    (case (exp 1)
       "+" (put exp 0 (string (+ l r)))
       "*" (put exp 0 (string (* l r)))
      (assert false "evaluate-inner"))
    (array/remove exp 1 2))
  exp)

(defn find-innermost-exp [str]
  (var parens 0)
  (def inner
    (seq [[i c] :pairs str
          :when (or (= c (chr "("))
                    (= c (chr ")")))]
      (if (= c (chr "("))
        (do (++ parens) [parens :open i])
        (do (-- parens) [parens :close i]))))

  (def [l t start] (max ;inner))
  (assert (= :open t))

  (def len (string/find ")" (string/slice str start)))

  #(pp (string/slice str (inc start) (+ start len)))
  [(inc start) (+ start len)])

(defn evaluate [str]
  (if (string/find "(" str)
    (let [[start end] (find-innermost-exp str)
          res (->> (string/slice str start end)
                   (string/split " ")
                   (evaluate-inner)
                   (first)
                   (string))
          new-str (string
                    (string/slice str 0 (dec start))
                    res
                    (string/slice str (inc end)))]
      (evaluate new-str))

    # no parens
    (scan-number ;(evaluate-inner (string/split " " str)))))

(defn evaluate-inner2 [exp]
  (var i (find-index (partial = "+") exp))
  (while i
    (def l (scan-number (exp (dec i))))
    (def r (scan-number (exp (inc i))))
    (put exp (dec i) (string (+ l r)))
    (array/remove exp i 2)
    (set i (find-index (partial = "+") exp)))

  (while (< 1 (length exp))
    (def l (scan-number (exp 0)))
    (def r (scan-number (exp 2)))
    (put exp 0 (string (* l r)))
    (array/remove exp 1 2))
  exp)


(defn evaluate2 [str]
  (if (string/find "(" str)
    (let [[start end] (find-innermost-exp str)
          res (->> (string/slice str start end)
                   (string/split " ")
                   (evaluate-inner2)
                   (first)
                   (string))
          new-str (string
                    (string/slice str 0 (dec start))
                    res
                    (string/slice str (inc end)))]
      (evaluate2 new-str))

    # no parens
    (scan-number ;(evaluate-inner2 (string/split " " str)))))

(def input
  (->> (slurp "input")
       (string/trim)
       (string/split "\n")))

(print "Part 1: " (sum (map evaluate input)))
(print "Part 2: " (sum (map evaluate2 input)))
