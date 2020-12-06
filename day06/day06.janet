(def input
  (->> (slurp "input")
       (string/trim)
       (string/split "\n\n")))

(defn count-yes-answers [group]
  (->> (string/replace-all "\n" "" group)
       (frequencies)
       (length)))

(defn count-all-yes-answers [group]
  (def people (string/split "\n" group))
  (def l (length people))
  (def f (frequencies (string/replace-all "\n" "" group)))

  (count (partial = l) (values f)))

(print "Part 1: " (sum (map count-yes-answers input)))
(print "Part 2: " (sum (map count-all-yes-answers input)))
