(def bag
  (peg/compile
    ~{:num (cmt ':d+ ,scan-number)
      :name '(* :a+ :s :a+)
      :empty (/ "no other bags." (0, ""))
      :child (* :num :s :name (* :s "bag" (? "s") (? (set ",.")) (? :s)))
      :main (* :name " bags contain " (some (+ :empty (cmt :child ,tuple))))}))

(defn parse-bag [bags str]
  (def b (peg/match bag str))
  (put bags (first b) (drop 1 b)))

(defn contains_shiny_gold [bags]
  (let [parents @{}
        seen @{}
        q @["shiny gold"]]

    (loop [[child ps] :pairs bags
           [_ parent] :in ps
           :when (not= parent "")]
      (update parents parent |(if (nil? $) @[child] (array/push $ child))))

    (while (not (empty? q))
      (def color (array/pop q))

      (loop [c :in (get parents color @[])
             :when (not (seen c))]
        (put seen c true)
        (array/push q c)))

    (length seen)))

(def input
  (->> (slurp "input")
       (string/trim)
       (string/split "\n")
       (reduce parse-bag @{})))

(print "Part 1: " (contains_shiny_gold input))

(defn bags-req [bags color]
  (reduce
    (fn [a [n c]] (+ a (* n (bags-req bags c))))
    1
    (get bags color [])))

(print "Part 2: " (dec (bags-req input "shiny gold")))
