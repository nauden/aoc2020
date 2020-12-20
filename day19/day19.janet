(def rule-peg
  (peg/compile
    ~{:num (cmt ':d+ ,scan-number)
      :char (* "\"" ':a "\"")
      :list (/ (some (* :s* :num)) ,tuple)
      :sublist (/ (* :list " | " :list) ,tuple)
      :main (* :num ": " (+ :char :sublist (/ :list ,tuple)))}))

(defn parse-input [str]
  (let [[rules-str messages] (string/split "\n\n" str)
        rules (reduce
                (fn [a rule-str]
                  (def [id rule] (peg/match rule-peg rule-str))
                  (put a id rule))
                @{} (string/split "\n" rules-str))]

    [rules (string/split "\n" messages)]))

(defn match-rule [rules str &opt ids]
  (default ids [0])

  (cond
    (and (empty? str)
         (empty? ids)) true

    (or (empty? str)
        (empty? ids)) false

    (< (length str)
       (length ids)) false

    (let [rule (rules (first ids))]
      (case (type rule)
        :string (if (= rule (string/slice str 0 1))
                  (match-rule rules (drop 1 str) (drop 1 ids))
                  false)
        (some (fn [new-ids]
                (match-rule rules str (tuple ;new-ids ;(drop 1 ids))))
              rule)))))

(defn count-valid [rules messages]
  (count (partial match-rule rules) messages))

(defn replace-rules [rules]
  (def new-rules (table/clone rules))
  (put new-rules 8 [[42] [42 8]])
  (put new-rules 11 [[42 31] [42 11 31]]))

(def [rules messages]
  (->> (slurp "input")
       (string/trim)
       (parse-input)))

(print "Part 1: " (count-valid rules messages))
(print "Part 2: " (count-valid (replace-rules rules) messages))
