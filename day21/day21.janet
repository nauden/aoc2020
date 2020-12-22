(def food
  (peg/compile
    ~{:list (/ (some (* ':a+ (? ",") (? :s))) ,tuple)
      :main (* :list  "(contains " :list ")")}))

(defn parse-food [str]
  (peg/match food str))

(defn intersection [a b]
  (seq [v :in a
        :when (find (partial = v) b)] v))

(defn determine-allergens [list]
  (def cand @{})

  (loop [[ingredients allergens] :in list
         allergen :in allergens]
    (if (get cand allergen false)
      (update cand allergen (partial intersection ingredients))
      (put cand allergen ingredients)))

  (while (some |(< 1 (length $)) (values cand))
    (loop [[allergen ingredients] :pairs cand
           :when (one? (length ingredients))
           :let [ingredient (first ingredients)]]
      (loop [[other-allergen other-ing] :pairs cand
             :when (not= allergen other-allergen)]
        (update cand other-allergen (partial filter (partial not= ingredient))))))


  (eachk allergen cand
    (update cand allergen first))

  cand)

(defn count-safe-ingredients [list allergens]
  (def bad-ingredients (table ;(interpose true (values allergens)) true))

  (sum
    (seq [[ingredients _] :in list
          ingredient :in ingredients
          :when (not (bad-ingredients ingredient))]
      1)))

(defn dangerous-ingredients [allergens]
  (string/join
    (seq [allergen :in (sorted (keys allergens))]
      (allergens allergen))
    ","))

(def input
  (->> (slurp "input")
       (string/trim)
       (string/split "\n")
       (map parse-food)))

(def allergens (determine-allergens input))

(print "Part 1: " (count-safe-ingredients input allergens))
(print "Part 2: " (dangerous-ingredients allergens))
