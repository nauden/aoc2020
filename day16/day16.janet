(def rule-peg
  (peg/compile
    ~{:num (cmt ':d+ ,scan-number)
      :range (* :s* :num "-" :num)
      :name (some (+ :a :s))
      :main (* ':name ": " :range " or " :range)}))

(defn match-any-rule [rules n]
  (find-index
    (fn [[lo1 hi1 lo2 hi2]]
      (or (<= lo1 n hi1)
          (<= lo2 n hi2)))
    (values rules)))

(defn invalid? [rules xs]
  (cond
    (empty? xs) false
    (not (match-any-rule rules (first xs))) (first xs)
    (invalid? rules (drop 1 xs))))

(defn parse-input [str]
  (def rules @{})
  (def lines (string/split "\n" (string/trim str)))

  (var i 0)
  (loop [line :in lines
         :until (empty? line)]
    (def m (peg/match rule-peg line))
    (put rules (first m) (drop 1 m))
    (++ i))

  (+= i 2)
  (def my-ticket
    (->> (lines i)
         (string/split ",")
         (map scan-number)))

  (+= i 3)
  (var error-rate 0)
  (def nearby-tickets @[])
  (loop [line :in (drop i lines)
         :let [ns (map scan-number (string/split "," line))
               inv (invalid? rules ns)]]
    (if inv
      (+= error-rate inv)
      (array/push nearby-tickets ns)))

  {:rules rules
   :my-ticket my-ticket
   :nearby-tickets nearby-tickets
   :error-rate error-rate})


(def input (parse-input (slurp "input")))

(print "Part 1: " (input :error-rate))

(defn matches-rule? [rules name n]
  (def [lo1 hi1 lo2 hi2] (rules name))

  (or (<= lo1 n hi1)
      (<= lo2 n hi2)))

(defn determine-fields [{:rules rules
                         :my-ticket my-ticket
                         :nearby-tickets nearby-tickets}]

  (def all-tickets (array my-ticket ;nearby-tickets))
  (def l (length my-ticket))

  (def cand @[])
  (repeat l
    (array/push cand (table ;(array ;(interpose true (keys rules)) true))))

  (def fields @{})
  (while (> l (length fields))
    (loop [xs :in all-tickets
           i :range [0 l]
           :when (< 1 (length (cand i)))
           :let [x (xs i)]]

      (eachk rule (cand i)
        (unless (matches-rule? rules rule x)
          (put-in cand [i rule] nil)))

      (loop [k :range [0 l]
             :let [found (first (keys (cand k)))]
             :when (and (one? (length (cand k)))
                        (not (get fields found false)))]
          (put fields found k)
          (for j 0 l
            (when (not= j k)
              (put-in cand [j found] nil))))))

  (product
    (seq [[name i] :pairs fields
          :when (string/has-prefix? "departure" name)]
      (my-ticket (fields name)))))


(defn determine-fields2 [{:rules rules
                          :my-ticket my-ticket
                          :nearby-tickets nearby-tickets}]

  (def all-tickets (array my-ticket ;nearby-tickets))
  (def all-rules (keys rules))

  (def l (length my-ticket))
  (def ll (length all-tickets))

  (def cand @{})
  (loop [i :range [0 l]
         :let [ns (seq [ticket :in all-tickets] (ticket i))]
         rule :in all-rules]
    (when (all (partial matches-rule? rules rule) ns)
       (put-in cand [i rule] true)))

  (def fields @{})
  (while (> l (length fields))
    (loop [i :range [0 l]
           :let [rule (first (keys (cand i)))]
           :when (and (not (fields rule))
                      (one? (length (cand i))))]
      (for j 0 l
       (when (not= j i)
         (put-in cand [j rule] nil)))
      (put fields rule i)))

  (product
    (seq [[name i] :pairs fields
          :when (string/has-prefix? "departure" name)]
      (my-ticket (fields name)))))

    
    
  
    
    
          

(print "Part 2: " (determine-fields2 input))

