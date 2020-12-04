(defn parse-passport [str]
  (table
    ;(->> (string/replace-all "\n" " " str)
          (string/split " ")
          (map (partial string/split ":"))
          (flatten))))

(def input
  (->> (slurp "input")
       (string/trim)
       (string/split "\n\n")
       (map parse-passport)))

(defn valid? [passport]
  (and (passport "byr")
       (passport "iyr")
       (passport "eyr")
       (passport "hgt")
       (passport "hcl")
       (passport "ecl")
       (passport "pid")))

(print "Part 1: " (count valid? input))


(defn valid-number? [str lo hi]
  (<= lo (scan-number str) hi))

(defn valid-height? [height]
  (def h (string/slice height 0 -3))
  (def u (string/slice height -3))

  (match u
    "cm" (valid-number? h 150 193)
    "in" (valid-number? h 59 76)))

(def hair-color
  (peg/compile
    ~{:main (* "#" (6 (range "09" "af")) -1)}))

(def passport-id
  (peg/compile
    ~{:main (* (9 (range "09")) -1)}))

(defn valid2? [passport]
  (and
     (valid? passport)
     (valid-number? (passport "byr") 1920 2002)
     (valid-number? (passport "iyr") 2010 2020)
     (valid-number? (passport "eyr") 2020 2030)
     (valid-height? (passport "hgt"))
     (peg/match hair-color (passport "hcl"))
     (find (partial = (passport "ecl"))
           ["amb" "blu" "brn" "gry" "grn" "hzl" "oth"])
     (peg/match passport-id (passport "pid"))))

(print "Part 2: " (count valid2? input))