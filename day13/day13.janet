(defn parse-busses [lines]
  (let [departure (scan-number (first lines))
        busses-str (string/split "," (last lines))
        busses (map scan-number (filter (partial not= "x") busses-str))
        offs (seq [[i s] :pairs busses-str
                   :when (not= s "x")
                   :let [id (scan-number s)]]
               (- id i))]

    [departure busses offs]))

(defn first-bus [[departure busses]]
  (let [wait-times (reduce
                     (fn [a bus]
                       (put a bus (- bus (mod departure bus))))
                     @{}
                     busses)
        bus-id (index-of (min ;(values wait-times)) wait-times)]

    (* bus-id (wait-times bus-id))))

(defmacro swap [a b]
  ~(do
     (set ,a (bxor ,a ,b))
     (set ,b (bxor ,b ,a))
     (set ,a (bxor ,a ,b))))

# https://rosettacode.org/wiki/Chinese_remainder_theorem
(defn mul-inv [a0 b0]
  (var a (int/s64 a0))
  (var b (int/s64 b0))
  (var x0 (int/s64 0))
  (var r (int/s64 1))

  (if (one? b)
    (set r 1)
    (while (> a (int/s64 1))
      (def q (/ a b))
      (set a (mod a b))
      (swap a b)
      (-= r (* q x0))
      (swap x0 r)))

 (if (neg? r)
   (+ r b0)
   r))

(defn chinese-remainder [[_ xs rs]]
  (def prod (product xs))
  (def sum_ (reduce
              (fn [s [i x]]
                (def p (math/trunc (/ prod x)))
                (+ s (* (rs i)
                        (* (mul-inv p x) p))))
              (int/s64 0)
              (pairs xs)))

  (mod sum_ prod))


(def input
  (->> (slurp "input")
       (string/trim)
       (string/split "\n")
       (parse-busses)))

(print "Part 1: " (first-bus input))
(print "Part 2: " (chinese-remainder input))
