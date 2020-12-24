(defn make-decks [str]
  (let [[p1 p2] (->> (string/trim str)
                     (string/split "\n\n")
                     (map (partial string/split "\n")))

        deck1 (map scan-number (drop 1 p1))
        deck2 (map scan-number (drop 1 p2))]
    [deck1 deck2]))

(defn score [deck]
  (reduce
    (fn [a [i c]] (+ a (* (inc i) c)))
    0
    (pairs (reverse deck))))

(defn play-game [deck1 deck2]
    (cond
      (empty? deck1) (score deck2)
      (empty? deck2) (score deck1)
      (let [card1 (first deck1)
            card2 (first deck2)
            next-deck1 (drop 1 deck1)
            next-deck2 (drop 1 deck2)]

        (if (< card1 card2)
          # player 2 won
          (play-game next-deck1 (tuple ;next-deck2 card2 card1))

          # player 1 won
          (play-game (tuple ;next-deck1 card1 card2) next-deck2)))))

(defn play-recursive-game [deck1 deck2 &opt game seen]
    (default game 1)
    (default seen @{})
    (def key (string/format "%p %p" deck1 deck2))

    (cond
      # player 2 wins this game
      (empty? deck1) (if (one? game) (score deck2) 2)
      # player 1 wins this game
      (empty? deck2) (if (one? game) (score deck1) 1)

      # cards seen before, player 1 wins this game
      (seen key) (if (one? game) (score deck1) 1)

      (let [card1 (first deck1)
            card2 (first deck2)
            next-deck1 (drop 1 deck1)
            next-deck2 (drop 1 deck2)]
        (put seen key true)

        (def winner (if (and (<= card1 (length next-deck1))
                             (<= card2 (length next-deck2)))
                      # play recursive game
                      (play-recursive-game (take card1 next-deck1)
                                           (take card2 next-deck2)
                                           (inc game))
                      # normal round
                      (if (< card1 card2) 2 1)))

        (case winner
          # player 1 won this round
          1 (play-recursive-game (tuple ;next-deck1 card1 card2) next-deck2
                                 game seen)

          # player 2 won this round
          2 (play-recursive-game next-deck1 (tuple ;next-deck2 card2 card1)
                                 game seen)

          # 'winner' is actually the score for game 1
          winner))))

(def [deck1 deck2]
  (make-decks (slurp "input")))

(print "Part 1: " (play-game deck1 deck2))
(print "Part 2: " (play-recursive-game deck1 deck2))
