(ns Projects.own.advent-of-code-2019.day7)

(def puzzle-input
  [3,8,1001,8,10,8,105,1,0,0,21,42,55,76,89,114,195,276,357,438,99999,3,9,1001,9,3,9,1002,9,3,9,1001,9,3,9,1002,9,2,9,4,9,99,3,9,102,2,9,9,101,5,9,9,4,9,99,3,9,102,3,9,9,101,5,9,9,1002,9,2,9,101,4,9,9,4,9,99,3,9,102,5,9,9,1001,9,3,9,4,9,99,3,9,1001,9,4,9,102,5,9,9,1001,9,5,9,1002,9,2,9,101,2,9,9,4,9,99,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,99,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,99,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,99
   ])

(defn read-next [state]
  [(nth (:ints state) (:pos state))
   (update state :pos inc)])

(defn read-at [state addr mode]
  (if (= mode 0)
    (nth (:ints state) addr)
    addr))

(defn add [state modes]
  (let [[input1 state] (read-next state)
        [input2 state] (read-next state)
        [output state] (read-next state)
        [mode1 mode2] modes]
    (update state :ints #(assoc % output (+ (read-at state input1 mode1)
                                            (read-at state input2 mode2))))))

(defn mult [state modes]
  (let [[input1 state] (read-next state)
        [input2 state] (read-next state)
        [output state] (read-next state)
        [mode1 mode2] modes]
    (update state :ints #(assoc % output (* (read-at state input1 mode1)
                                            (read-at state input2 mode2))))))

(defn read-input [state]
  (let [[x & xs] (:inputs state)]
    [x (assoc state :inputs xs)]))

(defn write-input [state input]
  (update state :inputs (fnil conj []) input))

(defn read-output [state]
  (let [[x & xs] (:outputs state)]
    [x (assoc state :outputs xs)]))

(defn input [state modes]
  (let [[addr state] (read-next state)
        [i state] (read-input state)]
    (update state :ints #(assoc % addr i))))

(defn output [state modes]
  (let [[addr state] (read-next state)
        [mode1] modes
        v (read-at state addr mode1)]
    (update state :outputs (fnil conj []) v)))

(comment
  (output
   {:pos 1
    :inputs [1234]
    :outputs []
    :ints [4 2 99]}))

(defn digits [n]
  (map (fn [n] (Character/digit n 10)) (str n)))

(defn jump-to [state pos]
  (assoc state :pos pos))

(defn jump-if-true [state modes]
  (let [[p1 state] (read-next state)
        [p2 state] (read-next state)
        [mode1 mode2] modes
        p1-val (read-at state p1 mode1)
        p2-val (read-at state p2 mode2)]
    (if-not (zero? p1-val)
      (jump-to state p2-val)
      state)))

(defn jump-if-false [state modes]
  (let [[p1 state] (read-next state)
        [p2 state] (read-next state)
        [mode1 mode2] modes
        p1-val (read-at state p1 mode1)
        p2-val (read-at state p2 mode2)]
    (if (zero? p1-val)
      (jump-to state p2-val)
      state)))

(defn less-than [state modes]
  (let [[p1 state] (read-next state)
        [p2 state] (read-next state)
        [p3 state] (read-next state)
        [mode1 mode2 mode3] modes
        p1-val (read-at state p1 mode1)
        p2-val (read-at state p2 mode2)]
    (if (< p1-val p2-val)
      (update state :ints #(assoc % p3 1))
      (update state :ints #(assoc % p3 0)))))

(defn equals [state modes]
  (let [[p1 state] (read-next state)
        [p2 state] (read-next state)
        [p3 state] (read-next state)
        [mode1 mode2 mode3] modes
        p1-val (read-at state p1 mode1)
        p2-val (read-at state p2 mode2)]
    (if (= p1-val p2-val)
      (update state :ints #(assoc % p3 1))
      (update state :ints #(assoc % p3 0)))))

(defn halt [state]
  (assoc state :halted true))

(defn exec-once [state]
  (let [[opcode next-state] (read-next state)
        [opcode2 opcode1 mode1 mode2 mode3] (reverse (digits opcode))
        opcode2 (or opcode2 0)
        opcode1 (or opcode1 0)
        mode1 (or mode1 0)
        mode2 (or mode2 0)
        mode3 (or mode3 0)
        modes [mode1 mode2 mode3]]
    (case [opcode1 opcode2]
      [0 1] (add next-state modes)
      [0 2] (mult next-state modes)
      [0 3] (input next-state modes)
      [0 4] (output next-state modes)
      [0 5] (jump-if-true next-state modes)
      [0 6] (jump-if-false next-state modes)
      [0 7] (less-than next-state modes)
      [0 8] (equals next-state modes)
      [9 9] (halt next-state))))

#_(defn exec [state]
  (if (:halted state)
    state
    (recur (exec-once state))))

(defn exec [state]
  (if (or (:halted state) (seq (:outputs state)))
    state
    (recur (exec-once state))))

#_(defn exec-until-first-output [state]
  (cond
    (:halted state) state
    (seq (:outputs state)) state
    :else (recur (exec-once state))))

(defn start-amp [id phase code]
  {:id id
   :pos 0
   :inputs [phase]
   :outputs []
   :ints code})

#_(defn run-amps [phases code]
  (println "---")
  (reduce
   (fn [input state]
     (let [state (-> state
                     (write-input input)
                     exec)
           [output] (:outputs state)
           halted (:halted state)]
       (println "exec amp" (:id state) " input:" input "output:" output "halted: " halted)
       output))
   0
   (map-indexed (fn [i phase] (start-amp i phase code)) phases)))

(defn run-amps-loop [phases code]
  (println "--- [loop mode]")
  (loop [input 0
         amps (vec (map-indexed (fn [i phase] (start-amp i phase code)) phases))]
    (let [[amp & rest-amps] amps]
      (if-not amp
        input
        (let [state (-> amp
                        (write-input input)
                        (exec))
              [output state] (read-output state)
              halted (:halted state)
              amps (if halted
                     (vec rest-amps)
                     (conj (vec rest-amps) state))]
          (println "exec amp" (:id amp) "input:" input "output:" output "halted:" halted)
          (recur (or output input) amps))))))

(defn optimal-phases [code]
  (->> (for [p1 (range 5)
             p2 (range 5)
             p3 (range 5)
             p4 (range 5)
             p5 (range 5)
             :when (distinct? p1 p2 p3 p4 p5)]
         (let [phases [p1 p2 p3 p4 p5]]
           [phases (run-amps-loop phases code)]))
       (sort-by #(- (second %)))
       first))

(defn optimal-phases-loop [code]
  (->> (for [p1 (range 5 10)
             p2 (range 5 10)
             p3 (range 5 10)
             p4 (range 5 10)
             p5 (range 5 10)
             :when (distinct? p1 p2 p3 p4 p5)]
         (let [phases [p1 p2 p3 p4 p5]]
           [phases (run-amps-loop phases code)]))
       (sort-by #(- (second %)))
       first))

(comment
  ;; test
  (run-amps [1 0 4 3 2]
            [3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,
             1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0])

  (-> (start-amp "test " 1
                 [3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,
                  1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0])
      (write-input 0)
      (exec))

  (run-amps-loop [1 0 4 3 2]
                 [3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,
                  1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0])

  (run-amps [1 1 1 1 1]
            [3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,
             1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0])

  (->> (optimal-phases [ 3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,
                        1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0 ]))

  (->> (optimal-phases puzzle-input))

  (->> (optimal-phases-loop [3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,
                             -5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,
                             53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10]))

  (->> (optimal-phases-loop puzzle-input))

  )
