(ns Projects.own.advent-of-code-2019.day5)

(def puzzle-input
  [3,225,1,225,6,6,1100,1,238,225,104,0,1002,114,19,224,1001,224,-646,224,4,224,102,8,223,223,1001,224,7,224,1,223,224,223,1101,40,62,225,1101,60,38,225,1101,30,29,225,2,195,148,224,1001,224,-40,224,4,224,1002,223,8,223,101,2,224,224,1,224,223,223,1001,143,40,224,101,-125,224,224,4,224,1002,223,8,223,1001,224,3,224,1,224,223,223,101,29,139,224,1001,224,-99,224,4,224,1002,223,8,223,1001,224,2,224,1,224,223,223,1101,14,34,225,102,57,39,224,101,-3420,224,224,4,224,102,8,223,223,1001,224,7,224,1,223,224,223,1101,70,40,225,1102,85,69,225,1102,94,5,225,1,36,43,224,101,-92,224,224,4,224,1002,223,8,223,101,1,224,224,1,224,223,223,1102,94,24,224,1001,224,-2256,224,4,224,102,8,223,223,1001,224,1,224,1,223,224,223,1102,8,13,225,1101,36,65,224,1001,224,-101,224,4,224,102,8,223,223,101,3,224,224,1,223,224,223,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,8,677,226,224,1002,223,2,223,1006,224,329,1001,223,1,223,1108,226,226,224,1002,223,2,223,1005,224,344,101,1,223,223,1108,226,677,224,1002,223,2,223,1006,224,359,101,1,223,223,107,226,226,224,1002,223,2,223,1005,224,374,101,1,223,223,1107,226,226,224,1002,223,2,223,1005,224,389,101,1,223,223,107,677,677,224,102,2,223,223,1006,224,404,101,1,223,223,1008,226,226,224,1002,223,2,223,1006,224,419,101,1,223,223,108,677,226,224,1002,223,2,223,1006,224,434,101,1,223,223,1108,677,226,224,102,2,223,223,1005,224,449,101,1,223,223,1008,677,226,224,102,2,223,223,1006,224,464,1001,223,1,223,108,677,677,224,102,2,223,223,1005,224,479,101,1,223,223,7,677,677,224,102,2,223,223,1005,224,494,1001,223,1,223,8,226,677,224,102,2,223,223,1006,224,509,101,1,223,223,107,677,226,224,1002,223,2,223,1005,224,524,1001,223,1,223,7,677,226,224,1002,223,2,223,1005,224,539,1001,223,1,223,1007,226,677,224,1002,223,2,223,1005,224,554,1001,223,1,223,8,677,677,224,102,2,223,223,1006,224,569,101,1,223,223,7,226,677,224,102,2,223,223,1006,224,584,1001,223,1,223,1008,677,677,224,102,2,223,223,1005,224,599,101,1,223,223,1007,677,677,224,1002,223,2,223,1006,224,614,101,1,223,223,1107,677,226,224,1002,223,2,223,1006,224,629,101,1,223,223,1107,226,677,224,1002,223,2,223,1006,224,644,101,1,223,223,1007,226,226,224,102,2,223,223,1005,224,659,1001,223,1,223,108,226,226,224,102,2,223,223,1006,224,674,101,1,223,223,4,223,99,226])


{:pos 0
 :inputs []
 :outputs []
 :ints ints}

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

(defn input [state modes]
  (let [[addr state] (read-next state)
        [i state] (read-input state)]
    (update state :ints #(assoc % addr i))))

(defn output [state modes]
  (let [[addr state] (read-next state)
        [mode1] modes
        v (read-at state addr mode1)]
    (update state :outputs conj v)))

(comment
  (input {:pos 1
          :inputs [1234]
          :ints [3 0 99]})

  )

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
  (println "before equals state" state)
  (let [[p1 state] (read-next state)
        [p2 state] (read-next state)
        [p3 state] (read-next state)
        [mode1 mode2 mode3] modes
        p1-val (read-at state p1 mode1)
        p2-val (read-at state p2 mode2)]
    (println "equals " [p1 p2 p3] " vals" [p1-val p2-val] "modes" modes)
    (if (= p1-val p2-val)
      (update state :ints #(assoc % p3 1))
      (update state :ints #(assoc % p3 0)))))

(defn halt [state]
  (assoc state :halted true))

(defn exec [state]
  (if (:halted state)
    state
    (recur
     (let [[opcode next-state] (read-next state)
           [opcode2 opcode1 mode1 mode2 mode3] (reverse (digits opcode))
           opcode2 (or opcode2 0)
           opcode1 (or opcode1 0)
           mode1 (or mode1 0)
           mode2 (or mode2 0)
           mode3 (or mode3 0)
           modes [mode1 mode2 mode3]]
       (println "exec - pos:" (:pos state) "opcode" opcode "ints" (:ints state))
       (case [opcode1 opcode2]
         [0 1] (add next-state modes)
         [0 2] (mult next-state modes)
         [0 3] (input next-state modes)
         [0 4] (output next-state modes)
         [0 5] (jump-if-true next-state modes)
         [0 6] (jump-if-false next-state modes)
         [0 7] (less-than next-state modes)
         [0 8] (equals next-state modes)
         [9 9] (halt next-state))))))

(comment
  ;; part 1
  (exec {:pos 0
         :inputs [1]
         :outputs []
         :ints puzzle-input})

  ;; test 1
  (exec {:pos 0
         :inputs [7]
         :outputs []
         :ints [3,9,8 9,10,9,4,9,99,-1,8]})

  (exec {:pos 0
         :inputs [9]
         :outputs []
         :ints [3,9,8,9,10,9,4,9,99,-1,8]})

  ;; test 2
  (exec {:pos 0
         :inputs [8]
         :outputs []
         :ints [3,3,1108,-1,8,3,4,3,99]})



  (exec {:pos 0
         :inputs [8]
         :outputs []
         :ints [3,9,8,9,10,9,4,9,99,-1,8]})

  ;; part 2
  (exec {:pos 0
         :inputs [5]
         :outputs []
         :ints puzzle-input})
  )
