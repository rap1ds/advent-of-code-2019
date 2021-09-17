(ns Projects.own.advent-of-code-2019.day11)

(def puzzle-input
  [3,8,1005,8,330,1106,0,11,0,0,0,104,1,104,0,3,8,102,-1,8,10,101,1,10,10,4,10,1008,8,0,10,4,10,102,1,8,29,2,9,4,10,1006,0,10,1,1103,17,10,3,8,102,-1,8,10,101,1,10,10,4,10,108,0,8,10,4,10,101,0,8,61,1006,0,21,1006,0,51,3,8,1002,8,-1,10,101,1,10,10,4,10,108,1,8,10,4,10,1001,8,0,89,1,102,19,10,1,1107,17,10,1006,0,18,3,8,1002,8,-1,10,1001,10,1,10,4,10,1008,8,1,10,4,10,1001,8,0,123,1,9,2,10,2,1105,10,10,2,103,9,10,2,1105,15,10,3,8,102,-1,8,10,1001,10,1,10,4,10,1008,8,0,10,4,10,102,1,8,161,3,8,102,-1,8,10,101,1,10,10,4,10,108,1,8,10,4,10,101,0,8,182,3,8,1002,8,-1,10,101,1,10,10,4,10,1008,8,0,10,4,10,101,0,8,205,2,1102,6,10,1006,0,38,2,1007,20,10,2,1105,17,10,3,8,102,-1,8,10,1001,10,1,10,4,10,108,1,8,10,4,10,1001,8,0,241,3,8,102,-1,8,10,101,1,10,10,4,10,108,1,8,10,4,10,101,0,8,263,1006,0,93,2,5,2,10,2,6,7,10,3,8,102,-1,8,10,101,1,10,10,4,10,108,0,8,10,4,10,1001,8,0,296,1006,0,81,1006,0,68,1006,0,76,2,4,4,10,101,1,9,9,1007,9,1010,10,1005,10,15,99,109,652,104,0,104,1,21102,825594262284,1,1,21102,347,1,0,1105,1,451,21101,0,932855939852,1,21101,358,0,0,1106,0,451,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,21102,1,235152649255,1,21101,405,0,0,1105,1,451,21102,235350879235,1,1,21102,416,1,0,1106,0,451,3,10,104,0,104,0,3,10,104,0,104,0,21102,988757512972,1,1,21101,439,0,0,1106,0,451,21102,1,988669698828,1,21101,0,450,0,1106,0,451,99,109,2,22101,0,-1,1,21102,40,1,2,21102,1,482,3,21102,472,1,0,1106,0,515,109,-2,2105,1,0,0,1,0,0,1,109,2,3,10,204,-1,1001,477,478,493,4,0,1001,477,1,477,108,4,477,10,1006,10,509,1101,0,0,477,109,-2,2106,0,0,0,109,4,1202,-1,1,514,1207,-3,0,10,1006,10,532,21102,1,0,-3,21202,-3,1,1,21202,-2,1,2,21102,1,1,3,21102,1,551,0,1106,0,556,109,-4,2105,1,0,109,5,1207,-3,1,10,1006,10,579,2207,-4,-2,10,1006,10,579,22101,0,-4,-4,1105,1,647,21201,-4,0,1,21201,-3,-1,2,21202,-2,2,3,21102,598,1,0,1105,1,556,21202,1,1,-4,21101,0,1,-1,2207,-4,-2,10,1006,10,617,21102,1,0,-1,22202,-2,-1,-2,2107,0,-3,10,1006,10,639,21202,-1,1,1,21102,1,639,0,105,1,514,21202,-2,-1,-2,22201,-4,-2,-4,109,-5,2105,1,0])

(def black 0)
(def white 1)

(def turn-left -1)
(def turn-right 1)

(def up [0 -1])
(def right [1 0])
(def down [0 1])
(def left [-1 0])

(def directions [up right down left])

(defn turn [current-dir turn-dir]
  (case turn-dir
    0 (mod (+ current-dir turn-left) 4)
    1 (mod (+ current-dir turn-right) 4)))

(defn +pos [[x1 y1] [x2 y2]]
  [(+ x1 x2) (+ y1 y2)])

(defn forward [current-dir pos]
  (+pos pos (nth directions current-dir)))

(defn ensure-size [ints addr]
  (if (contains? ints addr)
    ints
    (vec (take (inc addr) (concat ints (repeat 0))))))

(defn log [& msg]
  #_(apply println msg))

(defn read-at [state addr]
  (let [{:keys [ints]} state
        ints (ensure-size ints addr)]
    (nth ints addr)))

(defn read-next [state]
  [(read-at state (:pos state))
   (update state :pos inc)])

(defn rel-addr [state addr]
  (+ (:rel-base state) addr))

(defn write [state addr val mode]
  (let [{:keys [ints]} state
        addr (case mode
               0 addr
               2 (rel-addr state addr))
        ints (ensure-size ints addr)]
    (log "  write" val "to addr" addr "mode" mode)
    (assoc state :ints (assoc ints addr val))))

(defn position-val [state addr]
  (read-at state addr))

(defn immediate-val [state addr]
  addr)

(defn relative-val [state addr]
  (read-at state (rel-addr state addr)))

(defn read-input [state]
  (let [[x & xs] (:inputs state)]
    [x (assoc state :inputs xs)]))

(defn write-input [state input]
  (update state :inputs (fnil conj []) input))

(defn read-output [state]
  (let [[x & xs] (:outputs state)]
    [x (assoc state :outputs xs)]))

(defn val-at [state addr mode]
  (case mode
    0 (position-val state addr)
    1 (immediate-val state addr)
    2 (relative-val state addr)))

(defn add [state modes]
  (let [[p1 state] (read-next state)
        [p2 state] (read-next state)
        [p3 state] (read-next state)
        [mode1 mode2 mode3] modes
        p1-val (val-at state p1 mode1)
        p2-val (val-at state p2 mode2)]
    (log "  add" {:p1 p1
                  :mode1 mode1
                  :p1-val p1-val
                  :p2 p2
                  :p2-val p2-val})
    (write state p3 (+ p1-val p2-val) mode3)))

(defn mult [state modes]
  (let [[p1 state] (read-next state)
        [p2 state] (read-next state)
        [p3 state] (read-next state)
        [mode1 mode2 mode3] modes
        p1-val (val-at state p1 mode1)
        p2-val (val-at state p2 mode2)]
    (log "  mult" {:p1 p1
                   :mode1 mode1
                   :p1-val p1-val
                   :p2 p2
                   :mode2 mode2
                   :p2-val p2-val})
    (write state p3 (* p1-val p2-val) mode3)))

(defn input [state modes]
  (let [[p1 state] (read-next state)
        [mode1] modes
        [i state] (read-input state)]
    (log "  input" {:p1 p1
                    :mode1 mode1
                    :i i})
    (write state p1 i mode1)))

(defn output [state modes]
  (let [[p1 state] (read-next state)
        [mode1] modes
        p1-val (val-at state p1 mode1)]
    (log "  output" {:p1 p1
                     :mode1 mode1
                     :p1-val p1-val})
    (update state :outputs (fnil conj []) p1-val)))

(defn digits [n]
  (map (fn [n] (Character/digit n 10)) (str n)))

(defn jump-to [state pos]
  (assoc state :pos pos))

(defn jump-if-true [state modes]
  (let [[p1 state] (read-next state)
        [p2 state] (read-next state)
        [mode1 mode2] modes
        p1-val (val-at state p1 mode1)
        p2-val (val-at state p2 mode2)]
    (log "  jump-if-true" {:p1 p1
                           :mode1 mode1
                           :p1-val p1-val
                           :p2 p2
                           :mode2 mode2
                           :p2-val p2-val})
    (if-not (zero? p1-val)
      (jump-to state p2-val)
      state)))

(defn jump-if-false [state modes]
  (let [[p1 state] (read-next state)
        [p2 state] (read-next state)
        [mode1 mode2] modes
        p1-val (val-at state p1 mode1)
        p2-val (val-at state p2 mode2)]
    (log "  jump-if-fals" {:p1 p1
                           :mode1 mode1
                           :p1-val p1-val
                           :p2 p2
                           :mode2 mode2
                           :p2-val p2-val})
    (if (zero? p1-val)
      (jump-to state p2-val)
      state)))

(defn less-than [state modes]
  (let [[p1 state] (read-next state)
        [p2 state] (read-next state)
        [p3 state] (read-next state)
        [mode1 mode2 mode3] modes
        p1-val (val-at state p1 mode1)
        p2-val (val-at state p2 mode2)]
    (log "  less-than" {:p1 p1
                        :mode1 mode1
                        :p1-val p1-val
                        :p2 p2
                        :mode2 mode2
                        :p2-val p2-val})
    (if (< p1-val p2-val)
      (write state p3 1 mode3)
      (write state p3 0 mode3))))

(defn equals [state modes]
  (let [[p1 state] (read-next state)
        [p2 state] (read-next state)
        [p3 state] (read-next state)
        [mode1 mode2 mode3] modes
        p1-val (val-at state p1 mode1)
        p2-val (val-at state p2 mode2)]
    (log "  equals" {:p1 p1
                     :mode1 mode1
                     :p1-val p1-val
                     :p2 p2
                     :mode2 mode2
                     :p2-val p2-val})
    (if (= p1-val p2-val)
      (write state p3 1 mode3)
      (write state p3 0 mode3))))

(defn adjust-rel-base [state modes]
  (let [[p1 state] (read-next state)
        [mode1] modes
        p1-val (val-at state p1 mode1)]
    (log "  adjust-rel-base" {:p1 p1
                              :mode1 mode1
                              :p1-val p1-val})
    (update state :rel-base + p1-val)))

(defn halt [state]
  (log "  halt")
  (assoc state :halted true))

(defn step [state]
  (let [[opcode state] (read-next state)
        [opcode2 opcode1 mode1 mode2 mode3] (reverse (digits opcode))
        opcode2 (or opcode2 0)
        opcode1 (or opcode1 0)
        mode1 (or mode1 0)
        mode2 (or mode2 0)
        mode3 (or mode3 0)
        modes [mode1 mode2 mode3]]
    (log "opcode" opcode1 opcode2 "modes" modes)
    (case [opcode1 opcode2]
      [0 1] (add state modes)
      [0 2] (mult state modes)
      [0 3] (input state modes)
      [0 4] (output state modes)
      [0 5] (jump-if-true state modes)
      [0 6] (jump-if-false state modes)
      [0 7] (less-than state modes)
      [0 8] (equals state modes)
      [0 9] (adjust-rel-base state modes)
      [9 9] (halt state))))

(defn init [id code]
  {:id id
   :pos 0
   :rel-base 0
   :inputs []
   :outputs []
   :ints code})

(defn exec [state]
  (if (or (:halted state) (seq (:outputs state)))
    state
    (recur (step state))))

(defn exec-all [state]
  (if (:halted state)
    state
    (recur (step state))))

(defn paint-one [state]
  (let [{:keys [prog pos dir panels]} state
        panel-color (get panels pos black)
        prog (-> prog
                 (write-input panel-color)
                 exec)
        [color prog] (read-output prog)
        panels (if color
                 (assoc panels pos color)
                 panels)
        prog (-> prog exec)
        [turn-dir prog] (read-output prog)
        new-dir (if turn-dir
                  (turn dir turn-dir)
                  dir)
        new-pos (if turn-dir
                  (forward new-dir pos)
                  pos)]
    (if (and color turn-dir)
      (println (if (zero? panel-color) "black" "white")
               "panel" pos "painted" (if (zero? color) "black" "white")
               "turning to" (if (zero? turn-dir) "left" "right")
               "pointing to" (case new-dir
                               0 "^"
                               1 ">"
                               2 "v"
                               3 "<")
               "at" new-pos)
      (println "halting at"
               (if (zero? panel-color) "black" "white")
               "panel" new-pos "pointing to" (case new-dir
                                               0 "^"
                                               1 ">"
                                               2 "v"
                                               3 "<")))
    (-> state
        (assoc :panels panels)
        (update :route conj [pos color])
        (assoc :prog prog)
        (assoc :dir new-dir)
        (assoc :pos new-pos))))

(defn paint-all [state]
  (if (-> state :prog :halted)
    state
    (recur (paint-one state))))

(comment
  (def robot {:x 0
              :y 0
              :dir :up})

  (def painted-panels {[0 0] 0})

  (->> (paint-all {:pos [0 0]
                   :dir 0
                   :panels {}
                   :route []
                   :prog (init "prog" puzzle-input)})
       :panels)

  (def panels
    (->> (paint-all {:pos [0 0]
                     :dir 0
                     :panels {[0 0] white}
                     :route []
                     :prog (init "start on white" puzzle-input)})
         :panels))

  (def x-bounds (->> panels
                     keys
                     (map first)
                     (apply max)))

  (def y-bounds (->> panels
                     keys
                     (map second)
                     (apply max)))

  (map #(apply str %)
   (for [y (range 10)]
     (for [x (range 50)]
       (let [color (get panels [x y] 0)]
         (if (zero? color) " " "#")))))

  )
