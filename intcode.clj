(ns Projects.own.advent-of-code-2019.intcode)

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
